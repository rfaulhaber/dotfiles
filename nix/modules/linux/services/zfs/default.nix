{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.zfs;
in {
  options.modules.services.zfs = {
    enable = mkEnableOption false;
    datasets = mkOption {
      description = "Declarative ZFS datasets.";
      default = {};
      type = types.attrsOf (types.submodule {
        options = {
          type = mkOption {
            type = types.enum ["filesystem" "volume"];
            description = "Type of ZFS dataset to create";
            default = "filesystem";
          };
          properties = mkOption {
            description = "ZFS dataset options.";
            type = types.attrsOf types.str;
            default = {};
            example = {
              "mountpoint" = "/mnt/dataset";
              "encryption" = "on";
            };
          };
        };
      });
    };
    # NOTE in order for this to work properly, see the following:
    # - https://wiki.archlinux.org/title/ZFS (section 6.1.2)
    # - https://www.reddit.com/r/NixOS/comments/tzksw4/comment/i4dw7f8
    encryptedHome = mkOption {
      description = "Home dataset to be decrypted on boot.";
      type = types.nullOr types.str;
      default = null;
    };
    encryptedDatasets = mkOption {
      description = ''
        Non-root encrypted ZFS datasets unlocked in stage 2 by a userspace
        systemd unit, ordered after sops-install-secrets so the keyfile is
        guaranteed to exist before `zfs load-key` runs. Each entry generates
        `zfs-load-key-<name>.service`. Datasets configured here should also
        carry `canmount=noauto` so the early `zfs-mount.service` skips them.
      '';
      default = {};
      type = types.attrsOf (types.submodule (_: {
        options = {
          dataset = mkOption {
            description = "Full ZFS dataset name (e.g. data/apps/immich/files).";
            type = types.str;
          };
          keyFile = mkOption {
            description = ''
              Path on the running system where the raw key material can be read.
              Typically `config.sops.secrets."<svc>/zfs-key".path`.
            '';
            type = types.str;
          };
          consumers = mkOption {
            description = ''
              systemd units that depend on this dataset being unlocked and
              mounted. Each gets `Requires=` and `After=` the generated
              `zfs-load-key-<name>.service` so they wait for the unlock
              and fail closed if it fails.
            '';
            type = types.listOf types.str;
            default = [];
            example = ["podman-immich_server.service"];
          };
        };
      }));
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.boot.kernelPackages.${pkgs.zfs.kernelModuleAttribute}.meta.broken == false;
        message = "ZFS is marked as broken in this kernel package. Please address.";
      }
    ];

    services.zfs = {
      autoScrub.enable = true;
      trim.enable = true;
      autoSnapshot.enable = true;
    };

    # TODO throw in some assertions to check this?
    security.pam.zfs = mkIf (cfg.encryptedHome != null) {
      enable = true;
      homes = cfg.encryptedHome;
    };

    # Datasets in `encryptedDatasets` get a dedicated zfs-load-key-<name> unit
    # below. Leaving upstream's blanket sweep on would load the same keys a
    # second time during pool import, and a single failure there fails
    # zfs-import-<pool>.service and every mount that requires it. Note this
    # also disables the initrd root-pool passphrase prompt; a host needing
    # both must use upstream's list form instead of this boolean.
    boot.zfs.requestEncryptionCredentials = cfg.encryptedHome == null && cfg.encryptedDatasets == {};

    systemd.services = mkMerge [
      (mkIf (cfg.datasets != {}) {
        zfs-manage-datasets = {
          description = "Create ZFS datasets.";
          wantedBy = ["multi-user.target"];
          after = ["zfs-import.target" "zfs-mount.service"];
          path = with pkgs; [nushell zfs];

          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = let
              zfsManageScript =
                builtins.readFile "${config.dotfiles.binDir}/zfs-manage.nu"
                |> lib.my.writeNushellScriptBin pkgs "zfs-manage";
              # Pass the dataset spec via a file instead of argv: nushell's
              # shebang-script argv parser treats `{...}` as a record literal and
              # re-serializes it, double-encoding the JSON before main() sees it.
              datasetsFile = pkgs.writeText "zfs-datasets.json" (builtins.toJSON cfg.datasets);
            in "${zfsManageScript}/bin/zfs-manage --file ${datasetsFile}";
          };
        };
      })
      # One unlock unit per encrypted dataset. Ordering rationale:
      #   - After=sops-install-secrets.service guarantees the keyfile exists.
      #   - After=zfs-import.target guarantees the dataset is importable.
      #   - After=zfs-manage-datasets.service guarantees the dataset itself
      #     has been created. The earlier version of this module ordered
      #     Before=zfs-manage on the theory that mutable property tweaks
      #     needed an unlocked dataset, but that's wrong — mutable props
      #     (recordsize, mountpoint, keylocation) accept zfs set against
      #     a locked dataset just fine. Running After lets zfs-manage do
      #     the create-with-encryption-and-keylocation atomic step, after
      #     which this unit's idempotent guards turn into mount-only.
      # Consumers (e.g. podman containers using this dataset's mountpoint)
      # should add `After`/`Requires` on this unit themselves.
      (mkMerge (mapAttrsToList (name: ds: {
          "zfs-load-key-${name}" = {
            description = "Load ZFS encryption key for ${ds.dataset}";
            wantedBy = ["multi-user.target"];
            # sops-nix renders secrets via an activation script, not a
            # systemd unit — there's no `sops-install-secrets.service` to
            # depend on. By the time `zfs-import.target` is reached, the
            # activation script has already populated /run/secrets/, so
            # the keyfile this unit reads is guaranteed to exist.
            after = ["zfs-import.target" "zfs-manage-datasets.service"];
            # `before = consumers` ensures consumers wait; `requiredBy = consumers`
            # adds the hard dependency so a failed unlock fails the consumer
            # cleanly rather than letting it start with a missing bind-mount source.
            before = ds.consumers;
            requiredBy = ds.consumers;
            path = [pkgs.zfs];
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
            };
            script = ''
              set -euo pipefail
              if [ "$(zfs get -H -o value keystatus ${ds.dataset})" != "available" ]; then
                zfs load-key -L "file://${ds.keyFile}" ${ds.dataset}
              fi
              if [ "$(zfs get -H -o value mounted ${ds.dataset})" != "yes" ]; then
                zfs mount ${ds.dataset}
              fi
            '';
          };
        })
        cfg.encryptedDatasets))
    ];
  };
}
