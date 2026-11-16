# Catalog of Wolf session apps, referenced by attribute name from
# profiles.<id>.includeApps. Entries are partial records in the shape
# Wolf's profile API uses, based on the upstream definitions in
# games-on-whales/gow, apps/<name>/assets/wolf.config.toml. The API
# requires every App field and has no server-side defaults, so the
# booleans below must be spelled out; the fields nix can't know — resolved
# gstreamer pipelines, render_node — are completed from a live app record
# by wolf-profiles-sync at apply time and must NOT be vendored here. Like
# all session images these are floating tags pulled by Wolf at runtime —
# not pinned via oci-images.json, kept resident by keepSessionImages.
{
  # ES-DE reads its ROM library from /ROMs (ROMDirectory in the image's
  # es_settings.xml) and RetroArch its BIOS files from ~/bioses, so a ROM
  # mount targets /ROMs rather than a subpath of the session home.
  es-de = {
    id = "es-de";
    title = "EmulationStation";
    icon_png_path = "https://games-on-whales.github.io/wildlife/apps/es-de/assets/icon.png";
    support_hdr = false;
    start_virtual_compositor = true;
    start_audio_server = true;
    runner = {
      type = "docker";
      name = "WolfES-DE";
      image = "ghcr.io/games-on-whales/es-de:edge";
      env = [
        "RUN_SWAY=1"
        "MOZ_ENABLE_WAYLAND=1"
        "GOW_REQUIRED_DEVICES=/dev/input/* /dev/dri/* /dev/nvidia*"
      ];
      devices = [];
      mounts = [];
      ports = [];
      base_create_json = builtins.toJSON {
        HostConfig = {
          IpcMode = "host";
          Privileged = false;
          CapAdd = ["NET_RAW" "MKNOD" "NET_ADMIN"];
          DeviceCgroupRules = ["c 13:* rmw" "c 244:* rmw"];
        };
      };
    };
  };

  heroic = {
    id = "heroic";
    title = "Heroic";
    icon_png_path = "https://games-on-whales.github.io/wildlife/apps/heroic-games-launcher/assets/icon.png";
    support_hdr = false;
    start_virtual_compositor = true;
    start_audio_server = true;
    runner = {
      type = "docker";
      name = "WolfHeroic";
      image = "ghcr.io/games-on-whales/heroic-games-launcher:edge";
      env = [
        "RUN_SWAY=true"
        # Upstream entry verbatim; the base image skips required-device
        # globs that match nothing, so /dev/nvidia* is harmless on
        # non-NVIDIA hosts.
        "GOW_REQUIRED_DEVICES=/dev/input/* /dev/dri/* /dev/nvidia*"
      ];
      devices = [];
      mounts = [];
      ports = [];
      base_create_json = builtins.toJSON {
        HostConfig = {
          IpcMode = "host";
          CapAdd = ["SYS_ADMIN" "SYS_NICE" "SYS_PTRACE" "NET_RAW" "MKNOD" "NET_ADMIN"];
          SecurityOpt = ["seccomp=unconfined" "apparmor=unconfined"];
          Ulimits = [
            {
              Name = "nofile";
              Hard = 10240;
              Soft = 10240;
            }
          ];
          Privileged = false;
          DeviceCgroupRules = ["c 13:* rmw" "c 244:* rmw"];
        };
      };
    };
  };
}
