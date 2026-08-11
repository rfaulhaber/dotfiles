{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.registryAuth;
in {
  options.modules.linux.oci.registryAuth = {
    enable = mkEnableOption "declarative registry credentials for image pulls";

    registries = mkOption {
      description = ''
        Registry host (exactly as written in image references, including
        any port) to credential mapping. Each secret is a sops key whose
        value is base64("<user>:<token>"): auth.json's `auth` field takes
        only that form, and sops templates substitute literally, so the
        encoding happens when the secret is created:

          $"<user>:(input -s 'token: ')" | encode base64

        Credentials are looked up by literal host string — the same
        server reached under two names (host-published port vs public
        domain) needs an entry per name, pointing at one shared secret.
      '';
      type = types.attrsOf (types.submodule {
        options.secret = mkOption {
          description = "sops secret holding the base64 user:token pair.";
          type = types.str;
        };
      });
      default = {};
    };

    authFile = mkOption {
      description = ''
        Where the rendered auth.json lands. The default is root's XDG
        fallback path, so interactive podman/skopeo on the host find the
        credentials with no flags.
      '';
      type = types.str;
      default = "/root/.config/containers/auth.json";
    };
  };

  config = mkIf cfg.enable {
    sops.secrets = mapAttrs' (_: r: nameValuePair r.secret {}) cfg.registries;

    sops.templates."oci-registry-auth.json" = {
      content = builtins.toJSON {
        auths =
          mapAttrs (_: r: {auth = config.sops.placeholder.${r.secret};})
          cfg.registries;
      };
      path = cfg.authFile;
    };

    # Container units get the explicit env var instead of relying on
    # auth-path discovery: REGISTRY_AUTH_FILE outranks the runtime
    # auth.json a manual `podman login` writes, so unit pulls can't be
    # shadowed by ad-hoc login state — and root units run without $HOME,
    # making the XDG fallback a passwd-lookup gamble. Mapping over the
    # declared containers covers every service, whether or not its module
    # uses mkServiceConfig.
    systemd.services =
      mapAttrs' (
        name: _:
          nameValuePair "podman-${name}" {
            environment.REGISTRY_AUTH_FILE = cfg.authFile;
          }
      )
      config.virtualisation.oci-containers.containers;
  };
}
