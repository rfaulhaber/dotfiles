{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf mkOption mkEnableOption types;
  cfg = config.modules.darwin.airvpn;

  wg = "${pkgs.wireguard-tools}/bin/wg";
  wg-quick = "${pkgs.wireguard-tools}/bin/wg-quick";

  airvpn-cli = lib.my.writeNushellScriptBin pkgs "airvpn" ''
    const config = "${cfg.configFile}"

    def main [] {
      print "usage: airvpn <up|down|status>"
    }

    def "main up" [] {
      sudo ${wg-quick} up $config
    }

    def "main down" [] {
      sudo ${wg-quick} down $config
    }

    def "main status" [] {
      let interfaces = (sudo ${wg} show interfaces | str trim)
      if ($interfaces | is-empty) {
        print "airvpn: down"
      } else {
        sudo ${wg} show
      }
    }
  '';
in {
  options.modules.darwin.airvpn = {
    enable = mkEnableOption "on-demand AirVPN WireGuard tunnel";

    configFile = mkOption {
      type = types.str;
      default = "/run/secrets/airvpn.conf";
      description = ''
        Path to a WireGuard config from AirVPN's Config Generator. The file
        embeds the device private key, so it should be a sops secret. wg-quick
        derives the interface name from the basename and requires a `.conf`
        suffix, hence the secret is named `airvpn.conf`.
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = lib.hasSuffix ".conf" cfg.configFile;
        message = "modules.darwin.airvpn.configFile must end in .conf for wg-quick to accept it";
      }
    ];

    # wireguard-tools is on PATH for debugging (`wg show`, etc.); the airvpn
    # CLI itself uses absolute store paths. On darwin wg-quick runs
    # wireguard-go under the hood, creating a userspace utun device.
    user.packages = [
      pkgs.wireguard-tools
      airvpn-cli
    ];
  };
}
