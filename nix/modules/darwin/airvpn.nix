{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf mkOption mkEnableOption removeSuffix types;
  cfg = config.modules.darwin.airvpn;

  wg = "${pkgs.wireguard-tools}/bin/wg";
  wg-quick = "${pkgs.wireguard-tools}/bin/wg-quick";

  # wg-quick records the utun it was assigned here and deletes it on down.
  nameFile = "/var/run/wireguard/${removeSuffix ".conf" (baseNameOf cfg.configFile)}.name";

  airvpn-cli = lib.my.writeNushellScriptBin pkgs "airvpn" ''
    const config = "${cfg.configFile}"

    def main [] {
      print "usage: airvpn <up|down|status>"
    }

    def "main up" [] {
      sudo ${wg-quick} up $config
    }

    def "main down" [] {
      let iface = (do { sudo cat ${nameFile} } | complete | get stdout | str trim)
      sudo ${wg-quick} down $config
      if ($iface | is-not-empty) {
        rebind-netbird $iface
      }
    }

    def "main status" [] {
      let interfaces = (sudo ${wg} show interfaces | str trim)
      if ($interfaces | is-empty) {
        print "airvpn: down"
      } else {
        sudo ${wg} show
      }
    }

    # Polls `done` until it returns true; false once `timeout` has elapsed.
    def wait-for [timeout: duration, done: closure]: nothing -> bool {
      let deadline = (date now) + $timeout
      while not (do $done) {
        if (date now) > $deadline { return false }
        sleep 200ms
      }
      true
    }

    # NetBird pins its sockets (IP_BOUND_IF) to whichever interface 0.0.0.0
    # routes through when its engine starts, and wg-quick's /1 routes win that
    # lookup. An engine start while the tunnel was up (every wake from sleep is
    # one) leaves NetBird pinned to the utun that just went away, and its
    # network monitor ignores /1 routes, so restart it once teardown is done.
    def rebind-netbird [iface: string] {
      if (which netbird | is-empty) { return }
      let status = (do { netbird status --json } | complete)
      if $status.exit_code != 0 { return }
      if ($status.stdout | from json | get daemonStatus?) not-in [Connected Connecting] { return }

      # wg-quick down only signals wireguard-go; the utun disappears after.
      if not (wait-for 10sec { (do { /sbin/ifconfig $iface } | complete).exit_code != 0 }) {
        error make {msg: $"($iface) still exists; restarting netbird now would pin it there again"}
      }
      # wg-quick's background monitor restores the saved DNS servers only once
      # it sees the utun gone. The bracket keeps the pattern from matching the
      # command line that carries it.
      let monitor = $"[w]g-quick.* up ($config)"
      if not (wait-for 10sec { (do { sudo /usr/bin/pgrep -f $monitor } | complete).exit_code != 0 }) {
        print -e "airvpn: wg-quick monitor still running; netbird may snapshot the tunnel's DNS servers"
      }

      print "airvpn: restarting netbird to re-bind it to the physical uplink"
      netbird down
      netbird up
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
