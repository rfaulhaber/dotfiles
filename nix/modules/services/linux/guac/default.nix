{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.guac;
in {
  options.modules.services.guac = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    services = {
      guacamole-server = {
        enable = true;
      };
      guacamole-client = {
        enable = true;
      };
    };

    environment.etc."guacamole/user_config.xml" = {
      text = ''
        <?xml version="1.0" encoding="UTF-8"?>
        <user-mapping>
            <!-- User using SHA-256 to hash the password -->
            <authorize
                username="ryan"
                password="sha256 hash"
                encoding="sha256">

                <connection name="NixOS Server SSH">
                    <protocol>ssh</protocol>
                    <param name="hostname">127.0.0.1</param>
                    <param name="port">14625</param>
                </connection>
            </authorize>
        </user-mapping>
      '';
    };
  };
}
