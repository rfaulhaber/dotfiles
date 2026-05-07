{lib, ...}: let
  inherit (lib.importJSON ./ips.json) pangolin netbird;
  pangolinIp = pangolin;
  netbirdIp = netbird;
in {
  modules.linux.oci = {
    enable = true;

    networks = {
      pangolin.enable = true;
      netbird.enable = true;
    };

    services =
      lib.importJSON ./oci-images.json
      |> lib.recursiveUpdate {
        pangolin = {
          enable = true;
          domain = "3679.space";
          dashboardDomain = "pangolin.3679.space";
          bindAddress = pangolinIp;
          baseDir = "/docker/config";
          adminEmail = "ryf@sent.as";
          email = {
            smtpHost = "smtp.fastmail.com";
            smtpPort = 465;
            smtpUser = "ryf@sent.as";
            noReply = "no-reply@3679.space";
          };
          openFirewall = true;
        };

        netbird = {
          enable = true;
          domain = "netbird.3679.space";
          authDomain = "auth.3679.space";
          bindAddress = netbirdIp;
          baseDir = "/docker/config/netbird";
          acmeEmail = "ryf@sent.as";
          openFirewall = true;
        };

        pocket-id = {
          enable = true;
          appUrl = "https://auth.3679.space";
          bindAddress = pangolinIp;
          baseDir = "/docker/config/pocket-id";
          networks = ["pangolin"];
        };
      };
  };
}
