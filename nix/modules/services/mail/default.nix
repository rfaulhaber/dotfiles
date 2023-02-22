{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.mail;
in {
  options.modules.services.mail = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    home = {
      programs.mbsync = {enable = true;};

      accounts.email = {
        maildirBasePath = "mail";
        # my primary fastmail account. not very modular
        accounts.fastmail = {
          address = config.userInfo.primaryEmail;
          userName = config.userInfo.primaryEmail;
          primary = true;
          realName = config.userInfo.fullName;
          signature = {
            text = ''
              Ryan Faulhaber
              https://keybase.io/rfaulhaber
            '';
            showSignature = "append";
          };
          gpg = {
            key = config.userInfo.primaryGPGKey;
            signByDefault = true;
          };
          passwordCommand = "pass mail/fastmail";
          imap.host = "imap.fastmail.com";
          smtp.host = "smtp.fastmail.com";
          mbsync = {
            enable = true;
            create = "maildir";
          };
        };
      };
    };
  };
}
