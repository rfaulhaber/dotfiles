# since I use home-manager for SSH configuration, I couldn't use the standard programs.ssh.sshAgent module.
# this is taken from:
# https://github.com/NixOS/nixpkgs/blob/nixos-23.05/nixos/modules/programs/ssh.nix
# NOTE: should this be part of the ssh client module?
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.systemd.modules.sshAgent;
in {
  options.modules.services.systemd.modules.sshAgent = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    systemd.user.services.sshAgent = {
      description = "SSH Agent";
      wantedBy = ["default.target"];
      unitConfig.ConditionUser = "!@system";
      serviceConfig = {
        ExecStartPre = "${pkgs.coreutils}/bin/rm -f %t/ssh-agent";
        ExecStart = "${pkgs.openssh}/bin/ssh-agent -a %t/ssh-agent";
        StandardOutput = "null";
        Type = "forking";
        Restart = "on-failure";
        SuccessExitStatus = "0 2";
      };
      # Allow ssh-agent to ask for confirmation. This requires the
      # unit to know about the user's $DISPLAY (via ‘systemctl
      # import-environment’).
      # environment.SSH_ASKPASS = optionalString cfg.enableAskPassword askPasswordWrapper;
      environment.DISPLAY = "fake"; # required to make ssh-agent start $SSH_ASKPASS
    };
  };
}
