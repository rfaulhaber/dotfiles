# since I use home-manager for SSH configuration, I couldn't use the standard programs.ssh.sshAgent module.
# this is taken from:
# https://github.com/NixOS/nixpkgs/blob/nixos-23.05/nixos/modules/programs/ssh.nix
{
  lib,
  pkgs,
  ...
}:
with lib; {
  name = "sshAgent";
  user.service = {
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
}
