{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.virt;
in {
  options.modules.services.virt = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [qemu_kvm qemu qemu-utils];
  };
}
