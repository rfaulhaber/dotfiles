{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  inherit (lib.my) mkOptDesc;
  cfg = config.modules.hardware.sensors;
in {
  options.modules.hardware.sensors = {
    enable = mkEnableOption false;

    superIO = {
      enable = mkEnableOption false;

      module = mkOptDesc types.str "nct6775" ''
        hwmon driver for the board's Super I/O chip. Both ASUS X570 and
        ASRock B550 boards in the fleet carry Nuvoton parts, which nct6775
        covers; ITE-based boards would need it87 instead.
      '';
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {environment.systemPackages = [pkgs.lm_sensors];}

    (mkIf cfg.superIO.enable {
      boot.kernelModules = [cfg.superIO.module];
      # Fan tachometers live behind the Super I/O chip, whose port range ASUS
      # firmware also claims in its ACPI tables — the hwmon driver refuses to
      # bind until the conflict check is relaxed. Temperatures are unaffected:
      # asus_ec_sensors reaches the EC over ACPI proper and binds regardless.
      boot.kernelParams = ["acpi_enforce_resources=lax"];
    })
  ]);
}
