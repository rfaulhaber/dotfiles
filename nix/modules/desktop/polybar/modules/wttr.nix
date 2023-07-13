{
  config,
  colors,
  pkgs,
}: {
  type = "custom/script";
  exec = let
    latitude = toString config.userInfo.location.latitude;
    longitude = toString config.userInfo.location.longitude;
  in "${pkgs.nushell}/bin/nu ${config.dotfiles.binDir}/polybar/wttr.nu --latitude ${latitude} --longitude ${longitude}";
  interval = "3600";
  # TODO avoid hard code, refactor
  click-left = "${pkgs.firefox-devedition-bin}/bin/firefox-devedition https://openweathermap.org/city/5150529 >/dev/null 2>&1 &";
}
