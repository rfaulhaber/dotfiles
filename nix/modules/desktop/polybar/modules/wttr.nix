{ config, colors, pkgs }: {
  type = "custom/script";
  exec =
    "${config.dotfiles.binDir}/polybar/wttr ${config.userInfo.location.city}";
  interval = "900";
  # TODO avoid hard code, refactor
  click-left = ''
    "${pkgs.firefox-devedition-bin}/bin/firefox-devedition https://openweathermap.org/city/5150529" &'';
}
