{ config, colors, pkgs }: {
  type = "custom/script";
  exec =
    "${config.dotfiles.binDir}/polybar/wttr ${config.userInfo.location.city}";
  interval = "900";
}
