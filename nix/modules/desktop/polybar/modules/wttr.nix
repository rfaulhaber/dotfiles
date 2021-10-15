{ config, colors }: {
  type = "custom/script";
  exec = "${config.dotfiles.binDir}/polybar/wttr";
  interval = "900";
}
