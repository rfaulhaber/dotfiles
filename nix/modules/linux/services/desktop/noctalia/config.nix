{
  homePath,
  font,
}: {
  # Bar-only deployment. mako, fuzzel, swaylock and awww keep their roles, so
  # every noctalia subsystem that would contend for them is off. The launcher
  # has no toggle — it stays inert as long as nothing binds or summons it.
  notification.enable_daemon = false;
  lockscreen.enabled = false;

  wallpaper = {
    enabled = false;
    automation.enabled = false;
  };

  idle.behavior = {
    lock.enabled = false;
    screen-off.enabled = false;
  };

  weather.enabled = false;
  calendar.enabled = false;
  location.auto_locate = false;

  shell = {
    # Defaults to sans-serif, which is not guaranteed to carry the Nerd Font
    # glyphs the widget formats use.
    font_family = font;
    launcher.fetch_exchange_rates = false;
  };

  theme = {
    mode = "dark";
    source = "builtin";
    builtin = "Tokyo-Night";
    templates = {
      # Templates render theme files into *other* applications' config dirs,
      # which this repo already owns declaratively via base16.
      enable_builtin_templates = false;
      enable_community_templates = false;
    };
  };

  # Poll intervals carried over from the waybar module rather than noctalia's
  # slower defaults.
  system.monitor = {
    enabled = true;
    cpu_poll_seconds = 1.0;
    memory_poll_seconds = 1.0;
    disk_poll_seconds = 30.0;
  };

  bar.main = {
    position = "top";
    thickness = 45;
    reserve_space = true;

    # Widget labels render at Style::fontSizeBody (14px) times this; waybar drew
    # the bar at 20px.
    scale = 1.5;

    # noctalia floats the bar as an inset rounded pill by default; waybar drew a
    # plain full-width strip, which is what the rest of this desktop expects.
    margin_ends = 0;
    margin_edge = 0;
    radius = 0;
    shadow = false;

    start = ["workspaces" "taskbar" "active_window"];
    center = [];
    end = ["tray" "cpu_usage" "ram_pct" "ram_used" "disk_home" "disk_nix" "clock"];
  };

  widget = {
    # waybar folded per-window icons into its workspace buttons; noctalia splits
    # those into two widgets, since its workspaces widget renders at most one
    # icon and only in focus_hint style.
    taskbar = {
      group_by_workspace = true;
      workspace_group_content = "icons";
    };

    cpu_usage = {
      type = "sysmon";
      stat = "cpu_usage";
    };

    # One waybar module showed percentage and absolute together; sysmon reports
    # a single stat per instance, so the pair is split.
    ram_pct = {
      type = "sysmon";
      stat = "ram_pct";
    };

    ram_used = {
      type = "sysmon";
      stat = "ram_used";
    };

    # `path` only expands a leading `~`, never $VAR, so pass an absolute path.
    disk_home = {
      type = "sysmon";
      stat = "disk_used";
      path = homePath;
    };

    disk_nix = {
      type = "sysmon";
      stat = "disk_used";
      path = "/nix";
    };

    clock = {
      format = "󰥔 {:%a, %b %d %Y %I:%M:%S %p}";
      # No calendar token exists for the bar clock; noctalia puts the calendar
      # in the control center panel instead.
      tooltip_format = "{:%A, %B %d, %Y}";
    };
  };
}
