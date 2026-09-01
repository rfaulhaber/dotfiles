{
  homePath,
  font,
  networkInterface,
}: let
  # Left-click already opens noctalia's summary panel on every sysmon widget;
  # right-click goes to the tool that can answer the follow-up question.
  sysmon = args:
    {
      type = "sysmon";
      actions.right = "exec ghostty --command='btop'";
    }
    // args;
in {
  # swaylock and awww keep their roles, so every noctalia subsystem that would
  # contend for them is off. The launcher, notifications, and clipboard history
  # are the active subsystems: niri's Mod+D and the bar dead-zone summon the
  # launcher over IPC, and the picker scripts in bin/ reach it via
  # `noctalia dmenu`.
  notification.enable_daemon = true;
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

  # The launcher and control center expose no size options of their own, so
  # this global scale is the only lever that grows them. It deliberately does
  # not touch the bar, whose widgets scale from bar.main.scale instead.
  # Range 0.5-2.5, step 0.05.
  accessibility.ui_scale = 1.35;

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
    # Graph widgets scroll at the fastest poll of any metric, so leaving this at
    # its 3s default would stair-step the network graphs against a 1s scroll.
    network_poll_seconds = 1.0;
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
    end = [
      "clipboard"
      "tray"
      "net_rx"
      "net_tx"
      "cpu_usage"
      "cpu_temp"
      "ram_pct"
      "ram_used"
      "disk_home"
      "disk_nix"
      "lock_keys"
      "clock"
    ];

    # The gap between the start and end sections is a click target the width of
    # the screen. Right-click keeps its default (control center).
    dead_zone.actions = {
      left = "panel-toggle launcher";
      middle = "panel-toggle clipboard";
    };
  };

  widget = {
    # waybar folded per-window icons into its workspace buttons; noctalia splits
    # those into two widgets, since its workspaces widget renders at most one
    # icon and only in focus_hint style.
    taskbar = {
      group_by_workspace = true;
      workspace_group_content = "icons";
    };

    workspaces = {
      # One bar per output, so without this both bars paint a focused pill and
      # neither says which head holds keyboard focus. Unfocused outputs fall
      # back to the occupied color.
      focused_output_only = true;
      # niri keeps a trailing empty workspace at all times; labelling it is
      # noise. The active workspace stays labelled even while empty.
      labels_only_when_occupied = true;
    };

    # Only meaningful for a metric that swings across orders of magnitude, where
    # the graph carries the shape and the number just needs to stop reflowing
    # the bar every second.
    net_rx = sysmon {
      stat = "net_rx";
      interface = networkInterface;
      visualization = "graph";
      network_speed_compact = true;
      label_min_width = 40;
    };

    net_tx = sysmon {
      stat = "net_tx";
      interface = networkInterface;
      visualization = "graph";
      network_speed_compact = true;
      label_min_width = 40;
    };

    cpu_usage = sysmon {
      stat = "cpu_usage";
      visualization = "graph";
    };

    cpu_temp = sysmon {stat = "cpu_temp";};

    # One waybar module showed percentage and absolute together; sysmon reports
    # a single stat per instance, so the pair is split.
    ram_pct = sysmon {stat = "ram_pct";};

    ram_used = sysmon {stat = "ram_used";};

    # `path` only expands a leading `~`, never $VAR, so pass an absolute path.
    disk_home = sysmon {
      stat = "disk_used";
      path = homePath;
    };

    disk_nix = sysmon {
      stat = "disk_used";
      path = "/nix";
    };

    # zellij's share plugin silently drops <SPACE> while Num or Caps Lock is on,
    # with nothing in its UI to say why. Surface the widget only in that
    # abnormal state so its presence is the signal.
    lock_keys.hide_when_off = true;

    clock = {
      format = "󰥔 {:%a, %b %d %Y %I:%M:%S %p}";
      # No calendar token exists for the bar clock; noctalia puts the calendar
      # in the control center panel instead.
      tooltip_format = "{:%A, %B %d, %Y}";
      actions.right = "exec ghostty --command='zellij'";
    };
  };
}
