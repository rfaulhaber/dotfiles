{
  colors,
  homePath,
}: {
  height = 45;
  layer = "top";
  modules-left = ["niri/workspaces" "wlr/taskbar" "niri/window"];
  modules-center = [];
  modules-right = ["tray" "cpu" "memory" "disk" "disk#nix" "clock"];
  "wlr/taskbar" = {
    format = "{icon}";
    tooltip-format = "{title} | {app_id}";
    on-click = "activate";
    on-click-middle = "close";
    icon-size = 28;
  };
  clock = {
    format = "󰥔 {:%a, %b %d %Y %I:%M:%S %p}";
    interval = 1;
    tooltip-format = "<span size='10pt'>{calendar}</span>";
    actions = {
      on-click = "mode";
      on-scroll-up = "shift_up";
      on-scroll-down = "shift_down";
      on-right-click = "shift_reset";
    };
    calendar.format = {
      months = "<span color='${colors.green}'><b>{}</b></span>";
      days = "<span color='${colors.base07}'><b>{}</b></span>";
      weeks = "<span color='${colors.cyan}'><b>W{}</b></span>";
      weekdays = "<span color='${colors.magenta}'><b>{}</b></span>";
      today = "<span color='${colors.blue}'><b><u>{}</u></b></span>";
    };
  };
  cpu = {
    interval = 1;
    format = " {usage}%";
    states = {
      low = 10;
      low-medium = 35;
      medium = 50;
      high = 70;
      max = 90;
    };
  };
  memory = {
    interval = 1;
    format = "󰘚 {percentage}% ({used:0.1f}/{avail:0.1f}/{total:0.1f}) GiB";
    states = {
      low = 10;
      low-medium = 35;
      medium = 50;
      high = 70;
      max = 90;
    };
  };
  # TODO make modular
  disk = {
    interval = 30;
    format = "󰋊 {path}: {used}";
    path = homePath;
    tooltip = true;
  };
  "disk#nix" = {
    interval = 30;
    format = "󰋊 {path}: {used}";
    path = "/nix";
    tooltip = true;
  };
}
