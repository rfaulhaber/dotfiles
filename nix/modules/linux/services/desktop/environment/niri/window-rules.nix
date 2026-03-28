[
  # WezTerm initial configure bug workaround
  {
    matches = [{app-id = "^org\\.wezfurlong\\.wezterm$";}];
    default-column-width = {};
  }
  # Firefox PiP floats
  {
    matches = [
      {
        app-id = "firefox$";
        title = "^Picture-in-Picture$";
      }
    ];
    open-floating = true;
  }
  # Firefox opens maximized
  {
    matches = [{app-id = "firefox|firefox-aurora";}];
    open-maximized = true;
  }
  # Emacs opens maximized
  {
    matches = [{app-id = "Emacs";}];
    open-maximized = true;
  }
]
