{...}: {
  config = {
    system = {
      defaults = {
        dock = {
          # disable hot corners. see
          # https://nix-darwin.github.io/nix-darwin/manual/index.html#opt-system.defaults.dock.wvous-bl-corner
          wvous-bl-corner = 1;
          wvous-br-corner = 1;
          wvous-tl-corner = 1;
          wvous-tr-corner = 1;
        };
        CustomUserPreferences = {
          WindowManager = {
            AppWindowGroupingBehavior = false;
            AutoHide = false;
            GloballyEnabled = true;
            HideDesktop = true;
            StageManagerHideWidgets = false;
            StandardHideWidgets = false;
          };
          NSGlobalDomain = {
            AppleInterfaceStyle = "Dark";
            ApplePressAndHoldEnabled = false;
            InitialKeyRepeat = 15;
            KeyRepeat = 2;
            NSAutomaticCapitalizationEnabled = false;
            NSAutomaticDashSubstitutionEnabled = false;
            NSAutomaticPeriodSubstitutionEnabled = false;
            NSAutomaticQuoteSubstitutionEnabled = false;
            NSAutomaticSpellingCorrectionEnabled = false;
            NSTableViewDefaultSizeMode = 2;
          };
        };
      };
      keyboard = {
        enableKeyMapping = true;
        remapCapsLockToEscape = true;
      };
    };
  };
}
