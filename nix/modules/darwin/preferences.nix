{
  ...
}:
{
  config = {
    system = {
      defaults.CustomUserPreferences = {
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
          ApplePressAndHoldEnabled = 0;
          InitialKeyRepeat = 15;
          KeyRepeat = 2;
          NSAutomaticCapitalizationEnabled = 1;
          NSAutomaticDashSubstitutionEnabled = 1;
          NSAutomaticPeriodSubstitutionEnabled = 1;
          NSAutomaticQuoteSubstitutionEnabled = 1;
          NSAutomaticSpellingCorrectionEnabled = 1;
          NSTableViewDefaultSizeMode = 2;
        };
      };
      keyboard = {
        enableKeyMapping = true;
        remapCapsLockToEscape = true;
      };
    };
  };
}
