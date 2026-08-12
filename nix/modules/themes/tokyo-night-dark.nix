# Palette corrections for tinted-theming's tokyo-night-dark scheme, whose
# named accent slots don't hold the colors their labels claim: base08 ("red")
# is a pale lavender, base09 ("orange") is the foreground grey, and base0A
# ("yellow") is a cyan-blue. Left uncorrected, every consumer that asks for a
# semantic accent gets something unreadable or simply wrong — matched
# characters in fuzzel vanish into the selection row, failed exit codes in
# zellij render as ordinary text, and resolved commands in nushell turn blue.
#
# Values are taken from upstream Tokyo Night rather than invented: base0F
# already carries the scheme's real red, and #ff9e64 is Tokyo Night's orange.
# Merged over the scheme before host-level `themes.overrides`, so a host can
# still retune any of these.
#
# Each `bright-*` alias is corrected alongside its base: base16.nix derives
# both from the same mislabelled slot, and every other bright alias in this
# scheme already equals its base counterpart — fixing only `red` would leave
# ANSI color1 salmon while color9 stayed lavender.
{
  red = "#f7768e";
  bright-red = "#f7768e";
  orange = "#ff9e64";
  yellow = "#e0af68";
  bright-yellow = "#e0af68";
  # base0D ("blue") holds Tokyo Night's `blue1`, a cyan close enough to base0C
  # that ANSI blue and cyan were near-indistinguishable. #7aa2f7 is the theme's
  # actual blue, which the scheme omits entirely.
  blue = "#7aa2f7";
  bright-blue = "#7aa2f7";
  # `dark-blue` is derived from base0D as well, so it inherited the same cyan —
  # leaving niri's inactive focus ring more saturated than its active one. This
  # is Tokyo Night's `blue0`, the darkest blue in its ramp.
  dark-blue = "#3d59a1";
}
