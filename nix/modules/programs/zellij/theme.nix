# Maps the base16 palette onto zellij's UI-component theme spec.
#
# zellij accepts two theme shapes: a legacy one naming only terminal-ish slots
# (fg/bg/red/green/...), and this one, naming each UI component it actually
# draws. Only the latter reaches the tab bar, status bar, pane frames and the
# plugin surfaces (session manager, strider), so the legacy shape leaves most
# of the UI on zellij's built-in defaults no matter which base16 theme is set.
#
# Every component takes `base` (foreground), `background`, and `emphasis_0`
# through `emphasis_3` — the accent slots plugins use to highlight parts of a
# string, such as the shortcut keys in the status bar.
{
  colors,
  # Role-keyed overrides for the semantic roles below (e.g. `{ error = "..."; }`),
  # values `#`-prefixed. Components reference these roles rather than the raw
  # base16 slots, so retuning one role here retunes every component using it.
  colorOverrides ? {},
}: let
  # Semantic roles sitting between the palette and zellij's components. These
  # reference the named aliases rather than raw baseXX slots wherever one
  # exists, so per-theme custom files and host-level `themes.overrides`
  # corrections reach zellij too. The accent assignments follow the mapping
  # zellij's own bundled themes use, so a scheme lands here looking the way its
  # author intended rather than arbitrarily recolored.
  p =
    {
      inherit (colors) bg;
      selection = colors.base02;
      dim = colors.grey;
      text = colors.fg;
      text-bright = colors.bright-white;

      # Drawn on the accent-backed surfaces (selected ribbon, focused frame,
      # table titles) — zellij's bundled themes use their green for "this is
      # the active thing".
      accent = colors.green;
      success = colors.green;
      error = colors.red;
      warning = colors.yellow;
      # Frames in a transient state — renaming a tab, an active search.
      highlight = colors.orange;

      emphasis0 = colors.orange;
      emphasis1 = colors.blue;
      emphasis2 = colors.green;
      emphasis3 = colors.magenta;
    }
    // colorOverrides;

  # The emphasis quartet most components share. Components whose background
  # differs from the pane background restate it, since these accents are only
  # legible against base00.
  emphasis = {
    emphasis_0 = p.emphasis0;
    emphasis_1 = p.emphasis1;
    emphasis_2 = p.emphasis2;
    emphasis_3 = p.emphasis3;
  };
in {
  text_unselected =
    {
      base = p.text;
      background = p.bg;
    }
    // emphasis;

  text_selected =
    {
      base = p.text-bright;
      background = p.selection;
    }
    // emphasis;

  # Ribbons are the pill-shaped widgets in the status bar and tab bar. Both
  # states invert the usual pairing — dark glyphs on a filled background — so
  # the bar reads as a distinct band rather than as pane content.
  ribbon_unselected = {
    base = p.selection;
    background = p.text;
    emphasis_0 = p.error;
    emphasis_1 = p.text-bright;
    emphasis_2 = p.emphasis1;
    emphasis_3 = p.emphasis3;
  };

  ribbon_selected = {
    base = p.selection;
    background = p.accent;
    emphasis_0 = p.error;
    emphasis_1 = p.emphasis0;
    emphasis_2 = p.emphasis3;
    emphasis_3 = p.emphasis1;
  };

  table_title =
    {
      base = p.accent;
      background = p.bg;
    }
    // emphasis;

  table_cell_unselected =
    {
      base = p.text;
      background = p.bg;
    }
    // emphasis;

  table_cell_selected =
    {
      base = p.text-bright;
      background = p.selection;
    }
    // emphasis;

  list_unselected =
    {
      base = p.text;
      background = p.bg;
    }
    // emphasis;

  list_selected =
    {
      base = p.text-bright;
      background = p.selection;
    }
    // emphasis;

  frame_selected = {
    base = p.accent;
    background = p.bg;
    emphasis_0 = p.emphasis0;
    emphasis_1 = p.emphasis1;
    emphasis_2 = p.emphasis3;
    emphasis_3 = p.bg;
  };

  frame_highlight = {
    base = p.highlight;
    background = p.bg;
    emphasis_0 = p.emphasis3;
    emphasis_1 = p.highlight;
    emphasis_2 = p.highlight;
    emphasis_3 = p.highlight;
  };

  exit_code_success = {
    base = p.success;
    background = p.bg;
    emphasis_0 = p.emphasis1;
    emphasis_1 = p.selection;
    emphasis_2 = p.emphasis3;
    emphasis_3 = p.emphasis1;
  };

  exit_code_error = {
    base = p.error;
    background = p.bg;
    emphasis_0 = p.warning;
    emphasis_1 = p.bg;
    emphasis_2 = p.bg;
    emphasis_3 = p.bg;
  };

  # Cursor colors for other users in a shared session. zellij's own themes
  # leave the tail of this list unset; spreading the full accent ramp across
  # all ten keeps later joiners distinguishable.
  multiplayer_user_colors = {
    player_1 = colors.base0E;
    player_2 = colors.base0D;
    player_3 = colors.base0C;
    player_4 = colors.base0A;
    player_5 = colors.base0B;
    player_6 = colors.base09;
    player_7 = colors.base08;
    player_8 = colors.base0F;
    player_9 = colors.base04;
    player_10 = colors.base06;
  };
}
