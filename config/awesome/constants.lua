local terminal = "kitty"
local editor = "emacs"

local editor_cmd = string.format("%s -e %s", terminal, editor)

return {
	terminal = terminal,
	editor = editor,
    editor_cmd = editor_cmd,
	browser_cmd = "firefox-developer-edition",
	lock_exec = "dm-tool lock",
	font = "Hack Nerd Font Mono 9",

	keys = {
		modkey = "Mod4",
		shift_key = "Shift",
		ctrl_key = "Control",
		alt_key = "Mod1",
	},
}
