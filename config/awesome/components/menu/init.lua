local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")
local constants = require("constants")
local beautiful = require("beautiful")

local terminal = constants.terminal

local awesome_menu = {
	{
		"hotkeys",
		function()
			hotkeys_popup.show_help(nil, awful.screen.focused())
		end,
	},
	{ "manual", terminal .. " -e man awesome" },
	{ "edit config", constants.editor_cmd .. " " .. awesome.conffile },
	{ "restart", awesome.restart },
	{
		"quit",
		function()
			awesome.quit()
		end,
	},
}

local lock_exec = "dm-tool lock"

local function lock_screen()
	awful.spawn(lock_exec)
end

local main_menu = awful.menu({
	items = {
		{ "awesome", awesome_menu, beautiful.awesome_icon },
		{ "open terminal", terminal },
		{ "lock screen", lock_screen },
	},
})

local launcher = awful.widget.launcher({
	image = beautiful.awesome_icon,
	menu = main_menu,
})

return {
	main_menu = main_menu,
	launcher = launcher,
}
