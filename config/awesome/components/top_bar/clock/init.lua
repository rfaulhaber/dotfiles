local wibox = require("wibox")

local clock_component = wibox.widget({
	format = "%a, %b %d %I:%M:%S %p",
	widget = wibox.widget.textclock,
	refresh = 1,
})

return clock_component
