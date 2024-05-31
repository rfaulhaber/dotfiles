local awful = require("awful")

local make_tags = function(screen)
	awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, screen, awful.layout.layouts[1])
end
