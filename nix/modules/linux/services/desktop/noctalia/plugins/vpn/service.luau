-- Background poll of the profile's state, published on the plugin's state
-- channel as "active" so the tile is right the moment the control center
-- opens. Runs whether or not any panel is visible.

local nm = require("./nm.luau")

local profile = noctalia.getConfig("profile")

local function poll()
  nm.query(profile, function(active)
    noctalia.state.set("active", active)
  end)
end

-- One D-Bus round trip per poll, cheap enough to stay honest without a signal
-- subscription.
noctalia.setUpdateInterval(3000)

function update()
  poll()
end

function onConfigChanged()
  profile = noctalia.getConfig("profile")
  poll()
end

poll()
