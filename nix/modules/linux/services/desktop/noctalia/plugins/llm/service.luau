-- Background poll of the model server unit's state, published on the plugin's
-- state channel as "active" so the tile is right the moment the control center
-- opens. Runs whether or not any panel is visible.

local unit = require("./unit.luau")

local function poll()
  unit.query(function(active)
    noctalia.state.set("active", active)
  end)
end

-- One systemctl spawn per poll, cheap enough to stay honest without watching
-- the unit over D-Bus.
noctalia.setUpdateInterval(3000)

function update()
  poll()
end

poll()
