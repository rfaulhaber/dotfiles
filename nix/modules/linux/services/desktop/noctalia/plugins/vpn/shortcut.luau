-- Control-center tile for the on-demand VPN. State comes from the polling
-- service through the plugin's state channel, so a toggle made anywhere else
-- (the bar widget, `vpn`, the Network tab) is reflected here without the tile
-- polling on its own.

local nm = require("./nm.luau")

local PROFILE = noctalia.getConfig("profile")

local active = noctalia.state.get("active") == true
local busy = false

local function render()
  shortcut.setLabel("VPN")
  shortcut.setIcon("shield-check", "shield-off")
  shortcut.setActive(active)
  shortcut.setEnabled(not busy)
end

local function apply(value)
  value = value == true
  if value ~= active then
    active = value
    render()
  end
end

noctalia.state.watch("active", apply)

-- The tile's own timer only runs while the panel is open; re-reading the
-- channel here covers a change that landed while it was closed, without
-- spawning anything.
function update()
  apply(noctalia.state.get("active"))
end

function onClick()
  if busy then
    return
  end
  busy = true
  render()
  noctalia.runAsync({ "vpn", "toggle", PROFILE }, function(result)
    busy = false
    if result.exitCode ~= 0 then
      noctalia.notify("VPN", "Could not toggle " .. PROFILE .. ": " .. (result.stderr or ""))
    end
    -- Publish the outcome now rather than waiting for the service's next poll.
    nm.query(PROFILE, function(value)
      noctalia.state.set("active", value)
      apply(value)
    end)
  end)
end

render()
