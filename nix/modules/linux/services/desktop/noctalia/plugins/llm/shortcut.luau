-- Control-center tile for the local model server. State comes from the polling
-- service through the plugin's state channel, so a start or stop made anywhere
-- else (`llm-server`, systemctl) is reflected here without the tile polling on
-- its own.

local unit = require("./unit.luau")

local active = noctalia.state.get("active") == true
-- `llm-server toggle` can block for up to 30 s while the router comes up or
-- the model processes exit, so the tile stays disabled until it returns.
local busy = false

local function render()
  shortcut.setLabel("Local LLM")
  shortcut.setIcon("cpu", "cpu-off")
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
  noctalia.runAsync({ "llm-server", "toggle" }, function(result)
    busy = false
    if result.exitCode ~= 0 then
      noctalia.notify("Local LLM", "Could not toggle the model server: " .. (result.stderr or ""))
    end
    -- Publish the outcome now rather than waiting for the service's next poll.
    unit.query(function(value)
      noctalia.state.set("active", value)
      apply(value)
    end)
  end)
end

render()
