-- Bar label for the current fcitx5 input method. State comes from the polling
-- service through the plugin's state channel; a click toggles between the
-- keyboard layout and the active input method, then publishes the result
-- itself instead of waiting for the next poll.

local fcitx = require("./fcitx.luau")

local labels = noctalia.getConfig("labels") or {}

-- watch() does not fire on registration, so read whatever the service has
-- already published.
local name = noctalia.state.get("im")

local function render()
  if name == nil or name == "" then
    barWidget.setText("--")
    barWidget.setColor("on_surface")
    barWidget.setTooltip(name == nil and "Waiting for fcitx5" or "fcitx5 is not running")
    return
  end
  barWidget.setText(labels[name] or name)
  -- Anything other than a plain layout is a composing input method; tint it
  -- so a glance says which mode the next keystroke lands in.
  if string.sub(name, 1, 9) == "keyboard-" then
    barWidget.setColor("on_surface")
  else
    barWidget.setColor("primary")
  end
  barWidget.setTooltip(name)
end

local function apply(value)
  if value ~= name then
    name = value
    render()
  end
end

noctalia.state.watch("im", apply)

-- The service does the polling; this instance only reacts. Without this the
-- widget timer ticks every 250 ms for nothing.
noctalia.setUpdateInterval(3600000)

function onClick()
  noctalia.runAsync({ "fcitx5-remote", "-t" }, function(result)
    if result.exitCode ~= 0 then
      noctalia.notify("Input method", "Could not toggle fcitx5: " .. (result.stderr or ""))
      return
    end
    fcitx.current(function(value)
      noctalia.state.set("im", value)
      apply(value)
    end)
  end)
end

render()
