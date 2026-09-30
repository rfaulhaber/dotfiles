-- Tracks the current fcitx5 input method and publishes it on the plugin's
-- state channel as "im". Every bar's widget watches that key, so fcitx5 is
-- asked once per poll however many monitors there are.
--
-- The keybind that toggles the input method pokes this service right after
-- with `noctalia msg plugin ryan/ime:poll all refresh`, so the usual switch
-- shows up at once. The poll only has to catch switches made elsewhere (the
-- tray icon, fcitx5's own hotkeys) and the daemon coming or going.

local fcitx = require("./fcitx.luau")

local last = nil
local busy = false

local function poll()
  -- A stalled D-Bus call must not queue up a query per tick behind it.
  if busy then
    return
  end
  busy = true
  local started = fcitx.current(function(name)
    busy = false
    -- state.set notifies watchers even when the value is unchanged.
    if name ~= last then
      last = name
      noctalia.state.set("im", name)
    end
  end)
  if not started then
    busy = false
  end
end

noctalia.setUpdateInterval(2000)

function update()
  poll()
end

function onIpc(event, _payload)
  if event == "refresh" then
    poll()
  end
end
