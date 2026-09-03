-- Control-center tile for the random-wallpaper service. The fetch itself
-- lives in the systemd units that service already owns; this tile only picks
-- which one to start. Left click targets the focused monitor, right click
-- every monitor.
--
-- The focused output is noctalia's global notion (on niri: the output of the
-- focused workspace), not the output this panel is drawn on. They coincide
-- when the control center was opened by clicking that monitor's bar.

local UNIT_ALL = "random-wallpaper.service"
local UNIT_ONE = "random-wallpaper@%s.service"

shortcut.setLabel("New wallpaper")
shortcut.setIcon("shuffle")

local function start(unit, target)
  -- --no-block: the download takes seconds; report queueing failures only and
  -- leave the run's own result to the journal.
  noctalia.runAsync({ "systemctl", "--user", "start", "--no-block", unit }, function(result)
    if result.exitCode ~= 0 then
      noctalia.notify("Wallpaper", "Could not start " .. unit .. ": " .. (result.stderr or ""))
    end
  end)
  noctalia.notify("Wallpaper", "Fetching a new wallpaper for " .. target)
end

function onClick()
  local output = noctalia.focusedOutputName()
  if output == nil or output == "" then
    start(UNIT_ALL, "every monitor (no focused output)")
    return
  end
  start(string.format(UNIT_ONE, output), output)
end

function onRightClick()
  start(UNIT_ALL, "every monitor")
end
