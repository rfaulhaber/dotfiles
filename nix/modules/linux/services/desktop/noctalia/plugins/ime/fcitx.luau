-- Asks fcitx5 for the unique name of the current input method (`rime`,
-- `keyboard-us`, ...) and hands it to `cb(name)`. When the daemon is not
-- running, fcitx5-remote prints "Failed to get reply." and exits non-zero;
-- that is reported as "". Returns false if the process could not be started,
-- in which case `cb` never runs.

local M = {}

function M.current(cb)
  return noctalia.runAsync({ "fcitx5-remote", "-n" }, function(result)
    if result.exitCode ~= 0 then
      cb("")
    else
      cb(result.stdout or "")
    end
  end)
end

return M
