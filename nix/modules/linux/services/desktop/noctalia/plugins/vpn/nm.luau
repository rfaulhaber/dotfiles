-- Asks NetworkManager whether a profile is up and hands the answer to
-- `cb(active)`. Shared by the polling service and the tile's post-toggle
-- check. An inactive profile prints an empty GENERAL.STATE rather than a
-- word, so anything but "activated" counts as down.

local M = {}

function M.query(profile, cb)
  noctalia.runAsync({ "nmcli", "-g", "GENERAL.STATE", "connection", "show", profile }, function(result)
    cb(result.exitCode == 0 and string.match(result.stdout or "", "^%s*activated") ~= nil)
  end)
end

return M
