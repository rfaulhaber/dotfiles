-- Asks systemd whether the model server's user unit is up and hands the answer
-- to `cb(active)`. Shared by the polling service and the tile's post-toggle
-- check. Only the exact word "active" counts: "activating" covers the
-- pre-start step and the wait before an automatic restart, when nothing is
-- serving, and "deactivating" is a stop still draining the model processes.

local M = {}

function M.query(cb)
  noctalia.runAsync({ "systemctl", "--user", "is-active", "llama-server.service" }, function(result)
    cb(string.match(result.stdout or "", "^%s*active%s*$") ~= nil)
  end)
end

return M
