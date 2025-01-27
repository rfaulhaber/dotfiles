local os = require "os";

return {
	font_size = 20,
	front_end = "WebGpu",
    -- wezterm uses its own SSH agent. we don't want this
    default_ssh_auth_sock = os.getenv 'SSH_AUTH_SOCK',
}
