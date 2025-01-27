# on hyperion we use gpg-agent as our ssh-agent. nix doesn't set this correctly
# for nushell, so we need to make sure it's set properly
$env.SSH_AUTH_SOCK = "/run/user/1000/gnupg/S.gpg-agent.ssh"
