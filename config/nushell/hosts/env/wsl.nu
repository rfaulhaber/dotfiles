$env.PATH = (
	$env.PATH
	| split row (char esep)
	| prepend "/nix/var/nix/profiles/default/bin"
	| prepend "/home/ryan/.nix-profile/bin"
)

# exemple for env.nu
let sshAgentFilePath = $"/tmp/ssh-agent-($env.USER).nuon"

if ($sshAgentFilePath | path exists) and ($"/proc/((open $sshAgentFilePath).SSH_AGENT_PID)" | path exists) {
    # loading it
    load-env (open $sshAgentFilePath)
} else {
    # creating it
    ^ssh-agent -c
        | lines
        | first 2
        | parse "setenv {name} {value};"
        | transpose -r
        | into record
        | save --force $sshAgentFilePath
    load-env (open $sshAgentFilePath)
}
