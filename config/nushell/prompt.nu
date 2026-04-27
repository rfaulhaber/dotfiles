# Shared prompt for remote/server hosts.
# Sourced from env.nu when the current hostname is in the server list.
let hostname = (sys host | get hostname)

$env.PROMPT_COMMAND = {||
    let path = ($env.PWD | str replace $env.HOME '~')
    $"(ansi green_bold)($env.USER)@($hostname)(ansi reset):(ansi blue_bold)($path)(ansi reset)"
}
