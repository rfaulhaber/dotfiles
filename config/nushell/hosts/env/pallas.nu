let hostname = (sys host | get hostname)

$env.PROMPT_COMMAND = {||
    let path = ($env.PWD | str replace $env.HOME '~')
    $"(ansi green_bold)($env.USER)@($hostname)(ansi reset):(ansi blue_bold)($path)(ansi reset)"
}
