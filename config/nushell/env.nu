# Nushell Environment Config File
#
# version = "0.94.1"

# this is the same on every system I use
const dotfiles_path = "~/Projects/dotfiles/config/nushell"

def create_left_prompt [] {
    let dir = match (do { $env.PWD | path relative-to $nu.home-path }) {
        null => $env.PWD
        '' => '~'
        $relative_pwd => ([~ $relative_pwd] | path join)
    }

    let path_color = (if (is-admin) { ansi red_bold } else { ansi green_bold })
    let separator_color = (if (is-admin) { ansi light_red_bold } else { ansi light_green_bold })
    let path_segment = $"($path_color)($dir)"

    $path_segment | str replace --all (char path_sep) $"($separator_color)(char path_sep)($path_color)"
}

def create_right_prompt [] {
    # create a right prompt in magenta with green separators and am/pm underlined
    let time_segment = ([
        (ansi reset)
        (ansi magenta)
        (date now | format date '%x %X') # try to respect user's locale
    ] | str join | str replace --regex --all "([/:])" $"(ansi green)${1}(ansi magenta)" |
        str replace --regex --all "([AP]M)" $"(ansi magenta_underline)${1}")

    let last_exit_code = if ($env.LAST_EXIT_CODE != 0) {([
        (ansi rb)
        ($env.LAST_EXIT_CODE)
    ] | str join)
    } else { "" }

    ([$last_exit_code, (char space), $time_segment] | str join)
}

# Use nushell functions to define your right and left prompt
$env.PROMPT_COMMAND = {|| create_left_prompt }
$env.PROMPT_COMMAND_RIGHT = {|| create_right_prompt }

# The prompt indicators are environmental variables that represent
# the state of the prompt
$env.PROMPT_INDICATOR = {|| "> " }
$env.PROMPT_INDICATOR_VI_INSERT = {|| ": " }
$env.PROMPT_INDICATOR_VI_NORMAL = {|| "> " }
$env.PROMPT_MULTILINE_INDICATOR = {|| "::: " }

# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded
$env.ENV_CONVERSIONS = {
  "PATH": {
    from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
    to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
  }
  "Path": {
    from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
    to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
  }
}

# Directories to search for scripts when calling source or use
$env.NU_LIB_DIRS = [
    ($nu.default-config-dir | path join "hosts")
    ($nu.default-config-dir | path join "hosts/env")
    ($nu.default-config-dir | path join "hosts/config")
    ($nu.default-config-dir | path join "themes")
    $dotfiles_path
]

# Directories to search for plugin binaries when calling register
#
# By default, <nushell-config-dir>/plugins is added
$env.NU_PLUGIN_DIRS = [
    ($nu.default-config-dir | path join 'plugins')
]

# use bat as the default pager and manpager
$env.PAGER = "bat -p"
# thank you https://www.reddit.com/r/Nushell/comments/15jul5o/comment/jv3fe9h/
$env.MANPAGER = "sh -c 'col -bx | bat -l man -p'"
# bat documentation recommends setting this variable as well if the above is set
$env.MANROFFOPT = "-c"

# To add entries to PATH (on Windows you might use Path), you can use the following pattern:
# $env.PATH = ($env.PATH | split row (char esep) | prepend '/some/path')

# add doom bin to path
# TODO should this exist somewhere else?
let emacs_bin_path = $'($env.HOME)/.emacs.d/bin'
let emacs_config_path = $'($env.HOME)/.config/emacs/bin'

let emacs_config_paths = [ $emacs_bin_path $emacs_config_path ]

if ([$emacs_bin_path $emacs_config_path] | path exists | any { |v| $v == true } ) {
    $env.PATH = ($env.PATH | split row (char esep) | prepend $emacs_config_paths)
}

# nushell can't source files dynamically, so we have to do this
match $nu.os-info.name {
      "macos" => { source "./hosts/env/darwin.nu" },
      "linux" => { source "./hosts/env/linux.nu" },
}

match (sys host | get hostname) {
      "hyperion" => { source "./hosts/env/hyperion.nu" },
      "eos" => { source "./hosts/env/eos.nu" },
      "ponos" => { source "./hosts/env/ponos.nu" }
}

if ('/proc/version' | path exists) and (open '/proc/version' | find -i "microsoft" | length) > 0 {
  source "./hosts/env/wsl.nu"
}
