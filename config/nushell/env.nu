# Nushell Environment Config File
#
# version = 0.82.1

let dotfiles_path = "~/Projects/dotfiles/config/nushell"

def create_left_prompt [] {
    mut home = ""
    try {
        if $nu.os-info.name == "windows" {
            $home = $env.USERPROFILE
        } else {
            $home = $env.HOME
        }
    }

    let dir = ([
        ($env.PWD | str substring 0..($home | str length) | str replace --string $home "~"),
        ($env.PWD | str substring ($home | str length)..)
    ] | str join)

    let path_color = (if (is-admin) { ansi red_bold } else { ansi green_bold })
    let separator_color = (if (is-admin) { ansi light_red_bold } else { ansi light_green_bold })
    let path_segment = $"($path_color)($dir)"

    $path_segment | str replace --all --string (char path_sep) $"($separator_color)/($path_color)"
}

def create_right_prompt [] {
    # create a right prompt in magenta with green separators and am/pm underlined
    let time_segment = ([
        (ansi reset)
        (ansi magenta)
        (date now | date format '%Y/%m/%d %r')
    ] | str join | str replace --all "([/:])" $"(ansi green)${1}(ansi magenta)" |
        str replace --all "([AP]M)" $"(ansi magenta_underline)${1}")

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
    # By default, <nushell-config-dir>/scripts is added
    ($nu.default-config-dir | path join 'scripts')
    $dotfiles_path
]

# Directories to search for plugin binaries when calling register
#
# By default, <nushell-config-dir>/plugins is added
$env.NU_PLUGIN_DIRS = [
    ($nu.default-config-dir | path join 'plugins')
]

# use bat as the default pager and manpager
$env.PAGER = "bat bat -p"
# thank you https://www.reddit.com/r/Nushell/comments/15jul5o/comment/jv3fe9h/
$env.MANPAGER = "sh -c 'col -bx | bat -l man -p'"

# To add entries to PATH (on Windows you might use Path), you can use the following pattern:
# $env.PATH = ($env.PATH | split row (char esep) | prepend '/some/path')

# add doom bin to path
let emacs_bin_path = $'($env.HOME)/.emacs.d/bin'

if ($emacs_bin_path | path exists) {
    $env.PATH = ($env.PATH | split row (char esep) | prepend $emacs_bin_path)
}

# macOS-specific configuration
if $nu.os-info.name == 'macos' {
   # add zoxide
   if (which zoxide | length) > 0 {
      zoxide init nushell | save -f ~/.zoxide.nu
   } else {
     echo 'zoxide not installed'
   }

   # /usr/local/bin doesn't exist by default on macOS, but I need it
   # should be low in the PATH
   $env.PATH = ($env.PATH | split row (char esep) | append '/usr/local/bin')

   # add homebrew to PATH
   $env.PATH = ($env.PATH | split row (char esep) | prepend '/opt/homebrew/bin')

   # add nix profile
   $env.PATH = ($env.PATH | split row (char esep) | append $'($env.HOME)/.nix-profile/bin')
}
