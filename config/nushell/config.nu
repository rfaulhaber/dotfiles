# Nushell Config File

use themes
let theme = (themes tokyo-night)

# Non-default settings
$env.config.edit_mode = "vi"
$env.config.color_config = $theme
$env.config.footer_mode = "auto"
$env.config.use_ansi_coloring = true
$env.config.use_kitty_protocol = true
$env.config.highlight_resolved_externals = true
$env.config.datetime_format.normal = '%a, %d %b %Y %H:%M:%S %z'
$env.config.history.file_format = "sqlite"
$env.config.history.isolation = true
$env.config.show_banner = false

$env.config.cursor_shape = {
  emacs: line
  vi_insert: block
  vi_normal: underscore
}

# Custom menus
$env.config.menus = ($env.config.menus | append [
  {
    name: commands_menu
    only_buffer_difference: false
    marker: "# "
    type: {
      layout: columnar
      columns: 4
      col_width: 20
      col_padding: 2
    }
    style: {
      text: green
      selected_text: green_reverse
      description_text: yellow
    }
    source: { |buffer, position|
      scope commands
      | where name =~ $buffer
      | each { |it| {value: $it.name description: $it.usage} }
    }
  }
  {
    name: vars_menu
    only_buffer_difference: true
    marker: "# "
    type: {
      layout: list
      page_size: 10
    }
    style: {
      text: green
      selected_text: green_reverse
      description_text: yellow
    }
    source: { |buffer, position|
      scope variables
      | where name =~ $buffer
      | sort-by name
      | each { |it| {value: $it.name description: $it.type} }
    }
  }
  {
    name: commands_with_description
    only_buffer_difference: true
    marker: "# "
    type: {
      layout: description
      columns: 4
      col_width: 20
      col_padding: 2
      selection_rows: 4
      description_rows: 10
    }
    style: {
      text: green
      selected_text: green_reverse
      description_text: yellow
    }
    source: { |buffer, position|
      scope commands
      | where name =~ $buffer
      | each { |it| {value: $it.name description: $it.usage} }
    }
  }
])

# Custom keybindings for the menus above
$env.config.keybindings = ($env.config.keybindings | append [
  {
    name: commands_menu
    modifier: control
    keycode: char_t
    mode: [emacs, vi_normal, vi_insert]
    event: { send: menu name: commands_menu }
  }
  {
    name: vars_menu
    modifier: alt
    keycode: char_o
    mode: [emacs, vi_normal, vi_insert]
    event: { send: menu name: vars_menu }
  }
  {
    name: commands_with_description
    modifier: control
    keycode: char_s
    mode: [emacs, vi_normal, vi_insert]
    event: { send: menu name: commands_with_description }
  }
])

# Aliases
alias l = ls -la

# platform-specific config
match $nu.os-info.name {
  "macos" => { source "./hosts/config/darwin.nu" },
  "linux" => { source "./hosts/config/linux.nu" },
}

# Host-specific config
match (sys host | get hostname) {
  "hyperion" => { source "./hosts/config/hyperion.nu" },
  "eos" => { source "./hosts/config/eos.nu" },
  "ponos" => { source "./hosts/config/ponos.nu" },
}

if ('/proc/version' | path exists) and (open '/proc/version' | find -i "microsoft" | length) > 0 {
  source "./hosts/config/wsl.nu"
}
