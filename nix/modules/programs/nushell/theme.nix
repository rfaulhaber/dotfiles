{
  colors,
  themeName,
}: ''
  # Auto-generated from system theme (${themeName}).
  # Do not edit — changes will be overwritten on rebuild.
  $env.config.color_config = {
      binary: '${colors.magenta}'
      block: '${colors.blue}'
      cell-path: '${colors.fg}'
      closure: '${colors.dark-cyan}'
      custom: '${colors.fg-alt}'
      duration: '${colors.yellow}'
      float: '${colors.red}'
      glob: '${colors.fg-alt}'
      int: '${colors.magenta}'
      list: '${colors.dark-cyan}'
      nothing: '${colors.red}'
      range: '${colors.yellow}'
      record: '${colors.dark-cyan}'
      string: '${colors.green}'

      bool: {|| if $in { '${colors.dark-cyan}' } else { '${colors.yellow}' } }

      date: {|| (date now) - $in |
          if $in < 1hr {
              { fg: '${colors.red}' attr: 'b' }
          } else if $in < 6hr {
              '${colors.red}'
          } else if $in < 1day {
              '${colors.yellow}'
          } else if $in < 3day {
              '${colors.green}'
          } else if $in < 1wk {
              { fg: '${colors.green}' attr: 'b' }
          } else if $in < 6wk {
              '${colors.dark-cyan}'
          } else if $in < 52wk {
              '${colors.blue}'
          } else { 'dark_gray' }
      }

      filesize: {|e|
          if $e == 0b {
              '${colors.fg}'
          } else if $e < 1mb {
              '${colors.dark-cyan}'
          } else {{ fg: '${colors.blue}' }}
      }

      shape_and: { fg: '${colors.magenta}' attr: 'b' }
      shape_binary: { fg: '${colors.magenta}' attr: 'b' }
      shape_block: { fg: '${colors.blue}' attr: 'b' }
      shape_bool: '${colors.dark-cyan}'
      shape_closure: { fg: '${colors.dark-cyan}' attr: 'b' }
      shape_custom: '${colors.green}'
      shape_datetime: { fg: '${colors.dark-cyan}' attr: 'b' }
      shape_directory: { fg: '${colors.green}' attr: 'b' }
      shape_external: '${colors.dark-cyan}'
      shape_external_resolved: { fg: '${colors.yellow}' attr: 'b' }
      shape_externalarg: { fg: '${colors.green}' attr: 'b' }
      shape_filepath: { fg: '${colors.dark-cyan}' attr: 'b' }
      shape_flag: { fg: '${colors.blue}' attr: 'b' }
      shape_float: { fg: '${colors.red}' attr: 'b' }
      shape_garbage: { fg: '#FFFFFF' bg: '#FF0000' attr: 'b' }
      shape_glob_interpolation: { fg: '${colors.dark-cyan}' attr: 'b' }
      shape_globpattern: { fg: '${colors.dark-cyan}' attr: 'b' }
      shape_int: { fg: '${colors.magenta}' attr: 'b' }
      shape_internalcall: { fg: '${colors.dark-cyan}' attr: 'b' }
      shape_keyword: { fg: '${colors.magenta}' attr: 'b' }
      shape_list: { fg: '${colors.dark-cyan}' attr: 'b' }
      shape_literal: '${colors.blue}'
      shape_match_pattern: '${colors.green}'
      shape_matching_brackets: { attr: 'u' }
      shape_nothing: '${colors.red}'
      shape_operator: '${colors.yellow}'
      shape_or: { fg: '${colors.magenta}' attr: 'b' }
      shape_pipe: { fg: '${colors.magenta}' attr: 'b' }
      shape_range: { fg: '${colors.yellow}' attr: 'b' }
      shape_raw_string: { fg: '${colors.fg-alt}' attr: 'b' }
      shape_record: { fg: '${colors.dark-cyan}' attr: 'b' }
      shape_redirection: { fg: '${colors.magenta}' attr: 'b' }
      shape_signature: { fg: '${colors.green}' attr: 'b' }
      shape_string: '${colors.green}'
      shape_string_interpolation: { fg: '${colors.dark-cyan}' attr: 'b' }
      shape_table: { fg: '${colors.blue}' attr: 'b' }
      shape_vardecl: { fg: '${colors.blue}' attr: 'u' }
      shape_variable: '${colors.magenta}'

      foreground: '${colors.fg-alt}'
      background: '${colors.bg}'
      cursor: '${colors.fg-alt}'

      empty: '${colors.blue}'
      header: { fg: '${colors.green}' attr: 'b' }
      hints: '${colors.bright-blue}'
      leading_trailing_space_bg: { attr: 'n' }
      row_index: { fg: '${colors.green}' attr: 'b' }
      search_result: { fg: '${colors.red}' bg: '${colors.fg}' }
      separator: '${colors.fg}'
  }
''
