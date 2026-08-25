# Auto-generated from system theme (Tokyo Night Dark).
# Do not edit — changes will be overwritten on rebuild.
$env.config.color_config = {
    binary: '#bb9af7'
    block: '#7aa2f7'
    cell-path: '#a9b1d6'
    closure: '#b4f9f8'
    custom: '#cbccd1'
    duration: '#e0af68'
    float: '#f7768e'
    glob: '#cbccd1'
    int: '#bb9af7'
    list: '#b4f9f8'
    nothing: '#f7768e'
    range: '#e0af68'
    record: '#b4f9f8'
    string: '#9ece6a'

    bool: {|| if $in { '#b4f9f8' } else { '#e0af68' } }

    date: {|| (date now) - $in |
        if $in < 1hr {
            { fg: '#f7768e' attr: 'b' }
        } else if $in < 6hr {
            '#f7768e'
        } else if $in < 1day {
            '#e0af68'
        } else if $in < 3day {
            '#9ece6a'
        } else if $in < 1wk {
            { fg: '#9ece6a' attr: 'b' }
        } else if $in < 6wk {
            '#b4f9f8'
        } else if $in < 52wk {
            '#7aa2f7'
        } else { 'dark_gray' }
    }

    filesize: {|e|
        if $e == 0b {
            '#a9b1d6'
        } else if $e < 1mb {
            '#b4f9f8'
        } else {{ fg: '#7aa2f7' }}
    }

    shape_and: { fg: '#bb9af7' attr: 'b' }
    shape_binary: { fg: '#bb9af7' attr: 'b' }
    shape_block: { fg: '#7aa2f7' attr: 'b' }
    shape_bool: '#b4f9f8'
    shape_closure: { fg: '#b4f9f8' attr: 'b' }
    shape_custom: '#9ece6a'
    shape_datetime: { fg: '#b4f9f8' attr: 'b' }
    shape_directory: { fg: '#9ece6a' attr: 'b' }
    shape_external: '#b4f9f8'
    shape_external_resolved: { fg: '#e0af68' attr: 'b' }
    shape_externalarg: { fg: '#9ece6a' attr: 'b' }
    shape_filepath: { fg: '#b4f9f8' attr: 'b' }
    shape_flag: { fg: '#7aa2f7' attr: 'b' }
    shape_float: { fg: '#f7768e' attr: 'b' }
    shape_garbage: { fg: '#FFFFFF' bg: '#FF0000' attr: 'b' }
    shape_glob_interpolation: { fg: '#b4f9f8' attr: 'b' }
    shape_globpattern: { fg: '#b4f9f8' attr: 'b' }
    shape_int: { fg: '#bb9af7' attr: 'b' }
    shape_internalcall: { fg: '#b4f9f8' attr: 'b' }
    shape_keyword: { fg: '#bb9af7' attr: 'b' }
    shape_list: { fg: '#b4f9f8' attr: 'b' }
    shape_literal: '#7aa2f7'
    shape_match_pattern: '#9ece6a'
    shape_matching_brackets: { attr: 'u' }
    shape_nothing: '#f7768e'
    shape_operator: '#e0af68'
    shape_or: { fg: '#bb9af7' attr: 'b' }
    shape_pipe: { fg: '#bb9af7' attr: 'b' }
    shape_range: { fg: '#e0af68' attr: 'b' }
    shape_raw_string: { fg: '#cbccd1' attr: 'b' }
    shape_record: { fg: '#b4f9f8' attr: 'b' }
    shape_redirection: { fg: '#bb9af7' attr: 'b' }
    shape_signature: { fg: '#9ece6a' attr: 'b' }
    shape_string: '#9ece6a'
    shape_string_interpolation: { fg: '#b4f9f8' attr: 'b' }
    shape_table: { fg: '#7aa2f7' attr: 'b' }
    shape_vardecl: { fg: '#7aa2f7' attr: 'u' }
    shape_variable: '#bb9af7'

    foreground: '#cbccd1'
    background: '#1a1b26'
    cursor: '#cbccd1'

    empty: '#7aa2f7'
    header: { fg: '#9ece6a' attr: 'b' }
    hints: '#7aa2f7'
    leading_trailing_space_bg: { attr: 'n' }
    row_index: { fg: '#9ece6a' attr: 'b' }
    search_result: { fg: '#f7768e' bg: '#a9b1d6' }
    separator: '#a9b1d6'
}
