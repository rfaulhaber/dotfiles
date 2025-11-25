# "impure" is not a good name for this file I wanted to distinguish it from the
# other functions in this folder, which do not have an explicit dependency on
# pkgs while functions in here might. note that this file is loaded differently,
# as we don't know what platform we'll be loading until we get to the host
# configuration. see nix/lib/nixos:47-54
{pkgs}: {
  writeNushellScriptBin = name: text:
    pkgs.writeScriptBin name ''
      #!${pkgs.nushell}/bin/nu
      
      ${text}
    '';
}
