{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.noctalia-greeter;

  # The greeter runs its own compositor and cannot read niri's config, so the
  # output geometry has to be restated in its own syntax. Derive it from the
  # niri outputs rather than duplicating coordinates that would drift apart.
  niriOutputs = config.modules.desktop.environment.niri.outputs;

  joinOutputs = f: concatStringsSep "; " (mapAttrsToList f niriOutputs);

  layout = joinOutputs (name: o: "${name}:${toString o.position.x},${toString o.position.y}");
  scales = joinOutputs (name: o: "${name}:${toString o.scale}");
  transforms = joinOutputs (name: _: "${name}:normal");

  # Bounding box of every output, which is what the greeter wants for its
  # virtual screen rather than a per-output size.
  spanFor = axis: dim:
    foldl' max 0 (mapAttrsToList (_: o: o.position.${axis} + o.mode.${dim}) niriOutputs);

  derivedSettings = {
    session.default = "niri";
    output = {
      inherit layout transforms scales;
      width = spanFor "x" "width";
      height = spanFor "y" "height";
    };
  };
in {
  imports = [inputs.noctalia-greeter.nixosModules.default];

  options.modules.desktop.noctalia-greeter = {
    enable = mkEnableOption false;
    settings = mkOption {
      description = ''
        Extra `greeter.toml` settings, merged over the session and output
        geometry derived from the niri output configuration.
      '';
      type = types.attrsOf types.anything;
      default = {};
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.modules.desktop.environment.niri.enable;
        message = "noctalia-greeter derives its output layout from the niri module";
      }
    ];

    nixpkgs.overlays = [inputs.noctalia-greeter.overlays.default];

    programs.noctalia-greeter = {
      enable = true;
      package = pkgs.noctalia-greeter;
      settings = recursiveUpdate derivedSettings cfg.settings;
    };
  };
}
