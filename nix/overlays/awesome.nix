(final: prev: {
  awesome-git = (final.awesome.override {lua = final.luajit;}).overrideAttrs (old: rec
    {
      pname = "awesome-git";
      version = "4.3-master";
      src = final.fetchFromGitHub {
        owner = "awesomeWM";
        repo = "awesome";
        rev = "e6f5c7980862b7c3ec6c50c643b15ff2249310cc";
        sha256 = "sha256-9cIQvuXUPu8io2Qs3Q8n2WkF9OstdaGUt/+0FMrRkXk=";
        fetchSubmodules = false;
      };
      patches = [];

      # this is the real magic. awesome won't build without this
      postPatch = ''
        patchShebangs tests/examples/_postprocess.lua
        patchShebangs tests/examples/_postprocess_cleanup.lua
      '';
    });
})
