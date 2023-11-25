(final: prev: {
  awesome-git = (final.awesome.override {lua = final.luajit;}).overrideAttrs (old: rec
    {
      pname = "awesome-git";
      version = "4.3-master";
      src = final.fetchFromGitHub {
        owner = "awesomeWM";
        repo = "awesome";
        rev = "375d9d723550023f75ff0066122aba99fdbb2a93";
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
