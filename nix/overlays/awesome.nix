(final: prev: {
  awesome-git = (final.awesome.override {lua = final.luajit;}).overrideAttrs (old: rec
    {
      pname = "awesome-git";
      version = "4.3-master";
      src = final.fetchFromGitHub {
        owner = "awesomeWM";
        repo = "awesome";
        rev = "8b1f8958b46b3e75618bc822d512bb4d449a89aa";
        sha256 = "sha256-ZGZ53IWfQfNU8q/hKexFpb/2mJyqtK5M9t9HrXoEJCg=";
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
