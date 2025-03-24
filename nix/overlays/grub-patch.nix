# fix for this issue:
# https://github.com/NixOS/nixpkgs/issues/391308
(final: prev: let
  patchName = "18_fs_ntfs_implement_attribute_verification.patch";
in {
  grub2 = prev.grub2.overrideAttrs (old: {
    patches = builtins.filter (p: builtins.isPath p || p.name != patchName) old.patches;
  });
})
