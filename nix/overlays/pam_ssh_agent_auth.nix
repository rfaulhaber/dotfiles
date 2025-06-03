# thank you pSub
# https://github.com/pSub/configs/blob/64a784b4b88d3579475294cb3e8797a7de68dddc/nixos/server/overlays/pam_ssh_agent_auth.nix
# Temporary fix for issue https://github.com/NixOS/nixpkgs/issues/386392.
final: prev: {
  pam_ssh_agent_auth = prev.pam_ssh_agent_auth.overrideAttrs (old: {
    fixupPhase = ''
      patchelf --add-needed ${prev.libgcc}/lib/libgcc_s.so.1 $out/libexec/pam_ssh_agent_auth.so
    '';
  });
}
