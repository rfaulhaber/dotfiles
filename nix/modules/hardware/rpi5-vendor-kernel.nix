# nixpkgs >= 26.11 makes the raspberrypi bootloader module read
# `config.boot.kernelPackages.kernel.target`, and hardware.deviceTree reads
# `kernel.buildDTBs`, but the Pi 5 vendor kernel is built by an older nixpkgs
# pin that never added those passthrus, so eval fails with "attribute
# missing". mkForce can't help: the module-system priority filter forces the
# broken definition before discarding it. Instead, graft the missing
# attributes onto the kernel's passthru. passthru doesn't affect the
# derivation, so the store path is unchanged and the cached kernel still hits.
# Remove once upstream's vendor kernel carries both.
#
# Imported by every consumer of the Pi 5 vendor kernel — the prometheus host
# and the rpi5 installer image — so the graft cannot drift between them. Not
# in nix/modules/default.nix's import list: it sets boot.kernelPackages
# unconditionally and needs the `nixos-raspberrypi` specialArg, so it stays
# opt-in and is reached by explicit relative path.
{
  pkgs,
  nixos-raspberrypi,
  ...
}: {
  boot.kernelPackages = let
    rpi5 = nixos-raspberrypi.packages.${pkgs.stdenv.hostPlatform.system}.linuxPackages_rpi5;
  in
    rpi5.extend (_final: prev: {
      kernel = prev.kernel.overrideAttrs (old: {
        passthru =
          (old.passthru or {})
          // {
            # nixpkgs b7c2ada also stopped elaborating `linux-kernel` on the
            # host platform, so the platform lookup needs its own fallback
            # to the aarch64 default target.
            target = old.passthru.target or (pkgs.stdenv.hostPlatform.linux-kernel.target or "Image");
            # The vendor kernel does build and install DTBs; only the flag
            # announcing that is missing.
            buildDTBs = old.passthru.buildDTBs or true;
          };
      });
    });
}
