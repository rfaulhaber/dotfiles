{
  lib,
  pkgs,
  ...
}:
pkgs.stdenv.mkDerivation rec {
  name = "roc-rk3328-cc-bootloader";
  pname = name;
  src = pkgs.fetchurl {
    url = "http://boot.libre.computer/release/roc-rk3328-cc/roc-rk3328-cc-boot.bin";
    sha256 = "1q5m2y04wm05z87vq80z0nmvikq4x60asbyyid4wbj6b9ycqr733";
  };

  phases = ["buildPhase"];

  buildPhase = ''
    mkdir -p $out
    cp $src $out/bootloader.bin
  '';
}
