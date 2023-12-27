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
    sha256 = "1ip6y6i59klqkkalncdy64crp9vlxb8xrw8ig6cy1h9bpicrc16f";
  };

  phases = ["buildPhase"];

  buildPhase = ''
    mkdir -p $out
    cp $src $out/bootloader.bin
  '';
}
