# Even though pCloud Drive is redistributed as a plug-n-play AppImage, it
# requires a little bit more love because of the way Nix launches those types
# of applications.
#
# What Nix does, simplifying a bit, is that it extracts an AppImage and starts
# it via buildFHSUserEnv - this is totally fine for majority of apps, but makes
# it by-design *impossible* to launch SUID wrappers [^1]; in case of pCloud,
# it's fusermount.
# (so pCloud starts, but silently fails to mount the FUSE drive.)
#
# To overcome this issue, we're manually extracting the AppImage and then treat
# it as if it was a regular, good-ol' application requiring some standard path
# fixes.
#
# ^1 https://github.com/NixOS/nixpkgs/issues/69338
#
# Update checklist:
# - Update version
# - Update key from PKGBUILD
# - Update sha256 value

{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;

let
  pname = "pcloud";
  version = "1.9.9";
  key = "XZWTVkVZQM0GNXA4hrFGPkefzUUWVLKOpPIX";
  name = "${pname}-${version}";

  # Archive link's code thanks to: https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=pcloud-drive
  src = fetchzip {
    url = "https://api.pcloud.com/getpubzip?code=${key}&filename=${name}.zip";
    sha256 = "sha256-8566vKrE3/QCm4qW9KxEAO+r+YfMRYOhV2Da7qic48M=";
    stripRoot = true;
  };

  appimageContents = appimageTools.extractType2 {
    inherit name;
    src = "${src}/pcloud";
  };

in stdenv.mkDerivation {
  inherit pname version;

  src = appimageContents;

  dontConfigure = true;
  dontBuild = true;

  nativeBuildInputs = [ autoPatchelfHook ];

  buildInputs =
    [ alsaLib dbus-glib fuse gtk3 libdbusmenu-gtk2 xorg.libXdamage nss udev ];

  installPhase = ''
    mkdir "$out"
    cp -ar . "$out/app"
    cd "$out"
    # Remove the AppImage runner, since users are not supposed to use it; the
    # actual entry point is the `pcloud` binary
    rm app/AppRun
    # Adjust directory structure, so that the `.desktop` etc. files are
    # properly detected
    mkdir bin
    mv app/usr/share .
    mv app/usr/lib .
    # Adjust the `.desktop` file
    mkdir share/applications
    substitute \
      app/pcloud.desktop \
      share/applications/pcloud.desktop \
      --replace 'Name=pcloud' 'Name=pCloud' \
      --replace 'Exec=AppRun' 'Exec=${pname}'
    # Build the main executable
    cat > bin/pcloud <<EOF
    #! $SHELL -e
    # This is required for the file picker dialog - otherwise pcloud just
    # crashes
    export XDG_DATA_DIRS="${gsettings-desktop-schemas}/share/gsettings-schemas/${gsettings-desktop-schemas.name}:${gtk3}/share/gsettings-schemas/${gtk3.name}:$XDG_DATA_DIRS"
    exec "$out/app/pcloud"
    EOF
    chmod +x bin/pcloud
  '';
}
