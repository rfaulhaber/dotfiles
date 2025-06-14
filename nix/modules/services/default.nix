{
  lib,
  isLinux,
  isDarwin,
  ...
}: {
  imports =
    [
      # Always import cross-platform services
      ./common
    ]
    ++ lib.optionals isLinux [
      # Linux-specific services
      ./linux
    ]
    ++ lib.optionals isDarwin [
      # Darwin-specific services
      ./darwin
    ];
}
