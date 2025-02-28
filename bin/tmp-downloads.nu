def main [--link: path] {
  let target = (mktemp -d -p /tmp)

  ^ln -sfT $target $link
}
