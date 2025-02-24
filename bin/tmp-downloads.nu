def main [--target: path, --link: path] {
  if not ($target | path exists) {
    mkdir $target
  }

  ^ln -sfT $target $link
}
