#!/usr/bin/env nu

# Static checks for the elisp under doom.d/. Run by doom-lint.yml on pull
# requests and pushes; runs locally too:
#
#     nu .github/scripts/check-doom-elisp.nu
#
# Two passes:
#
#  1. Reader pass over every *.el — catches unbalanced parens and other
#     reader errors anywhere in the config, including files full of doom
#     macros that can't be compiled outside doom.
#
#  2. Byte-compile pass over autoload/*.el only, with a curated set of
#     warning classes promoted to errors. Everything under lisp/ leans on
#     doom macros (map!, after!, load!) that a bare emacs can't expand, so
#     compiling those files produces only noise. The enabled classes are the
#     ones that flag real bugs in plain defuns — malformed let bindings,
#     empty bodies, wrong arg counts to known functions — while free-vars and
#     unresolved stay off: package symbols are never loadable in a bare
#     emacs, so those warnings fire on perfectly good code here.
#
# Emacs comes from PATH; CI provides it via `nix shell nixpkgs#emacs-nox`.

def repo-root [] {
  git rev-parse --show-toplevel | str trim
}

def reader-pass [files: list<string>] {
  let check = '(let (failed)
  (dolist (f command-line-args-left)
    (with-temp-buffer
      (insert-file-contents f)
      (goto-char (point-min))
      (condition-case err
          (while t (read (current-buffer)))
        (end-of-file (message "read OK   %s" f))
        (error (setq failed t)
               (message "read FAIL %s: %S" f err)))))
  (kill-emacs (if failed 1 0)))'
  ^emacs --batch --eval $check ...$files
}

def compile-pass [files: list<string>] {
  let settings = '(setq byte-compile-warnings
      (quote (suspicious empty-body callargs constants mutate-constant
              lexical lexical-dynamic mapcar redefine obsolete))
      byte-compile-error-on-warn t
      byte-compile-dest-file-function
      (let ((dir (make-temp-file "doom-elisp-check" t)))
        (lambda (f) (expand-file-name (concat (file-name-nondirectory f) "c") dir))))'
  ^emacs --batch --eval $settings -f batch-byte-compile ...$files
}

def main [] {
  let root = (repo-root)
  let all = (glob $"($root)/doom.d/**/*.el" | sort)
  let autoloads = (glob $"($root)/doom.d/autoload/*.el" | sort)

  print $"reader pass: ($all | length) files"
  try { reader-pass $all } catch { |e|
    print "reader pass failed"
    exit ($e.exit_code? | default 1)
  }

  print $"byte-compile pass: ($autoloads | length) files"
  try { compile-pass $autoloads } catch { |e|
    print "byte-compile pass failed"
    exit ($e.exit_code? | default 1)
  }

  print "doom.d elisp checks passed"
}
