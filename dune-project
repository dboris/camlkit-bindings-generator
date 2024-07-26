(lang dune 3.13)
(using menhir 3.0)

(name camlkit-bindings-generator)
(version 0.1.1)

(generate_opam_files true)

(source (github dboris/camlkit-bindings-generator))

(authors "Boris D. <borisd@gmx.com>")
(maintainers "Boris D. <borisd@gmx.com>")
(license ISC)

(package
 (name camlkit-bindings-generator)
 (synopsis "Tools for generating OCaml bindings to Cocoa frameworks")
 (description
 "Tools for introspecting the Objective-C runtime, parsing bridgesupport
 files, and generating OCaml bindings")
 (depends
  ocaml
  dune
  ctypes
  ctypes-foreign
  camlkit-base
  markup
  lambdasoup
  menhir
  alcotest)
 (tags (camlkit objc bindings macos ios cocoa gnustep)))
