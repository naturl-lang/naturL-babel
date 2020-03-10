(lang dune 1.11)
(name naturL)
(source (github TheNaturLFoundation/naturL))
(version 1.0.0)

(generate_opam_files true)

(package
    (name naturL)
    (depends
        (ocaml (>= 4.4.0))
        (dune (>= 1.11))
        ppx_inline_test
    ))
