(lang dune 2.5)
(name naturL)
(source (github TheNaturLFoundation/naturL))
(version 1.0)
(authors
  "Simon Scatton <simon.scatton@epita.fr"
  "Rostan Tabet <rostan.tabet@epita.fr"
  "Vlad Argatu <vlad.argatu@epita.fr>"
  "Adrian Le Comte <adrian.le-comte@epita.fr>"
)
(maintainers
  "Simon Scatton <simon.scatton@epita.fr"
  "Rostan Tabet <rostan.tabet@epita.fr"
  "Vlad Argatu <vlad.argatu@epita.fr>"
  "Adrian Le Comte <adrian.le-comte@epita.fr>"
)

(generate_opam_files true)

(package
    (name naturL)
    (synopsis "Pseudocode to Python transpiler")
    (description "A transpiler from french pseudocode to Python.
Analyzes syntax and semantic, and generates error messages when necessary")
    (depends
        (ocaml (and (>= 4.8.0) (<= 4.10.0)))
        (dune (>= 1.11))
        yojson
        zarith
        ppx_inline_test
        ppx_expect
        ppx_yojson_conv
    ))
