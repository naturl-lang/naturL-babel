if ! [ -x "$(command -v opam)" ]; then
    echo 'Error: opam is not installed. Please install opam using your packet manager or source compilation' >&2
    exit 1
fi

if ! [ -x "$(command ls ~/.opam > /dev/null)" ]; then
    echo 'Info : opam is not initialized, running opam init...' >&2
    opam init --yes
    eval $(opam env)
fi

OCAMLVERSION = $(ocamlc --version)

if [$OCAMLVERSION != "4.08.0" && $OCAMLVERSION != "4.09.0" && $OCAMLVERSION != "4.10.0"]; then
    echo 'No compatible ocaml version detected, installing latest'
    opam switch create ocaml-base-compiler.4.10.0
    eval $(opam env)
fi

echo 'Installing the programm'
opam install .
echo 'Compiling'
eval $(opam env)
dune build @install
echo 'Setting environment variable and exporting'
echo export NATURLPATH=$PWD > ~./bashrc
exit 0
