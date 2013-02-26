#!/bin/bash

echo "Installing packages:"
echo "ocaml-batteries-included"
echo "libatd-ocaml libatd-ocaml-dev libatdgen-ocaml libatdgen-ocaml-dev"
echo "libfacile-ocaml-dev"
echo "libmenhir-ocaml-dev menhir"

sudo apt-get install\
 ocaml-batteries-included\
 libatd-ocaml libatd-ocaml-dev libatdgen-ocaml libatdgen-ocaml-dev\
 libfacile-ocaml-dev\
 menhir
