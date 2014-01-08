Zephyrus
========

Zephyrus tool is a part of the [Aeolus Project](http://www.aeolus-project.org).


Compilation and installation
============================

In order to compile the Zephyrus tool simply type "make".

To obtain basic information about using the tool type "./zephyrus.native -help".

For simple examples of use see files in "tests" subdirectory, especially
all the test scripts "test*.bash".

For several tests based on a more complicated example (the one presented
in the paper), see files in the "example-inputs" subdirectory. Some results
of these tests are available in the "example-results" subdirectory.


Requirements
------------

Ingredients (libraries and software components) required to successfuly compile and run Zephyrus:

*A) OCaml libraries*

In order to compile Zephyrus you need to have these OCaml libraries installed:

  1. [Extlib](http://code.google.com/p/ocaml-extlib/)
  2. [Atd & Atdgen](http://oss.wink.com/atdgen/)
  3. [FaCiLe](http://www.recherche.enac.fr/opti/facile/)
  4. [Menhir](http://gallium.inria.fr/~fpottier/menhir/)

*Installing through OPAM *

The most simple way to get all the required OCaml libraries is to use OPAM (a package manager for OCaml):

1. Follow the instructions on the [OPAM website](http://opam.ocamlpro.com) in order to install OPAM on your machine.

2. Add Aeolus OPAM repository to your OPAM installation:

```
opam repo add aeolus git@github.com:jakub-zwolakowski/opam.git"
```

3. Update the repository data:

```
opam update
```

4. Install the zephyrus package:

```
opam install zephyrus
```

Now you should be able to compile Zephyrus using the standard Makefile.


*Notes:*

In some cases (if OPAM was compiled from source) before attempting to
compile Zephyrus you may need to execute:

```
eval `opam config -env`
```

Also make sure that you are using a recent version of OCaml compiler:

```
opam switch 4.00.1
```

* Installing using Debian packages *

If you are using a Debian-based Linux distribution you can use apt-get to install these packages:

libextlib-ocaml libextlib-ocaml-dev
libatd-ocaml libatd-ocaml-dev libatdgen-ocaml libatdgen-ocaml-dev
libfacile-ocaml-dev
menhir



*B) Solvers*

If you want to use the C12 solver:

1. [G12 MiniZinc Distribution (version 1.6)](http://www.g12.csse.unimelb.edu.au/minizinc/download.html)

You should install it following its installation instructions and make sure that binaries "mzn2fzn" and "flatzinc" are in your PATH when you execute Zephyrus.


If you also want to use the GeCode solver (you still need C12 installed, because it contains the mzn2fzn MiniZinc to FlatZinc converter):

2. [GeCode](http://www.gecode.org/)

You can install it using a Debian package: flatzinc
