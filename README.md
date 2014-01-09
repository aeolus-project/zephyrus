Zephyrus
========

Zephyrus is a tool aiming to automate to a great extent the process of assembling complex distributed applications. Given as input a high-level specification of the desired system (a description of available software components and their relationships) and the current state of the system, Zephyrus is able to generate the system's architecture: place components in an optimal manner on the available machines and connect them as needed.

The Zephyrus tool is a part of the [Aeolus Project](http://www.aeolus-project.org).


Download, compile and run
-------------------------

####Download

The latest version of the tool is available at the Zephyrus git repository at github: https://github.com/aeolus-project/zephyrus.

You can get it by cloning the repository to your machine (you obviously need [git](http://git-scm.com/) to do this):

```sh
git clone git@github.com:aeolus-project/zephyrus.git
```

####Compile

To compile the Zephyrus tool simply type `make`. However, before attempting the compilation you should make sure, that you have all the necessary ingredients installed on your system:
1. [OCaml](http://caml.inria.fr/ocaml/) in at least version 3.12
2. Required OCaml libraries (specified below in detail)
3. At least one compatible constraint solver

#####Coinst

If you want to use some features which require the [coinst](http://coinst.irill.org/) tool, you will need to compile it as well. As it is included as a submodule int the Zephyrus git repository, you do not need to download it separately, it is enough if you type:

```sh
git submodule init
git submodule update
```

Then you can compile coinst by entering the *coinst* directory and typing `make`. Remeber that **coinst** also has some dependencies which need to be satisfied.

Note: Currently we've got a little issue with the makefiles, so if after compiling **coinst** you cannot compile Zephyrus anymore, you should try to execute this line (from the main Zephyrus directory):

```sh
make; _build/sanitize.sh; make
```

####Run

To check if Zephyrus works properly you can run a simple test:

```sh
./zephyrus.native -settings tests/test-1.settings
```

If it does not exit with some kind of an error, you can probably assume that Zephyrus works fine.

####Installing required OCaml libraries

In order to compile Zephyrus you need to have these OCaml libraries installed:

1. [Extlib](http://code.google.com/p/ocaml-extlib/)
2. [Atd & Atdgen](http://oss.wink.com/atdgen/)
3. [Menhir](http://gallium.inria.fr/~fpottier/menhir/)

There are two basic ways to install these:
1. Through [OPAM](http://opam.ocamlpro.com) **(recommended)**
2. Using Debian packages, i.e. through apt-get

#####Installing required OCaml libraries through OPAM

The most simple way to get all the required OCaml libraries is to use OPAM (a package manager for OCaml):

1. Follow the instructions on the [OPAM website](http://opam.ocamlpro.com) in order to install OPAM on your machine.
  
2. *(optional step; you probably don't need to do it, as Zephyrus is also available now in the main OPAM repository)*
   
   Add Aeolus OPAM repository to your OPAM installation:

  ```sh
  opam repo add aeolus git@github.com:jakub-zwolakowski/opam.git"
  ```

3. Update the repository data:

  ```sh
  opam update
  ```

4. Install the zephyrus package:

  ```sh
  opam install zephyrus
  ```

Now you should be able to compile Zephyrus using the standard makefile (i.e. by typing `make`).


*Notes:*

In some cases (e.g if OPAM was compiled from source), before attempting to compile Zephyrus you may need to execute:

```sh
eval `opam config -env`
```

Also make sure that you are using a recent version of OCaml compiler:

```sh
opam switch 4.00.1
```

#####Installing required OCaml libraries using Debian packages

If you are using a Debian-based Linux distribution you can also use apt-get to get the required OCaml libraries. Simply install following packages:

- libextlib-ocaml
- libextlib-ocaml-dev
- libatd-ocaml
- libatd-ocaml-dev
- libatdgen-ocaml
- libatdgen-ocaml-dev
- menhir

The command to execute:

```
sudo apt-get install libextlib-ocaml libextlib-ocaml-dev libatd-ocaml libatd-ocaml-dev libatdgen-ocaml libatdgen-ocaml-dev menhir
```

####Installing external Solvers

There are currently two constraint solvers supported by Zephyrus:
1. Solver from the C12 project
2. [GeCode](http://www.gecode.org/) solver

For now however you *always* need to install the C12 solver, as it containts **mzn2fzn**: a MiniZinc to FlatZinc converter.

#####Installing the C12 Solver

In order to install the C12 constraint solver, follow instructions from this website: [G12 MiniZinc Distribution (version 1.6)](http://www.g12.csse.unimelb.edu.au/minizinc/download.html)

After the installation you should make sure that the binaries **mzn2fzn** and **flatzinc** are in your PATH when you execute Zephyrus. Otherwise Zephyrus will not be able to find it by itself and you will have to specify custom solver settings to use it.

#####Installing the GeCode Solver
You can install the GeCode solver in two ways:
1. Either follow the installation instructions from its website: [GeCode website](http://www.gecode.org/).
2. Or you can install it using a Debian package "flatzinc": `sudo apt-get install flatzinc`


Tutorial
--------

To obtain basic information about using the tool type `./zephyrus.native -help`.

For simple examples of use see files in *tests* subdirectory, especially all the test scripts "test*.bash".

For several tests based on a more complicated example (the one presented in the paper), see files in the *example-inputs* subdirectory. Some results of these tests are available in the *example-results* subdirectory.
