Zephyrus
========

Introduction
------------

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
  
2. *(optional step; you don't need to do this anymore, as the zephyrus package is now available directly in the main OPAM repository)*
   
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


User Guide
----------

####Basics

In this User Guide we assume that:

- you have successfuly managed to compile the Zephyrus tool
- and the generated main executable file is called "zephyrus.native".

To obtain a short description of basic options available when using the tool type `./zephyrus.native -help`.

For simple examples of use see files in *tests* subdirectory, especially all the test scripts "test*.settings".

For several tests based on a more complicated examples, see files in the *example-inputs* (the standard Wordpress use cases, appearing often in our papers) and *openstack* (our OpenStack use cases) subdirectories.

####Features

(What does Zephyrus do and how?)

#####Modes

The Zephyrus tool works in three different modes:

1. *Classic mode* : find the final configuration.

  This is the standard functioning mode of Zephyrus. In this mode it reads all the provided inputs (it requires at least a universe and a specification), generates the final configuration according to given parameters and outputs it to specified output files.

2. *Validate mode* : check if the input configuration is valid.

  In this mode Zephyrus reads the provided inputs (it requires at least a universe, a specification and an initial configuration) and checks if the given configuration is valid with respect to the given universe and specification. As an output, Zephyrus will either assure us that the configuration is valid or print a list of the universe and configuration constraints which the configuration violates (as detailed as possible).

3. *No-solving mode* : translate between different formats.

  This is a mode which serves principally to translate between different formats of data. For example we can use it to generate DOT configuration graphs from solutions already computed before and saved in the form of JSON files.
  
  In this mode Zephyrus takes the input data (it requires at least a universe and a configuration), it omits the solving step, producing the output in the standard way (exactly as in the *classic mode*), but using the initial configuration directly as the final one.


####Parameters

(Usage of the program)

In order to tell Zephyrus what to do (which input files to read, what outputs to produce, which constraint solver to use, etc.) we need to pass him some parameters. There are two ways to do it:

1. Through the *setting files*

   We write the parameters in the form of setting files and tell Zephyrus to read them.

2. Through command line arguments

   We pass the parameters to Zephyrus directly on the command line when we execute the compiled program.

The setting files mechanism is more complete and recommended on the long term (we do not need to retype the long list of parameters every time), but using the command-line arguments is often easier and faster on the short term. In practice we usually pass some parameters one way and some the other.

(Moreover it would be difficult to completely avoid using the command line arguments, as we have to tell Zephyrus somehow where are the setting files we want to him to read).

###### Example

This is how a typical simple settings file looks like:

*my.settings*
```
# Input
input-file-universe         = "u.json"
input-file-configuration    = "ic.json"
input-file-specification    = "spec.spec"
input-optimization-function = compact

# Output
results = [("json", "output.json"); ("graph-deployment", "output.dot")]
```

Executing Zephyrus using this settings file would be very simple: `./zephyrus.native -settings my.settings`.

The exact command-line arguments equivalent is a little heavier and looks like that:

```
./zephyrus-native -u u.json -ic ic.json -spec spec.spec -opt compact -out json output.json -out graph-deployment output.dot
```

#####Parameters handling

(How setting files and command line parameters are handled and append to / override each other)

TODO

CHECK: If you want you can specify multiple settings files for Zephyrus to read and add some command-line arguments on top of it. All the   parameters form the settings files and all the command-line arguments will be taken into accound in a specific order. What happens when a parameter is declared twice depends on, in some cases parameter value is simply overriden and in others they stack.

#####Setting files syntax

Zephyrus setting file syntax is quite straightforward as settings are basically a list of key-value pairs.

Assigning a value to a parameter takes form of a pair: the parameter name and the value, divided by an equality sign ('='):
```
parameter = value
```

There can be at most one assignment per line. Empty lines are ignored. We can also include comments in the setting files, as everything in a given line following a '#' symbol is also ignored.

######Example:
```
key1 = value1
key2=value2
key3    =value3

key4 = value4 # This is a comment.
# This is also a comment.
```

Every parameter can be assigned values of a certain type and there are five types available:

- Boolean
  - `true`, `yes` and `y` mean **true**
  - `false`, `no` and `n` mean **false**
- Integer
- String
  (You can put quotes or double quotes around the strings, but they are necessary only if the string contains whitespaces.)
- List: `[element_1; element_2; element_3; ... el_n]`
- Pair: `(element_1, element_2)`

Of course lists of pairs, pairs of lists etc. are also possible.

######Example:
```
# A string without quotes:
input-file-universe = u.json

# A string with quotes:
input-file-configuration = "ic.json"

# An integer:
verbose-level = 2

# A boolean:
verbose-data = y

# A list of pairs of strings:
results = [("json", "output.json"); ("graph-deployment", "output.dot")]
```


#####Available parameters

TODO: Reformat, fill information

- mode                                (* The Zephyrus functioning mode: {classic|validate|no-solving}. *)
- input_file_universe                 (* The universe file. *)
- input_file_configuration            (* The initial configuration file. *) (* The file where Zephyrus should look for the input configuration. *)
- input_file_specification            (* The specification file. *)
- input_file_repositories             (* The external repositories files. *)
- input_optimization_function         (* The optimization function choice: {simple|compact|conservative|spread|none}*)
- append_repository_to_package_name   (* Prefix every package name with its repository name in the output. *)
- solver                              (* Choose the main constraint solver: {none|g12|gcode|custom} *)
- custom_solver_command               (* Defined a custom command used to launch the external FlatZinc solver. *)
- custom_mzn2fzn_command              (* Defined a custom command used to launch the MiniZinc-to-FlatZinc converter. *)
- results                             (* Which forms of output should Zephyrus produce and to which files. *)
- verbose_level                       (* How much information should Zephyrus print: 0,1,2,3 *)
- verbose_data                        (* Should Zephyrus print the input data during execution. *)

#####Command-line only parameters

TODO: Reformat, fill information

- settings
- print-path

####Input / Output files syntax

(Syntax of the Universe, Configuration, Specification and External Repository files)

TODO
