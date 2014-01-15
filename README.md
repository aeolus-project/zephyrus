Zephyrus
========

1. Introduction
2. Download, compile and run
3. User guide

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
  
2. *(OPTIONAL STEP: you don't need to do this anymore, as the zephyrus package is now available directly in the main OPAM repository)*
   
   Add Aeolus OPAM repository to your OPAM installation:

  ```sh
  opam repo add aeolus git@github.com:jakub-zwolakowski/opam.git
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

  This is a mode which serves principally to translate between different formats of data. For example we can use it to generate *dot* configuration graphs from solutions already computed before and saved in the form of JSON files.
  
  In this mode Zephyrus takes the input data (it requires at least a universe and a configuration), it omits the solving step, producing the output in the standard way (exactly as in the *classic mode*), but using the initial configuration directly as the final one.


####Settings

(Usage of the program)

In order to tell Zephyrus what to do (which input files to read, what outputs to produce, which constraint solver to use, etc.) we need to pass him some parameters. From now on we will refer to each parameter passed to Zephyrus by a user as a "setting".

There are two ways to pass settings to Zephyrus:

1. Through the *setting files*

   We write the settings in the form of setting files and tell Zephyrus to read them.

2. Through command line arguments

   We pass the settings to Zephyrus directly on the command line as the arguments when we are executing the program.

The setting files mechanism is more complete and recommended on the long term (we do not need to retype the long list of parameters every time), but using the command-line arguments is often easier and faster on the short term. In practice we usually pass some settings one way and some the other.

Note: It is of course impossible to completely avoid using the command line arguments in favor of setting files, as we have to tell Zephyrus somehow where are the setting files we want to him to read.

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

The exact command-line arguments equivalent is definitely a little heavier and looks like that:

```
./zephyrus-native -u u.json -ic ic.json -spec spec.spec -opt compact -out json output.json -out graph-deployment output.dot
```

#####Handling settings

######How setting files and command line parameters are handled and how individual settings override / stack with each other.

If you want you can specify multiple setting files for Zephyrus to read and add some command-line arguments on top of it. All the parameters from the setting files and all the command-line arguments will be taken into accound.

The order in which the settings are handled can be important if the same setting is declared twice. What happens in this situation depends on the parameter itself: in most cases the the value declared before will be simply overriden by the one declared later ("before" or "later" in the settings handling order), but in some cases the two values will stack (i.e. the new value is appended to the old one).

Zephyrus handles the settings always exactly following the order of the command line arguments. This includes the settings declared in the setting files as all the setting files are included on with a command line argument (`-settings <settings-file-path>`). Also before doing anything else, Zephyrus reads the default settings file "default.settings" (if it exists).

So for example when you execute

```
./zephyrus.native -u u.json -settings my-params.settings -ic ic.json
```

what happens is: 

1. first Zephyrus will parse the "default.json" settings file and handle all the settings declared inside,
2. then it will handle the "-u" argument (assigning the value "u.json" to the "input_file_universe" setting),
3. after it will parse the settings file "my-params.settings" and handle all the setting assignments which are inside (possibly overriding the value of the "input_file_universe" declared by the command line parameter `-u` and / or override any other settings already declared in the "default.json" settings file),
4. and finally it will handle the "-ic" argument (assigning the value "ic.json" to the "input_file_configuration" parameter, possibly overriding the previous value if this setting was declared in any of the two parsed setting files).

Note: As we said before, in some cases a setting's value is not overriden, but extended. Details can be found below, in the part of this document describing each available setting. However if you plan on using this feature, you should check how it behaves in specific situations, because for now it was not tested thoroughly and there may be some strange things happening sometimes (e.g. in case of the "results" setting, corresponding to the `-out` command line argument, apparently all the values declared in the setting files are appended to each other correctly, but they override all the values declared as command line arguments, so only the `-out` arguments appearing after the last `-settings` argument will not be overriden, but appended to the parameter's value).

#####Setting files syntax

Zephyrus setting file syntax is quite straightforward as settings are basically key-value pairs.

Assigning a value to a setting takes form of a pair: the setting name and its value, divided by an equality sign:
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

Every setting can be assigned values of a certain type and there are five types available:

- Boolean
  - `true`, `yes` and `y` mean **true**
  - `false`, `no` and `n` mean **false**
- Integer
- String
- List: `[element_1; element_2; element_3; ... el_n]`
- Pair: `(element_1, element_2)`

Complex types (lists of pairs, pairs of lists, etc.) are also possible.

Important Note: As the selection of characters which are allowed in strings is very broad (and includes some parsing-sensitive characters like commas, semicolons, parentheses, etc.) in case of any doubt it is strongly recommended to put quotes or double quotes around your string unless you are sure it will not interfere with the parsing (like in simple cases like `solver = custom` or `input-file-universe = u.json`).

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

- Mode
  - The Zephyrus functioning mode.
  - The setting name: `mode`
  - Type: string
  - The command line argument: `-mode <mode>`
  - Values: {classic|validate|no-solving}
- Universe
  - The universe file.
  - The setting name: `input_file_universe`
  - Type: string
  - The command line argument: `-u <universe-file-path>`
- Initial Configuration
  - The initial configuration file.
  - The setting name: `input_file_configuration`
  - Type: string
  - The command line argument: `-ic <initial-configuration-file-path>`
- Specification
  - The specification file.
  - The setting name: `input_file_specification`
  - Type: string
  - The command line argument: `-spec <specification-file-path>`
- External Repositories
  - The external repositories files.
  - The setting name: `input_file_repositories`
  - Type: list of pairs of strings
  - Setting file syntax: `input_file_repositories = [(<repository-1-name>, <repository-1-file-path>), (<repository-2-name>, <repository-2-file-path>), ...]`
  - Usage: each pair declares a repository name (any string you want, it will be used by Zephyrus to identify and reference this repository) and a path to the file which describes this repository (in the *External Repository* format)
  - Example: `[("debian-squeeze", "repositories/DebianSqueeze.json"); ("debian-wheezy", "DebianWheezy.json")]`
  - The command line argument: `-repo <repository-name> <repository-file-path>`
  - Example: `-repo debian-squeeze repositories/DebianSqueeze.json -repo debian-wheezy DebianWheezy.json`
- Optimization Function
  - The optimization function choice.
  - The setting name: `input_optimization_function`
  - Type: string
  - The command line argument: `-opt <optimization-function>`
  - Values: {simple|compact|conservative|spread|none}
- Prefix package names
  - Prefix every package name with its repository name in the output.
  - The setting name: `append_repository_to_package_name`
  - Type: boolean
  - The command line argument: `-prefix-repos`
- Solver
  - The main constraint solver choice.
  - The setting name: `solver`
  - Type: string
  - The command line argument: `-solver <solver>`
  - Values: {none|g12|gcode|custom}
- Custom solver command
  - Define a custom command used to launch the external FlatZinc solver.
  - The setting name: `custom_solver_command`
  - Type: string
  - The command line argument: `-custom_solver_command "<command>"`
- Custom MiniZinc to FlatZinc conversion command 
  - Define a custom command used to launch the MiniZinc-to-FlatZinc conversion.
  - The setting name: `custom_mzn2fzn_command`
  - Type: string
  - The command line argument: `-custom_mzn2fzn_command "<command>"`
- Outputs
  - Which forms of output should Zephyrus produce and to which files.
  - The setting name: `results`
  - Type: list of pairs of strings
  - The command line argument: `-out <output-type> <output-file-path>`
- Debug printing
  - How much information should Zephyrus print.
  - The setting name: `verbose_level`
  - Type: integer
  - Values: {0|1|2|3}
- Debug printing
  - Should Zephyrus print the input data during execution.
  - The setting name: `verbose_data`
  - Type: boolean
  
#####Command-line only parameters

TODO: Reformat, fill information

- Settings
  - A settings file to parse.
  - The command line argument: `-settings <settings-file-path>`
- Debug PATH
  - Print what is the value of the PATH variable when Zephyrus executes and exit.
  - The command line argument: `-print-path`

####Classic Mode

TODO

####Validation Mode

In the **validation mode** Zephyrus checks if a given configuration is valid with respect to the given universe and specification. It does not generate the final configuration at all. Instead it will verify if the initial configuration satisfies all the constraints imposed on it by the given universe and specification. As result, Zephyrus will either output a list of violated constraints (as detailed as possible) or it will simply inform us, that the configuration is valid.

For example running Zephyrus in the validation mode with an invalid initial configuration:
```
./zephyrus.native -mode validate -u tests/u-1.json -spec tests/spec-1.spec -ic tests/c-1-incorrect.json
```

will tell us exatly which constraints this configuration violates:
```
ERROR: configuration validation error: dependencies of package "requirer_package" on location "location" are not satisfied
ERROR: configuration validation error: dependencies of package "provider_package" on location "location" are not satisfied
ERROR: configuration validation error: component's "Requirer-1" requirements on port "@port" are not satisfied (not enough bindings)
```

On the other hand if we change the used initial configuration to a valid one:
```
./zephyrus.native -mode validate -u tests/u-1-json-v1.json -spec tests/spec-1.spec -ic tests/c-1-correct.json
```

Zephyrus will assure us, that it is valid indeed:
```
Initial configuration has passed all checks and is completely valid!
```

Note: The universe constraints checking is implemented in a quite detailed way, so the information printed by Zephyrus that concern the violated universe constraints is rather precise and useful. Unfortunately doing the same thing with the specification constraints is a lot harder (due to the different and more complex nature of these constraints), so at this moment Zephyrus is only able to produce a very vague `specification constraints are not satisfied` message.

####No-solving Mode

TODO

####Input / Output files syntax

(Syntax of the Universe, Configuration, Specification and External Repository files)

TODO: Find a good syntax to describe the JSON structure of each file.

#####Universe

```
Universe ::=
{
  "version"         : integer
  "component_types" : component_type list
  "implementation"  : (component_type -> package name list) mapping
  "repositories"    : repository list
}
```

```
Component type ::=
{
  "name"    : <component name>,

  "provide" : {
                <port name 1> : <provide arity 1>,
                <port name 2> : <provide arity 2>,
                ...
                <port name n> : <provide arity n>
              },

  "require" : {
                <port name 1> : <require arity 1>,
                <port name 2> : <require arity 2>,
                ...
                <port name n> : <require arity n>
              },

  "conflict" : [<port name 1>, <port name 2>, ... <port name n>],

  "consume" : {
                <resource name 1> : <consume arity 1>,
                <resource name 2> : <consume arity 2>,
                ...
                <resource name n> : <consume arity n>
              }
}
```

```
Implementation ::=
{ 
  <component name 1> : package name list,
  <component name 2> : package name list
  ...
  <component name n> : package name list
}
```

```
Repository ::=
{
  "name"     : <repository name>
  "packages" : package list
}
```

```
Package ::=
{
  "name" : <package name>,
  "depend" : package name list list,
  "conflict" : [<package name 1>, <package name 2>, ... <package name n>],
  "consume" : {
                <resource name 1> : <consume arity 1>,
                <resource name 2> : <consume arity 2>,
                ...
                <resource name n> : <consume arity n>
              }
}
```

######Example:

```
{
  "version" : 1,
  "component_types": [
    {
      "name": "Provider",
      "provide": {"@port" : 3},
      "consume": {"resource" : 5}
    },
    {
      "name": "Requirer",
      "require": {"@port" : 2},
      "consume": {"resource" : 3}
    },
    {
      "name": "Conflicter",
      "conflict": ["@port"],
      "consume": {"resource" : 7}
    }
  ],
  "implementation": {
    "Provider" :
    [["repository", "provider_package"]],

    "Requirer" :
    [["repository", "requirer_package"]],
    
    "Conflicter" :
    [["repository", "conflicting_package"]]
    
  },
  "repositories": [
    {
      "name": "repository",
      "packages": [
        {
          "name": "provider_package",
          "depend": [["common_package"]],
          "consume": {"resource" : 1}
        },
        {
          "name": "requirer_package",
          "depend": [["common_package"]],
          "consume": {"resource" : 1}
        },
        {
          "name": "common_package"
        },
        {
          "name": "conflicting_package",
          "conflict": ["common_package"]
        }
      ]
    }
  ]
}
```

TODO

#####Configuration

TODO

#####Specification

TODO

#####External repository

TODO
