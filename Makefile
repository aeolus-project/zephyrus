# (****************************************************************************)
# (*                                                                          *)
# (*    This file is part of Zephyrus.                                        *)
# (*                                                                          *)
# (*    Zephyrus is free software: you can redistribute it and/or modify      *)
# (*    it under the terms of the GNU General Public License as published by  *)
# (*    the Free Software Foundation, either version 3 of the License, or     *)
# (*    (at your option) any later version.                                   *)
# (*                                                                          *)
# (*    Zephyrus is distributed in the hope that it will be useful,           *)
# (*    but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
# (*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
# (*    GNU General Public License for more details.                          *)
# (*                                                                          *)
# (*    You should have received a copy of the GNU General Public License     *)
# (*    along with Zephyrus.  If not, see <http://www.gnu.org/licenses/>.     *)
# (*                                                                          *)
# (****************************************************************************)

.PHONY: default
default: opt

ATDGEN_SOURCES = aeolus_types.atd
ATDGEN_FLAGS = -j-std
include Atdgen.mk

SOURCES = \
helpers.mli helpers.ml\
 \
aeolus_types_t.mli aeolus_types_t.ml aeolus_types_j.mli aeolus_types_j.ml\
aeolus_types_output.ml\
 \
typing_context.mli                    typing_context.ml\
 \
configuration_output.ml\
universe_input.mli                    universe_input.ml\
specification_parser.mly              specification_lexer.mll\
specification_input.mli               specification_input.ml\
configuration_input.mli               configuration_input.ml\
 \
variable_keys.mli                     variable_keys.ml\
solution.mli                          solution.ml\
generic_constraints.mli               generic_constraints.ml\
 \
component_type_global_constraints.mli component_type_global_constraints.ml\
location_constraints.mli              location_constraints.ml\
repository_constraints.mli            repository_constraints.ml\
package_constraints.mli               package_constraints.ml\
resource_constraints.mli              resource_constraints.ml\
specification_constraints.mli         specification_constraints.ml\
optimization_functions.mli            optimization_functions.ml\
matching_algorithm.ml\
constraints.mli                       constraints.ml\
 \
configuration_generation.mli          configuration_generation.ml\
variables.mli                         variables.ml\
 \
facile_variables.mli                  facile_variables.ml\
facile_constraints.mli                facile_constraints.ml\
 \
flatzinc_solution_parser.mly          flatzinc_solution_lexer.mll\
minizinc_constraints.mli              minizinc_constraints.ml\
 \
universe_trimming.mli                 universe_trimming.ml\
solvers.mli                           solvers.ml\
zephyrus.ml

RESULT = zephyrus

PACKS = atdgen facile extlib unix
#batteries

# "include OCamlMakefile" must come after defs for SOURCES, RESULT, PACKS, etc.
include OCamlMakefile

.PHONY: sources opt all dist
sources: $(SOURCES)
opt: sources
	$(MAKE) native-code
all: sources
	$(MAKE) byte-code

dist: clean
	./dist_make.bash
