.PHONY: default
default: opt

ATDGEN_SOURCES = aeolus_types.atd
ATDGEN_FLAGS = -j-std
include Atdgen.mk

SOURCES = \
helpers.mli helpers.ml\
aeolus_types_t.mli aeolus_types_t.ml aeolus_types_j.mli aeolus_types_j.ml\
aeolus_types_output_facade.ml configuration_output_facade.ml\
universe_input_facade.ml specification_input_facade.ml configuration_input_facade.ml\
typing_context.mli                    typing_context.ml\
variable_keys.mli                     variable_keys.ml\
solution.mli                          solution.ml\
generic_constraints.mli               generic_constraints.ml\
component_type_global_constraints.mli component_type_global_constraints.ml\
location_constraints.mli              location_constraints.ml\
repository_constraints.mli            repository_constraints.ml\
package_constraints.mli               package_constraints.ml\
resource_constraints.mli              resource_constraints.ml\
specification_constraints.mli         specification_constraints.ml\
optimization_functions.mli            optimization_functions.ml\
matching_algorithm.ml\
constraints.ml\
configuration_generation.ml\
variables.mli          variables.ml\
facile_variables.mli   facile_variables.ml\
facile_constraints.mli facile_constraints.ml\
minizinc_constraints.ml\
specification_parser.mly specification_lexer.mll\
universe_trimming.mli universe_trimming.ml\
zephyrus.ml

RESULT = zephyrus

PACKS = atdgen facile extlib batteries

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