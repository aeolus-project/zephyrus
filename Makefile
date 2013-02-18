.PHONY: default
default: opt

ATDGEN_SOURCES = aeolus_types.atd
ATDGEN_FLAGS = -j-std
include Atdgen.mk

SOURCES = \
aeolus_types_t.mli aeolus_types_t.ml aeolus_types_j.mli aeolus_types_j.ml\
helpers.mli helpers.ml\
aeolus_types_output_facade.ml\
configuration_output_facade.ml\
typing_context.mli typing_context.ml\
variable_keys.ml\
generic_constraints.mli generic_constraints.ml\
component_type_global_constraints.mli component_type_global_constraints.ml\
location_constraints.mli location_constraints.ml\
repository_constraints.mli repository_constraints.ml\
package_constraints.mli package_constraints.ml\
resource_constraints.mli resource_constraints.ml\
specification_constraints.mli specification_constraints.ml\
matching_algorithm.ml\
constraints.ml\
configuration_generation.ml\
facile_variables.ml\
facile_constraints.ml\
universe_input_facade.ml specification_input_facade.ml configuration_input_facade.ml\
gentlewestwind.ml

RESULT = gentlewestwind

PACKS = atdgen facile extlib batteries

# "include OCamlMakefile" must come after defs for SOURCES, RESULT, PACKS, etc.
include OCamlMakefile

.PHONY: sources opt all
sources: $(SOURCES)
opt: sources
	$(MAKE) native-code
all: sources
	$(MAKE) byte-code