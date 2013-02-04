.PHONY: default
default: opt

ATDGEN_SOURCES = hello.atd
ATDGEN_FLAGS = -j-std
include Atdgen.mk

SOURCES = \
resource_types_t.mli resource_types_t.ml resource_types_j.mli resource_types_j.ml\
helpers.mli helpers.ml\
typing_context.mli typing_context.ml\
variable_keys.ml\
generic_constraints.mli generic_constraints.ml\
capacity_constraints.mli capacity_constraints.ml\
binding_constraints.mli binding_constraints.ml\
max_rest_constraints.mli max_rest_constraints.ml\
specification_constraints.mli specification_constraints.ml\
matching_algorithm.ml\
constraints.ml\
resource_generation.ml\
facile_variables.mli facile_variables.ml\
facile_constraints.ml\
resource_types_input_facade.ml specification_input_facade.ml resource_output_facade.ml\
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