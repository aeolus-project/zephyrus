
VERSION=0.1
NAME=zephyrus

CFLAGS=-g -O2 -fPIC -Wall -pedantic -Werror -Wno-long-long
LDFLAGS=-fstack-protector

OCAMLFIND=ocamlfind

DESTDIR =
ifeq ($(DESTDIR),)
exec_prefix=/usr/local
BINDIR=${exec_prefix}/bin
LIBDIR=$(shell ocamlfind printconf destdir)

INSTALL=$(OCAMLFIND) install -destdir $(LIBDIR)
UNINSTALL=$(OCAMLFIND) remove -destdir $(LIBDIR)
else
LIBDIR = $(DESTDIR)/$(shell ocamlc -where)
BINDIR = $(DESTDIR)/usr/bin

INSTALL = $(OCAMLFIND) install -destdir $(LIBDIR)
UNINSTALL = $(OCAMLFIND) remove -destdir $(LIBDIR)
endif

OCAMLBEST=native
OCAMLBUILD=ocamlbuild #-ocamlc ocamlc.opt -ocamlopt ocamlopt.opt -ocamldep ocamldep.opt
INSTALLOPTS=-s

TARGETS= \
   zephyrus.$(OCAMLBEST) 

BYTELIBS=
OPTLIBS=
CMXSLIBS=
CMXSLIBS=
ALIBS=

DIST_DIR = $(NAME)-$(VERSION)
DIST_TARBALL = $(DIST_DIR).tar.gz

OBFLAGS := -j 10 -classic-display
#OBFLAGS := $(OBFLAGS) -tag debug -tag profile
#OBFLAGS := $(OBFLAGS) -classic-display

all: $(CAMLP4CMXS) $(BYTELIBS) $(ALIBS) $(OPTLIBS) $(CMXSLIBS) man
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

fast: $(CAMLP4CMXS) $(OPTLIBS)
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

apps:
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

DOSELIBS = _build/doselibs

clean:
	$(OCAMLBUILD) -clean

distclean: clean

test: 

# stuff not not put in a distribution tarball
DIST_EXCLUDE = 

INSTALL_STUFF_ = META
INSTALL_STUFF_ += $(wildcard _build/*.cma _build/*.cmi)
INSTALL_STUFF_ += $(wildcard _build/*.cmxa _build/*.cmxs)
INSTALL_STUFF_ += $(wildcard _build/*.a)
INSTALL_STUFF_ += $(wildcard _build/*.mli)

exclude_libs =
INSTALL_STUFF = $(filter-out $(exclude_libs), $(INSTALL_STUFF_))

install: META
	test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	test -d $(LIBDIR)/stublibs || mkdir -p $(LIBDIR)/stublibs
	$(INSTALL) -patch-version $(VERSION) $(NAME) $(INSTALL_STUFF)

uninstall:
	$(OCAMLFIND) remove -destdir $(LIBDIR) $(NAME)

dist: ./$(DIST_TARBALL)
./$(DIST_TARBALL):
	if [ -d ./$(DIST_DIR)/ ] ; then rm -rf ./$(DIST_DIR)/ ; fi
	if [ -d ./$(DIST_TARBALL) ] ; then rm -f ./$(DIST_TARBALL) ; fi
	mkdir ./$(DIST_DIR)/ ; git archive --format=tar HEAD | tar -x -C ./$(DIST_DIR)/ ; \
	for f in $(DIST_EXCLUDE) ; do rm -rf ./$(DIST_DIR)/$$f; done
	tar cvzf ./$(DIST_TARBALL) ./$(DIST_DIR)
	rm -rf ./$(DIST_DIR)
	@echo "Distribution tarball: ./$(DIST_TARBALL)"

changelog:
	dch -c CHANGES --package $(NAME) -v $(VERSION)

credits:
	@git log --pretty=format:'%aN        %aE' | LC_ALL=C sort -u | awk -F'\t' '{printf("\t%s <%s>\n",$$1,$$2)}';

doc:
	$(OCAMLBUILD) $(OBFLAGS) zephyrus.docdir/index.html

man:
#	cd doc/manpages && $(MAKE)

.PHONY: all opt clean top-level headers test tags install uninstall dist doc man
