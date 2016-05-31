
include autoconf/Makefile.config

all: ocp-build-build

install:
	for tool in tools/*; do $(MAKE) -C $$tool install; done

clean: ocp-build-clean
	find . -name '*~' -exec rm -f {} \;

distclean: clean ocp-distclean

include autoconf/Makefile.rules

