all:
	ocp-build init
	ocp-build

install:
	for tool in tools/*; do $(MAKE) -C $$tool install; done

clean:
	ocp-build init
	ocp-build clean
	find . -name '*~' -exec rm -f {} \;

distclean: clean
	rm -rf _obuild
	rm -f autoconf/Makefile.config
	rm -f autoconf/config.status
	rm -f autoconf/config.log

