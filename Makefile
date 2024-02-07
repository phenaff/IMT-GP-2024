SUBDIRS := Module-*/ TP
TARGETS = pdf clean

$(TARGETS):
	@for d in $(SUBDIRS); do \
		cd $$d ; \
		$(MAKE) $@ ; \
		cd .. ; \
	done
