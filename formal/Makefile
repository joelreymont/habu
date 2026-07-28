# Single entry point for the Rocq proofs: `make -C formal`.
#
# _CoqProject is the only place the logical mapping and the source order are
# written down. `rocq makefile` turns it into the real build rules, so the
# generated makefile is a build output and is never committed. Editing
# _CoqProject regenerates it.

ROCQ_MAKEFILE := Makefile.rocq

all: $(ROCQ_MAKEFILE)
	$(MAKE) -f $(ROCQ_MAKEFILE)

clean: $(ROCQ_MAKEFILE)
	$(MAKE) -f $(ROCQ_MAKEFILE) cleanall
	rm -f $(ROCQ_MAKEFILE) $(ROCQ_MAKEFILE).conf .lia.cache

$(ROCQ_MAKEFILE): _CoqProject
	rocq makefile -f _CoqProject -o $(ROCQ_MAKEFILE)

.PHONY: all clean
