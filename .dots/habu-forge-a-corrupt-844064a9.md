---
title: Forge a corrupt baked protected-WID band
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T01:15:55.998700+02:00"
---

test/aot-wid-suite.f lost master's corrupt-registry probe in the proofs/master merge. Master's variant forged AOT-PWID-N (the table-era count cell), which the bitmap format deleted. The bitmap's two invalid shapes are a frame whose LAOTNPWID tag is not PROT-REG-TAG and a band with bit 0 set; EMIT-AOT-PROT-RESTORE (src/habu/habu2.f) rejects both with exit ENGINE-ERROR:AOT-SEED and 'hb: AOT protected-WID corrupt' + newline, and nothing exercises that path today. The test/aot-wid-build.f fixture cannot produce one by injecting after CAPTURE-REPL: the maker hb-pwid-mk is itself emitted from the injected source, so a corrupt band kills the maker run (RUN-MAKER BF-RC0) before hb-pwid exists. Needs a bake-time forge hook that corrupts only the FINAL image, or a separate one-shot emitter, plus the suite probes master had: build succeeds, engine exists, running it exits ENGINE-ERROR:AOT-SEED, stderr contains the named diagnostic, and the diagnostic ends in a newline.
