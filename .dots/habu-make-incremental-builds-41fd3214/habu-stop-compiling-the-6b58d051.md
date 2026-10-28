---
title: Stop compiling the image writer per build
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T14:59:32.118982+03:00"
---

Problem: every product build compiles tools/native-emit.f from source to write the image (READ-SOURCE-WRITER / LOAD-SOURCE-WRITER, tools/native-build-core.f ~:962-1015): 0.77 s of a 6.8 s warm engine build. Evidence: `hb-build --engine` builds timed with `/usr/bin/time -l`. Acceptance: the writer is part of the support image (or otherwise compiled once per support image) and no product build parses native-emit.f; warm engine build wall drops by about that time; products byte-identical. Files: tools/native-build-core.f, tools/native-build-image.f. Verify: warm engine build timing before/after, two-generation, test/native-file-auto-e2e.f. Depends: none. Claim: unassigned.

Measured 2026-10-27 (lane W1, change luxzrnrz commit 58430597, retired unlanded: the tier-1 selection is obsolete under tier 0 everywhere): loading the writer at tier 1 through the cache (A1; LOAD-SOURCE-WRITER selects tier 1 and restores, because NATIVE-BUILD-SUPPORT:RUN reaches it at tier 0) removes the tier refusal, but all 38 writer files still refuse: 26 earlier-write-before-load, 12 earlier-write (word=SEQ, PTRA-BOOT, TRAIL-BOOT). Each edit build recompiles all 38: memory.f edit with diagnostics 186 s vs 117 s base, without diagnostics +7.6 s CPU; cold +23 s CPU. Products stay byte-identical. A1 does not land until the writer files stop writing earlier DATA (SEQ, PTRA-BOOT, TRAIL-BOOT); those writes are the blocker, with the data-base holders of aa3c25a5.
