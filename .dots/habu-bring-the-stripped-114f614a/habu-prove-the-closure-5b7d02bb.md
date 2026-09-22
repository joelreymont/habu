---
title: Prove the closure walker shakes at word grain
status: open
priority: 1
issue-type: task
created-at: "2026-09-22T11:30:53.090387+03:00"
---

Problem: Joel (2026-09-22): 'dead code should be eliminated during tree shaking. this is not being done correctly' - unproved either way. Probe: a stripped program whose MAIN calls one word of lib/string.f, sized against the 65,728 B stripped hello (~/.cache/tender/habu-gaps/stripped-determinism/hello-a): growth by about one word means word granularity, growth by the file or the package means the walker keeps more than MAIN reaches; then tools/hb-build-report.f's per-package split of Tender's bin/tenderd 1,783,176 code bytes, naming the packages that own them. Acceptance: both measurements recorded with the engine sha, the walker's granularity stated, the per-package table for bin/tenderd; a walker defect gets its fix and bin/tenderd is re-measured. Verification: the probe images and the report output under ~/.cache/tender/habu-gaps/closure-grain/, then Tender's python3 scripts/habu.py build --server. Ownership: alder. Claim: agent=alder workspace=.jj-ws/alder-size-probes.
