---
title: Keep same-source Kaizen AOT bytes stable
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T08:14:45.931832+03:00"
---

Phil reports consecutive builds on qualified public 600eb464-4849828a with the same host/cwd and equal-length outputs differ in two ARM64 ADR data-address immediates near file offsets 61837 and 62405, plus the code-directory page hash. Reduce the exact source/data owners before changing capture or linking. Reproducer and binaries: /private/tmp/claude-501/-Users-joel-Work-kaizen/8f2e8a6d-8f0e-4872-a0d3-b14b0341a05d/scratchpad/rep.sh, rep1/opc-name, rep2/opc-name. Existing stripped-cache same-source equality is the acceptance path; preserve runnable results and genuine pointer refusals. No Kaizen workaround.
