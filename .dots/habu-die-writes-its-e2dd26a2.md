---
title: die writes its message without a trailing newline
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T08:05:57.076845+03:00"
---

Problem (seen in the adr-name lane's Tender probe, /tmp/hazel-adr-name/tender.log): die (src/habu/habu1.f BDIE) writes its span to stderr and exits without a newline, so a parent that reports the child's exit on the same stream continues the line: 'icode: adr out of reach site=844 target=1363312 delta=1362468 limit=1048576habu: standalone build: exit 72; argv=...'. Acceptance: decide the layer - die ends the message with one newline (and a call site that already passes one is found by rg and loses it), or the child-exit reporter in tools/hb-build-lib.f starts on a fresh line when the child's last stderr byte was not one; a fixture runs a dying child and pins both lines separately; test/run.f green. Files: src/habu/habu1.f or tools/hb-build-lib.f, tools/hb-build-test.f. Depends: none. Ownership: hazel. Claim: unassigned.
