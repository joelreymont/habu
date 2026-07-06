---
title: Raise DICT-CAP or slice gate-runner-support so the whole DAG loads
status: open
priority: 2
issue-type: task
created-at: "2026-07-06T09:01:28.789474+02:00"
---

Follow-up to habu-standalone-support-load-7c3d9f16. RCA there PROVED (not E-LINT-TOKEN-CAP) that loading test/gate-runner-support.f's whole 53-file require list into ONE image dies rc 77 at the ENGINE dict-room exit ($4D, habu2.f C-COLON/C-TRUSTED room check writes the pending token then exit-groups; token.f's E-LINT-TOKEN-CAP is a catchable throw that merely shares the number 77). Evidence: after the first 42 requires NDICT = 8144 of DICT-CAP 8192 (src/habu/layout.f, 8192 constant DICT-CAP), and defining 120 plain dummy variables after those requires dies at #49 -- exactly the 48 free slots; require #43 (public-signatures-core.f, ~150 defs) crosses the ceiling. The dot's own goal (per-phase harness without the whole DAG) is met by per-entry standalone loadability (landed), so this is NOT blocking. Two options for actually loading the aggregate: (a) raise DICT-CAP -- an engine layout change (DICT-SIZE/CFSTK-OFF/dict-record region in src/habu/layout.f + habu1.f/habu2.f room checks and the snapshot/AOT dict image), needs the habu1/habu2 owner and a fixpoint rebuild; or (b) split test/gate-runner-support.f into per-family support entries (each requiring only its slice) and retire the aggregate usage line in test/gate-runner-lib.f. OWNER: habu1/habu2 for (a); test-harness owner for (b). Off-limits to the seal-hardening worker (habu1/habu2/checker). Not urgent -- no current caller loads the full aggregate standalone.
