---
title: Size the capture tables from the record count, not AOT-REC-MAX
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T13:52:58.158994+03:00"
---

Problem (audit of 4cad8774): doubling AOT-REC-MAX to 32768 silently doubled three static create...allot tables sized from it, +524,288 bytes of build-host DP in every engine that loads aot-decl.f/aot-capture.f: ACAP-NAMED-BIT (aot-capture.f:825, 131,072 -> 262,144), AOT-SPAN:BUF (aot-decl.f:494-496, +131,072), AOT-SIG-BUF (aot-decl.f:546-547, +262,144), and the REC-STORAGE reserve 1,114,136 -> 2,228,272. They are zero at capture so they cost the image only bitmap/run bytes, but they consume DATA address space under DATA-START/DATA-SIZE and the commit body did not state the cost. No hard limit crossed: DICT-CAP 65536 (layout.f:275), AOT-SECTION-CAP $1E00000, 32768 fits an imm16. Acceptance: the tables that only the capture uses are allocated at capture time from the live record count (the allocate-don't-size class), or the fixed reserve is justified in a comment with its measured bytes and the engine-size tool charges it by name; engine prefix DATA before/after in the commit body; gen2 == gen3. Files: src/habu/aot-decl.f, src/habu/aot-capture.f, tools/engine-size.f. Verify: tools/engine-size.f, test/gate-aot-image.f, three generations, test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
