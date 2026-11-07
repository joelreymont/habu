---
title: Stop the stripped AOT scan refusing byte data by mapping layout
status: open
priority: 1
issue-type: task
created-at: "2026-10-31T19:53:59.141641+02:00"
---

kevin 2026-10-31: hb-build's stripped AOT persistent-data scan refuses Tender's server image in 2 of 6 identical builds: 'stripped AOT persistent data holds a pointer into memory the build mapped word=<unknown> data-off=18452731 value=4311810305' (0x0101010101 at an odd offset), exit 70. SCAN-DATA-CELL (src/habu/aot-common.f:178) appears to read a cell at byte offsets over byte data with no owner; AOT-CELL:MAPPED? (aot-data-cell.f:294) is true only when the value lands in a PROC-MAPS area, so the result follows ASLR. Fix at the root (which data is scanned as cells is decided by structure, not by the value or the mapping layout), with a regression test that fails deterministically. kevin makes the change in .jj-ws.noindex/kevin-aotscan and hands it to carl.

Census 2026-11-04: fix now. Tender's build hits it today, and tier 1 owns this path until the stripped image moves to T0, so DECISIONS "tier 1 bugs before Wasm moves to T0" allows the tier-1 fix.

2026-11-07 (kevin, again): one ERROR:CODE with a 96-byte text fails a stripped build deterministically (exit 70, value 0x600000000000, the text-length cell read through a window 5 bytes below it; 95 bytes builds); FFI declaration rows (2<<32, 3<<32) and json.f JW-ESC-W (0x101010101) do the same by ASLR. Reproducer and probes: /tmp/kevin-habu-aot-undeclared.md. Approach decided by the lead under Joel's night words ("make your own judgement and do everything without asking me. apply the what's best for long term criteria"), following DECISIONS "typed data" ("Whether data can hold an address is decided by its declared type, not by how it was built or by its value"): the scan stops reading undeclared bytes through byte-shifted windows; SCAN-DATA-CELL's reason for value-scanning raw storage, the `0 set-check` window (aot-common.f :170), is gone (DECISIONS "the checker's switch"). kevin first establishes, with path:line, whether checked code can still put a code, dictionary or mapped pointer into undeclared data without a cast or declaration; if not, undeclared bytes are never read as cells; if so, he stops and reports (the hole closes at the checker, a design for the lead). Not the fix: declaring spans in error.f, the FFI rows or json.f. Regression test: mini96.f's case through hb-build, failing deterministically before the fix. Claim: agent=kevin workspace=.jj-ws.noindex/kevin-aotscan; hands the change id to carl for gating.
