---
title: "Hand every refused row's name to its renderer"
status: active
priority: 3
issue-type: task
created-at: "2026-11-01T05:07:52.626231+02:00"
---

Problem: found by lane clrref's worker. Three refusals keep the same kind of record lane clrref removes for generates: rows, never cleared: PARSES-REFUSE and NAMES-REFUSE store the row's name in PRS-TOK-A/U (src/core/checker.f ~27140, ~27149, ~27251), TRUST-STALE and the qualified-record refusal in TSR-TOK-A/U and TSR-AT-A/U (~11865, ~11889, ~14686), the using refusals in USH-TOK-A/U and USH-PKG-A/U (~11893, ~13388, ~13395, ~16952, ~26946; ~13419 copies them into UHELD-TOK-A/U). render.f reads them (~1492-1606, ~1650-1700, ~1944-1978). A refusal caught before an image save leaves cells that differ from a build that never ran it. Acceptance: each refusal hands its names to its renderer as clrref did for generates: rows; the cells go; diagnostics byte-identical to before; probes for each code in plain text and --json-errors. Files: src/core/checker.f, src/core/render.f. Verify: rows load-reject-diag, native-gate-diagnostics, check-verify; the using rows. Depends: habu-clear-a-refused-5de0fec8 (lands the pattern). Ownership: src/core/checker.f, src/core/render.f.

Lead design: two kinds of record, two mechanisms that fit them. A record set only for the renderer call that follows it (PRS-TOK for parses:/names: rows, TSR-TOK/TSR-AT for the trust and qualified-record refusals) goes to the renderer as arguments, as lane clrref did for generates: rows, and its cells go. The using refusal's state is held by the walk until the scan's end raises it (USING-HOLD, RAISE, USING-SAVE into UHELD-*), so USH-* and UHELD-* move into a CHECKER-REG:SCRATCH{ … }SCRATCH span beside USING-HOLD, which SR-RETIRE zeroes at each file boundary and in a captured image. Dispatch on top of clrref once its review passes.

Also in scope (clrref review): GENR-BADSIG-PROSE reads SGBAD, SGBAD-A, SGBAD-U and SGBAD-KIND (src/core/checker.f, beside SGBAD-CLEAR). Those cells, with SGBAD-AR-DECL, SGBAD-AR-GOT and SGBAD-SIZE, are the checker's signature verdict, read by many words, not a record kept only for a renderer; they sit outside any CHECKER-REG:SCRATCH{ span and only the next SGBAD-CLEAR resets them. They join a SCRATCH span, as their saved copies SV-SGBAD-* already do.

Claim: agent=jerry workspace=.jj-ws.noindex/jerry-refrec (brief /tmp/jerry-big/refrec-brief.md). Lead refinement: TSR-TOK/TSR-AT go into a SCRATCH span, not arguments: RECORD-DIAG-XT stays one selector hook (pre-trust slots), and its three diagnostics read different names.
