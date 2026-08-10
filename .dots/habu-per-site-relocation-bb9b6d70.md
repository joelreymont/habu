---
title: Per-site relocation record for AOT capture
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T16:56:23.143178+02:00"
---

Follow-up from the literal-split landing (6856f799): the emission-side separation is complete, but aot-capture.f's value-range scans (ACAP-SCAN-DATA/CODE) remain as the backstop because fully removing them needs an explicit per-site relocation record consumed at capture - a runtime recording table written at emit time (site offset + kind), reset per capture window in stdin.f, mirrored in forth.fs, serialized carefully in the fixpoint-critical AOT area. The register-kind-marker alternative was evaluated and REJECTED as value-fragile (an incidental 4-chunk scalar into x9 would collide with DATA recognition). With the record in place the scans retire and the linker chain-reject stays the only heuristic.

SCOPE WIDENED 2026-08-10 (quotations design lane, ruling by the cut owner):
this leaf now explicitly owns BOTH shape gaps in AOT capture, code AND data.
(1) ['] of a named word from the chain needs the engine's canonical x9
four-chunk absolute carrier plus an ADDRMAP bit set at final placement
(a new RELOC-ADDRS in publish.f mirroring RELOC-CALLS, fed by emitter-recorded
site indices, never byte-decoding) - 1 census definition, deferred until this
lands. (2) LATENT ON MASTER: the chain already emits DATA address literals in
MATERIALISE's minimal shape (string bodies via NSTR, fixed words), which
ACAP-SCAN-DATA cannot see (it recognises only the x9 four-chunk shape); this
bites the moment chain-compiled code enters an AOT capture window, which the
: cut makes true. Ordered BEFORE THE CUT, as already ruled on the strings
tranche. Quotation bodies themselves need NOTHING here (PC-relative adr,
P_pc_relative_adr class).
