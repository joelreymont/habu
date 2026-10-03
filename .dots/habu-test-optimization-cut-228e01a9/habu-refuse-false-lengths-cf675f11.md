---
title: Refuse false lengths at artifact, trust and intern
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T05:11:02.508262+02:00"
---

Problem (found by the r4-wrap-src lane on muwwmxlv 2cf4008b, dot 97c6ef4c's census): (1) src/habu/aot-file.f:865 RESTORE-CLOSURE guards with CUR @ 8 + pu + CLEN @ >, pu read from the artifact; a forged artifact (payload digest recomputed) with negative pu dies in AOT-IDENT:PATH+'s BYTE-COPY (rc 76) and a wrapping one at PATH+'s cap (rc 74), never with the reader's own refusal. (2) AOT-IDENT:PATH+ does not refuse a negative length; BYTE-COPY catches it. (3) trust with a name length of the maximum cell hangs in CHECKER-COLON-SCAN (rc 124 under timeout 20). (4) src/compiler/ir/symbol.f IR-SYMBOL:INTERN hashes all u bytes before ROOM-CK (:455), whose sum also wraps through BYTES>CELLS, so a near-max length crashes in HASH. Acceptance: each length is refused at the layer that receives it, before any read, with that layer's error (RESTORE-CLOSURE: 'the closure list ends inside a path'); a case per path written first and seen to crash, hang or die in the wrong layer (an artifact forger for (1)); no out-of-bounds read or hang. Files: src/habu/aot-file.f, src/habu/aot-ident.f (or where PATH+ lives), the trust path in src/core/checker.f, src/compiler/ir/symbol.f, tests. Verify: the new cases, aot artifact round-trip, checker trust rows, IR symbol rows, convergence and two-generation build. Depends: habu-audit-the-wrapping-97c6ef4c (r4-wrap-src). Ownership: false lengths at these four entries.

Also (r4-wrap2 lane, same class): trust-raw (src/core/checker.f:14294) and trust-decl (:14211) did not check the name length as trust (:14267) does: a -1 name recorded rc 0, a max-cell name died rc 76 in TOKFOLD. Folded into this dot's commit: one name guard for all three trust words.
