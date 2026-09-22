---
title: Refuse a quotation past ADR reach of its parent by name
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T07:28:42.454829+03:00"
---

Problem (found by the ADR reach study, unmeasured): the native compiler emits ADR for a quotation's code address (src/compiler/native/emit.f:1168-1179, select.f:2617-2625; 21-bit form a64ir.f:183). In a stripped image the quotation and its parent are separate closure members and COPY-COMPACT-BLOB places them independently, so at Tender scale the pair can be more than 1 MiB apart and RELOC-W32's ADR arm refuses 'aot: ADR target out of range' (src/habu/aot-lib.f:503-506, 569-571) - by its own name, but with no site, and after the four startup ADRs (fc5a1dba) are converted this becomes the next reach wall a large application meets. Acceptance: measure whether compaction keeps a quotation adjacent to its parent (PLAN-BLOBS order) or can separate them; if it can, either keep them adjacent by construction or address the quotation with the long form (ADRP+ADD, the code-address literal scheme aot-lib.f:605-614), and the refusal names its site and target like MAP-TARGET!'s; a fixture or a synthetic relocation case pins it. Files: src/habu/aot-lib.f, src/compiler/native/emit.f, src/compiler/native/select.f, tools/hb-build-test.f. Verify: tools/hb-build-test.f; test/run.f. Depends: none - habu-reach-the-adr-fc5a1dba landed (1d46de1f): the startup's four sites now go through aot-lib.f TEXT-ADR, (LOFF, + ADR LTEXT + ADD), so this quotation ADR is the next reach wall; the same base+offset form, or adjacency by construction, is the choice here. Ownership: hazel. Claim: unassigned.
