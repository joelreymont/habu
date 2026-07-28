---
title: Pin IR-ID public surface in parity gate
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T16:00:36.049032+02:00"
---

Full context: destruction review finding 2, HIGH but mitigated by a sibling gate. Adding a public ': RESTART-SERIALS ( -- ) NEXT-SERIAL 0 swap ! ;' to src/compiler/ir/id.f passes test/compiler/ir-id-proof.f at exit 0 - the frozen-state check only requires exactly one occurrence of the ADJACENT token pair 'NEXT-SERIAL !', and a token between name and store evades lexical adjacency; the Rocq model has no Store action so it cannot represent a non-CAS writer, and the uniqueness theorem's sole-writer scope assumption is nowhere stated or checked. A public forging 'CAST: FORGE-SOURCE ( n -- IR-ID:ir-source-id ) ;' also passes the parity gate. MITIGATION: test/compiler/ir-id.f's IR-ID-AUDIT:PUBLIC-SURFACE pins the public word count and names and catches both (exit 1), so the composite holds - but the binding dot's focused command names ir-id-proof.f ALONE, so the claim as stated is false, and a PRIVATE helper called from unfrozen code evades both (that is the NEW-MODULE dot). Required result: the parity gate itself pins the IR-ID public surface (names and count, read from the live registry the way the family list already is) and rejects any writer to NEXT-SERIAL outside the frozen TRY-SERIAL body - structural, not lexical adjacency. Also state the sole-writer assumption explicitly in the generated obligations header. Acceptance: both escapes above fail ir-id-proof.f alone; gate green unmutated; existing mutations still fail.
