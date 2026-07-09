---
title: Bootstrap mirror pass-2 + wide-marking parity
status: open
priority: 2
issue-type: task
created-at: "2026-07-09T13:28:23.589159+02:00"
---

Mirror the native width-aware pass-2 lowering (habu2.f EM-COMPILE-P2WIDE / EM-P2-CARVE / EM-P2-LOCREF / EM-P2-TRIGGER family) and the interpret wide-marking publish-tail call (EM-REC-WIDE-PUBLISH) into bootstrap/cg/forth.fs + jit.fs. TFAM 12 item (4) verdict 2026-07-09: parity is VACUOUS today, proven by corpus scan — no SUMTYPE/PRODUCT/TYPEFAMILY/ENUM declaration exists in src/, lib/, tools/, or maki/ non-test source (rg '^SUMTYPE |^PRODUCT |^TYPEFAMILY |^ENUM ' = 0 rows; only the implementation words in src/core/sumtype.f and the dispatch mirrors), so no definition compiled by a Gforth-recovered engine can carry a wide width fact before the immediate native fixpoint refresh replaces it (docs/bootstrap.md recovery contract). The boundary rots the day a wide layout family lands in the recovery corpus: implement this parity BEFORE (or with) the first non-test SUMTYPE/PRODUCT declaration. No current gate exercises the bootstrap emitter (tools/bootstrap.sh recovery is the only exposure).
