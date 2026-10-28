---
title: Share T0 control in one label layer
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T17:58:01.134090+02:00"
---

Problem: the control and loop bookkeeping in src/habu/t0-arm64.f 617-795 copies src/habu/t0-x64.f 338-551 (L5b2 review finding). Acceptance: new src/habu/t0-label.f (package T0-LABEL) holds the CF stack (POSITION/CASE-START), loop levels (LV-HEAD/ENTRY/FIRST), LEAVES, and IF ELSE THEN BEGIN WHILE REPEAT UNTIL AGAIN CASE OF OF-LIT ENDOF ENDCASE LOOP-OPEN LEAVE LOOP-CLOSE plus head bookkeeping for LOOP-NEXT/LOOP-STEP. It calls per-arch branch primitives through per-slot cells: FWD-JUMP ( -- br ), FWD-ZERO ( vreg -- br ), FWD-NE ( vreg vreg -- br ), FWD-NE-LIT ( vreg n -- br ), FWD-GE ( vreg vreg -- br ), BACK-JUMP ( head -- ), BACK-ZERO ( vreg head -- ), RESOLVE ( br -- ), ENTRY-EQ ( br -- ), LOOP-NEXT ( vreg head -- ), LOOP-STEP ( vreg vreg head -- ). The x86 and arm64 INSTALLs store T0-LABEL's structure words into their seam slots; the seam keeps per-arch IF,.. cells (Wasm L9 is structured). Smallest mechanism. Files: src/habu/t0-label.f (new), t0-x64.f, t0-arm64.f. Verify: x86 bins identical to jerry-t0-l1/.out/dis-x64-l2b, arm64 bins identical to .out/dis-a64-l5b2, .out/t0-dis.f and .out/t0-dis-a64.f all PASS, .out/load-all.f rc 0, tools/error-code-lint.f 0. Depends: L5b2 (pqrqmqko). Ownership: those files. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-t0-l1 (new commit on pqrqmqko).
