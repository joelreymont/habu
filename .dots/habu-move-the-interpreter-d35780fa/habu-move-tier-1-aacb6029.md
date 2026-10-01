---
title: Parse the colon head in Habu
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.655350+03:00"
closed-at: "2026-10-01T08:44:23.737673+03:00"
close-reason: "done: src/habu/definers.f reads the `:`/`kernel:`/`trusted:` head and the tier-1 body capture; bin/hb --load test/outer-interpret.f agrees with the engine on 109 cases (28 new head cases) plus the tier-0 Habu-only refusals, ThinkPad and spark."
blocks:
  - habu-add-the-engine-ebe5d757
  - habu-move-pkgs-using-22f18b81
---

Problem: colon definitions start in the assembly interpreter (`habu2.f:7613-7660`, `EM-INTERPRET-COLON`). First of the five tier-1 compile-mode leaves I5a-e.
Acceptance: `:`/`:trusted`/`TRUSTED:`/`CAST:` name parse, the qualified-name rules (`C-QUALIFY-DEF`), the pending record, signature capture (`C-COLON-MAYBE-SIG`), the per-definition state reset and the tier dispatch cell, as `habu2.f:7613-7660` does, in `src/habu/definers.f`.
Files: `src/habu/definers.f`, `src/habu/outer.f` (compile-mode dispatch), cases beside `test/outer-interpret.f`.
Verify: spark: colon-head cases through the Habu loop under the feature cell; gate.
Depends: habu-scan-and-interpret-eea996a2 (I4).
Route: Alder (shared: src/habu/definers.f, src/habu/outer.f and the test).
Ownership: krait (Intel lane).
Claim: krait.

Design corrections (2026-09-30; override the lines above where they differ):
- Keywords: `:` (the one byte), `kernel:` and `trusted:` (A-Z folded; `habu2.f:7819-7868`, `3814-3849`). `:trusted` is not a keyword; `cast:` moves to I5e.
- Depends: I4c (`habu-add-the-engine-ebe5d757`: `def-open`, `body-append`, `trust-sig!`, `namespace-record`) and I8 (interpret.f, TASK-GUARD, PROTECTED?, namespace-row helpers). Habu only. Files: `src/habu/definers.f` (reopens `package OUTER`, requires outer.f and packages.f, `DEF-` prefix), `src/habu/interpret.f` (STEP runs COMMENT? `COMPILING?` LITERAL? PACKAGE? `DEFINE?` DISPATCH), `test/outer-interpret.f`.
- Head, in the engine's order: (1) tier 0 refuses first: `hb: tier 0 is not in the Habu loop: ` plus the token, THROW-AT 76 (I6 replaces this); (2) TASK-GUARD; (3) $4C, then $4D, naming the keyword token (`7830-7836`); (4) no name: `hb: : missing definition name after ` for `:`/`kernel:`, OPERAND's text for `trusted:`, both $4A; (5) BODYLEN := 0, then `body-append` of the token; (6) the qualifier (`3570-3642`): SEAL-GUARD; DEF-TKA/DEF-TKL := token; split at the first colon; a second colon is $4B; look up the namespace row, else `false namespace-record`; (7) the compile-keyword wall: EM-COMPILE-KEYWORDS rows (`9538-9543`), folded, rc 70; (8) $4D, then dup $4E; (9) PROTECTED? after the seal: `hb: cannot publish into protected word: ` + DEF-TKA + newline, exit 84; (10) `def-open` (tail, wid, 0); (11) TRUSTED-CELL := 1 for `trusted:`; (12) the signature (`2958-2982`): optional for `:`; for `trusted:` a missing one is the token plus 76; `trust-sig!` gets the inner span, `body-append` gets `( … )`; (13) a zero XT-CELL dies AOT-SEED with `hb: native compiler dispatch unset`.
- Tier-0 work belongs to I6 (P2-nesting refusal, JIT resets); the unit def-name guard to I8b. No Habu code holds a PROT window.
- Body tokens: with PEND set and DEF-TIER 1 each token is `body-append`ed, as the engine's tier-1 LBCAP (`habu2.f:7759-7760`); with DEF-TIER 0 the tier-0 refusal. Nothing compiles until I5e's `;`, so I5a lands alone with no stub. LBCAP/LBCS and the rc-71 refusal (`10107-10118`) move here from I5b.
- Verify: `bin/hb --load test/outer-interpret.f`: every refusal above; a pending head dumped at exit by a prelude exit hook (name, wid, flags, [0] = OPEN-CELL, BODYBUF, TSIG, TRUSTED, DEF-TIER, a long name's code-origin); a qualified name into a prelude package and a new one; names with a colon at either edge; `: IF`; a body past 8000 bytes (71); `cp!` near the ceiling ($4C). Tier 0 asserted on the Habu route only. Gate.
- Pre-change: `:` at end of input: engine rc 74, Habu loop `E-UNDEFINED: :` rc 70.
- Not determined by the design: who sets TIER-CELL to 1 at x86 boot (x86 `set-tier` stores only 1, `kernel-x64.f:2591`; no boot writer found); whether `DICT-CAP ndict!` can reach $4D in tests.
