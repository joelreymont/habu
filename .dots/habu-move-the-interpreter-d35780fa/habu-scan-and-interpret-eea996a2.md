---
title: Scan and interpret in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.646748+03:00"
blocks:
  - habu-provide-the-interpret-32e92f89
  - habu-find-dictionary-names-e8f56969
  - habu-parse-numbers-in-86b27302
---

Problem: the token scanner and interpret loop are assembly (`LMAIN`, the `habu2.f:4370` top-row hook block).
Acceptance: token scan over `INP`/`INE`, comments, the depth-floor guard (`E-UNDERFLOW`), the min-in guard (`DNAME-MIN-IN`), the `DNAME-WIDE`/`DNAME-INT` fail-closed refusals, the top-row hook events (`habu2.f:4370` block), execute; drives a `--load` of a test file behind a feature cell (not yet the default).
Files: `src/habu/outer.f`, `test/outer-interpret.f`.
Verify: spark `bin/hb --load test/outer-interpret.f`; a whole `--load` of a test file through the Habu loop under the feature cell.
Depends: habu-find-dictionary-names-e8f56969 (I2), habu-parse-numbers-in-86b27302 (I3).
Route: Alder (shared: src/habu/outer.f, test/outer-interpret.f).
Ownership: krait (Intel lane).
Claim: unassigned.

Preflight corrections (these override the lines above where they differ):
- Scope: the per-token loop over numbers and records. The literal keywords (`s"`, `c"`, `."`, the escaped forms, `char`, `'`) are I4b (`habu-interpret-literal-keywords-0fa50d62`); the guards the loop reads are I4a (`habu-provide-the-interpret-32e92f89`), which this leaf depends on.
- Base: `intel/habu-find-dictionary-names-e8f56969` (I2 on I3), rebased on I4a once I4a lands. Workspace `.jj-ws/habu-scan-and-interpret-eea996a2`.
- Seam (the "feature cell"): `src/core/include.f` gains, in `SOURCE-ROOT` beside `INCLUDE-EVALUATE` (`include.f:682`), `defer INCLUDE-INTERPRET ( ptr u8 n -- )` bound by an installer word `[: INCLUDE-EVALUATE ;] is INCLUDE-INTERPRET` (shape `include.f:1077-1080`); `LOAD-CURRENT` (`include.f:880`) calls it. It is a `SOURCE-ROOT` name because `include.f` refuses an 85th global (`include.f:1070`). New `test/outer-loop-on.f`: `require src/habu/outer.f`, then `[: OUTER:INTERPRET ;] is SOURCE-ROOT:INCLUDE-INTERPRET`; every file after it on a `--load` line loads through the Habu loop. Pre-change failing check: `bin/hb --load test/outer-loop-on.f` refuses `hb: is: no deferred word named`, rc 70. I9a retires the defer when `evaluate` becomes the one loop.
- `OUTER:INTERPRET` is `TRUSTED: ( ptr u8 n -- )` (a dynamic stack, as `INCLUDE-EVALUATE`): save `INP-CELL`, `INE-CELL` and `SRCLOC:INB-CELL`, set them as B-EVAL does (`habu1.f:1545-1547`), and run the loop under `finally`, restoring them. Scanner: the LTOK rule (bytes <= 32 separate, `habu1.f:3705-3717`) into `TKA-CELL`/`TKL-CELL`; `\` and `(` comments as `habu2.f:7726-7733`, including `(` at end of input.
- Per token: `OUTER:NUMBER`: a range-refused token writes `E-UNDEFINED: ` + token + newline to fd 2 and `70 throw`s without a lookup (`habu2.f:8365-8366`); a number pushes, then hooks `TOP-EV-NUM` (`8368`). Otherwise `OUTER:FIND` under `catch` with the record kept in a cell: `E-USING-AMBIGUOUS` writes the `habu2.f:8243` text + token + ` at <path>:<line>` (from `SRCLOC:PATH-CELL`/`PATHLEN-CELL` and the newlines in [INB, INP), `habu2.f:9901-9913`) + newline and `94 throw`s; a miss is E-UNDEFINED; a record is gated in order WIDE (`LWIDEMSG`), INT (`LINTMSG`), then its min-in byte > `depth` (`LMINMSG`), each message + token + newline then `70 throw`; then hook `TOP-EV-WORD` with the LFIND-folded flags (`habu1.f:4461-4469`) through `data-base TOP-HOOK-CELL + @` and a `TRUSTED: ( ptr u8 n n n n -- ) execute`; then `XREF-START execute-floor`, and true writes `E-UNDERFLOW: ` + token and `70 throw`s. The words that call `execute-floor` or the hook declare no locals; their state lives in DATA cells.
- Excluded here: the keywords (I4b; they refuse E-UNDEFINED, and fixtures are keyword-free), `PEND`, `EM-PKG-RESYNC` (I9b), `EVALD` (I9a).
- Test `test/outer-interpret.f`: per case, spawn `HABU_UNDER_TEST` twice (`--load test/outer-loop-on.f <fixtures> <case>` and without the switch, as `test/top-row-hook-test.f:243-250` spawns) and assert rc, stdout and stderr equal. The fixture prelude (generated into `HB_TMP`) defines `TRUSTED: OI-UF ( -- ) drop ;`, a certified `( n n -- )` word and a hook installer logging to stdout. Cases: decimal, hex, float and negative numbers; comments including `(` at end of input; E-UNDEFINED; a range-refused number; `OI-UF` on an empty stack (E-UNDERFLOW); underdepth; `CORE-STR=` (INT); a wide word; the hook window. Ambiguity: an in-process fork calling `OUTER:INTERPRET` under two usings.
- Files: `src/habu/outer.f`, `src/core/include.f`, `test/outer-loop-on.f`, `test/outer-interpret.f`, `test/gate-stdlib-cases.f` (`SUITE outer-interpret`).
- Verify (spark): the suite; rebuild (a prefix change, `native-runtime.f:83`); chain gen1 == gen2; the gate.
- Route: Alder (`src/core/include.f`, `test/gate-stdlib-cases.f`).
