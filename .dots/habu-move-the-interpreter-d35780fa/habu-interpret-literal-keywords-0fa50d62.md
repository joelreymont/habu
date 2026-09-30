---
title: Interpret literal keywords in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T17:23:41.532885+03:00"
---

Problem: the interpret-mode literal keywords are dispatched before find and are not records (`habu2.f:8352-8362`), so under I4's seam `s"` `c"` `."` `s\"` `c\"` `.\"` `char` `'` die E-UNDEFINED.
Acceptance: a keyword step before `NUMBER` in `OUTER`'s loop: `C-ISDQ`/`C-ICQ`/`C-IDOTQ` and their escaped twins (`habu2.f:4452-4528`, decoder `4330-4365`); `c"` over 255 bytes rc 76 via the compile-die tail; an unterminated quote rc 74; `char` (`4530-4538`) and `'` (`4549-4568`) with no name rc 74; `C-QUALIFY-SEAL-GUARD`; the WIDE/INT fail-closed refusals; an undefined tick is a no-op; the used-publics retry; hook events `TOP-EV-STR/CSTR/CHAR/TICK`.
Files: `src/habu/outer.f`, cases beside `test/outer-interpret.f`.
Verify: spark: the route-equality cases through `--load` (the `test/top-row-hook-test.f:125-147` window rows are the oracle); the gate.
Depends: habu-scan-and-interpret-eea996a2 (I4).
Route: krait lands it on master after the Linux proof (rebuild, five-generation chain, gate); Alder pools the Mac gate.
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-interpret-literal-keywords-0fa50d62.

Preflight corrections (2026-09-30; override the lines above where they differ):
- Seal guard, before FIND: when `SEAL-NDICT@` (`src/habu/xref.f:448`) is nonzero and the token has a first colon not at either edge (FIND-SPLIT's edge rule; a second colon does not exempt it), a prefix that `CHECKER-SEALED-PKG?` (`src/core/checker.f:9369`, the declared mirror of the native RESTAB, `habu2.f:2145-2164`) accepts writes the whole token to fd 2, then `s" " ENGINE-ERROR:SEAL-PACKAGE die` (an empty span prints no newline, `habu1.f:1757`), matching the native `exit_group(84)` shape (`habu2.f:3505-3507,3531-3548`). One route-equality case: `' engine-error:x`, rc 84.
- Line refs (drifted ~12): keyword table `habu2.f:8350-8374` (`'` 8364, `char` 8365, strings 8368-8374); bodies 4464-4540; `char` 4542-4550; `'` 4561-4580; escape decoder 4311-4363; oracle rows `test/top-row-hook-test.f:127-149`.
- Keyword match folds A-Z on the token side (LKWCMP `habu2.f:2039-2051`); the oracle has `S\"` (`test/top-row-hook-test.f:170`).
- Tick gates WIDE/INT only, no min-in (4571-4573); `outer.f:463` GATE has the depth check, so tick needs its own; a miss after the used retry is a silent no-op (4578).
- Location line: C-QUOTE-EOF fires before INP is consumed (4223, 4234); the `c"` cap check is after consume (4484), `c\"` before (4519); missing-name prints the baked lowercase keyword (3688-3691, 4546, 4565) then ` at path:line`.
- `allot` before the copy so DP-CHECK (`habu1.f:1706`) refuses before any write, as 4470 does; `here`/`allot`/`c!` are rows (`prims.f:434-439`).
- Ownership: every new word inside `package OUTER private`, as I4 landed.
- Pre-change failing check: `s" hi" 2drop` under `test/outer-loop-on.f` dies E-UNDEFINED rc 70 (`outer.f:452`); the engine returns 0.
- Base master; chain `docs/bootstrap.md:256-276`; suite row `test/gate-stdlib-cases.f:1474`. I8 lands after this leaf.
