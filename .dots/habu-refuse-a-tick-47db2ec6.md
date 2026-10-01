---
title: Refuse a tick of an undefined name
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T12:34:32.136160+03:00"
closed-at: "2026-10-01T12:49:51.119124+03:00"
close-reason: "' NO-SUCH is E-UNDEFINED rc 70 (caught 70 under evaluate) on both loops: outer-interpret TICK-UNDEFINED red on base, green on product ab1c5ac5 (gen2 equal)."
---

Problem: at interpret level `' NAME` with no word NAME pushes nothing and raises nothing. `src/habu/habu2.f` `C-TICK` ("undefined `' X` stays the pre-existing no-op") and `src/habu/outer.f` `TICKED` ("a name no word has is a quiet miss") both keep the miss silent, while a body's `['] NAME` is already `E-UNDEFINED` (rc 70). Measured on master 5c32: `1 ' NO-SUCH-WORDX 2 .s` prints `1 2`, rc 0. `s" 1 drop" ' VERIFY:SOURCE-BUF catch` without `require src/habu/verify-source.f` hands the string length to `catch`: `hb: top-row: catch expected: xt actual: n`, then catch runs cell 6, SIGSEGV, rc 134. The PS lane (`habu-restore-pkg-state-d09394b9`) read that missing require as a catch crash. The top-row line is the tier-1 warning working as designed; the defect is the silent tick.
Acceptance: an interpret-level `' NAME` that misses after the open scope, the globals and the used publics is `E-UNDEFINED: NAME`: rc 70 when unhandled, a nonzero code under `evaluate` + `catch`, as for a bare undefined name. The engine's `C-TICK` and the Habu `outer.f` `TICKED` agree. Any in-tree source that ticks an optional word is fixed to require it or test for it. Rejected programs: `' NO-SUCH` at top level (rc 70, the name on stderr) and `s" ' NO-SUCH" ' evaluate catch` (nonzero), in the suite that already covers interpret-level undefined names.
Files: `src/habu/habu2.f` (C-TICK), `src/habu/outer.f` (TICKED), that suite, `docs/forth.md` where it describes `'`.
Verify: the suite; chain (habu2.f is baked) and full gate.
Depends: none.
Ownership: krait.
Claim: unassigned.
