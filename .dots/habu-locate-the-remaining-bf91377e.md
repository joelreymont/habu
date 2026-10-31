---
title: Locate the remaining load refusals and bare throws
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T14:31:40.370636+01:00"
---

Follow-up to habu-name-the-err-f8c7f3ac, whose lane locates E-UNDEFINED (both loops) and names residue with file and count. Still unlocated: the ten other LDIAGRET sites in src/habu/habu2.f (LWIDE, LINTERNAL, LTRUSTTICK, LMININ, LFLOORREC, LPREFMISS with its baked newline, LORPHAN, LCFKIND, LCFCAP, LLOCWIDE, LLOCMANY) and their Habu twin REFUSE in src/habu/outer.f; route them through LCOMPILEDIE / THROW-AT. Pins to sweep: test/outer-interpret.f ~306, 315, 322, 373, 437, 667, 674, 682, 694, 1075, 1101, 1910. Also a bare library throw during --load ('-2001 throw' in a file) prints only 'hb: uncaught throw code -2001', rc 67: no name (src/lib/errors.f is constants only, no code-to-name table) and no file (include frames are popped before the uncaught reporter runs). Plain-program refusals (`bin/hb file.f`, the root stream with no include frame) still print `E-UNDEFINED: nosuchword` with no file or line (test/main-argv.f ~201, 205 pin it). A tier-1 undefined word inside a checked body prints `habu: in f: undefined word 'x'` with no file or line (the checker's diagnostic path, not LDIAGRET). Acceptance: each such load failure names its file and, where the site knows it, its error name.

`FUNCTION:` under --load is one of these (lane nsig, public hb 1a41c5ee): with no library selected, `require src/lib/ffi-abi.f` then `FUNCTION: CKF getpid` prints only `hb: uncaught throw code -3503` (E-FFI-LIBRARY from OPEN-FUNCTION's CHECK-SCOPE, rc 67); with a library selected and no signature, `-3501` (E-FFI-SYNTAX from EFFECT). tools/check.f refuses the same source first as a missing signature (7157 at `FUNCTION:`), so the two front ends also disagree on which fault comes first. Probes: /tmp/jerry-find/f4.f, f5.f.
