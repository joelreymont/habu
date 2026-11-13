---
title: "Refuse a declared type nested past the checker's scratch"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T02:05:26.766083+02:00"
---

Problem: on master hb, a word whose signature names a declared type nested 250 levels certifies, but 300 and 400 levels end the process: `checker: transport group scratch full`, rc 76, from src/core/checker.f:17438 (XG-TN reaching XG-TCAP). A die belongs to a broken invariant, not to a program the checker cannot take; the REPL dies with it. Reproduce: `PRODUCT sw-one<a> FIELD a a FIELD b n ;PRODUCT` then `: USE ( sw-one<...300 deep...<n>...> -- ) drop ;` (s300.f: `sw-one<` 300 times, `n`, `>` 300 times; s250.f, 250 deep, certifies). On the installed hb (2026-11-13) s250.f certifies and s300.f and s400.f print `checker: transport group scratch full`, rc 76. Acceptance: the 300- and 400-deep words are refused with a named error at their token and the process goes on; s250.f still certifies; one rejected-program test. Files: src/core/checker.f (XG- transport group). Verify: bin/hb --load each probe; the named rejected-program test. Depends: none. Ownership: checker.f's XG- words. Claim: unassigned.
