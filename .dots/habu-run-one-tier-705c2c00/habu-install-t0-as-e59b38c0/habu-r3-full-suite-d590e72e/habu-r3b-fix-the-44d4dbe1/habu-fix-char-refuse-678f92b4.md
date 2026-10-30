---
title: "fix char: refuse a body char at every tier"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T22:41:49.787151+02:00"
---

Problem: ': CKT-OC ( -- n ) char A ; CKT-OC .' master JIT E-UNDEFINED: char rc 70; candidate T0 and tier 1 print 65 and check.f accepts (checker.f CHAR-LIT-TOK). docs/forth.md 'Parsing operands are data' admits only top-level char and body [char]. Lead ruling: body char is refused as body ' is (OUT-OF-STATE, E-UNDEFINED rc 70) at check.f, T0, tier 1; supersedes habu-type-a-body-2520214f and habu-compile-char-in-d98cb44f. Acceptance: check-test-lib.f:3288 back to OUT-OF-STATE; the literal path for body char removed from checker, T0 and tier 1. Workspace: .jj-ws.noindex/jerry-char, brief /tmp/jerry-big/fix-char-brief.md. Claim: jerry.
