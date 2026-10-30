---
title: "fix char: refuse a body char at every tier"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T22:41:49.787151+02:00"
---

Problem: ': CKT-OC ( -- n ) char A ; CKT-OC .' master JIT E-UNDEFINED: char rc 70; candidate T0 and tier 1 print 65 and check.f accepts (checker.f CHAR-LIT-TOK). docs/forth.md 'Parsing operands are data' admits only top-level char and body [char]. Lead ruling: body char is refused as body ' is (OUT-OF-STATE, E-UNDEFINED rc 70) at check.f, T0, tier 1; supersedes habu-type-a-body-2520214f and habu-compile-char-in-d98cb44f. Acceptance: check-test-lib.f:3288 back to OUT-OF-STATE; the literal path for body char removed from checker, T0 and tier 1. Workspace: .jj-ws.noindex/jerry-char, brief /tmp/jerry-big/fix-char-brief.md. Claim: jerry.

Worker launched 2026-10-30 23:00 in .jj-ws.noindex/jerry-char on lqwprrlr.

First worker handed off unbuilt (/tmp/jerry-big/char-handoff.md): PARSE-LIT? char arm removed, DO-TOK1 refuses a body char by REFUSE-UNDEFINED, CKT-OC back to OUT-OF-STATE. Fresh worker continues in jerry-char: top-level char guard, user-defined char and tick parity with master, CKT-OC names the token.

Second worker (handoff /tmp/jerry-big/char-handoff2.md): DO-TOK1 sees bodies only (top-level char unaffected); master's JIT calls a user char or tick in a body, so the checker refuses a body char only when no engine word answers it (UNBOUND-CHAR?); engine-suite gets ES-CHAR-CALL in place of master's literal case, which master's own JIT fails; CKT-OC asserts the refused token. Third worker runs the proof. Finding for a next lane: habu2.f CAPTURE-OPERAND and verify-source.f BODY-PARSER? take the next token as data after a body char or tick even when a user word of that name is called.

Proven: nnyxtrru c8c20e8a in jerry-char; rows check-cli-boundary, check-verify, tier, outer-interpret, whitebox engine-suite green; check/local-operand fails on the baseline. A user char or tick is called at every tier as on master (sh2 1 7, uc2 E-UNDEFINED x). Under review.

Review FOLD-IN (unchecked char typed by PRIM: char; capture reads the token after a body char or ' raw; comments). Fold-in worker in jerry-char, brief /tmp/jerry-big/fix-char-fold-brief.md; ' gets the same fix.
