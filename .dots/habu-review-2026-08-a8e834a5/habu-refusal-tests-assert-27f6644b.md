---
title: refusal tests assert only an exit code
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.042962+02:00"
---

Problem: test/protection-span.f:22-28 EXPECT/REJECTS assert rc 83 and drop stderr (28 cases); test/addrmap-set.f:162-171 and test/code-window.f:71-78 TRAPS assert a crash rc only; test/engine-suite.f:515-537 ES-RES-REFUSE:OUTCOME returns the bare throw code and four 'refused' cases assert only 70, the generic checker-reject code (tools/boot-pin.f:37, gate-engine-lib.f:1627) so an unrelated load failure passes; gate-engine-lib.f has 32 GE-EXPECT-RC vs 58 GE-EXPECT-ERR-HAS, gate-dictionary-lib.f 5 vs 11. test/lower-txn-protection.f:80-87 and gate-engine-lib.f:1625-1628 show the paired form. Acceptance: every negative capture asserts the diagnostic name or E- code with the rc; lib/test/outcome.f gains one combinator taking both; the listed sites converted. Files: as listed, lib/test/outcome.f. Verify: the suites green; a mutant that changes which refusal fires reds them. Depends: none. Ownership: test harness. Claim: unassigned.
