---
title: all-errors reports one of two refusals at evaluated tokens
status: open
priority: 1
issue-type: task
created-at: "2026-11-10T18:24:27.425584+02:00"
---

Problem: with 'TRUSTED: EV ( -- ) s" drop" evaluate ; immediate' / 's" EV" 0 parse-imm' / ': P ( -- ) ( a comment here ) 1 drop EV ;' / ': Q ( -- ) 1 drop EV ;', 'tools/check.f -- --json-errors --all-errors' prints one packet (P) where the same two definitions with a plain 'drop' print two. Cause (hunt): the replay applies an immediate's declared row and runs nothing, so the check tool falsely refuses ': ACCW ( x -- ) EV ;', which the load accepts; and the run stops at its first refusal. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed".

Part A, unlanded: vorvvruq 9e0d3197 "Defer a body that uses an unrun immediate" (the replay defers to the run a body that uses a word with a parse-imm row; ACCW certifies, ': B ( -- ) drop EV ;' is still refused at drop), on nykylzmy be147d89 of habu-the-pre-pass-3587fdd1. Its review accepted it on the condition that it lands in one stack with part B: with 'EV' as above, ': F ( x -- ) EV NOPE ;' / ': G ( -- ) drop ;' reports F and G before A and only G with A (F is deferred, the replay refuses G, CA-RUN-COMPOSE throws and the run never starts).

Part B, not started: the run reports every refusal under --all-errors, and duplicates are removed in the parent from packet data, never by a new hook input. It also covers a refusal the replay raises at a top-level statement: packages PA and PB each export a public immediate PW; then 'using PA', 'using PB', 's" PW" 0 parse-imm'. The load is refused 70 with E-USING-AMBIGUOUS and --verify-only gives a placed packet, rc 70; --all-errors exits 67 with 'check.f: uncaught throw code 7144'. Cause as a review read it: tools/check-all-errors-core.f CA-RUN-COMPOSE-DEFS (:566-575) treats a refusal it has already reported as a raw failure and CA-RUN-COMPOSE (:589-594) throws it again with nothing to catch it.

Acceptance: --all-errors reports every refused definition of the file, each once, whether its refused token is the file's or one an immediate evaluated, including the F and G file in both orders; the two-package file under --all-errors gives one placed packet and exits 70, as --verify-only does, for every refusal the replay raises at a top-level statement.

Depends: habu-the-pre-pass-3587fdd1 (part A sits on it); habu-count-only-refusals-e8b5dd42; the places of habu-an-evaluate-inside-d3da5e06. habu-a-refused-evaluate-e43ada27 names a defect on part B's path.

Claim: unassigned
