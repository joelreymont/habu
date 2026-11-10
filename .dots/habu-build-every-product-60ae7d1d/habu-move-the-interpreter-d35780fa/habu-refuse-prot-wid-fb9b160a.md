---
title: Refuse prot-wid-add while a checker overlay is open
status: closed
priority: 2
issue-type: task
created-at: "2026-11-01T04:36:12.510174+02:00"
closed-at: "2026-11-10T21:52:35.366691+02:00"
close-reason: "an acceptance case of habu-user-src-can-979e61d7: prot-wid-add and the header's protected-wordlist list are removed by decision (DECISIONS.md:1086-1091)"
---

Problem: habu1.f BPROTWIDADD (~4629) has no OVERLAY-EMIT:GUARD, unlike the other writers an open checker replay must not reach (forth-card section 1: a definition, package, wordlist or undefine inside a replay dies ENGINE-ERROR:OVERLAY-OPEN). A replay can hand out wordlist ids and give them back (habu2.f replay-widn! ~4304 lowers WIDN to the overlay's saved value), so a bit set inside the replay stays set on an id the engine later hands to a fresh wordlist, which is then sealed from birth. Found reading the code (/tmp/jerry-big/protwid-design-opus.md); not yet shown by a program. Acceptance: a program that seals inside a replay is refused at the seal, naming it, as the other overlay writers are; or a reproduction shows no replay can reach prot-wid-add, and the dot closes with that proof. Files: src/habu/habu1.f BPROTWIDADD, the Habu loop's twin (src/habu/outer.f), src/core/checker.f CHECKER-OVERLAY. Verify: a row in test/checker-completion.f or test/outer-interpret.f; candidate build. Claim: unassigned.

Census 2026-11-04: an acceptance case of its parent; fix it in the code that stays (named above), never in the machinery the parent replaces.
