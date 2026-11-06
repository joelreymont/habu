---
title: "Name a build file's anonymous wordlist in its object"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T16:57:03.737301+02:00"
---

Problem: a build file that defines into a wordlist no package holds (wordlist set-current : X ; or 0 set-current : X ; habu1.f:5130-5131) cannot be written into its file object: src/habu/linker.f WORDLIST+ throws E-COMP-STATE, whose text (src/lib/error.f) names an unrelated failure. Wordlists minted by 'wordlist' in a file are also not re-minted by any record, so a linked file would shift every later wid. Found by the L2b review (finding 2). Acceptance: the design (design-fobj.md section 3) names a minted-wordlist record and a role for it, or a named refusal; whichever is chosen, a fixture file defining into a fresh wordlist gives the chosen result. Files: src/habu/linker.f, test/file-objects.f, design-fobj.md. Verify: test/file-objects.f. Depends: habu-file-objects-l2b-eaf5d63f. Ownership: linker.f record writer. Claim: unassigned.
