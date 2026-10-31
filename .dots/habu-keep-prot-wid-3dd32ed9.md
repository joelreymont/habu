---
title: Protect any wordlist a program seals
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T20:32:22.663154+02:00"
---

Problem: the protected-wordlist set is a fixed bitmap band of 8192 bits (src/habu/layout.f ~742-761, PROT-WID-MAX), while a wordlist id runs up to WID:MAX ($FFFFFFFE, layout.f ~157). `prot-wid-add` (habu1.f BPROTWIDADD ~4629) refuses an id at or above 8192 with `hb: protected-WID id above the bound`, exit 84, which is ENGINE-ERROR:SEAL-PACKAGE: lane sealed makes every other exit 84 print `E-SEALED-PACKAGE: <name> at <place>`, so this exit gives 84 a second meaning (sealed review finding 1; /tmp/jerry-sealedrv-tmp/pw.f `999999 prot-wid-add`, rc 84 on both loops). Programs do reach it: libraries seal their own packages at load with `get-current prot-wid-add` (src/lib/task.f ~2574-2578, ffi-abi.f ~1959-1963, byte-edit.f ~200-202), and tools and tests require them, so a program with 8192 wordlists before it requires task.f dies. The 8192 is a layout preset, not a property of wordlists.

Decided (lead, 2026-10-31): remove the bound, so the set holds any wordlist id; the refusal and its second meaning of exit 84 go with it, and no new engine error code is needed. Neither stripping the word (programs and libraries use it to seal their packages, docs/forth.md ~2298) nor a new code fixes the limit.

Acceptance: `prot-wid-add` protects any live wordlist id, including one above 8192, at tier 0 and tier 1 and in a saved image; every reader of the set (LPROTWIDQ, the sealed-WID guards, EM-AOTWIDGATE, capture and snapshot restore, PROT-GUARD over the band) agrees; test/seal.f SLV-ASSERT-PWID-OOR becomes a row that seals a wordlist above 8192 and refuses a reopen of it with E-SEALED-PACKAGE. Design first at the owner: where the bit lives (a record per wordlist or a set that grows) and what an id that names no wordlist does. Files: src/habu/layout.f, src/habu/habu1.f (BPROTWIDADD, LPROTWIDQ, PROT-BITS-*), src/habu/habu2.f (EM-AOTWIDGATE, capture), test/seal.f. Verify: candidate build, rows seal, package-seal, sealed-system-package, aot-wid, snapshot-writer; full suite and the generation chain (capture format). Depends: lane sealed landed (habu-answer-tools-check-745b0ccd). Claim: unassigned.
