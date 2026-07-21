---
title: Emit one shared definer publication path
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-19T19:52:41.962044+02:00\""
---

Proven clone count at master 3909bbac. C-QUALIFY-DEF is expanded as target machine code five times: CREATE, CONSTANT, TRUSTED:, DEFER, and colon publication at src/habu/habu2.f around 2170, 2205, 2260, 2397, and 4255. C-STORE-DEF-NAME is expanded six times at the corresponding five sites plus EXPORT around 4552. These are metacompiler composition words, so a source call does not emit BL; it copies the complete qualification scan, package lookup/create path, duplicate wall, protected-wordlist guard, name storage, and state restoration into each target handler. The current interpret/define region is 16564 bytes. Root cause: source factoring hid generated-code duplication. Fix: emit one native qualification helper and one native guarded-name-publication helper with explicit input/result registers, clobber sets, failure exits, W^X requirements, and return ABI; make each definer call those shared routines and retain only its genuinely different state/body publication. Do not weaken duplicate, package, sealed-WID, long-name, dictionary-capacity, rollback, or checker ordering rules. Acceptance: add temporary label-span attribution that records the exact current bytes of both expanded bodies and every clone before editing; final disassembly proves one body and five/six BL sites; interpret/define and total CODELEN fall by the measured duplicate span minus call stubs; CREATE, VARIABLE, CONSTANT, TRUSTED:, DEFER, colon, EXPORT, qualified/unqualified/package names, duplicate and protected publication, long names, evaluate rollback, AOT, snapshot, bootstrap mirror, clobber lint, fixpoint x2, both targets, full gates, and exact ratchets pass. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, definer/package/export tests, clobber contracts, engine-size attribution, and size gates.

Claim: agent=defpub workspace=.jj-ws/fable-defpub machine=spark (owns the one shared definer publication path)
