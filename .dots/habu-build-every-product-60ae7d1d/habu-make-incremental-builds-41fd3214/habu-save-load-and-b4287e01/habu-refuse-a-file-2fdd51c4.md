---
title: "Refuse a file that patches an earlier file's does> slot"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T19:47:12.555947+02:00"
---

Problem: a file whose compile runs does> on the newest created word when that word is an earlier file's (or an earlier piece's) patches the earlier emission's slot (NSHADOW:PATCH-DOES sets EM-PATCH-TGT), which neither the record compare nor the emission compare saw, and the file's object does not carry it, so a warm link silently drops the patch. Reproduced (/tmp/carl-dp, .out/a2w/census-dp.f: census rows < 50, then a.f defines BOX, RE and 7 BOX RGC-X, p.f's only act is RE, r.f prints RGC-X): cold printed value 8, warm with r.f edited value 7, no refusal. DECISIONS 2026-11-06 'a file that changes what earlier files made' makes earlier code changes E-STORE. Fix (carl-c5a2 @ mtmtpxkv, uncommitted): PATCH-DOES logs each patched emission (NSHADOW:PATCHES, NSHADOW:PATCHED@); CHECK-EARLIER refuses with LINKER:E-STORE a patch since the piece started of an emission below FILE-EM. Proven: the case's cold build now ends with -9691 E-STORE; with p.f's RE removed all 49 files compile (a.f's patch of its own word passes); census obj-equal and out-equal; test/file-objects.f ok. Acceptance: lands with the stack; a fixture case once an app build's file can create through a does> definer (today rc 70 at rec-wide-publish, test/fixtures/file-objects/build.f:47). Files: src/compiler/native/shadow.f, src/habu/linker.f. Verify: zsh /tmp/carl-dp/run.sh cold /tmp/carl-dp/w from fresh /tmp/carl-dp/t0. Depends: none. Ownership: carl. Claim: unassigned.
