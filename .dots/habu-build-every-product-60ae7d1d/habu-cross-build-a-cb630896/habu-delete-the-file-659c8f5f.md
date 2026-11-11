---
title: "Delete the file writers' dead words"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-11T08:37:06.083319+02:00"
closed-at: "2026-11-11T09:30:21.176394+02:00"
close-reason: "landed: words deleted, BUILD-ELF private; review's comment fixes folded; engine twice, Linux engine cross-builds, suite 273 of 273"
---

Problem: the census of the Mach-O and ELF writers' callers (carl, 2026-11-11) found words nothing calls: IMG-TAIL-N, IMG-TAIL-NAME and IMG-TAIL-BYTES in sign2.f and elf.f, whose comments name a DRV-SIZE-MARKS that does not exist; MACHO-DATA-WROTE and MACHO-FIXUPS-WROTE, read only by IMG-TAIL-BYTES; IMAGE-BYTES:IMG-PAYLOAD-PREFIX (macho.f:58 stores it, nothing reads it) and IMG-PAYLOAD! (no caller); ELF's BUILD-ELF is public with no caller outside elf.f; test/engine-suite.f:697-703 and :1189 test stand-ins for the deleted snapshot words (check whether roles.f SNAP-PHASE has another user); macho.f:80-85 says a cross-check dot is owed. Acceptance: the words, their comments and the stand-in tests are gone, BUILD-ELF is private, and rg -i --no-ignore finds no name left; the engine builds twice and the second equals the first. Files: src/os/macos/macho.f, src/os/macos/sign2.f, src/os/linux/elf.f, src/os/linux/sign.f, test/engine-suite.f, src/core/roles.f. Verify: the engine built twice byte-equal; the rg sweep. Depends: step 4b of the parent (same files). Ownership: those files after 4b lands. Claim: agent=carl workspace=.jj-ws.noindex/carl-xos3 (brief carl-layout/.out/briefs/xos-4c-dead-writer-words.md; the engine-suite stand-ins and roles.f SNAP-PHASE stay for the x86 step, x86's elf.f BUILD-SNAP-HDR still uses SNAP-PHASE).

Found by the 4a worker (2026-11-11), same files, fixed in this lane: macho.f's comment above MPAGE and driver-io.f's comment on DRV-FAIL tell history ("What used to hold them", "the comparison is gone", "once exited 75"), which the card's comment rules forbid; elf.f's header line `\ Retirement: habu-campaign-c2-mem-c3d7662b.` names a dot; driver-io.f uses IMAGE-BYTES' MBUF and MLEN@ without `require src/os/image-bytes.f` and loads only because link-data.f loads a writer first. Left in src/os/linux-x86-64/elf.f for the x86 step: SNAP-EXTRA-PTR, SNAP-EXTRA-SIZE (:294-299) and BUILD-SNAP-HDR (:322), uncalled.
