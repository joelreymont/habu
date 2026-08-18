---
title: A persisted execution token the relocation never rewrote
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T00:37:45.284275+02:00"
---

Found by single-prefix-4's ASLR-intersect discriminator (2026-08-18): heap offset 0x5046b8 in the persisted image holds a build-process CODE address (region base + 0x281e48 - an xt the snapshot restore's address-cell relocation did not rewrite), in a 12.7KB gap between CU and TIOB0 with no create owner - snap-heap-owner's documented under-report (it matches only the engine's fixed x9 chains, not natively-compiled ones). Present before and after the DEV fix; nothing observed reads it; green is not proof it is dead - it is the exact class just fixed (a persisted host pointer), latent. Method: the ASLR-intersect (dump the heap from two runs of one image, intersect - persisted pointers identical, live ones vary) finds the full population; then name the owner by extending snap-heap-owner to natively-compiled chains (the tool gap is itself the sub-task), then either relocate the cell or reset its owner at snapshot per the DEV precedent. Also verify: are there MORE unmatched cells in the intersect set (the lane counted 27 stale-band cells post-fix - classify all).

RIDES THE SAME ARTIFACT SURGERY (2026-08-18): the buffers-at-startup fix rebuilds what persists - run the ASLR intersect ON THE NEW payload first; the cell may simply be gone. Only if it survives does the owner hunt run.
