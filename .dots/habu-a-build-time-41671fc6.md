---
title: A build-time read past its extent links a stale object
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T18:52:55.057744+02:00"
---

Problem: the linker's reach walk (src/habu/linker.f SEE, USED-EXT+, REACHED-CK) takes a build-time run to read only the extents its addresses name; the language lets a pointer step past one (docs/forth.md 'Pointer types and arithmetic': ptr a n + admits any offset). On the installed engine 38496dc3: p.f 'package XEXT public variable XA variable XB 5 XB ! : XB-VIA-XA ( -- n ) XA cell+ @ ; ;package', q.f 'XEXT:XB-VIA-XA constant SEEN', main prints SEEN; edit 5 to 6, warm build prints 5, cold prints 6 (reviewer's reproducer ~/.cache/tmp/rv-store-reach/src-v2, runs v2-*). AGENTS.md: a warm build never differs where a cold build would not. Acceptance: the warm build equals the cold one for that case, as a case of test/file-objects.f, and for the same read stepping from one file's last cell into the next file's first; the rule chosen is sound for every offset the language admits, or the language refuses the step. Files: src/habu/linker.f, test/file-objects.f. Verify: test/file-objects.f rc 0; the six-edit census warm = cold. Depends: none. Ownership: linker.f reach walk. Claim: unassigned.

## Known rule, not a new cause (lead, 2026-11-11)
docs/forth-card.md section 4 already states it: "A build tracks a compile-time read of storage as a read of the record whose address it derives from, within that record's extent: `L 8 +` past `create L 8 allot` reads the next record, whose edit a warm build misses." So the linker is working as recorded and the defect is the language's open reach (docs/forth.md "Pointer types and arithmetic"). The fix is the checker refusing a dereference of a stepped pointer, habu-refuse-an-unbounded-05e30f47 (band 3 of habu-bound-pointers-with-719ba3f4): this dot's case is an acceptance case there, and it closes when that lands and the card line is deleted. No linker lane; the hunt started for it was stopped (workspace carl-past retired, nothing committed).
