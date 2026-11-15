---
title: A build-time read past its extent links a stale object
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T18:52:55.057744+02:00"
---

Problem: the linker's reach walk takes a build-time run to read only the extents its addresses name, while the language lets a pointer step past one (docs/forth.md "Pointer types and arithmetic": ptr a n + admits any offset); docs/forth-card.md:374-376 records the gap. Reproducer ~/.cache/tmp/rv-store-reach/src-v2: p.f 'package XEXT public variable XA variable XB 5 XB ! : XB-VIA-XA ( -- n ) XA cell+ @ ; ;package', q.f 'XEXT:XB-VIA-XA constant SEEN', main prints SEEN; edit 5 to 6, the warm build prints 5, the cold one 6. AGENTS.md: a warm build never differs where a cold build would not. The fix is the checker refusing a dereference of a stepped pointer (habu-refuse-an-unbounded-05e30f47), not a linker change. Acceptance: that case, and the same read stepping from one file's last cell into the next file's first, are refused or build warm equal to cold, as cases of test/file-objects.f; the card's line is deleted. Depends: habu-refuse-an-unbounded-05e30f47. Claim: unassigned.
