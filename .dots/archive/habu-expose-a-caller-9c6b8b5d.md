---
title: Expose a caller-owned ZIP archive slot
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T08:14:45.677180+03:00"
closed-at: "2026-10-16T09:56:18.697535+03:00"
close-reason: "Expose ZIP:ARCHIVE-SLOT beside INPUT-SLOT in the owning library; public aligned heap-cell consumer retains the archive through late read and cleanup. Astra review clear; source and stripped native ZIP/XML consumers pass."
---

Kaizen retains ZIP:archive in aligned caller-owned document storage from READER:OPEN to READER:CLOSE. ZIP has INPUT-SLOT but no archive equivalent; the correct caller CAST is refused with E-CAST-OWNER 7135. Add the owner-defined ZIP:ARCHIVE-SLOT ( ptr n -- ptr archive ) beside INPUT-SLOT, document storage/lifetime requirements, and verify retained archive use through public source/native paths without weakening cast ownership. Reproducer: Phil scratchpad/cast/repro.f under /private/tmp/claude-501/-Users-joel-Work-kaizen/a4daa590-5fff-41d1-9268-2f1e2f6c1898.
