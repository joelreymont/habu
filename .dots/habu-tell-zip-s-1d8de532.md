---
title: "Tell ZIP's host faults from its refusals"
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T23:23:20.780684+02:00"
---

Problem: ZIP throws E-OPEN or E-READ for a host fault as well as for an archive whose bytes it refuses. src/lib/zip.f:40-48 SNAPSHOT-OPEN (unusable TMPDIR, refused snapshot) and :82-86 (source open, short copy, mmap) throw E-OPEN beside :88-92 (bytes libzip refuses, member count mismatch); :52 SOURCE-STEP (read), :65 CLOSE-SOURCE-FD (close), :95 (munmap) and :132-133 MEMBER-SIZE (libzip stat) throw E-READ. A client (Tender's tenderd/archive.f:96-106, FORMAT? and ZIP-CODE$) cannot tell a full TMPDIR or a failed read from an archive that refuses its bytes, so it files the host fault as the archive's. Reported by kevin for Tender (Tender dot 'Tell a zip's host faults from its refusals', under Tender-run-the-imports-2fd9ada5). Acceptance: a host fault in ZIP throws a code that names the host fault (prefer the code the failing FS/FD call already uses, E-FS-IO, E-FS-STAT or FS-MUTATE's own, over a new ZIP code); E-OPEN, E-READ and E-SIZE mean only that the archive's bytes were refused; zip.f's header and each throw site's comment say which; a regression test through the real load path: an unusable TMPDIR (or a refused snapshot) gives the host code, bytes that are no zip give E-OPEN. Design: classify each site in the list above at dispatch (libzip's stat failing on a valid index is the archive's); a new public code, if one is needed, is a new public API (CLAUDE.md Authority). Files: src/lib/zip.f, a test under test/ beside zip-temp-e2e.f. Verify: the new test and the zip-* tests through bin/hb --load test/run.f -- <names>. Depends: none. Settled 2026-11-10: no new code. SNAPSHOT-OPEN passes on the code FS:MKTEMP threw; a source that will not open is E-FS-OPEN; a failed read, close, mmap or munmap is E-FS-IO. Ownership: src/lib/zip.f. Claim: agent=jerry workspace=.jj-ws.noindex/habu-tell-zip-s-1d8de532.

Consumers: still met in Tender, tenderd/archive.f:99-107 (FORMAT?, ZIP-CODE$), :124 and :201; not observed in a run.
