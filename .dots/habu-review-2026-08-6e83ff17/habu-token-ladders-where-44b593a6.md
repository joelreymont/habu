---
title: token ladders where the standard requires tables
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:25.964851+02:00"
---

Problem: docs/forth.md:700-703 puts classification in row data; ladders of 'STR=CI if ... exit' arms: lib/ptx/ad.f:289-310 (21 arms) and 468-494, ad-gen.f:344-379, ad-ir.f:590-597, object.f:252-286 PARSE-LINE (18 tags), object-link.f:518-550, date.f:255-268, build-cache.f:441-449, argv.f:424-432, process-pty-handle.f:297-319 DISTINCT-FDS? (21 pairwise terms). Acceptance: each ladder a table or an ENUM MATCH; behaviour tests unchanged. Files: as listed. Verify: owning tests. Depends: none. Ownership: lib. Claim: unassigned.
