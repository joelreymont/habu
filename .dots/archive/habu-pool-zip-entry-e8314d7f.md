---
title: Pool ZIP entry and name copies
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T03:25:26.193477+03:00"
closed-at: "2026-10-15T03:35:15.330604+03:00"
close-reason: Pooled entry identities and NAME$ copies in archive-owned chunks; independent Astra and lead reviews clear. Exact b685+XML installed candidate passes source ZIP/lifecycle/ZIP-XML, XML-owner source/stripped E2E and public ZIP/XML stripped and REPL applications with unchanged e4404e86 engine and 78e2c2b2 map. Whole Office writer measurement remains with the parent task.
---

Native Office full writer has ~1 MiB OPEN and ~1 MiB WALK increments for five ZIP entries. ZIP NEW-ENTRY uses a 37-cell mapping per handle and NAME-COPY uses COPY-IN with another 37-cell buffer node plus tiny mapping. Store three-cell entry identities and copied name bytes in archive-owned 16 KiB bump chunks; retain stable pointers, validation, zero initialization, and cleanup on close/failure. Keep independently owned INPUT and replacement/read buffers unchanged, including failed REPLACE-FILL release. Verify a public multi-chunk handle/name stability case and relevant ZIP suites with installed hb; measure bounded public ZIP consumer memory before/after if feasible.
