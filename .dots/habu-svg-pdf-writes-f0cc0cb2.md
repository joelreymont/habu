---
title: SVG-PDF writes the wall clock into every PDF
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T19:54:23.254180+02:00"
---

Problem (tim 2026-11-11, /tmp/tim-pdf-date.md; Maki dot maki-write-a-drawing-e38e8664): src/lib/svg-pdf.f:59 opens the surface with cairo_pdf_surface_create and sets no metadata, so cairo stamps /CreationDate with the wall clock and the same SVG gives different PDF bytes each run; SOURCE_DATE_EPOCH does not change it. Acceptance: the same input bytes give the same PDF bytes, the caller able to state the date (cairo_pdf_surface_set_metadata, CAIRO_PDF_METADATA_CREATE_DATE, an ISO-8601 string) or none. Files: src/lib/svg-pdf.f, its test. Verify: SVG-PDF:WRITE twice a second apart, cmp equal. Depends: none. Not blocking Maki's release. Ownership: carl. Claim: unassigned.
