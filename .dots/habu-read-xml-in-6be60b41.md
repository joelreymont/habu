---
title: Read XML in every Encoding Standard encoding
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T11:37:55.046046+01:00"
---

Problem: Habu's XML reader accepts only UTF-8, UTF-16 and US-ASCII declarations (src/lib/xml/source.f:10-16, E-ENCODING), and its US-ASCII rejects bytes above $7F where the Encoding Standard reads us-ascii as windows-1252; Zen sends every application/xml, text/xml and +xml upload there (zen src/document/media.f:33-35), so an XML original declared in a legacy encoding fails. The source keeps reversible original-byte coordinates (source.f:1-9), which a whole-text decode does not give. Acceptance: an XML original in any encoding the Standard names reads as browsers read it, its coordinates still mapping to the original bytes. Depends: habu-decode-every-encoding-1336a560. Claim: unassigned.
