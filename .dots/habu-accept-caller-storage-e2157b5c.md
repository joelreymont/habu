---
title: Accept caller storage at any address in EDIT, XML and SSE
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T07:08:03.584313+02:00"
---

Problem: EDIT:INIT (src/lib/byte-edit.f STORAGE-CHECK :82), XML SOURCE-INIT (src/lib/xml/source.f SRC-STORAGE-CHECK :261) and SSE:OPEN (src/lib/net/event-stream.f :227) refuse caller storage whose address is not cell-aligned with E-STORAGE, though every target reads and writes cells at any byte address and none of the three uses an atomic or a foreign call on it (docs/forth.md alignment bullet). Tender copies the refusal (tender/src/answers.f:388) and names XML:E-STORAGE in its failure table (tender/src/failures.f:365). Acceptance: the three alignment refusals and the comments that cite them (byte-edit.f:31-32, xml/source.f:313) are gone, each error constant kept while another throw or a consumer names it; Tender's copy goes to its owner with the reason; Zen, Tender and Maki build cold and warm. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-align
