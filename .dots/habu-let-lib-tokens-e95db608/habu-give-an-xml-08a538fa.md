---
title: Give an XML source a header record
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T12:32:58.556160+01:00"
closed-at: "2026-11-22T13:29:58.912829+01:00"
---

Problem: src/lib/xml/source.f:37-45 lays the source header out as byte offsets in one allocation with its marks and lexical bytes; SRC-MINT and SRC-ERASE (:63-64) carry ^n, and >ENCODING-CELL and >ALLOCATOR-CELL (:33-35) cast into it. Acceptance: the header is a STRUCTURE the token carries; no cast or offset reaches it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-xsrc.
