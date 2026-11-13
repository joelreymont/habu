---
title: Delete the newest-row wordlist lookup
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T10:28:06.792187+02:00"
---

Problem: XREF:INDEXED-WL (src/habu/xref.f:58) calls xref-wl-newest (src/habu/habu1.f WLFIND:LNEWEST :4630-4670, BXREFNEWEST :4698-4701, row :4898; prims.f:964-966; kernel-x64.f:3447-3460, a linear dictionary scan on every lookup; link-band.f:170) because native unit publication could add duplicate rows to a real wordlist; that path is deleted and definers refuse duplicate live names, so the hash probe answers the same. Acceptance: no real wordlist holds two live rows with one name (shown for hb and for Zen, Tender and Maki loaded), XREF:INDEXED-WL uses the hash lookup, and the newest-row entry, both bodies and the rows are gone. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-npub.
