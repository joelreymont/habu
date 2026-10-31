---
title: Check the XML-C2 adapters
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T13:08:03.745887+02:00"
closed-at: "2026-10-31T14:16:13.184620+02:00"
close-reason: "Landed 98af8303: src/lib/xml/c2.f 31 -> 8 TRUSTED: (HIDE, CURSOR-UNPACK, DEST-UNPACK, REENTER, PARK, the three leaves); the other 23 are checked through the three leaves and four private casts. CURSOR-UNPACK and DEST-UNPACK stay because inlining 2dup into them crashed T0 (habu-refuse-a-trusted-b3747e41). Full suite 510/510 and x86-64 cross-build rc 0 on the batch stack."
---

Problem: src/lib/xml/c2.f has 31 TRUSTED: words; 23 check as : through three one-line leaves (CURSOR-BASE, DEST-SPAN, SOURCE-SLICE) and four private casts (census /tmp/jerry-big/trusted-census-tc1.md, probe tc1-xml-c2-probe.diff). Acceptance: xml/c2.f keeps 8 TRUSTED: (HIDE, CURSOR-UNPACK, DEST-UNPACK, REENTER, PARK, the three leaves); check.f rc 0; rows xml-c2-consumer, c2-xml-source, xml-byte-edits pass. Files: src/lib/xml/c2.f. Verify: those rows. Depends: none. Ownership: src/lib/xml/c2.f. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-tc1
