---
title: "Check the XML C2 reader's last TRUSTED: words"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-01T16:05:39.294626+02:00"
closed-at: "2026-11-01T16:09:22.507135+02:00"
close-reason: "No change: all 8 stay TRUSTED: as boundaries (lead ruling). Each checked form, measured with check.f on master ed087c68, is refused. REENTER and PARK as CAST: give E-CAST-LINEAR, as LINEAR: E-LINEAR-OWNER, and a bare drop of XML:reader gives an error with no code (habu-name-the-err-2290c7ab). SOURCE-SLICE, CURSOR-UNPACK and DEST-UNPACK as CAST: give E-CAST-SCOPE (only C2-MEM packs or unpacks a view). CURSOR-BASE and DEST-SPAN as : copy an exclusive view. HIDE as : gives E-CAP-TRUSTED (int-mark). Checked forms would need a public XML word minting XML:reader from raw cursor storage, or a C2-MEM word giving a raw base while keeping the view. Either hands every package the forgery the linear reader and exclusive view exist to stop, so these bridge two packages' private representations as DEF-PREFIX-BIND and BIND-OWNER do. Workspace jerry-xmlc2 retired with no commit."
---

Problem: src/lib/xml/c2.f keeps 8 TRUSTED: words (HIDE, CURSOR-UNPACK, DEST-UNPACK, CURSOR-BASE, DEST-SPAN, SOURCE-SLICE, REENTER, PARK) that waited on lane cown, landed as 792a7a10 'Let a cast name another package's type'. Acceptance: each gets its checked form (private CAST: or : over checked words) where the checker admits it; each that stays has its checked form's refusal measured. No new TRUSTED:, no new public word. Files: src/lib/xml/c2.f. Verify: check.f rc 0 on c2.f; rows xml-c2-consumer, c2-xml-source, xml-byte-edits. Brief /tmp/jerry-big/xmlc2-brief.md. Depends: none. Ownership: src/lib/xml/c2.f. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-xmlc2
