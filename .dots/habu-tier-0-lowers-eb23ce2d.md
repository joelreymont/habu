---
title: Tier 0 lowers a generic-row call at a wide type
status: active
priority: 2
issue-type: task
created-at: "2026-10-24T03:12:59.643153+03:00"
---

Tier 0 lowers a call to a signature-less word whose inferred row binds a type variable to a wide value; tier 1 refuses the same program (-8519). Reproduce on master: bin/hb --load ~/.cache/tmp/jerry-genwide-repro.f (': IC-RAW swap ;' then 'TRUSTED: INCOMPLETE ( n -- icpair n ) 1 2 ICPAIR:MAKE IC-RAW swap ;'): tier 0 prints '2 1 7', '1 set-tier' refuses. Blocks e122d857: its signature-less scan gives IC-RAW a row under 0 set-check, so test/tier.f INCOMPLETE-RAW stops refusing at tier 0. Acceptance: tier 0 refuses what tier 1 refuses here, through one shared rule where the certificate can carry it; INCOMPLETE-RAW and a checked-IC-RAW row in test/tier.f. Lane: jerry, worker in .jj-ws.noindex/jerry-genwide.
