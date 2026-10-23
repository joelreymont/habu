---
title: "Keep TRUSTED: width facts complete after a rejection"
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T20:44:30.976751+03:00"
---

Tier 0 lowers a TRUSTED: body from the checker's unjudged scan (slnrrlyu). When the scan rejects a token while a wide value is already on its stack, later facts for that value are missing or wrong, and pass 2 defaults a missing fact to one cell (habu2.f EMIT-WIDTH-LOOKUP), so tier 0 silently miscompiles where tier 1 (which lowers from each token's declared binding) is right. Probes (review of slnrrlyu 5788e81e, 2026-10-23), each with STRUCTURE pr FIELD a n FIELD b n ;STRUCTURE and ': SHOW ( -- ) 7 TBR . PR:UNMAKE . . cr ; SHOW': p15 'TRUSTED: TBR ( n -- pr n ) 1 2 PR:MAKE s" x" drop 0 * drop swap ;'; r3 quotation; r4 nested TRUSTED: INNER; r5 branch with early exit; q2 narrow signature throws -3804; p09 'depth drop' loses a cell. Tier 0 prints 1 2 7 where tier 1 prints 7 2 1. Acceptance: in an unjudged scan a rejected token still applies its declared effect so later facts stay complete; where facts cannot be known (depth, .s, opaque evaluate over a wide value) the certificate says so and tier 0 refuses with a diagnostic; never a silent one-cell default; the probes as test/tier.f rows. Lane: jerry, worker in .jj-ws.noindex/jerry-t0reject (on slnrrlyu). Claim: jerry.
