---
title: Tier 1 miscompiles opaque evaluate over a wide value
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T21:25:08.147512+03:00"
---

In a TRUSTED: body, an opaque evaluate over a live wide value: 'STRUCTURE pr FIELD a n FIELD b n ;STRUCTURE  TRUSTED: INCOMPLETE ( n -- pr n ) 1 2 PR:MAKE s" " evaluate swap ;  : SHOW ( -- ) 7 INCOMPLETE . PR:UNMAKE . . cr ; SHOW'. Tier 1 prints 1 7 2 (expected 7 2 1 or a refusal), rc 0; tier 0 printed 1 2 7 before the t0reject fix (dot habu-keep-trusted-width-3913bb32), which makes tier 0 refuse it as incomplete. Tier 1 should refuse a body whose layout facts cannot be known, as it does for an unchecked word over a wide value (-8519). Probe: ~/.cache/tmp/jerry-t0reject-probes/ev.f. Found by jerry, 2026-10-23. Not yet worked.
