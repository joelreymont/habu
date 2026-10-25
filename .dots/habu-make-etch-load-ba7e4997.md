---
title: Make etch load on current Habu
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T19:20:43.949961+03:00"
---

etch src/quantity.f 'NEWTYPE length 0' passes the width argument NEWTYPE no longer takes; the load dies E-EVAL-RESIDUE (-3804) on master bin/hb (tier census and timing, 2026-10-23). Etch owns its downstream checks: fix in etch's repo. etch is one of the corpora habu-decide-whether-one-358fc93b measures. Claim: jerry; lane workspace ~/Work/etch/.jj-ws.noindex/jerry-etchload (2026-10-25).
