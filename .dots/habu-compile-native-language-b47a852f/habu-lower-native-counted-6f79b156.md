---
title: Lower native counted loops
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:38.988167+02:00"
blocks:
  - habu-lower-native-quotations-de9829f9
---

Full context: design Wave 5 adds DO/?DO/LOOP/+LOOP/LEAVE/UNLOOP/I/J using explicit loop-frame IR and block arguments. Acceptance: zero-trip, step direction/overflow, nested indices, LEAVE/UNLOOP cleanup, and malformed frame mutations pass or reject deterministically.
