---
title: "Keep a holder's events when another task arms into its group"
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T21:37:47.377968+01:00"
---

Problem: src/lib/aio.f INTO-PUBLISH arms the descriptor in the holder's queue for the POLL-INTO record's events alone (Q-ARM, src/lib/aio/linux.f:67, a MOD that replaces the registration's events), and ADOPT-ONE joins a confirmed record to the chain without re-arming, so a holder's own armed record on the same descriptor (a POLL for WRITABLE, say) loses its events: the header's rule that the registration is armed for the union of the chain's events breaks. Likewise a holder's CHAIN-REARM between the push and the adoption drops the pushed record's events. Acceptance: a holder's POLL on a descriptor still ends ready on its event when another task POLL-INTOs the same descriptor for other events into the holder's group, and the POLL-INTO record ends ready too; an aio test that fails before pins it. Depends: none. Claim: unassigned.
