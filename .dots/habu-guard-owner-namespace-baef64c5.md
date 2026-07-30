---
title: Guard owner dictionary rewind
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T06:14:51.017614+02:00"
---

Problem: raw ndict! can remove an owner marker without using XREF retirement. Result: BNDSET consumes OWNER-GUARD:REC?, scans every record in the requested removed suffix, and exits through the existing uncatchable protected-region path before lowering NDICT if a marker is present. Preserve the task-live and protected-dictionary guards in their current order. Owner: native ndict! sink only; bootstrap uses the same builder path. Production red: direct ndict! truncates the marker, after which the package reopens. Acceptance: truncation at or below one marker fails closed before NDICT, dictionary bytes, permissions, or checker state change; the count after the marker and every unmarked suffix remain legal; HIDE, FORGET, snapshot, inline AOT, refresh, and fixpoint remain exact. Forbidden: XREF retirement edit, cp! edit, patch32 edit, new predicate, general rewind policy, raw-wordlist migration, compatibility, or lint. Smallest owning check: direct ndict! immediately before and after one marker rejects then succeeds without other mutation. Claim: unassigned.
