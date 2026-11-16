---
title: Reject a keyword name exactly where a definer makes a word of it
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T15:17:16.574410+01:00"
---

Problem: ARENA (src/core/arena.f RUN), HEAP (src/core/dynamic-storage.f DECLARE-HEAP), DEFLINEAR and VALUE-RECORD (src/core/roles.f) take 'if' and '42' as names with rc 0 under --load, check.f and --verify-only, while DEFTYPE, NEWTYPE and SUMTYPE reject them and docs/forth-card.md section 1 says every definer does. ARENA and HEAP make only suffixed words (NAME-NEW, ...); DEFLINEAR and VALUE-RECORD declare types; what each of the seven makes of its name is not yet established. Acceptance: each definer rejects a keyword or number-shaped name exactly when it makes a word of that name, under all three readers, and the card states the rule so. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-kd.
