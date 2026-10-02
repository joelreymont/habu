---
title: "Check the verifier's owner bridges"
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T04:59:27.153280+02:00"
---

Problem: src/habu/verify-source.f RECORD-SYM?, FIND-SYM, CREATES-SYM?, RECORD-CREATED and RENDERS-MARK? (:353-376 on 53e02ad8) are TRUSTED: bodies only because each reads a CHECKER-OWNER-ABI pre-hook offset constant, which has no axiom row on a from-source engine (docs/forth.md, "A pre-hook word needs an axiom row in a checked body on a from-source engine"; RENDERS-MARK? was made TRUSTED: for exactly this when its checked body broke tools/build-fixpoint.f's from-source candidate, dot 4eb90fc2). The block's comment (:322-325) says "Retirement: habu-builder-trust-rows-c5d41af6", a dot that no longer exists, so nothing owns retiring them. Review 226 found the documented remedy applies: bind each offset at top level (`CHECKER-OWNER-ABI:VERIFY-RENDERS-OFF constant RENDERS-OFF`) and keep the bridge a checked colon body; OWNER-XT is already checked and the *-ACTION words stay the trusted casts. Acceptance: the five bridges are checked bodies, the casts stay TRUSTED:, the stale retirement line goes or names a live owner, tools/build-fixpoint.f from-source candidate passes (the dot 4eb90fc2 case), check.f on verify-source.f rc 0. Files: src/habu/verify-source.f. Verify: verify-source.f is baked: rebuild, g1 = g2 with .names, two-generation build, tools/build-fixpoint.f.
