---
title: Run T0 compiles under the lease
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T19:51:43.958878+02:00"
---

Lane S2 of /tmp/jerry-big/t0state-design.md (revision 4). Problem: T0's COMPILE and COMPILE-FIXED (src/habu/t0.f) take no NLEASE, so they skip E-TASK and E-BUSY while writing the shared session state; NDEFINITION's public M-OPEN marks their compile instead, a second copy of the lease's 'a compile is running'. Acceptance: NLEASE's CURRENT/WORKING as a private ENUM phase idle/leased/working; T0's two entries under NLEASE:WITH; M-OPEN and NDEFINITION:IDLE-CK removed from all seven sites; DEFINITION-CACHEABLE?'s M-OPEN read removed; errors.f E-NCOMP-STATE comment states the code's meaning; forth-card's NLEASE:IDLE? clause removed. Public: NDEFINITION:M-OPEN and IDLE-CK removed; CALLBACK-STUB! and NCOMP:PUBLISHED! lose a check that cannot fire. Needs Joel's approval of those items (one interview after the critic accepts). Verify: builds, C2 rows, x86 rows, build wall/CPU against base (NLEASE:WITH at every ;). Depends: Joel's approval. Ownership: lease.f, definition.f, compiler.f, t0.f, errors.f, docs/forth-card.md. Claim: unassigned.
