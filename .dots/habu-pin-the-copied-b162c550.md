---
title: Pin the copied call rule per clause
status: active
priority: 2
issue-type: task
created-at: "2026-08-03T21:11:10.451719+02:00"
---

CRITICAL, destruction review of tools/codegen-workload-scan.f. MOVABLE? (scan.f:131-140) is a hand copy of the engine's C-CALL-REJECT-UNSAFE (src/habu/habu2.f:81-90) and INL-MAX (scan.f:88) copies habu2.f:38 — faithful today, but the acceptance suite lets the copy drift: mutations that pass green include INL-MAX 40->240 bytes, deleting any single one of the nine branch-refusal clauses (they mask each other — the only branchy witness T-RES-WALK has both a b.cond and a b), SCAN-SPAN dropping first or last instruction, CLOSE swapping before/after arms, DELTA-PERMILLE losing its sign. Fixture header claims are false: FX-BIG is 520 body bytes (13x the limit), not 'one instruction over' as codegen-workload-test.f:14 says; FX-BRANCH is 100 bytes — already over the size limit, so its refusal isolates nothing about branches despite the comment at :69-71. Fix: fixtures that bracket INL-MAX at plus/minus one instruction (40-byte pass, 44-byte fail, straight-line), and one under-40-byte fixture per reject clause (cbz, tbz, br, blr, adr, ret-slot, b, b.cond, bl) so only its own clause can refuse it; make the header claims true; add cases catching swapped arms and sign loss in the timing words (unscheduled timed test is fine for clocked assertions per the existing split, but the structural ones — spans, arms as identities — belong in the scheduled suite). This is the project's copied-validator rule: a copy whose gate permits 6x drift proves nothing.

Claim: agent=scan-fixtures workspace=.jj-ws/habu-pin-the-copied-b162c550
