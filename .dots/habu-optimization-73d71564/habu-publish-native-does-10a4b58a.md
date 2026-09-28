---
title: Publish native DOES contracts to the checker
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-28T11:33:36.101276+02:00\\\"\""
closed-at: "2026-09-28T12:20:02.917208+02:00"
close-reason: "Fixed resident native checked/trusted DOES publication through the checker owner; an existing RBF transaction preserves signature, control/CREATES, symbol and extension state on refused clauses or later failure. Public E2E reproduced missing contracts and phantom children before repair, then passed. Independent review, source-only replay, five native generations with B2-B5 byte identity, full 490-suite gate, strict signatures and both byte-identical Maki board smokes pass. Engine remains 2856823 bytes, SHA77270a103f32c6e7422554df65a36fa06680186d01ce50383aeea64c4b76e2b6. Evidence: ~/.cache/tmp/habu-native-creates-fix-20260928-01.md."
---

Sealed macOS ARM64 product cf9c706c at source57c2f12fae4e executes a trusted native CREATE/DOES definer correctly, but VERIFY:SOURCE-BUF-IN-SCOPE cannot reconstruct the created word effect. The same public-only reducer at tier0 accepts the typed reader and refuses the incompatible reader; tier1 reports both unresolved. Retained reducer: ~/.cache/tmp/habu-control-compaction-20260928-01/native-creates-public.f. Fix the responsible native publication ordering or owner contract after source confirmation, preserving trusted declaration semantics and JIT behavior. Add the genuine missing public E2E before production edits, verify both tiers through the real load path, native self-host convergence and full registry; retain a repeatable artifact. This is a prerequisite to real nonzero-CREATES capture coverage for habu-compact-captured-control-4969c5eb, not metadata injection or a new capture API. Lead owns integration and closure; Sol owns implementation and qualification.
