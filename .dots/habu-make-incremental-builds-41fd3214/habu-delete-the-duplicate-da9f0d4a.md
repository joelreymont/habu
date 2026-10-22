---
title: Delete the duplicate object cache
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T12:18:15.939124+03:00"
---

Problem: one maker run publishes both an artifact (tools/hb-build-lib.f:1225-1235) and an object .hbo/.idx/.hbo.map (:1304-1313) from the same inputs; an artifact hit never dates the object (:1174-1187); its only hits are a hand-deleted artifact (tools/hb-build-stripped-cache-test.f:180-190) or the --json-errors key split. Acceptance: HBB-OBJECT-* (:1189-1285), .hbo.map siblings and hb-build's OBJRES/OBJIDX/OBJSTORE/OBJIMG use deleted; tests that only exercise the object layer deleted; drop --json-errors from the artifact key (:1102, it only selects diagnostic format). Files: tools/hb-build-lib.f, tools/hb-build*.f, tests. Depends: none. Claim: unassigned.
