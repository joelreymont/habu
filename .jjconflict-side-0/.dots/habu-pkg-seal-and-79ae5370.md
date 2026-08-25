---
title: Package seal and loop emitter globals in habu2
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T17:23:26.161115+02:00"
---

Full context: the checked package gate (tools/package-diff-lint.f) reports six changed/new GLOBAL definitions in src/habu/habu2.f from the raw-storage seal commit: EMIT-KWDATA:1343 (changed), C-FIND-TRUST-RAW:1626, C-CALL-TRUST-LASTC:1701, C-CALL-TRUST-LASTC-PTR-A:1710, C-CALL-TRUST-LASTC-A:1720 (new), EMIT-LABEL-CONTROL:6844 (changed). The merged loop-family fix also added global J-LVREQUIRE:1530. habu2.f already opens real packages (OWNER-WID-EMIT:836, LOWER-TXN-CODE:2991), so package scope is expressible; per the package-first rule an exempt-list entry is not a fix. Work: move the new trust emitter words into a real package with a short tail, give the changed emitters and J-LVREQUIRE package owners, and migrate their in-file callers. Constraint: the emitted engine must be byte-identical — prove with twin builds (fixpoint refresh before/after, same CODELEN and file hash). Acceptance: package-diff-lint on the habu2-only diff artifact reports 0 findings; native engine build slice green; byte-identical engine proven.

Claim: agent=pkg-emit workspace=.jj-ws/habu-pkg-seal-and-79ae5370 (RELEASED 2026-08-21: workspace gone, no live lane - gc)
