---
title: "checker mirrors the engine's sealed-package table by hand"
status: closed
priority: 3
issue-type: task
created-at: "\"2026-08-22T22:38:25.871061+02:00\""
closed-at: "2026-09-16T14:34:52.136277+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Residue: the checker still lists the sealed package names by hand beside the engine's declared single authority, with nothing pinning the two together."
---

Problem: checker.f:7029-7036 CHECKER-SEALED-PKG? lists 'tfam type match checker-cert lower-cert lower-cert-hook engine-error' while habu2.f:1880-1893 RESTAB-BUF is declared the single authority; same for UNSAFE-TOK? (8655) vs UNSAFE-SET-SEAL (8763). Equal today, nothing pins them. Acceptance: a restab? primitive the checker queries; the hand list deleted; a test that adds a reserved name in one place and sees the checker follow. Files: src/core/checker.f, src/habu/habu2.f. Verify: package seal suites. Depends: none. Ownership: reserved names. Claim: unassigned.
