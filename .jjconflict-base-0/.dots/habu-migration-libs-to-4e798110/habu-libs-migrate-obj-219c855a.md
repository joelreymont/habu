---
title: "Libraries: migrate object line record"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-24T13:08:32.812603+02:00"
closed-at: "2026-07-24T17:43:03.132545+02:00"
close-reason: Reviewed implementation landed and verified at master@origin 4fb6f52fb815.
---

Why: lib/object.f still declares OBJ:line with legacy PRODUCT, blocking total declaration-event release. Owner: lib/object.f and lib/object-test.f only. Replace PRODUCT line 0 ... ;PRODUCT with STRUCTURE line 0 ... ;STRUCTURE in the existing public OBJ package section, preserving ptr/len fields and order, ptr u8/n schemas, OBJ-LINE:MAKE/UNMAKE spelling, two-cell layout, NEXT-LINE cursor behavior, load parsing, OPTION:SOME/NONE behavior, errors, and allocation behavior. Update comments. Forbidden: aliases, legacy parser edits, raw casts, cursor/parser redesign, object wire-format changes, unrelated cleanup. Acceptance: the real object test covers empty, one-line, multiple-line, malformed magic/records, MAKE, and UNMAKE before/after; exact reflection/layout and generated effects remain stable; no executable PRODUCT remains in lib/object.f; focused typed-local/package/trust gates pass. Dependency proof: master 227b5b349702 has green unified STRUCTURE and object baselines.

Claim: agent=codex-object-line workspace=.jj-ws/habu-libs-migrate-obj-219c855a
