---
title: Fix cold native body capability binding
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T14:38:23.738265+03:00"
---

The sealed cold-prefix scanner of the current native source refuses E-UNDEFINED at NATIVE-BODY:BODY-CAPABILITY in src/habu/engine-origin-early.f, while ordinary top-cold-prim call and tick paths pass. Reproduce against matching old donor, find the actual producer and scanner package boundary, then repair the smallest responsible binding and verify the scanner and adjacent native-body behavior through the real load path. This is a bounded regression under active habu-reuse-native-bodies-5d53ae37; no changes to checker-owner capture, defer-cell provenance, SOURCE tracing, or Mark's top-store reservation.

The real source-only recovery continuation also refuses `EXPORT-TAIL$` while
loading `tools/build-fixpoint.f`. Origin callbacks run after alias publication;
use that exact alias record's name length rather than the checker's private
operand parser, and install the callback through the existing target-owner
cell. Preserve the callback ABI, exact alias ordinal and source span. Check
the documented recovery library preamble and build-fixpoint load before and
after the change, then existing origin/export and cold scanner E2Es.
