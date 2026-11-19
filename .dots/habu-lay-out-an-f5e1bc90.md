---
title: Lay out an address-holding nested sum in linear time
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T15:17:14.296039+01:00"
---

Problem: STORAGE-MARK-TERM (src/core/layout-buffer.f, its T-WIDTH calls) asks a width at every level of a parametric family instance, and TFW-FAM-IMPL (src/core/type-family.f) works each width out by walking every level below, with no memo (closed families keep one, TFM-WIDTH), so a cell over a type nested k deep that holds an address costs about k squared family walks. Not measured: no reproducer yet; option<n> holds no address and takes another path. Acceptance: a reproducer (a NEWTYPE nested k deep over a family whose payload holds an address, then TYPED-VARIABLE) runs near 2x per doubling from 512 to 2047. Depends: none. Claim: unassigned.
