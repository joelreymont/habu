---
title: Repair-class list drift across four sites
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:22:15.989884+02:00"
---

Pre-existing bug found by TFAM 13 census (docs/census-tfam-13.md): fix_nominal_type is in docs/repair-diagnostics.md:104 and GJA-SUGGEST-FOR (tools/gate-json-assert-core.f:325) but is NOT emitted by render.f REPAIR-CLASS/SUGGEST-TEXT and NOT in RSD-TEST-DOC-CLASSES (tools/repair-schema-doc-test.f:163-173); that test also omits remove_dead_code. Fix: single source of truth or a completeness test asserting the four sites agree (render.f emitters, GJA-SUGGEST-FOR, docs table, RSD-TEST-DOC-CLASSES). Natural home: fold into TFAM 13 work (habu-tfaam-13-adt-5d3288f0) whose ADT classes must update all four sites anyway. Also: render.f RBUF (64 cells, render.f:167) has NO overflow guard in REND-COLLECT (170-174) - silent overrun; TFAM 13 bounded/growable work covers it, this dot records the latent bug.
