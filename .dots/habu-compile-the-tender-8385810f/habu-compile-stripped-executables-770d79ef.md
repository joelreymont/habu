---
title: Compile stripped executables with the retained optimizing engine
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-14T14:46:34.524316+03:00\""
closed-at: "2026-09-14T15:42:53.920014+03:00"
close-reason: Native stripped driver, scoped outer CLI and obsolete-maker removal integrated through e9111ac2; reviewed by Cedar. Combined hb-build-test passes with object-storage/C5 fixes on numeric engine. Default stripped quotation builds and executes on rebuilt DATA-site engine515efca8. Tier probes, explicit downgrade refusal, preseed, cache and REPL controls pass. Release qualification and retained pre-window literal ownership remain separately open.
---

Confirmed 2026-09-14: HBB default path emits a cached ENGINE-BUILD maker with cold prefix, zero TIER-CELL/XT-CELL, and no native executable scope in aot.f RUN. Thus default executable source silently uses JIT despite AOT report. Replace with a fresh current-engine child selecting tier 1 before dependencies and compiling application inside EXECUTABLE-BUILD:WITH. Preserve object/preseed/JSON/output handling, validate no JIT entry and real stripped execution, remove obsolete maker path. Internal Astra checker_followup_review owns tools routing in isolated workspace; Cedar reviews and owns C5 linker relocation separately.
