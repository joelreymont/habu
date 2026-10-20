---
title: Use current object preview interface
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T14:15:17.986162+03:00"
closed-at: "2026-10-19T14:31:36.826678+03:00"
close-reason: "Existing real AOT cache E2E now uses public OBJIMG:LOAD with all cache/refusal/map/publication/executable assertions retained. Matching f949native tools/hb-build-aot-cache-test.f passedexit0; independent Astra reviewclear. No production compatibility shim or added test."
---

Existing tools/hb-build-aot-cache-test.f fails to load on matching21b4c68d with E-UNDEFINED OBJIMG:RESET. Object extraction now exports LOAD/WRITE; the single remaining RESET ADD fixture sequence must use that real public interface while preserving object-cache poisoning, target/refusal and executable artifact checks. The unmodified failing real-load E2E is the regression. No production API compatibility shim or new isolated test.
