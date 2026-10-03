---
title: Make check-main.f load on its own
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T07:07:15.062011+02:00\""
closed-at: "2026-10-01T12:55:46.504319+02:00"
close-reason: Fixed by qzslsoqr 9017f179 (r4-small lane, reviews ACCEPT incl. 130)
---

Problem: tools/check-main.f:1-3 does not require tools/check-core.f, so bin/hb --load tools/check-main.f and bin/hb --load tools/check.f -- tools/check-main.f both die E-UNDEFINED CHECK:MAIN rc 70. Adding the require makes it the same program as tools/check.f (:3-5), and the gate list GE-CHECK-SUPPORT-ARGV (test/gate-common-lib.f:397-420, :574) names check-main.f right after check-core.f; one of the two entries should go. tools/build-fixpoint-main.f:3-5 also claims a require could not report the load list, which build-fixpoint.f:52-55 BF-NEED-PREAMBLE does (found by the r4-small review of 7c7d631b). Acceptance: one CLI entry for check, which loads standalone and under tools/check.f; every caller (gate lists, docs, tools) names the survivor; build-fixpoint-main.f's comment states the measured reason. Files: tools/check-main.f, tools/check.f, test/gate-common-lib.f, callers found by rg, tools/build-fixpoint-main.f. Verify: standalone load, tools/check.f on the entry, tools/check-test.f, the gate rows that load it. Ownership: check CLI entry.
