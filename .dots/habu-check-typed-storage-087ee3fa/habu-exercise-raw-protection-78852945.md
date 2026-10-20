---
title: Exercise raw protection with typed checks
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T12:53:45.222778+03:00"
closed-at: "2026-10-19T13:01:38.901594+03:00"
close-reason: Confine deliberately raw addresses to isolated unchecked test subjects; runtime guards remain enabled and every exit expectation unchanged. Matching native protection suite passes all71assertions; independent Astra review and red-green check clear.
---

Real matching-native test/protection-span.f has11failures because checker70 occurs before expected runtime83 or success0. Identify whether subjects intentionally exercise unchecked runtime/foreign writes; adapt the smallest explicit test boundary when justified, preserving every runtime protection assertion and successful control. Do not weaken the checker or guard. Existing full protection E2E must pass natively.
