---
title: Hoist native host lookup spans
status: active
priority: 2
issue-type: task
created-at: "2026-10-19T13:39:35.628418+03:00"
---

Exact warm self-build profile shows repeated dynamic-buffer ADDRESS-FIXED calls inside NHOST:ID-OF, including422 direct samples at its hottest range. This reverse lookup captures IMPL-N once and neither mutates arrays nor invokes callbacks. Borrow IMPL-LIVE and IMPL-ENTRY once per nonempty scan, preserve newest live exact-address match and empty allocation-free result, and keep retirement/reset behavior. Existing native host and closure consumer tests plus matching build qualification; measure changed lookup and independent Astra review.
