---
title: Delete the free-factor mark nobody sets
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:08:23.113226+01:00"
---

Problem: EXT-MARK-FREE-TAIL (src/core/checker.f) has no caller outside tests and no declaration word; the EXTPROD: definer its comment names exists in no Habu file and no consumer. Acceptance: the word and the checker state only it sets are deleted, after a case-insensitive caller search; extent-product-test keeps its other assertions. Depends: habu-restate-tests-off-5cb50894. Claim: agent=carl workspace=.jj-ws.noindex/carl-pub.
