---
title: "Move curl-test onto FD:pollfd"
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T14:28:16.517744+01:00"
blocks:
  - habu-take-curl-s-ed37df96
---

Problem: src/lib/net/curl-test.f:143,828-845 (RETRY-POLLS) packs two struct pollfds by LE offsets 4, 6, 8 and 14 beside FD's pollfd type. Acceptance: RETRY-POLLS holds FD:pollfd values built and read through FD:>POLLFD and FD:REVENTS; no offset spells a pollfd outside FD; the curl tests pass. Depends: habu-take-curl-s-ed37df96 (carl's lane edits curl-test.f). Claim: unassigned.
