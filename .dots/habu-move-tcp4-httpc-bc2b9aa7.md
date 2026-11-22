---
title: "Move tcp4, httpc and curl-test onto FD:pollfd"
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T14:28:16.517744+01:00"
blocks:
  - habu-take-curl-s-ed37df96
---

Problem: three more modules pack a struct pollfd by LE offsets beside FD's pollfd type: src/lib/net/tcp4.f:157-167,377-379 (POLLFD-ROW, POLL-FD), src/lib/net/httpc.f:164,218,265-268 (the conn's BUFFER pollfd 8, PFD, POLL-ONCE) and src/lib/net/curl-test.f:143,828-845 (RETRY-POLLS, two pollfds by offsets 4, 6, 8 and 14). Acceptance: each holds FD:pollfd values built and read through FD:>POLLFD and FD:REVENTS; no offset spells a pollfd outside FD; the tcp4, httpc and curl tests pass. Depends: habu-take-curl-s-ed37df96 for curl-test.f only (carl's lane edits it). Claim: agent=worker workspace=.jj-ws.noindex/jerry-tpfd.
