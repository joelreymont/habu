---
title: "Read a stream's headers before its body"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T13:19:15.417920+02:00"
---

Problem: src/lib/net/curl.f:1911 HEADERS refuses a handle in flight (E-STATE), so a streamed transfer's status line and headers can be read only after its whole body has gone to the caller. A client that must check a status, Content-Range or a result header before it writes any byte cannot. Acceptance: HEADERS answers the final header block of a streamed transfer from its first chunk on, and a whole-body transfer in flight stays E-STATE. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/habu-read-a-stream-84954a8a.
