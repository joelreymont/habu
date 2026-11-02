---
title: "Clear MODEL's transport when a transfer read throws"
status: open
priority: 3
issue-type: task
created-at: "2026-11-02T14:23:52.221172+02:00"
---

src/lib/net/model.f reads its transfers with CURL:NEXT (DISCARD-REPLY ~1570, the stream reader ~1601, FEED-CHUNK's caller ~2306). CURL:NEXT throws E-STATE when the handle is not streaming or a request is pending (src/lib/net/curl.f NEXT ~1807); the throw leaves FOLLOW without END-TRANSFER, so the call keeps its transport set and a later CANCEL acts on a transfer that is gone. Only a broken invariant reaches it today (MODEL fold-2 re-review, review-model-fold2-rr.md 'Not fold-ins'), so it is robustness, not a regression. Acceptance: every exit from a call's transfer, a throw included, ends the transfer and clears the transport (as SETTLE does on the normal path), and the throw still reaches the caller; a test that makes CURL:NEXT throw mid-call and then CANCELs shows no stale transport. Files: src/lib/net/model.f, src/lib/net/model-test.f. Depends: MODEL's stream lane landing (habu-let-a-model-c56be9a7).
