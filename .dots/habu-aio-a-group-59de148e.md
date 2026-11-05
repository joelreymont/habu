---
title: "AIO: a group hands back the caller's number for a finished wait"
status: active
priority: 2
issue-type: task
created-at: "2026-11-05T11:28:08.566148+02:00"
---

Joel 2026-11-05 chose 'Tag each wait (Recommended)' (DECISIONS 'which wait finished'): GROUP+ ( ticket n group -- ) takes the caller's number for the wait and AWAIT-ANY ( group -- n outcome ) hands it back. Problem: curl.f:781-784, ws.f:64,305-306 (and lane carl-http1's http.f:105-108) each CAST: AIO's ticket to n to compare it, and curl FD-SLOT-OF (:1368) and ws SLOT-OF (:889) scan their slots for the ticket AWAIT-ANY returned, while aio.f G-READY (:1262) already found its group position. Acceptance: the group row keeps the caller's number beside each handle; GROUP+/AWAIT-ANY take the new effects; curl, ws and their tests use the number (FD-SLOT-OF, SLOT-OF, SAME-TICKET? and their casts gone); aio-test.f and test/aio-capture-*-subject.f follow. Files: src/lib/aio.f, src/lib/net/curl.f, src/lib/net/ws.f, src/lib/aio-test.f, test/aio-capture-held-subject.f, test/aio-capture-sweep-subject.f. Verify: aio-test.f, curl-test.f, ws-test.f, the aio capture rows, macOS and spark. Depends: none. Ownership: carl. Claim: agent=worker workspace=.jj-ws.noindex/carl-aiotag.
