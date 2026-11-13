---
title: Delete the public words nothing calls
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T22:29:22.645345+01:00"
---

Problem: public words that no production code calls (a test caller is no caller), found by census on 2026-11-13 across Habu, Zen, Tender and Maki. No caller anywhere: GENIO FAULT@, LATCH, FORWARD and FORWARD-EMIT (src/lib/genio-core.f:319, :325, :437, :448), and LIVE-DEVICE (:330), whose only callers are FORWARD and FORWARD-EMIT; FFI FN-ROW@ and SYMBOL-CAP (src/lib/ffi-abi.f:772, :776); CREW E-FIRST and E-LAST (src/lib/crew.f:19-20). Only tests call: HTTP:REPLY (src/lib/net/http.f:900; http-test.f BODY-PROBE and LATER-REPLY), MCP:CANCEL (src/lib/net/mcp.f:1676; mcp-test.f CANCELLED and CLIENT-CANCEL), and the whole of src/lib/crew.f (only src/lib/crew-test.f requires it; suite crew in test/gate-stdlib-cases.f:488-489). Called only inside its own file: FFI LIB-ROW@ (ffi-abi.f:377, from REPORT-NAMES and SYMBOL-REPORT). ffi-abi.f is baked; the others are not. Acceptance: each word above is deleted together with the test cases that call it, LIB-ROW@ is private, crew.f, crew-test.f and the crew suite are gone, and a case-insensitive search (rg -i --no-ignore) of Habu, Zen, Tender and Maki finds no remaining caller. Depends: none. Claim: unassigned.
