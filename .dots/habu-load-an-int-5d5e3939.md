---
title: "Load an integer foreign call's arguments from the data stack"
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T18:15:15.714821+01:00"
---

Problem: each argument of a foreign call is copied into FFI-BUF through VALUE! or POINTER! (src/lib/ffi-abi.f:108, :110; FFI-SLOT :93, FFI-BUF :57), about 4.7 ns each on spark, and the arm64 primitive then reloads x0-x7 from that buffer (BFFI-LOAD-X0-X7, src/habu/habu1.f:1933), 14 ns of a 3-argument call's 44 ns (hunt fficall, 2026-11-17, Maki's /tmp/tim-fficall). Acceptance: a foreign call whose arguments are all integers or pointers loads them into its argument registers from the data stack, the count a literal, on every target that has foreign calls; ns per call measured before and after (fbench.f); the FFI and native tests pass. Depends: none. Claim: agent=tim workspace=.jj-ws.noindex/tim-ffiregs (Maki lead tim).
