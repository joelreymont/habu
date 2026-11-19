---
title: Reject a type nested past the depth limit
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T12:37:29.136514+01:00"
---

Problem: a structure field type nested about 4096 levels (sdnest<n,sdnest<...>> or ^^^...n) stops the run with a stack fault (exit 102) or 'term walk too deep (cyclic term)' (exit 76) before any depth check: TFW-NODE and REQUIRE-POINTEES recurse inside ?do, one loop frame per level, and the loop stack holds 4096 frames (src/habu/stack-abi.f LOOP-BYTES), the same as EFFECT-DEPTH-MAX; the parse (SD-PARSE-NODE-IMPL) overflows the data stack at 8187. Acceptance: every such type is rejected at the type with a message saying how to split it; test/structure-decl-suite.f has a case per stopping point that fails before. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-deepcut
