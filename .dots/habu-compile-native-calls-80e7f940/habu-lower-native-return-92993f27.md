---
title: Lower native return stack
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:59:20.040248+02:00\""
blocks:
  - habu-lower-native-typed-01db198b
---

Full context: design Wave 4 adds >R, R>, and R@ as explicit typed return-stack state, not hidden machine stack behavior. Acceptance: depth/type/ownership across branches and calls validates; underflow, leak, wrong role, and exception-unwind mutations reject.

Claim: agent=rstack workspace=.jj-ws/habu-rstack-design
