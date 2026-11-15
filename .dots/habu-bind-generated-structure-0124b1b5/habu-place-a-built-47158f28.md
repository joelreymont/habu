---
title: "Place a built definition's rejection in its source"
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T17:48:43.762073+01:00"
---

Problem: a rejected definition a definer builds from items names listing tokens, not source: 'at l0', 'hook: non-certified definition ... at l0', 'compiler: cannot compile ... at l0' (a built local's listing spelling is l<i>, EFFECT LOCAL$). A reader cannot map l0 to what the declaration wrote. A rejection a definer raises for a token it read earlier is placed where the definer stopped: an FSM table's undefined action or unknown state type (src/lib/fsm.f:187-235) points at ;DEFINE, not at the token. Acceptance: a rejection inside a built definition names the declaration's source line and the item as the declaration wrote it (the effect's type, the called name), with no listing token, as a build-items test; an FSM table's rejection names the token's line, as an fsm-test case. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-l0.
