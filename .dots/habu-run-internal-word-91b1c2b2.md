---
title: Run internal-word-gate runner in block
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T17:24:52.016685+02:00"
blocks:
  - habu-start-forked-child-6b8855de
---

Test leaf, blocked on the engine leaf that makes a forked child start at global scope.

Today test/internal-word-gate.f cannot call its runner from inside its own package block. Because a SUBJECT fork inherits the parent's open package scope, the suite builds a checked quotation with the ACTION word before ;package and then executes that quotation globally after the package closes - the arrangement and its reason are written down at test/internal-word-gate.f:757-758 on master. It works, but it is a workaround for an engine behavior, and it leaves the file's shape unlike every other suite.

Owned result: once forks start at global scope, convert the suite to its natural shape. The runner is called in-block, MAIN stays private and is never executed after the package closes, and the ACTION quotation and its explanatory comment come out. The suite going green in that shape, through its exact owning bin/hb --load path, IS the acceptance for the engine change - which is why this is a separate leaf rather than an assertion buried in the engine work.

Acceptance: test/internal-word-gate.f has no ACTION word and no post-;package execution; MAIN is private and unreferenced from outside the package; the suite is green through its exact owning bin/hb --load path and through the gate-stdlib slice that runs it; the package ownership and typed-locals diff lints pass on the change.
