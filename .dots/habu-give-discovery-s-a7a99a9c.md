---
title: "Give discovery's read route a caller or delete it"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T03:01:48.121973+01:00"
---

Problem: tools/source-discovery.f READ-THROW, reached only through DISCOVER:RUN and DISCOVER:RUN-IN, has no production caller: RUN is called only from tools/source-discovery-test.f and test/source-root-test.f, RUN-IN only from tools/event-closure-lib.f, which only test/source-root-test.f loads; its directory branch is pinned by no test. Acceptance: the route has a production caller whose behaviour a test pins, or RUN, RUN-IN, READ-SELECTED, READ-THROW and event-closure-lib.f are deleted with their tests. Depends: habu-campaign-c4-diagnostics-3b6de147. Claim: unassigned.
