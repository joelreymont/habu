---
title: "Name the code target of a stripped image's PC-relative refusal"
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T18:07:21.897842+03:00"
---

Problem: src/habu/aot-lib.f MAP-TARGET! prints the code refusal 'aot: PC-relative target removed or outside closure site=<name> target=<n> target-word=<unknown>' with the raw target address and no name when no record owns the target exactly, the same gap the span refusal closed in 835093fd (CODE-NEIGHBOUR names the nearest record below as NAME+off). Acceptance: the code refusal names target-word by the same rule, NAME+off from the nearest word record below when ADDRESS-OWNER answers XREF-NULL, <unknown> only when no record lies below; a test/stripped-address.f case pins the string through the real writer; docs/native-applications.md states it beside the span rule. Files: src/habu/aot-lib.f, test/stripped-address.f, docs/native-applications.md. Verify: test/stripped-address.f, tools/hb-build-test.f. Depends: none. Ownership: hazel. Claim: agent=hazel workspace=.jj-ws/hazel-code-target.
