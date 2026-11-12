---
title: "Print every interpreter refusal in the checker's shape, with a class"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T08:03:07.752747+02:00"
---

Problem: about 40 refusals in src/habu/interpreter.f print 'hb: text' with no class (e.g. 'hb: dictionary full at:', 'hb: defer: missing name after'), so such a refusal names no reason an agent can look up, while the classed refusals print the checker's shape, 'E-CLASS habu: path:line: prose'. Acceptance: every refusal interpreter.f prints starts with an E- class in the checker's shape, path:line first where a file is open; the tests that match the old 'hb:' text match the new. Map of each refusal and the class tools/check.f gives it: .jj-ws.noindex/jerry-diag-b/.out/map-report.md. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-diag-b.
