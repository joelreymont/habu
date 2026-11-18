---
title: Reject a deep signature in a split-str file at the depth limit
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T07:25:35.578204+01:00"
closed-at: "2026-11-18T07:35:06.342122+01:00"
---

Problem: a signature nested 2049 deep in a file marked split-str overflows the data stack (E-STACK-BOUNDS in RV-NEXT?, src/core/checker.f:2158, under SPLIT-ROW :12331) instead of getting the depth-limit rejection an unmarked file gets; reproducer /tmp/jerry-strmech/probe/sn-marked.f, same under the installed hb. It turns test/signature-nesting.f red once the str commit marks it. Acceptance: the marked reproducer gets the unmarked file's record; signature-nesting.f passes marked; a test fails first. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-snfix.
