---
title: "C2c-b: facts text of a generic sum with slot tags builds a different effect"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T00:13:25.150736+02:00"
closed-at: "2026-11-07T04:02:42.437469+02:00"
close-reason: "implemented, reviewed, landed with C2c (master after b05b406c): gate chain /tmp/jc.7p fixpoint at gen2 rc 0 (real 445 s), full suite 293 of 293 PASS rc 0 (/tmp/jt.yi/suite.log)"
---

Problem: C2c's check (jerry-c2c nkwlmvos) finds 12 definitions whose facts text parses but builds a different effect (/tmp/jc2.zz/diffs.txt, new=1 rows): generic sums over package types with slot tags, e.g. run-capture: ( ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> @result.slot2<pcap:captured,pcap:failed> @result.slot3<pcap:captured,pcap:failed> ) in src/lib/process.f (capture>result, capture-rc@, capture-finish-rc, run-capture, run-argv-*-capture), tools native-build smoke-result, src/lib/str.f split-next option<str:split>. Unknown: which part of the rebuilt record differs. Acceptance: (1) a root-cause report from a reproducer (.jj-ws.noindex/jerry-rca-sum, brief /tmp/jerry-big/brief-rca-sum.md); (2) a fix brief and lane; C2c reruns its proof a-d after the fix. Files: src/core/render.f, src/core/checker.f (by the RCA). Verify: the reproducer reads every row back same; C2c's build passes with the throw. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-rca-sum
