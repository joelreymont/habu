---
title: Drop the 16-byte inline-name shim from the capture
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T20:17:12.437764+02:00"
closed-at: "2026-11-06T21:26:30.375584+02:00"
close-reason: "landed: chain fixpoint gen 2 (/tmp/jc.4y, 188 s real), full suite 293 of 293 rc=0 (/tmp/jt.2W, 109.9 s wall); review ACCEPT (/tmp/jerry-rev8)"
---

Problem: C1 ('Keep names of up to 8 bytes inline', from its review 2026-11-06): src/habu/aot-capture.f ACAP-COMPACT-ONE's source-side refusal 'ext 0= len 16 >' and its 'len DNAME-INL > if DNAME-EXT 60 rshift else 0 then flags or' exist only for a building hb that keeps 16-byte names inline. Acceptance: once carl installs an hb carrying C1, the refusal becomes 'len DNAME-INL >' and the expression goes. Files: src/habu/aot-capture.f. Verify: candidate build from the installed hb; bin/hb --load test/run.f. Depends: an installed hb carrying C1. Ownership: src/habu/aot-capture.f. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-shim.
