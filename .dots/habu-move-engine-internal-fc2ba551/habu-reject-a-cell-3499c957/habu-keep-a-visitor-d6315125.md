---
title: "Keep a visitor's cursor for the next composition"
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T22:02:58.778625+01:00"
---

Problem: src/habu/verify-source.f CURSOR! (:4338-4340) writes the open composition's CSR-AT and CSR-STATE, so an ON-CANDIDATE visitor that calls it fires the open composition again (17 candidate calls against 8, CMPA and CMPB offered twice; /tmp/jerry-cc-nested-cursor-repro.f CASE-NESTED), and the composition's end (CSR-RESET, :4392) then drops the cursor its contract says is the next composition's. Acceptance: a CURSOR! during a composition changes nothing in it and arms the next one; test/checker-completion.f checks both. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-cn.
