---
title: "C2c-c: find why facts mode refuses three cast texts"
status: active
priority: 2
issue-type: task
created-at: "2026-11-07T00:41:14.526672+02:00"
---

Problem: on the C2c-a candidate (/tmp/jc2.GN/hb) the facts probe /tmp/jc2.Mo/fid-new.f leaves three records parse-refused (/tmp/jc2.GN/new.raw:24529, 24530, 24550): ( ptr u8 n -- read-view<a,b,u8> ), ( ptr u8 n -- mut-view<a,b,c,u8> ), ( ptr u8 n -- mut-view<a,b,c,init<b,c2-mem:owner-state>> ), the texts of CAST: READ-PACK and MUT-PACK (src/lib/c2-memory.f:41-42) and OWNER-VIEW-PACK (src/lib/c2-owner.f:13). Each has a type variable only among its outputs (hypothesis). C2c's refusal would fire on them if CAST: rows reach CHECK-FACTS. Acceptance: a reported cause with file:line, the rule the fix follows, and whether C2c's check reaches CAST: rows; the fix is a second brief. Files: read-only. Verify: the worker's reproducer under /tmp/jrv.*. Depends: C2c-a (habu-c2c-a-facts-23c32585). Brief: /tmp/jerry-big/brief-rca-view.md. Ownership: jerry. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-rca-view.
