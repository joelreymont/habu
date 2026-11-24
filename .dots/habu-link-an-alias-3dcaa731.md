---
title: "Link an alias of one of hb's records from an object"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T19:40:27.176937+01:00"
---

Problem: an alias of one of hb's records (RECORD+ finds SAME-START -1, ID-REF+ writes HB-FILE -1 0) reaches LINK-ALIAS and ID-REC (src/habu/linker.f about :4198 at mlmvuxml) with file -1, and -1 FILES-ROW reads before the FILES section; a warm build would then fail where a cold one passes. Inferred from the code by the damage lane; no file-objects fixture exports one of hb's words. Acceptance: a program that exports or aliases one of hb's words builds warm as it builds cold, with a test/file-objects.f case if the reproducer shows the failure. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-alias.
