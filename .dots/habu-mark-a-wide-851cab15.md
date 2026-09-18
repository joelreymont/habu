---
title: Mark a wide does> word so interpret mode refuses it
status: active
priority: 2
issue-type: task
created-at: "2026-09-18T19:58:45.781441+03:00"
---

Problem (does>-glue lane, measured on the UNMODIFIED engine cee4c05e at tier 0): a create ... does> word whose declared clause effect is wide is not marked DNAME-WIDE, so '64 SPAN-BUFFER: PBUF' then a bare 'PBUF' at the interpret prompt lands two cells on the interpret stack - the interpret-mode layout-value guard (docs/forth.md 'interpret-mode layout value', DNAME-WIDE gate at interpret dispatch) is bypassed for does> words. Cause: src/habu/habu2.f LASTC-TRUST:PUBLISH calls the raw registrar without the REC-WIDE-PUBLISH tail the compile publish paths use. Acceptance: a does>-created word whose clause effect is wide gets DNAME-WIDE at publication (the same tail the colon publish path uses, mirrored in bootstrap/cg/forth.fs), so a bare interpret-mode call is refused with the existing 'interpret-mode layout value' diagnostic while compiled calls and checked bodies keep working (lib/span-test.f, the SPAN-BUFFER: consumers); red-first fixture in the suite that owns the interpret guard (rg -l 'interpret-mode layout' test) using a does> definer; docs/forth.md's bullet names does> words. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, test/, docs/forth.md. Verify: the fixture; three generations with cmp; bootstrap check; test/run.f. Depends: habu-give-a-does-97cd0db2. Ownership: engine publication. Claim: agent=hazel workspace=.jj-ws/hazel-wide-does
