---
title: Retire values.f
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T08:59:18.166901+02:00"
---

Problem: typed storage comes only from typed mechanisms (N9 design, carl-dots/.out/carl-file/n9-design.md §1-§6, rulings R1-R10 in §7; Joel 2026-10-29 via tim, item 57). Ruling R8 was Joel's: on 2026-10-30 Joel answered (relayed by tim): retire values.f (VALUES, VALUE-SCOPE); Maki's STEP importer emits DYNAMIC-BUFFER declarations released through RELEASE-RANGE (Maki genie item 60). Acceptance: src/lib/values.f and its words VALUES and VALUE-SCOPE gone with their tests and docs lines; every caller in src, tools, test and the trees under ~/Work (Etch, Maki, Tender) moved to the typed mechanism N9 names for its case or shown to have none; forth.md and forth-card.md no longer mention them; the full suite green. Files: src/lib/values.f, its test, docs/forth.md, docs/forth-card.md, callers found by rg. Verify: rg finds no VALUES/VALUE-SCOPE outside history; bin/hb --load test/run.f. Depends: none (Maki's importer change is Maki's item 60). Ownership: values.f and its callers. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-values (brief carl-dots/.out/carl-file/brief-values.md). Census 2026-10-30 on d3b167c2: no caller in src or tools; test/values.f, the gate row, errors.f:1199 and test/checker-soundness-suite.f (family rows CS-K49, CS-K75, CS-K77); docs mention neither word; Etch and Tender none; Maki's step-source.f and weld-source.f still emit VALUES:values<…> text (Maki's item 60).
