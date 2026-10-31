---
title: "Refuse a TRUSTED: body that splits a view in T0"
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T13:37:06.903849+02:00"
---

Problem: a TRUSTED: body that treats a two-cell view as two stack items crashes the default arm64 tier (T0) instead of compiling or refusing. Reproducer: /tmp/jerry-big/t0-2dup-crash-c2.f in place of src/lib/xml/c2.f (master 55cc849f plus lane habu-check-the-xml-33e43169): CURSOR-BASE ( mut-view<..> -- mut-view<..> ptr u8 ) is '2dup drop' and DEST-SPAN ( mut-view<x,y,z,u8> -- mut-view<x,y,z,u8> ptr u8 n ) is '2dup', with no unpacking cast. Then bin/hb --load test/c2-xml-consumer-program.f test/c2-xml-consumer-refusals.f dies SIGSEGV rc 139 with a habu-crash register dump and no output; test/c2-xml-consumer-e2e.f (image save, tier 1) ends 'ncomp: cannot compile CURSOR-BASE', uncaught -8304 rc 1 (the tier-1 half is in habu-refuse-a-malformed-f1e58040). tools/check.f admits the file (TRUSTED: bodies are unchecked). Acceptance: reduced reproducer; root cause at the responsible layer (T0 compile of an unchecked body against its declared logical shape, or the runtime); T0 refuses with a named code and the token, rc 70, or compiles the raw-cell meaning; a rejected-program row that crashes on master. Files: per root cause. Verify: the reduced row; SUITE tier. Depends: none. Ownership: per root cause. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-t0tr
