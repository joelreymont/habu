---
title: "Say str in the JSON reader's remaining rows"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T23:12:49.085773+01:00"
---

Problem: src/lib/json/read.f READ-STR ( JSON:reader ^u8 n -- JSON:reader n ) decodes into a caller's ^u8 n buffer, beside READ-STR-APPEND, which appends to a VEC:vector<u8>; tools/gate-json-assert-core.f GJA-STRING-INTO ( SPAN:span<u8> ^u8 n -- str ) does the same; READ-LISTED's seen argument is a bare ^u8 the caller sizes with SEEN-BYTES and zeroes. Joel 2026-11-19: "it's either str or vector". Acceptance: READ-STR is deleted and its callers append with READ-STR-APPEND; GJA-STRING-INTO fills a vector; READ-LISTED takes a ^VEC:vector<u8> it sizes itself, empty before an object's first call; consumer owners (Tender, Zen, Maki, sapd) are told before the install. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-jsonvec.
