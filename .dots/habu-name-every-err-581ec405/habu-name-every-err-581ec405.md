---
title: Name every error code
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T15:05:29.255228+02:00"
---

Joel 2026-11-06: ERROR:CODE definer (2026-11-06: no global word), package ERROR in src/lib/error.f (errors.f renamed), ERROR:NAME$ and ERROR:TEXT$ ( n -- ptr u8 n ), text required, LOG:CODE+ ( n -- ) writing '-9701 MODEL:E-KEY: the provider refused the key' (the number alone when undeclared), tools/check.f's thrown-code records carry the real name; built now, in parallel with incremental builds. No global and no ERROR:DECLARE. Children: rename, the ERROR package, the constant sweep, thrown-code names. The 107 codes minted in the cold-load prefix before error.f loads stay plain constants. Campaign dot; do not dispatch. Ownership: carl (lead). Claim: unassigned.
