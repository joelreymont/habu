---
title: Match the public engine keyword table
status: open
priority: 1
issue-type: task
created-at: "2026-10-15T19:11:20.419408+03:00"
---

The selected public e6208777-zip-remove-987ed490 prefix contains ESQ-KW source from 239b2fa5 but its unchanged e620 engine rejects test/policy/escaped.f with exit 107, not in vocabulary S\". Reproduce from the immutable public root with bin/hb --load test/policy/allow.f -- test/policy/escaped.f. Diagnose the source/product mismatch at the release/build boundary; do not assume a cache identity defect without evidence. Ship a matching qualified engine, map and sources preserving reviewed XML/ZIP, JSONL 622e793d and HB_PG_CASE_MS. Existing escaped-policy E2E must pass from the installed prefix. Carl owns the matching core release; no Maki workaround.
