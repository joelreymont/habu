---
title: Escape control bytes in JSON renderer
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T09:01:14.729466+02:00"
---

Problem: src/core/render.f JCHAR (near line 370) escapes only newline, carriage return, tab, double quote, and backslash; every other control byte below 32 passes through raw, so a token containing one yields invalid JSON on both the legacy and the unified diagnostic legs, and the prose leg is unescaped on both. Pre-existing and shared - one escaping fix at the single renderer. Required result: JCHAR emits the JSON \\u00XX form for every remaining byte below 32, and the prose leg routes through the same escaping where it feeds JSON. The 96-byte token cap stays and the truncation MARKER stays per the recorded ruling (silent truncation rejected). Acceptance: a fixture with a control byte inside a token renders valid JSON byte-for-byte as expected on both legs; a fixture proves the marker still appears on truncation; a mutation restoring raw pass-through fails. Files: src/core/render.f and its focused render test. Verify: the render suite and one end-to-end diagnostic fixture through bin/hb. Depends: none. Ownership: the shared JSON character escaper only. Claim: unassigned.
