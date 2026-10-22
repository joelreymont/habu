---
title: Explain native compile fallbacks
status: open
priority: 2
issue-type: task
created-at: "2026-10-19T11:38:59.599898+03:00"
---

A real warm Habu replay has12989definitionhits but916compile attempts; COMPILE-N combines nestedbusy, eligibility refusal and cachemiss, so existing output cannot identify remaining speed cost. Add opt-in stderr diagnostics at actual fallback using current name/source facts and existing ordered admission predicates, with no repeated validation. Cover all paths to originalcompiler and preserve ordinary output/admission/productbytes. Count diagnostic attempts againstCOMPILE-N on successful real warm run, then optimize measured categories rather than guesses.
