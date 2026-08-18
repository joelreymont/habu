---
title: "Census the 41 unchecked files: what does each span need"
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T19:54:53.456298+02:00"
---

Cut Phase-B design probe (ruled on a5aa3f1f, 2026-08-18): 41 files under src/lib/maki/tools/test carry 0 set-check spans; they have NO checker tape, and the chain requires a certificate - nothing says who compiles them after the cut. Census each span: WHICH capability gap does it name (the Habu-Only rule: unchecked is a named boundary awaiting capability)? Output: the list classified into (i) chain-expressible today (convert, delete the span), (ii) awaiting an owned capability dot (name it), (iii) genuinely engine-resident (the named-exception list the cut's E must preserve compilation for - expected tiny). The cut cannot land while the set is unclassified.
