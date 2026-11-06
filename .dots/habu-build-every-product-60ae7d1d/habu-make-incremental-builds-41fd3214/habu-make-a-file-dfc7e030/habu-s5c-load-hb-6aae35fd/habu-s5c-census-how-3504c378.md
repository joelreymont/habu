---
title: "S5c census: how far the startup files load in a build"
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T02:15:58.339091+02:00"
---

Problem: nothing past util.f:7 has been loaded inside a build (S5a-2 census /tmp/jerry-s5a/s5a2-report.md); which of boot-files.f's rows through include.f load inside LINKER:WITH once the checker checks a build apart from hb, and why the first one that does not stops, is not known. Acceptance: a report (no tree edit) naming each row loaded with its time, the first refusal's file:line, code and message, its cause at the layer that is wrong (lookup, definer, checker, kernel) with probe evidence, and whether S5b3's design (/tmp/jerry-s5b3/design-s5b3.md sections 3 and 7) or S5b2-2/3 (/tmp/jerry-big/design-s5b2.md:119-134) names it; whether hb is intact after the close. Files: none in the tree; probes under /tmp/jerry-s5c/. Verify: the probe's log. Depends: none (runs on the S5b2-1 candidate /tmp/jerry-s5b2b/hb). Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-s5c.
