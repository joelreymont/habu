---
title: Give tier.f origin tests a per-run temp dir
status: active
priority: 3
issue-type: task
created-at: "2026-10-24T01:50:48.060590+03:00"
---

test/tier.f OR-PATH$/OR-IMAGE-PATH$ (hb-code-origin-capacity.f, hb-code-origin-grown-image) are fixed names under TMP-PATH: three tier.f runs sharing HB_TMP gave two -2105 failures (chkdoes review). Acceptance: per-run MAKE-TEMP-DIR, three concurrent runs on one HB_TMP pass, nothing left behind. Lane: jerry, worker in .jj-ws.noindex/jerry-tmpflake.
