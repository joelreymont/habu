---
title: Remove repeated test application builds
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T14:27:12.763605+03:00"
closed-at: "2026-10-20T14:42:16.219003+03:00"
close-reason: Removed eight repeated application builds while retaining distinct loader, alias, literal, lifecycle, number and stack behavior. Focused changed rows, auto actions and retained prefix-producer E2E pass; independent Astra review found no behavioral defect. Combined suite timing remains a separate campaign.
---

Remove repeated application builds in native-file-auto, native-file-missing, native-long-string-image, native-file-alias, hb-build-stripped-lifecycle and native-file-stack, plus six unused number-parser fixture path declarations and initializers. The five standalone rows pass before and after, reducing subject build invocations from 16 to 11; auto's unchanged actions slice also passes before and after (eight invocations). Removing its duplicate producer case drops three more full-suite invocations, with the retained prefix-producer E2E passing. Exact candidate a02380d3/1607423e, external HB_TMP, EOF and fd limit 512; no production edits or engine rebuild. Selected serial group wall/user/sys seconds: 240.94/150.32/9.20 before, 90.50/79.21/6.34 after; prefix-producer 62.74/60.44/1.32. Concurrent native-gate load and naturally warmed support caches prevent an isolated savings claim. Artifacts and logs: ~/.cache/tmp/habu-suite-builds.xr2Tnc. CLI forwarding and whole-image no-op remain covered by hb-build-cli-errors and hb-build-repl-fresh. Active pending the lead's independent review and integration.
