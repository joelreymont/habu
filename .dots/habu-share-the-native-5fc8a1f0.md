---
title: Share the native-build window with its regression
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T00:35:52.989806+03:00"
---

Problem: test/native-window-owner-child.f copies about 45 lines of tools/native-build.f's trusted reset machinery (LOGICAL-RESET, the core prefix, TRANSFER-CHECKER, the check-hook load) so the window-owner regression (landed with the checker fix 'Let the window's checker certify window source') can drive the handover in a bin/hb child. A copy diverges silently when the tool's window changes and the regression then stops testing the real window; nothing lints the pairing. Acceptance: the window's reset and transfer sequence lives once, in a library the tool and the test both load (tools/native-build-lib.f or the BUILD package in lib/build.f), with native-build.f's behaviour byte-identical (cold build from the seed produces the same product sha before and after) and the child test reduced to loading that library plus its fixture; the copied lines deleted. Files: tools/native-build.f, test/native-window-owner-child.f, lib/build.f or a new tools/native-build-lib.f, test/gate-stdlib-cases.f. Verify: seed-hosted cold build sha unchanged; test/native-window-owner.f green under bin/hb and under HABU_UNDER_TEST=/tmp/cedar-crossing-realpath/hb-stdin; the gate. Depends: none. Ownership: rowan (selfbuild). Claim: unassigned
