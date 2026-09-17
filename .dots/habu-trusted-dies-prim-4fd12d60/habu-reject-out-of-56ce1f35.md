---
title: Reject out-of-range script argument access before dereference
status: closed
priority: 1
issue-type: task
created-at: "\\\"2026-09-11T16:07:57.880042+03:00\\\""
closed-at: "2026-09-11T17:21:56.363718+03:00"
close-reason: Reviewed a4b13907 integrated in current source; updated-prefix CLI contracts pass2.066s including actual child zero/negative/one-past and first/last/empty arguments. Standalone valid/rejected args were checked in original candidate. RawARGV sentinel unchanged.
---

Owner: /root/argv_bounds (Sol), separate workspace based on b0b90daa; reported by Rowan on BB20260911-125858.119-rowan-0ae4. On frozen cedar-tender-gates engine, a file containing `SCRIPT-ARGC . cr 0 SCRIPT-ARGV$ type cr`, invoked as `bin/hb --load file.f </dev/null`, prints0 then aborts134/SIGSEGV from NULL argv. Current source src/os/script-argv.f adds SCRIPT-ARG-START without bounds; src/os/env-base.f ARGV dereferences raw index and ARGV$ applies ZLEN. Reconfirm on latest integrated candidate; no fix yet. Maki guards argc locally, but runtime boundary should reject invalid access.

Acceptance: negative index, argc/one-past and missing arguments produce a named catchable range error; valid first/last/empty arguments work on --load and standalone MAIN paths. Check intentional raw argv sentinel users before changing ARGV contract; place bounds at the responsible public API. Preserve startup argv/envp and image reset behavior.


Fresh combined c0bd71d4 prefix validation now passes: /tmp/cedar-current-layout-checkpoint-2 --load tools/hb-cli-contracts-test.f, rc0 in2.066s, test:ok and hb-cli-contracts-test:ok. Child processes exercise zero arguments, negative/one-past indexes and first/last/empty values. This is the updated-prefix check that was outstanding after source commit a4b13907; full optimizing compiler build is separate and remains blocked at PF-LAYOUT-REQUIRE.
