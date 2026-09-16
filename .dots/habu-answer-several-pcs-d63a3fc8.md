---
title: Answer several pcs in one imgdump scan
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T14:59:03.612795+03:00"
---

Problem: tools/imgdump.f --pc runs FIND-DICT's longest-run scan over the whole image (about 63 s for a 5.8 MB engine on a loaded host) once per invocation and answers one pc, so tools/imgdump-test.f's two real-engine --pc cases (62fdea1b) cost about 128 s and make suite imgdump-compare the long pole of the stdlib gate. Also pre-existing: in a no-trailer image E-S adds XTBASE to a namespace record's slot 0 (a WID role), so a package row's start column is garbage under a header that says start. Acceptance: one --pc invocation accepts several pcs and prints one line per pc after a single scan; tools/imgdump-test.f queries + and evaluate through one child and its wall time roughly halves (measure before and after); the namespace row prints its two WID roles under honest column names or E-S stops rebasing namespace records (ENT? and FIND-DICT must keep working, so verify the scan on a real engine and a snapshot image). Files: tools/imgdump.f, tools/imgdump-test.f. Verify: bin/hb --load tools/imgdump-test.f with timing; --pc on a real engine and on an hb-build --repl snapshot. Depends: none. Ownership: tools/imgdump.f tools/imgdump-test.f. Claim: unassigned. Source: audit-imgdump worker notes 2026-09-16.
