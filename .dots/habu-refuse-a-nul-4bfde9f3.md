---
title: "Refuse a NUL inside a caller's environment row"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:46:23.291960+02:00"
---

Problem: src/lib/process-env.f PROC-ENV-CHECK-ENTRY (:185) refuses an empty entry and one without '=' but not a NUL byte, and CMD:ENV+ (src/lib/process-command.f) checks its name through PROC-ENV-CHECK-NAME (which now refuses NUL) but not its value: every reader of a row stops at the first NUL, so CMD:ENV-ENTRY+ with 'HOME\0X=1' passes the check and the child gets a malformed 'HOME' row and no HOME, and CMD:ENV+ with the value 'a\0b' delivers 'a'. Found by the UNSETENV re-review (probe /tmp/carl-rev-unset2/probe/entry-probe.f, rc 0 on master); same cause as the NUL-in-name defect fixed in 'Let a command unset an inherited variable', outside that lane. Acceptance: PROC-ENV-CHECK-ENTRY refuses a NUL anywhere in the entry with E-PROC-ENV, in the same shape as PROC-ENV-CHECK-NAME; a value given to CMD:ENV+ (and any other value writer that forms a row, rg 'PROC-ENV-CHECK-NAME' src/lib) is refused on a NUL the same way, at the shared check; rows in src/lib/process-command-test.f (a NUL entry through ENV-ENTRY+, a NUL value through ENV+) that fail without the change and pass with it, beside NUL-ENV-NAME and NUL-UNSET-NAME. Files: src/lib/process-env.f, src/lib/process-command.f, src/lib/process-command-test.f. Verify: hb --load src/lib/process-command-test.f </dev/null; hb --load src/lib/process-env-test.f </dev/null; hb --load tools/check.f src/lib/process-env.f </dev/null; hb --load tools/check.f src/lib/process-command.f </dev/null. Depends: none. Ownership: src/lib/process-env.f PROC-ENV-CHECK-ENTRY, src/lib/process-command.f the value writers, src/lib/process-command-test.f. Claim: unassigned.
