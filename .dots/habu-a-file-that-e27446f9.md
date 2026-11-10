---
title: A file that makes the namespace wordlist current exits 83 with no message
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T21:01:24.051438+02:00"
---

Problem: a file whose first line is 'BOOT:NAMESPACE-WL set-current' followed by ': MAIN ( -- ) ;' ends hb-build and tools/check.f with exit code 83 and prints nothing, on the installed hb (built from 8654226d) and on the incremental-builds stack's engine alike (reproducer ~/.cache/tmp/carl-d6/probe2/ns.f: 'hb --load tools/check.f -- ns.f'). Cause not looked for. Acceptance: the refusal names the token and why, as other refusals do. Files: unknown until the cause is found. Verify: a rejected-program test through the real load path. Depends: none. Ownership: unassigned. Claim: unassigned.
