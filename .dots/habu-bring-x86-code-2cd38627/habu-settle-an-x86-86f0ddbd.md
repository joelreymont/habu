---
title: Settle an x86 stack run before disp8 ends
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T17:53:57.763529+01:00"
---

Problem: src/habu/t0-x64.f keeps the stack pointer pending over a whole straight run, so 367 of tenderd's runs reach past a byte displacement and 4,957 accesses carry disp32; settling where an access leaves disp8 saves 11,850 bytes, all in wide-record words such as ROLE-READER. Acceptance: tenderd's x86 code shrinks by that and no run gains a second lea it does not need. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-settle
