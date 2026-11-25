---
title: Name a return cell a loop changes
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T23:37:53.841222+01:00"
---

Problem: `: W ( n -- ) 0 >r begin r> drop true >r 1 - dup 0= until drop r> drop ;` changes a return-stack cell from n to bool at the loop's back edge, and is rejected with E-RETURN-STACK "the return row does not balance at exit" at `until`, expected and found both "(nothing)", though the row balances; the data-stack twin (`0 swap begin nip true swap 1 - dup 0= until 2drop`) is named as an E-MISMATCH with both types. Acceptance: the rejection says the return cell's type at the loop's entry and at its back edge, at the closer, with a fix. Depends: none. Claim: unassigned.
