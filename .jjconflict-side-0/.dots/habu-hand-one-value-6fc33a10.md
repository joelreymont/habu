---
title: Hand one value to two block arguments
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T14:36:49.857902+02:00"
---

src/compiler/native/regalloc.f refuses an edge whose terminator hands the SAME value to two different arguments of one successor: E-A64RA-EDGE, because the class rule wants one register per position and one value cannot be in two at once. Minimal program, on master and unchanged by dot habu-save-the-loop-5f07e0c3: ': Q-N ( n -- n ) dup begin 1 - dup 0 <= until drop ;' migrated with 1 1 16 - the vector holds one value twice when the begin header is opened, so the branch names it at two positions. It is a loud refusal, not wrong code, and it is reachable from ordinary Forth: 'dup' before a loop, and now 'i' copied onto the vector before a 'begin' inside a '?do' (': M ( n -- n ) 0 swap 0 ?do i begin 1 - dup 0 < until drop i DBL-N + loop ;'). The fix is a copy: an edge that names one value twice needs the allocator to insert a move so each position has its own register, which is ordinary parallel-copy resolution on a critical edge. Owner: A64RA.
