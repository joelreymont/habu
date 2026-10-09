---
title: Refuse a read under the data stack base on Gforth
status: open
priority: 1
issue-type: task
created-at: "2026-10-09T19:55:20.412746+03:00"
blocks:
  - habu-hand-the-rest-32631946
---

Problem: native maps the data stack with an inaccessible page under its base (src/habu/stack-abi.f:1-15). A load or store there by engine or compiled code is a word reaching below the base, and src/habu/crash.f:169-192 (ONE FAULT IS NOT AN EXIT) resumes it at the underdepth throw: `hb: interpret stack underdepth: <token>`, throw 70, which a catch receives. A closed text raises the base to the caller's cursor, so a read under its floor but above the mapping's base reads caller cells without a fault. The Gforth host refuses only at a word's min-in gate and by depth after the token, so a word reached through `execute` that reads under the base and pushes back as many cells is not refused at that token: `: R ( n -- n ) 1 + ;  ' R execute` as a whole program (codegen dot, ~/.cache/tmp/heron-arm64/evidence/gfcodegen/report-3.md). Gforth classifies a fault as -4 only within 128 bytes past NEXTPAGE(sp0) (engine/signals.c:235); a read just under an empty stack measured -9.
Acceptance: on the Gforth host a data-stack read under the base, by host or compiled code, is refused at the token that made it with native's message and code 70, and a catch receives it, on the load path the hand-the-rest dot leaves (OUTER:INTERPRET for the program, reader.fs for engine source). The base is a fault boundary as native's is, not a depth comparison: the first cell under it faults. A closed text keeps native's floor. Every other fault keeps the host's current exit. Cases in test/gforth/cases/ match native: the program above; `dup` on an empty stack in a trusted: body; `1 ' W execute` handing a two-cell word one cell; each again under `catch`, which receives 70 and goes on.
Files: src/host/gforth/boot.fs, src/host/gforth/prims.fs, src/host/gforth/reader.fs, src/host/gforth/layout.fs, test/gforth/cases/, test/gforth/host-test.f.
Verify: `HB_TMP=$PWD/build/tmp bin/hb --load test/gforth/host-test.f`.
Depends: the hand-the-rest dot. Ownership: the files above. Worker: worker-max. Claim: unassigned.
