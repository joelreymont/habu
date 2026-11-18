---
title: Say a path length below 0 is below 0, not empty
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T14:03:11.144660+01:00"
---

Problem: a path whose length is below 0 (SOURCE-ROOT:JOIN with root length -1, CHECK and LOADER-PATH-FAULT with u < 0) is rejected E-PATH-RANGE with the message 'a path is empty': src/core/include.f LOADER-PATH-RECORD and REJECTION:PATH-RANGE encode the fault in len (0 empty, -1 too long once resolved, else the length), so a negative length can only pass as 0. Acceptance: '-1 s" x" SOURCE-ROOT:JOIN' (a program's call) is rejected with a message that says the length is below 0 and a help that names the range; the empty and too-long records are unchanged. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-pathneg.
