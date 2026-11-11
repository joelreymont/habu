---
title: Give the running task a growable typed buffer of its own
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T11:30:37.339339+02:00"
---

Problem (tim, Maki lead, 2026-11-11; /tmp/tim-habu-limits.md H2, probes /tmp/tim-habu-limits/h2-shared.f, h2-user.f): a DYNAMIC-BUFFER is one mapping per name for the process, so task B reads the cell task A wrote (h2-shared.f prints 'B read cell 0 = 11', rc 0). Maki's sessions build two designs in two tasks, and about 305 scratch declarations in its construction, scene and operations code would be shared between them; src/kernel.f:184-226 in Maki does it by hand for six spans. Nearest words, none of which is one: TASK:+USER gives a fixed slot typed ptr n (a ptr rec result is refused, h2-user.f); BUF and VEC headers in a +USER row hold bytes or cells only; MEM:WITH-BYTES is the task's but fixed and untyped; the +USER band has 2,320 of 5,264 bytes free after task.f and log.f load. Acceptance: a growable typed buffer whose cells belong to the running task, declared as a DYNAMIC-BUFFER is, released when the task ends; a test in which two tasks write the same index and each reads its own value. Maki's ten package lanes for sessions need it after the run-time arena owner (dot habu-add-arena-with-9c2919e3, which Maki's first session landing waits on); designed with it, the design and its public words go to Joel. Files: src/core/dynamic-storage.f, src/lib/task.f, a test. Verify: the new test; Maki's scratch declarations on it. Depends: none. Ownership: unassigned. Claim: agent=carl (design).
