---
title: FS-WRITE-BY-FLAGS leaks the fd on refusal
status: active
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.921224+02:00"
---

Problem: lib/fs.f:311-314 checks FILE? before open and again after it; the second check throws E-FS-OPEN past the open fd, so every WRITE-ALL/APPEND-FILE on a fifo, device, or a path raced into a directory leaks one descriptor; the runner and gate pools call these in loops. Acceptance: close FS-IO-FD before the throw (the shape two lines below), drop the pre-open duplicate; a test writes to a fifo path and asserts the fd count unchanged. Files: lib/fs.f, lib/fs-test.f. Verify: the test. Depends: none. Ownership: fs. Claim: unassigned (released 2026-08-23; see the correction below).

NOT landed in this lane; contract corrected by the lane's measurement: the pre-open FILE? guard is what keeps open() from blocking forever on a fifo and must STAY; the leak is the TOCTOU race between the two checks (1,554 fds in 2,000 raced WRITE-ALL calls); the fix is `FS-IO-FD @ close` before the post-open throw plus the raced reproducer as the test; a descriptor-level check needs the fstat primitive (habu-no-fstat-primitive-938ab6f2). Blocked on lib/fs.f being frozen (267-file seal cascade) until habu-pkg-diff-lint-8bed2604 lands. Claim released 2026-08-23; reopen as a two-line lane after the lint policy.

Claim: alder, .jj-ws/alder-fs-refusal on 19caf2eb. Hazel released exactly the
post-open close-before-throw at fs.f:409 and its raced fs-test regression. The
pre-open guard stays; no walk, span, descriptor-identity or engine changes.
Private reduction on 1afd910c: 2,000 raced writes, 1,582 refusals, first free
fd 3 before and 30 after the worker finished. /tmp/alder-fs-race/before.log.

Repair: close FS-IO-FD before the post-open FILE? refusal. The fixture races
atomic symlink replacements between a regular file and /dev/null, alternates
4,000 WRITE-ALL/APPEND-FILE calls, joins the worker, and compares the lowest
free descriptor. The new fixture on unchanged source failed only that comparison
(F73, rc 1); after the close it passes. The pre-open guard remains unchanged.
Astra xhigh review found no blockers; the race remains scheduling-dependent.
Evidence: /tmp/alder-fs-refusal/before.log and after.log.
All 34 owning/reader registry rows passed in a private tree with private
host/HOME/HB_TMP, including the whole 11-file tail-pure-fixtures and hb-build
rows. build-fixpoint-fixtures invokes prohibited install --force and is deferred
to Hazel's serial gate. No full gate or shared-engine writes in this lane.
