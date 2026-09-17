---
title: Exercise the image-restore reset hooks of process-wide libs
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T16:06:41.060517+03:00"
---

Problem: libraries that hold process-wide OS state register an IMAGE-LIFECYCLE reset so a restored image starts clean (lib/signal.f RESET: descriptors, caught set, kept stub address; lib/process-env.f: the measured envp sizes and four mappings, 4067e600), and neither hook is exercised by a test: aspen's signal lane (38c4b5af) and the env-ceiling lane both state it as an untested boundary. A reset that forgets a cell would hand a restored process a descriptor number or a mapping its writer owned. Acceptance: one regression saves an image after INIT / after a builder allocated, restores it in a child, and asserts the facility reads as never initialised (SIGNAL:INIT succeeds, the env ceiling is re-measured from the child's own envp) and no stale descriptor is touched. Files: lib/signal-test.f or a test under test/, lib/process-env-test.f. Verify: the regression. Depends: habu-deliver-process-signals-b844698f landed. Ownership: lib tests. Claim: unassigned.
