---
title: Order candidate build dependencies
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T12:13:06.537842+02:00\""
---

Full cold gate reproduced E-FS-OPEN while standalone engine-build and tail-process slices passed. Evidence isolated two dependency violations: the not-yet-created output was exported through input-only HABU_UNDER_TEST, and candidate consumer phase 36 was scheduled before the build owning that artifact completed successfully. Use explicit --candidate-out, publish HABU_UNDER_TEST only after a green pool completion event, keep phase 36 out of early work, and add pool-event plus structural regressions. Acceptance: engine build precedes tail-process in the exact cold gate, tail-process is green, typed/local/host/filemap lints pass, and the final combined cold gate is green after PTY integration.
