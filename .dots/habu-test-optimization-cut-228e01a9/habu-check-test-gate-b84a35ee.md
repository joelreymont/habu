---
title: Check test/gate-aot-positive-lib.f through check.f
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T16:17:51.294324+03:00"
---

Found by lane 438 (7aa9cbb0, 7015f0a3): tools/check.f refuses test/gate-aot-positive-lib.f with E-UNDEFINED ASM-CODE at src/habu/driver-io.f:39 (DRV-EMIT-IMAGE), on fc7763d3 and its parent alike, while the file's gate rows pass through --load: check.f's view of the file's requires lacks the assembler that defines ASM-CODE. Acceptance: find whether the file's require order, driver-io.f's own requires, or check.f's pre-pass image is wrong; fix that layer so check.f certifies the file (or refuses it by a correct name the --load path agrees with); seen failing first.
