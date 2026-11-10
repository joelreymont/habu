---
title: Turn on the Zed device runs without HABU_ZED
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:34:19.527578+02:00"
---

Problem: tools/zed-run.f and tools/zed-run-lib.f run the Zed editor checks only when the environment variable HABU_ZED is set and not 0 (tools/zed-run-lib.f:166, SKIP line at :169, E-ZED-DISABLED src/lib/error.f:213). An environment census classed it a switch replaceable by a flag or argument. Source, Joel 2026-10-29: remove env vars except where Habu really needs one and cannot do without it. The replacement is a new flag, a public interface nobody has settled. Acceptance: Joel's choice of the flag or argument that turns these runs on, recorded here; then HABU_ZED is read nowhere. Files: tools/zed-run.f, tools/zed-run-lib.f, src/lib/error.f. Verify: rg -n HABU_ZED prints nothing outside .dots. Depends: none. Ownership: unassigned. Claim: unassigned.
