---
title: Run remote GPU gates from immutable manifests
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:00:40.916775+02:00"
---

tools/ptx/zed-device-suite.f and zed-gradcheck-suite.f run a persistent remote checkout, execute only a subset of the intended device cases, accept any nonzero status for at least one negative, and clean up only on the happy path. Define one checked manifest that names every remote case, exact source/artifact digest, target, command, expected outcome class, and required output. Materialize the reviewed bundle into a unique remote directory, verify its digest before execution, run only that bundle, and return digest-bound evidence. Drive runner scheduling directly from this manifest and prove every declared case executes exactly once. Assert exact named errors, not generic nonzero. Own local and remote directories, processes, modules, and artifacts through unconditional cleanup scopes that preserve primary and cleanup errors. Add injected stale-checkout, swapped bundle, missing case, wrong error, interrupted transfer, failed command, and cleanup-failure tests, plus a live device smoke. Pulling or resetting the persistent checkout is forbidden because it does not bind the reviewed tree. Files: remote device runners, checked manifest/codec, and focused tests. Verify the off-device mutation suite, remote smoke on the configured device, PTX standard library, host/dot lints, and full native gate.
