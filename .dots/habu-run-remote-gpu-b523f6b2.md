---
title: Run remote GPU gates from immutable manifests
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:00:40.916775+02:00"
---

tools/ptx/zed-device-suite.f and zed-gradcheck-suite.f run a persistent remote checkout, cover only a subset of the device tools counted by the coverage lint, accept any nonzero status for at least one negative, and clean up only on the happy path. Define one checked manifest that names every remote case, exact source/artifact digest, target, command, expected outcome class, and required output. Materialize the reviewed bundle into a unique remote directory, verify its digest before execution, run only that bundle, and return digest-bound evidence. Derive both runner scheduling and coverage inventory from this manifest so a counted case cannot be unexecuted. Assert exact named errors, not generic nonzero. Own local and remote directories, processes, modules, and artifacts through unconditional cleanup scopes that preserve primary and cleanup errors. Add injected stale-checkout, swapped bundle, missing case, wrong error, interrupted transfer, failed command, and cleanup-failure tests, plus a live device smoke. Pulling or resetting the persistent checkout is forbidden because it does not bind the reviewed tree. Files: remote device runners, checked manifest/codec, coverage owner, focused tests. Verify off-device mutation suite, remote smoke on the configured device, PTX standard library, host/filemap/dot lints, and full native gate.
