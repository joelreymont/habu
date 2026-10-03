---
title: Own host services behind structured APIs
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.993390+03:00"
---

Problem: path, process, file and tool invocation live per OS seam under lib/ and src/os/*, selected by HB-TARGET-*, with no HostServices contract (PA-r2 §15.1-15.2, §24.1-24.2, P8); staging exists (RESERVE-SIBLING, lib/fs-mutate.f) but is not one service. Acceptance: HostServices, ExternalToolAction and RunnerSpec as checked records; staged atomic publication reused, not duplicated; no host library path leaks into a target action; cross-generation on each brought-up host. Files: src/host/ (new), src/build/ (new), lib/fs-mutate.f, lib/process.f, src/os/*. Verify: full gate; a target action with the host's /usr/lib removed from the search still links from the pinned sysroot. Depends: habu-resolve-build-targets-ae8e65c1. Ownership: src/host/, src/build/. Lane: dave. Claim: unassigned.
