---
title: Re-measure the owed certification census rows
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T11:19:38.163497+02:00"
---

Static invariant: the STATUS.md 'Certified (<host>)' row equals what the engine build slice measures on that host, so the census ratchet gates real drift instead of accumulated debt.

The macos-arm64 row is owed. Measured on macOS arm64 in .jj-ws/habu-reject-a-bare-1f43a9a6 at parent commit 02c5859b (proofs, 'Close the reach-callers dot'), with src/core/checker.f at its committed content:

  bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test/runner.f test/gate-common.f lib/build.f lib/codesign.f tools/build-fixpoint.f test/gate-pool.f test/gate-engine.f -- build

  census ratchet: STATUS.md Certified (macos-arm64) is 4270
  the self-check measured 4275
  FAIL: census ratchet: certification count drift

So the row is 5 behind before any current work touches it, and the engine build slice of bin/hb --load test/run.f is red on macOS for that reason alone. STATUS.md already records that the linux-arm64 row (4197) is owed a linux-host re-measure as well, so both rows need one measured, transcribed update from their own hosts. docs/worker-briefing.md forbids guessing a host's row from another host, which is why no lane can close this as a side effect: it needs a run on each host with the number copied from the output.

Until it is re-measured, treat 'census ratchet: certification count drift' in the native engine build slice as a known pre-existing red and compare a candidate's measured count against the BASE tree's measured count, not against STATUS.md.
