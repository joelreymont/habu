---
title: "Kill the display peer's whole browser process group"
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T21:14:04.344956+02:00"
---

Problem: when a case in test/browser/display-test.f fails, test/browser/display-peer.mjs's headless Chrome can leave a renderer orphaned (ppid 1, its profile directory already deleted); passing runs leave nothing. Teardown at display-peer.mjs:337 is `child.kill(); await child.exited`, which signals only the browser process, not its group (seen by tim in tim-evbytes; orphan pid 32144 killed by hand). Acceptance: the peer starts Chrome in its own process group and teardown signals and reaps the whole group on every exit path, failing cases included; a failing-case run leaves no Chrome process. Files: test/browser/display-peer.mjs (a device peer, allowed non-Habu). Verify: force one display case to fail, then pgrep for Chrome children of the run: none; display-test passing run unchanged. Depends: none. Ownership: test/browser/display-peer.mjs. Claim: unassigned.
