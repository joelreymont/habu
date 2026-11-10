---
title: "The browser peers read Chrome's port file before the port is written"
status: closed
priority: 1
issue-type: task
created-at: "2026-11-10T20:45:51.852991+02:00"
closed-at: "2026-11-10T21:18:01.831634+02:00"
close-reason: "landed in master 17bf4285; its gate: engines converge, test/wasm/device.f (thirteen test: ok), arm64-link-run.f, error-code-lint.f, diag-position-test.f rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem: test/browser/display-peer.mjs (:153-155) and app-peer.mjs (:103-105) wait until DevToolsActivePort exists and then take its first line as the port. Chrome creates the file before it writes the port, so a peer that reads in between gets an empty port and fetches http://127.0.0.1/json/new, which is refused. Seen once: test/wasm/device.f in a gate run, display-test.f assertion 3 of 'SVG/input records, scenes, identities, refusals and CLI', ECONNREFUSED with path http://127.0.0.1/json/new?about:blank while that run's chrome.log says 'DevTools listening on ws://127.0.0.1:56868'; the same test passed alone ten minutes earlier on the same sources, and app-test.f passed in the same device run. Both files are the same on master. Acceptance: a peer takes the port only from a whole first line (digits, then a newline) and waits until there is one; any read error but a missing file is raised. Files: test/browser/display-peer.mjs, app-peer.mjs. Verify: test/browser/display-test.f and app-test.f end test: ok; test/wasm/device.f rc 0. Passing runs show the wait still works, not that the race is gone: that follows from where the port is read. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3b.
