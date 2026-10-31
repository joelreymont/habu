---
title: Run the real-Chrome display test only on --chrome
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T20:11:02.347589+02:00"
---

Problem: test/browser/display-test.f:62-76 (FIND-CHROME) passes --chrome whenever /Applications/Google Chrome.app or google-chrome exists, so every Habu test run by every agent starts a headless Chrome (~110 a day on Joel's Mac). Teardown in test/browser/display-peer.mjs:337-354 sends SIGTERM then SIGKILL to Chrome's whole process group and then runs trash on the profile. Each run left a Google Chrome code-sign clone in /var/folders/<user>/X/com.google.Chrome.code_sign_clone (403 accumulated from Oct 19, when the test landed) and a habu-display-chrome-* profile in Joel's Trash. Joel's decision (LESSONS.md, Tests, 2026-10-31): Chrome runs only on an explicit --chrome. Acceptance: (1) the real-Chrome part runs only when the caller passes --chrome; Chrome being installed does nothing. (2) A run ends Chrome cleanly (for example DevTools Browser.close, then wait for the process group to exit; kill only as a reported failure) so Chrome removes its own code-sign clone. (3) The test removes its profile itself, never with trash. (4) Proof: with --chrome, the clone count in the X directory and the Trash are unchanged after a run, and no Chrome process from the run survives; without --chrome no Chrome process starts. Files: test/browser/display-test.f, test/browser/display-peer.mjs. Verify: the display test with and without --chrome, counting clones and Chrome processes before and after. Depends: none. Ownership: those two files. Claim: unassigned.
