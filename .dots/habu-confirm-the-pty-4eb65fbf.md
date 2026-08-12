---
title: Confirm the pty acceptance cases on the Linux gate host
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T13:01:46.432136+02:00"
---

The two pty-boot acceptance cases from the bake landing (5dfbe3bb: content-travels and trap-dies-named in test/aot-data-span-forge.f) are Linux-only members - the file skips them on macOS (/dev/ptmx + /dev/pts/N shape) and the landing's evidence came from macOS script(1) probes outside the suite. Run the full gate on the Linux host and confirm both cases execute and pass there; until then the suite-scheduled proof exists on no machine that runs it. Files: test/aot-data-span-forge.f. Depends: device availability (see below).

STATUS 2026-08-12 (zedgate lane, deliberately unclaimed - a claim would hide a hardware block): zed is OFFLINE since 2026-08-09T06:31Z (spark dropped the same second - site event; ssh, direct tailnet IP, and tailscale ping all time out; user informed). The macOS HALF of the claim is verified structurally: both cases are real (ASSERT-CONTENT-TRAVELS forge:240, ASSERT-TRAP-DIES-NAMED :254), genuinely suite-scheduled (SUITE aot-wid-restore -> aot-wid-suite.f:345 PROBE-DATA-SPAN spawns the forge as a child and gates on its exit), and the macOS run demonstrably skips them through the documented guard (HB-TARGET-LINUX? 0= -> "PTY boot cases run on linux only; skipped") - the hole this dot names is demonstrated, not asserted. Remaining work is exactly the Linux run. Do together with habu-re-measure-the-1adcc3a9 (the Linux size decomposition, discovered to be a full byte-fixpoint re-measure, not a row nudge) on the same device visit. Probe reachability BEFORE dispatching.
