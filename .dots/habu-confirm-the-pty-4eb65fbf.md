---
title: Confirm the pty acceptance cases on the Linux gate host
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T13:01:46.432136+02:00"
---

The two pty-boot acceptance cases from the bake landing (5dfbe3bb: content-travels and trap-dies-named in test/aot-data-span-forge.f) are Linux-only members - the file skips them on macOS (/dev/ptmx + /dev/pts/N shape) and the landing's evidence came from macOS script(1) probes outside the suite. Run the full gate on the Linux host and confirm both cases execute and pass there; until then the suite-scheduled proof exists on no machine that runs it. Files: test/aot-data-span-forge.f. Depends: none.
