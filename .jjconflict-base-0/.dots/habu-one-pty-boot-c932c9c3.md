---
title: One pty-boot helper for the suites
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T13:01:46.438573+02:00"
---

test/aot-data-span-forge.f duplicates test/proc-pty.f's pty primitives and flags its own duplication as out-of-scope - now with a second consumer (the bake landing's two acceptance cases), and the intern dot 567d8484 will make a third. Extract one shared pty-boot helper (open/spawn-on-pty/expect/close) both suites load, delete the duplicated primitives, keep the fixtures' assertions where they are. Files: test/proc-pty.f, test/aot-data-span-forge.f. Depends: none.
