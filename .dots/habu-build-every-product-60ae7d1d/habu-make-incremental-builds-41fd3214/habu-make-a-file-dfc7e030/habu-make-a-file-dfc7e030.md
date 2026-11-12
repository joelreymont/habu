---
title: "Make a file's object its load instructions"
status: open
priority: 1
issue-type: task
created-at: "2026-11-03T12:52:55.873397+02:00"
---

Joel 2026-11-03: "whatever creates the object files (is it loader? should it be linker instead if it both saves and load?) has control over what executes when those object files are linked into an executable. it sounds like the current design is bad, prove me wrong!" and "i'm saying that we are not specifying the loading instructions correctly at the moment and have to do a lot of extra work, e.g. to discriminate loads. i'm saying that you can drastically simplify the approach, prove me wrong!"; then "yes, this is the linker. the files are machine code so the "prelude" can execute all kinds of forth words but ..." and "but whatever executes when linking has nothing to do with the file top-level as they are instructions added by the linker. prove me wrong!"; then "we also need to rename LOADER to LINKER, right? otherwise, keep going and keep it simple!"

Joel 2026-11-04: "building habu from habu is like cross-compiling from arm64 to arm64 indeed. make it happen".

Goal: the compiling load writes a file's object as its load instructions, and a reuse links it without reading the source; the loader (SOURCE, src/core/include.f) only picks link or compile. A top level runs once, when its file compiles. Builds are cross-builds (docs/architecture.md "Cross-compilation", docs/xcomp.md), arm64 to arm64 first. Remaining steps are children and siblings: hb written by the linker (habu-s6-l6-write-b8a1a80d), x86-64 and Wasm onto the linker (habu-build-x86-64-3ed6f45d), file objects (habu-save-load-and-b4287e01).

Acceptance cases:
- Object keys do not fold the checkout's absolute root (src/habu/linker.f FILE-KEY).
- A product is independent of the host engine that built it.
- The x86-64 byte fixpoint over generations holds for the cross-build.
- A file built into hb loads cleanly with require (require debug.f on x86-64 hit E-SEALED-PACKAGE; inferred, not probed).
- Retract cuts control flags (case).
- Same-source AOT bytes are stable (Kaizen's differed in ADR immediates).
- An app file that requires built-in files links.
- A killed build does not poison the linker's object store.
- Inline data in a code member relinks right or is refused.
- An engine build that meets a refusal in its checked prefix reports it and exits with the build's rc, not "stack bounds exceeded".
- A written product knows its text end (src/habu/code-bytes.f bounds the baked text band [rbase, dbase@)), with layout.f per OS.
- No host compiler or checker write lands in product DATA while a target space is open: the per-file DATA attribution names any byte no product declaration owns.
- src/habu/xref.f requires boot-cells.f, wl.f and layout.f instead of its comment's load-order rule (:1-4) once the arm64 cold prefix records each file as it loads.
- A warm build's product equals the cold one's; an edit rebuilds the edited file and its dependents only, a comment edit included; task reservations, FFI handles, exit hooks, does> and casts survive reuse; an environment variable or file a build reads is a build input; a moved, truncated or FIFO input is stale.

Claim: unassigned.
