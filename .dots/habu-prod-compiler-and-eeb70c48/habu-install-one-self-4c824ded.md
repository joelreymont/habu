---
title: Install one self-contained hb
status: open
priority: 2
issue-type: task
created-at: "2026-08-24T21:25:01.990138+02:00"
blocks:
  - habu-delete-the-old-679cfd35
---

Problem: the installed bin/hb fails outside its checkout because boot still opens repository source, and it has no help or version response. Acceptance: install produces one hb and no hb-host; from an unrelated directory that hb starts the REPL, loads a file, prints concise help, and reports its build version without opening checkout source. Reuse the existing baked artifact and install command; add no package manager, updater, manifest service, fallback compiler, or compatibility mode. Files: build/install and boot entry owners, README command example, and one isolated-directory fixture. Verify: fresh build/install, move or hide the checkout source, then run the four public commands. Depends: habu-delete-the-old-679cfd35. Ownership: installed product artifact and its minimal public argv only.
