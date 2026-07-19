---
title: "Docs hygiene: stale claims and tracked artifacts"
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T17:44:34.808950+02:00"
---

Review minors (pin 8195257e), one bundle: README.md:52 "<128 KB" engine claim is stale (macOS 165367, Linux 143552) and README.md:369 link is broken; LESSONS.md:622 falsely says vector indexing is linear; STATUS.md:97 + MISSING.md:130 contradict implemented block-local semantics; docs/paper/habu.pdf is a tracked regenerable artifact (remove, regenerate locally - repo rule); four unreferenced bench/ host benchmark files are dead ungated code (delete or gate). Each item is a small honest-docs fix; no code behavior changes.
