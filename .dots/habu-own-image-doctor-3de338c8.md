---
title: Own image doctor mappings
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T03:14:28.591042+02:00"
---

Full context: pending tools/image-doctor.f LOAD grows mmap-backed buffers without an owned lifetime or munmap, leaking mappings across repeated artifact mutations. Add a checked owned mapping/buffer package with DESTROY and replacement semantics; prove repeated load/write cycles release prior ownership.
