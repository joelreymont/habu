---
title: "Save, load and link each file's object in a build (carl, parallel)"
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T09:43:15.135548+02:00"
---

Problem: an incremental build reuses each unchanged file instead of compiling it again. Joel 2026-11-06: "your job is to generate code (can use t1), then save code, load code and link code"; "proceed with your parallel implementation ... we'll reconcile your file objects with jerry when we get to it. let jerry know you have completed that work WHEN you have completed it". Approach: design r2 section 6 (/tmp/jerry-big/design-xbuild-r2.md:490-519, approved 2026-11-04): a file's object is what its load added to a LINKER:WITH build's space (records with the checker's facts, shadow emissions with their sites, DATA with declared-cell rows, strings, stores into earlier files' cells, owner registrations, the files it loaded); reusing a file links its object without reading its source. Built on the build space as it stands (S5, src/habu/linker.f), in parallel with jerry's S5c (habu-s5c-load-hb-6aae35fd); files that cannot load into a build yet are failing cases until S5c lands. Acceptance: a cold build in a space writes each file's object; a warm build with every file fresh links every object, reads no source, and LINKER:WRITE writes the same program byte for byte; an edit rebuilds the edited file and its dependents only. Design: .jj-ws.noindex/carl-dots/.out/carl-file/design-fobj.md (to Joel before any worker: a new saved format). Children are the lanes. Files: per lane. Verify: per lane; test/compiler/arm64-link-run.f's programs as the first fixture. Depends: none. Ownership: carl. Claim: agent=carl.
