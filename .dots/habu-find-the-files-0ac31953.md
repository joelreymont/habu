---
title: "Find the files a build's compile reads besides its sources"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:52:57.490757+02:00"
---

Problem: the file linker reuses a part's object by its source stamp, its word dependencies, its requests and acts, and (dot a450cc5b) the root and environment facts its compile read; a file a part's top level reads by another path (a byte read through src/lib/fs.f or a direct syscall at compile time, e.g. to embed an asset) is no input of the object, so editing that file alone leaves a warm build linking the old bytes where a cold build reads the new: a break of AGENTS.md 'a warm build never fails or differs where a cold build would not'. Not yet shown to happen: no consumer case is known. Acceptance: a census of Zen, Tender and Maki masters and of Habu's tools/ and src/lib/ that lists each word that reads a file's bytes or metadata and is run at a file's top level during a build, path:line from the program's entry; none found closes this dot, any found becomes a design dot (record the read as a fact, as a450cc5b records roots). Files: none changed. Verify: the census list. Depends: habu-reuse-the-obj-a450cc5b. Ownership: none. Claim: unassigned.
