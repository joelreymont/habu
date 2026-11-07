---
title: Key a file outside every root by more than its name
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T03:39:34.016857+02:00"
---

Problem (L5-T worker, 2026-11-07, /tmp/carl-l5t/k3/): a file loaded by an absolute path outside every search root takes its own directory as root (src/core/include.f SOURCE-ROOT, ABS-OWNER), so LINKER KEY! (src/habu/linker.f, UNDER-ROOT) keys it by its file name alone: /tmp/.../x/a.f and /tmp/.../y/a.f share one key and one object under <out>/obj. Builds of the two overwrite each other's object, so each compiles every time; when the two files hold the same bytes after the same earlier files, one links the other's object, which carries anything the compile took from its own path (its directory for a later relative load, its name in a located message). test/fixtures/file-objects/build.f names tail's files c.f and d.f to avoid edit/a.f's key. Acceptance: two files of one name in two directories outside every root get two keys and two objects in one out root; a file under a search root keeps its root-relative key, so a moved tree's objects stay valid. Files: src/habu/linker.f (KEY!), src/core/include.f only if the runner must pass more, test/file-objects.f. Verify: the four suites (linker arm64-link-data arm64-link-run file-objects). Depends: L5 landed (same files). Ownership: carl (lead), worker lane. Claim: unassigned.
