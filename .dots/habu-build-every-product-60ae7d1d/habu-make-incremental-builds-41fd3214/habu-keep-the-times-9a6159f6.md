---
title: Keep the times of a file that never links
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T09:01:03.909279+02:00"
closed-at: "2026-11-10T09:22:38.872046+02:00"
close-reason: "landed; test/file-objects.f scope: compiled 3 3 2 and inside: compiled 3 1 3 1; the reproducer's third build leaves the dependent's object; suite 270 of 270"
---

Problem: src/habu/linker.f WRITE-OBJECT exits before the write when the file is UNLINKABLE, so COMPILED-TIME finds no old object and gives the file's piece the time NOW each build; every file whose FILES row names that piece compiles on every warm build (reproducer /tmp/jerry-inc/fx/u: w.f.hfo rewritten in build three; the pre-stack hb rewrote 0). Acceptance: an UNLINKABLE file with a stamp writes its object with INPUT 0, 0, -1, never links, and its pieces keep their times, so its dependents link; a file without a stamp writes none. Files: src/habu/linker.f (WRITE-OBJECT, HEADER-FRESH?, CHECKED), test/fixtures/file-objects/build.f, test/file-objects.f, docs/architecture.md. Verify: test/file-objects.f prints scope: compiled 3 3 2 and inside: compiled 3 1 3 1 with the staged hb; the fx/u third build leaves w.f.hfo untouched. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-kept (brief .jj-ws.noindex/jerry-inc/.out/briefs/kept.md).

Review (Opus code-reviewer, 2026-11-10): the code holds on every link path; its comment findings are folded in. A cost it found: a file loaded alone in one build and inside a package in another, in one out root, has its object replaced by the inside load, so the next build alone compiles it and the files whose objects name its piece. Not tested: a source without a stamp (a path that is not a regular file) loaded where a file that never links wrote an object; HEADER-FRESH? refuses it by reading.
