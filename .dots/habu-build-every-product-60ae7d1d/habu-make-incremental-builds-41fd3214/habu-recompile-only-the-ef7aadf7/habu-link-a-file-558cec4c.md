---
title: "Link a file's later parts after one of its parts compiles"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T19:04:18.327432+02:00"
---

Problem: when a part of a file does not link, LOAD (src/habu/linker.f) hands the interpreter the source from that part to the file's end, so every later part compiles though it uses nothing that changed. Census at stack tip 2d5cdbe6 (~/.cache/tmp/carl-d6): a body edit in src/core/util.f rewrites 146 parts, 18 of them such followers; a longer string 31, 20 followers; the two repl.f edits 3, none. Acceptance: the census shows no follower (util body 128, util string 11, repl 3); every warm program equals its cold one unsigned; file-objects passes with a case for a file whose middle part compiles and whose later parts link. Files: src/habu/linker.f, what the design names, test/file-objects.f. Verify: the census script, ~/.cache/tmp/carl-words/ev.sh, file-objects. Depends: the design (brief carl-layout/.out/briefs/design-link-after-compiled-part.md, Plan agent running 2026-11-10). Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-d6 (design)
