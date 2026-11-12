---
title: Recompile only the files that use a changed word
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T10:47:01.501718+02:00"
---

Goal: a build recompiles only the files that use a changed word. Joel 2026-11-10: 'didn't we take great care to figure out if any words have been recompiled and then only recompile packages that use those words?', 'what happened to figuring out what words have changed?'. Joel 2026-11-10: "you need to finish incremental builds for release and make them rock-solid!"

Problem left in this dot's own text: a number a file takes from the data layout at top level is baked into its object, which then links after an earlier file's data grows, so warm != cold. Reproducer (remake): s.f 'create A-BUF 8 allot', f.f 'here data-base - constant M', r.f 'require s.f / require f.f / : MAIN ( -- ) M . cr ;'; build, change 8 to 64, build warm and cold (fresh HABU_OUT): the two print different numbers. f.f ran only words below FIRST-REC, which RAN-RECORD (src/habu/linker.f) ignores, and names nothing of s.f, so nothing ties it to s.f. The same failure needs no `data-base`: `here A-BUF - constant M` after a file between the two grows, and the hidden form `: OFF ( -- n ) here data-base - ; OFF constant M`. No list of words catches it; the number is a difference of addresses in two pieces. No engine file is such a consumer; every "warm = cold" claim names this case as the exception. Design: .jj-ws.noindex/carl-layout/.out/design-layout-number.md (part 1, the subtraction at a file's top level, found from its two operands, needs a ruling on what a build does with such a file: never link it, or link while the positions are unchanged; part 2, the subtraction inside a run word, reads the run rule's per-word reach list). Joel 2026-11-10, rejecting the question on this design: "what the hell is this about and why is it needed? why is it a problem you decided to solve?" Parked then until word tracking was finished and measured.

Acceptance: the reproducer, direct and hidden form, builds warm to what a cold build gives, by a dependency rule that holds for every file that reads the layout at top level; a case of it in test/file-objects.f.

Depends: a design of the rule, approved by Joel before a worker starts. Open children: habu-a-link-s-1b847f18, habu-a-store-brings-1ba604bf, habu-three-stored-hooks-5f1d6294.

Claim: unassigned, owner carl.
