---
title: Choose the layout file of imgdump and image-size at top level
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T07:33:50.535012+02:00"
---

Problem: tools/imgdump.f:17-27 (IMG-LOAD-TARGET-LAYOUT) and tools/image-size-lib.f:22-32 (ES-LOAD-TARGET-LAYOUT) still choose a file to load inside a word that runs once at the file's top level and is then undefined. The lead chose (2026-11-07): a file chooses what it loads at its top level with [if] and [then], and no word loads a file. No build is affected: neither tool is loaded during a build. Found by the host-loads review (2026-11-10) and left out of that lane. Acceptance: both tools choose the layout file with a top-level [if]; the two words are gone. Files: tools/imgdump.f, tools/image-size-lib.f. Verify: imgdump and image-size run on an arm64 hb and print what they printed before. Depends: the host-loads lane (Load host backends at their file's top level) on master, and the source verifier following a top-level [if]. Ownership: the two tools. Claim: unassigned.
