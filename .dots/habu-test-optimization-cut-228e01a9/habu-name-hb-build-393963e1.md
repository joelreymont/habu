---
title: "Name hb-build's path and install refusals"
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T12:35:16.551759+02:00"
---

Problem (lane 294 bfxstage): tools/hb-build.f ends some refusals as 'hb: uncaught throw code ...' (rc 67) instead of a named CLI refusal: an -o in a directory that does not exist runs the whole build and then dies uncaught at the install step (tools/hb-build-lib.f HBB-CLI-MAKER-CODE path); a source path longer than FS-PATH-CAP dies uncaught with -2803 while parsing (HBB-SRC!). Acceptance: each is refused with a message naming the path and the reason and hb-build's documented usage/IO exit code, the -o case before any build work where it can be known up front; cases in tools/hb-build-cli-errors-test.f, seen failing first. Files: tools/hb-build-lib.f, tools/hb-build.f, tools/hb-build-cli-errors-test.f.

Lane 294 (r4-bfxstage, d4e78e7b) adds: an -o whose last component is under 255 bytes but too long for its `.tmp-<seed>-<n>` sibling passes HBB-OUT-ARG!'s total-length check; RESERVE-SIBLING then fails ENAMETOOLONG, reported as E-FS-OPEN from lib/fs-mutate.f FS-MUT-ATOMIC-OPEN-CANDIDATE after a whole build, uncaught through tools/hb-build-lib.f HBB-CLI-MAKER-CODE. Refuse it at parse by name (component bound = NAME_MAX minus the sibling suffix).
