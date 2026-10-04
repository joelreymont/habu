---
title: Re-own defect comments that cite missing dots
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:11:09.647423+03:00"
---

Found by review 523 (retptr, dot 9a853fa7), outside that dot: comments name a dot id as the owner of a defect or open question, and the id is not an open dot in .dots/ (closed, archived or never filed). A reader cannot find who owns the defect. Sites on master 8bc7609c (re-find; lines move): src/core/checker.f:1215, :17066; src/habu1.f:4741; src/ir/build.f:76; lib/string.f:229; tools/native-build-core.f:289; test/aot-seed-batch-suite.f:164; test/internal-word-gate.f:873, :1207; lib/num-types.f:60; lib/object.f:16; src/core/type-family.f:5359, :5385, :5414; test/type-ctor-suite.f:672. Acceptance: for each site, the defect or question is either gone (show it: a probe or the code that now handles it) and the comment is deleted or reworded to state the rule without a dot; or still live and the comment names an open dot that owns it (an existing one, or one the lead files from the lane's report). In the touched files, no comment that claims a dot owns a live defect, an open question or planned work names an id that is not an open dot. A citation that only records where a rule came from (a closed dot that decided or fixed it) is history and stays. No behaviour change; a baked file changes only in comments, so g1 is byte-identical to the base engine.
