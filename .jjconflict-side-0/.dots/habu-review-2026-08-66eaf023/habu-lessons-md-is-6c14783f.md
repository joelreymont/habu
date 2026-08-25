---
title: LESSONS.md is a campaign log, not lessons
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:26.041391+02:00"
---

Problem: LESSONS.md is 7814 lines; its header (:4-11) forbids status narrative and says the dated log was archived once (docs/archive/lessons-2026h1.md); :12-185 is an undated append zone, :2188-4266 one section holding 282 unrelated bullets, :4267-7814 198 dated essays (45 titled 'The X landing's three/four/five'); :3 says last updated 2026-08-10 while the last entry is 2026-08-20; superseded lessons :1163-1168 (budgets) and Orin-only ones :1202,1872,1889,1941,1971 still live; :12-27 contradicts docs/debugging.md:106-110; reference material at :1823-1847 (Darwin syscalls), :2003-2018 (page walls), :1508-1800 (codegen/AOT mechanics) belongs in docs/. CLAUDE.md requires reading it every session. Acceptance: lines 4267-7814 moved verbatim to docs/archive/lessons-2026-08.md with at most one rule sentence per essay lifted into a topical section; the 2188-4266 section redistributed; superseded and platform-retired lessons archived with a note; reference material moved to docs/porting.md/macho.md/codegen docs; result ~1.5-2k lines in the 14 sections; header date fixed. Files: LESSONS.md, docs/archive/, docs/porting.md. Verify: wc -l and a read-through. Depends: none. Ownership: docs. Claim: unassigned.
