---
title: Replace AGENTS.md with the approved text and delete DECISIONS.md
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T09:30:13.516837+02:00"
---

Problem: Habu's rules live in CLAUDE.md/AGENTS.md plus DECISIONS.md, whose rulings the audit (decisions-2026-11-10, with its drop brief; both files are lost) sorts into rules, dots, code comments and drops. Joel approved the new AGENTS.md text on 2026-11-10, option 'Yes, as printed': the 110-line draft at .jj-ws.noindex/carl-layout/.out/agents-md-draft.md goes in unchanged. Acceptance: one commit that replaces the rules file's text with the draft (check first whether CLAUDE.md or AGENTS.md is a symlink and edit the file it points to), carries out drop-brief.md, deletes DECISIONS.md, and leaves no path or quoted section name pointing at it in the tree (docs, comments, dots, test/gate-stdlib-cases.f:12); memory notes that name DECISIONS.md are updated the same day. Files: AGENTS.md/CLAUDE.md, DECISIONS.md, the files drop-brief.md names. Verify: `rg -n 'DECISIONS'` over the tree prints nothing outside .dots/archive. Depends: the host-loads lane (xtuxupux) and the linear type lane (kukntvwk) landing first. Ownership: those files. Claim: unassigned.

Note (2026-11-10): master c3a5642b added to the rules file, after the draft was written, the line "Agents write, build and debug everything in Habu and on it; no person uses Habu directly." The migration keeps that line under the title paragraph.

Today: DECISIONS.md is gone (e6d4c460 "Remove DECISIONS.md": its twelve standing rules moved into AGENTS.md, six subject choices into docs/gate.md, docs/forth.md and docs/forth-card.md, four dots carry the work it approved; the rest were done or superseded), and no file outside .dots names it; 157 open dot files still do. AGENTS.md (CLAUDE.md is a symlink to it) has 64 lines and is not the 110-line draft, which is still at .jj-ws.noindex/carl-layout/.out/agents-md-draft.md; it keeps the "Agents write, build and debug" line under the title paragraph.
