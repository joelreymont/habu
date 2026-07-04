---
name: fixme
description: Sweep FIXME notes from the current tree; answer, fix, or dot each; remove every note. Use when the user says /fixme or asks to address FIXME comments.
---

# fixme — sweep and discharge FIXME notes

FIXME notes are messages from the user to the agent, embedded at the exact
code site they concern. A sweep discharges every note and leaves zero FIXMEs
in the tree.

## Procedure

1. `rg -n 'FIXME' --type-add 'src:*.{f,fs,md}' -tsrc` over the current
   workspace AND the main worktree (~/Work/habu) — the user drops notes in
   whichever tree they are reading.
2. Classify each note:
   - **Question** — answer it in the reply, citing file:line evidence
     (docs/forth.md, src/, existing dots). If the answer is durable context the
     next reader needs, replace the note with a real comment stating the fact;
     otherwise just delete the note.
   - **Small defect/improvement** (single-file, mechanical, gate-cheap) — fix
     it now, run the owning gate for the touched path.
   - **Substantive work** — mint a detailed dot (`dot add "Title" -d "file:line,
     cause, fix"`), then delete the note; the dot is the durable record.
3. Every swept note is REMOVED in the same change. The pre-completion checklist
   (no TODOs/stubs) treats a surviving FIXME as unfinished work.
4. Gate whatever was touched (owning `bin/hb --load` path; typed-local-diff-lint
   on the diff). One commit: `Address FIXME notes` (or fold into the fix's own
   commit when there is exactly one fix).
5. Report per note: file:line, the note, the disposition (answered / fixed /
   dotted <dot-id>), one line each.

## Rules

- A FIXME question about a design choice gets a factual answer with evidence,
  never a guess; if the answer reveals a real gap, that is a dot, not prose.
- Never delete a note without discharging it (answer delivered, fix landed, or
  dot minted).
- Notes inserted in a tree owned by another agent: answer in the reply and say
  the note itself must be removed by that tree's owner.
