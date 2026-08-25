---
title: Let a migrated body name every constant and callee it mentions
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T13:10:00.315708+02:00"
---

src/compiler/native/migrate.f keeps ONE data-word slot (M-DATA) and ONE callee slot (M-CALLEE), so a definition the chain compiles may name at most one create'd data word or one callee, and never both. Two real consequences, both visible in the second benchmark corpus: lib/json-read.f:252's WS? names four byte constants (SP TAB LF CR) and could not be migrated as written; and tools/codegen-compare-corpus2.f's TV-NEXT? has to spell TAG and PAY inline as '7 and' and '3 rshift' because its one name is already spent on the binding table it reads. HIR-WORD:DECLARE-FIXED and HIR-WORD:DECLARE-CALLABLE already take one row each and the word model holds as many rows as the caller commits it to (HIR-WORD:NEW takes the count), so the ceiling is the migration's staging, not the dialect's. Widen M-DATA and M-CALLEE to lists, size MODEL-ROWS from their lengths, and let DEFINE-DATA / DEFINE-CALLING take several. Relates to habu-resolve-a-data-a1c8067f and habu-resolve-a-callee-0340dfde, which remove the caller's statement entirely and would make the multiplicity fall out; either order works, but until one of them lands a migrated body cannot name what it mentions.
