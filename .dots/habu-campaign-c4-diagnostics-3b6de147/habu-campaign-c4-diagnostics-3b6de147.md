---
title: "Campaign C4: diagnostics that name the fix"
status: open
priority: 1
issue-type: task
created-at: "2026-09-16T13:54:06.636339+03:00"
---

Problem: Joel 2026-11-14: "it doesn't matter if it prints the E-name instead of the code as both are wrong. it should print the issue to help the agent resolve it. a comprehensie rust-type error". On the installed hb `: F ( n -- n ) drop ;` prints three lines (`habu: in f: at 'drop' expected: n actual: ; suggestion: ...`, `hook: non-certified definition`, `compiler: cannot compile`), none with a file position or the source line; `package <300 q>` prints only `hb: uncaught throw code 7154`, `EXPORT dup` 7115; an ambiguous bare word in a body ends `hb: uncaught throw code 7144`; a file that leaves cells ends `uncaught throw code -3804`; a declaration under --load has no line; under --load JSON says "file":"<input>"; tools/check.f without --json-errors prints JSON; caught-then-rethrown rejection codes exit 70 with nothing (src/habu/layout.f REFUSAL-ABI:CODE-CELL).

Design (settled by the lead; Joel 2026-11-14 chose "Start lanes now" with this form):
- Form, one per rejection, nothing else printed for it:
  error[E-MISMATCH]: F ends with nothing on the stack; its signature promises n
   --> a.f:1:16
    |
  1 | : F ( n -- n ) drop ;
    |                ^^^^ the stack is empty after this
    = expected: n
    = found: (nothing)
    = declared: n -- n
    = inferred: n --
    = help: leave an n after `drop`, or declare ( n -- )
  An empty row prints (nothing); the source line prints whole; E-UNDEFINED's help names each package whose public wordlist holds the exact name.
- JSON: one object per line on stderr, keys severity, code, message, file, line, column, byte_start, byte_end, token, source_line, label, notes (an object of the = lines), help; place keys absent when no source holds the span. schema_version, verdict, repair_class, token_index and definition_source go.
- Modes: --load stops at the first rejection with one diagnostic and the site's status; tools/check.f prints the same, prose by default, JSON lines only under --json-errors, no check.f: trailer; --all-errors prints each and exits 70; every prose diagnostic ends with a blank line, as rustc's do; E-STATEMENT-THROW only for a statement that threw while the rejection count did not move.
- One package, REJECTION in src/core/rejection.f (the boot file before checker.f), owns the record text, its JSON escaping and the sink, replaces ERROR-TEXT:PREFIX and prints every rejection: an opener (code name, message), a place part (token span, label), a note part (key, value), a help part and two closers, one throwing a status in 1..255 and one counting and returning (--all-errors). Its cells are JSON, QUIET and TALLY. Prose and JSON come from the same calls. No state check: sites call opener .. closer.
- One place rule: a tool's armed source (REJECTION:SOURCE! and FILE!), else the loader's owned file bytes (SRCLOC), else the buffer read, named <input>; body tokens map back through REJECTION:>SOURCE.
- Every throw in src/core, src/habu, src/compiler is one of: a rejection, printed and thrown at the site that knows the cause (the 71xx rejection constants go); an engine defect, `76 die` with a sentence; a control code whose one catcher is in the same package (the declaration codes keep GUARD-CODE/TDECL-RUN, whose reasons gain fix texts). A catcher never prints and never asks whether a code was reported. Statuses 70/74/75/76/78 and ENGINE-ERROR stay. The uncaught exit: 1..255 exits as itself silently, any other code prints the uncaught line and exits 67.
- Deleted: REFUSAL-ABI:CODE-CELL and its compare, CHECKER-REFUSE, REPORTED-ERROR?, CAST-ERROR:SKIP's list, PINNED?, REPORT-REFUSAL, JSON-REPORTED, the THREW? list tests of tools/check-core.f and tools/check-all-errors-core.f, the hook: and compiler: lines, the generic suggestions (src/core/render.f "Inspect the token, signature, and raw stack evidence.").

Acceptance: under --load, tools/check.f (prose and --json-errors) and --all-errors, every rejection prints exactly one diagnostic in the form above with its place, source line and mark where a source holds it, and a specific help; none ends in a bare code, a duplicate or silence; the deleted machinery is gone; every child is closed.

Children: habu-report-a-def-2b4ff9c9, habu-report-checker-record-15362e5c, habu-give-declaration-refusals-b0c396eb and habu-report-the-interpreter-10ba8b10; then habu-move-the-diagnostic-783e0965 and habu-one-view-of-54f8320e; habu-delete-the-already-f1d40715 last. B, C, D, E and F merge in one integration workspace and land together: B to E change the JSON keys F's tools read. Depends: none. Claim: agent=jerry.
