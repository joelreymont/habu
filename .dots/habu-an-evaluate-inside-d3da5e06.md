---
title: An evaluate inside a checked file places its refusal at line 1 column 17
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T17:23:37.865464+02:00"
---

Problem: a refusal in text the checked file did not hand the engine as its own bytes is placed wrong: text run by evaluate, a file the subject requires, a does> clause, and the check tool's stdin run. Source: Joel, 2026-11-10: 'i want all issues identified to be dotted and fixed'.

Causes (hunts), still open on master:
(1) text with no map keeps the DIAG-ORIGIN! rule (src/core/checker.f header :670), an origin trusted for any text: evaluated text other than a slice of the file being loaded takes the last definition's origin plus the token's offset in the checked text, which skips comments;
(2) ROWS-ARM (src/core/check-hook.f :31) arms no source map for an evaluated string, whose bytes lie outside the file's;
(3) the does> clause check (checker.f CHECK-DOES! :26261) is never mapped, so JLOC-ORIGIN (src/core/render.f :1660) adds the token's offset in the clause to the name's place, in file text too: 'create does> ( a -- ) MADE 1 ;' answers 3/19 for 3/39;
(4) DFILE$ (render.f :1857) names the run's label, DIAGFB, for every definition packet, where TOP-FILE$ uses the label only while the load reads that file, so a refusal in a required file names the subject;
(5) the check tool's stdin run reads the subject inlined after a prefix (tools/check-core.f CHK-BUILD-PREFIX :862) and marked by the origin pass (CHK-RUN-ORIGIN+), so a place counted in the bytes the engine reads (the cast place, PLACE-AT) is the copy's.
Remaining fix, in this order: a packet names the file the token lies in; the does> clause check gets its own arming over the whole body; the stdin run's places are the subject's. A token in an evaluated string has no bytes in the file: its packet shape is a public item, not yet settled.

Acceptance: the packet's line, column and byte range name the token in the text it lies in, and say which text that is; a case in test/diag-position-test.f fails before. Cases:
- 's" : MYK ( n -- ) drop does> ( -- ) ;" evaluate' gives ';' at 1/10 bytes 9-10 under the check tool, inside the engine's copied text; the non-empty '... cr ;' puts cr at 1/10 the same way. habu-delete-the-trusted-42b30edd holds the TRUSTED: form of this case; this one stays after TRUSTED: goes.
- A cast or linear refusal raised in tools/check.f's run: (a) the 94-byte named file ': LRD-P ( -- ) 1 drop ;' / 'parse-name x drop 26 + 32 evaluate' / '\ cast: LRD-WIDE ( n -- ptr u8 n )': true place 3/9/67-75, the place --load gives; (b) on stdin 'parse-name x drop 26 + 32 evaluate \ cast: LRD-WIDE ( n -- ptr u8 n )': true place 1/44/43-51.
- A required file: q1-main.f is '\ main' / 'require sub.f'; sub.f holds a TRUSTED: RAW-IMM immediate, ': A ( -- ) ;' and ': P ( -- ) ( c ) 1 drop \ d' / tab 'RAW-IMM ;'. The packet names q1-main.f; the truth is sub.f 4/3/79-86.
- A file the subject loads by path: t.f = 's" <abs>/q.f" required' with q.f = ': MK ( -- ) create ; parses: MK 1' / 'MK MADE' / ': P ( n -- ) ( c ) MADE drop' / '   if ;'. The load says q.f:4; 'tools/check.f -- --json-errors t.f' names t.f, and through '--source-list base.f base.f' the file is named '<source-list>'. A refusal in a file reached by required, included or a source list names that file and the token's line, column and bytes in it, the same place the load names.
- An evaluated string: 's" : Q ( -- ) ( c ) 1 drop drop ;" evaluate' is refused at 1/17/16-20, inside '( c )'; the refusal names the token's own line, column and bytes in the file, or carries no place where none is true.
- A does> clause: under evaluate-closed with DIAG-JSON!, and through tools/hb-build.f --json-errors, a refused does> clause's packet gives its position from the start of the clause text (line 1, column 18) and its definition_source includes the signature; tools/check.f gives the true position (line 2, column 48) and the body. All three paths give the same position and definition_source for a clause.

Depends: none. Claim: unassigned.
