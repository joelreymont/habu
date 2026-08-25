---
title: Delete the old emitter and its bridges
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.620705+02:00"
---

CG-17 + CG-19 + CG-20 + CG-21, the deletion half of phase 4. After the cut: delete COMPILE-EMIT and the old direct emitter; NMIGRATE and its staged/address-keyed facts (reproduced SIGBUS: DEFINE + FORGET-DEFS-FROM + three ordinary recompiles exits 134 even without NREACH); NREACH, redirection, call-site scanners, and workload redirection ceremony (reach.f:194-204 rewrites BL displacements without relocation ownership; SAME-WORD-CK at reach.f:142 is a case-insensitive tail comparison); the 128-row replacement log (LOG-FIND returns the oldest row, rows never retire, migrate-forget-migrate resolves through the dead row); and the CODE-RECLAIM bridge watchers (publish.f:420-426, clobber.f:230-236, inline.f:742-747 — registrations survive FORGET, table publicly exhaustible at xref.f:455-504). Any metadata the final compiler needs lives on authoritative live XREF/dictionary records and retires with them. Do not build a watcher lifecycle framework, a live-row registry, or any replacement history. Blocked by the cutover dot.

Design note carried from habu-reach-the-absent-360162f5 (closed 2026-08-19 as
ceremony against dying code): the certified-arity read (NDICT:SPELL-ARITY off
the checker's certificate, landed b4329129) SURVIVES the cut - the sole
compiler still asks it. What dies is the migration entry around it, including
the E-NMIGRATE-ARITY guard's exact shape. The unresolved question moves here:
in the post-cut define path, can a definition certify while the checker holds
no queryable effect for its name (the census header's seal-stripped-name case)?
The post-cut path must handle the reader's documented absent answer with a
named loud failure at its single caller - same two-line shape - and whether a
reaching case exists should be answered THEN, against the final code, not
against scaffolding. thecut-2's four measured non-reaching shapes (package
close -8573, TRUSTED -8400, 0 set-check exit 70, unsigned infers) are the
starting evidence.

Also carried from habu-model-the-interpreter-f450db18 (closed 2026-08-19,
same ruling): src/compiler/native/input.f (NINP) exists only to serve
NMIGRATE:NEXT's evaluate-tail trick - the whole file, its two TRUSTED: rows,
its two error codes and the NINP:CLOSE call in feed.f ON-DONE die here with
the entry. The modeled-input-cursor checker capability the dot asked for has
no consumer that survives the cut; if the final define path ever needs a
stream-cursor seam, design it then against the final code. Until then the
boundary stays exactly as landed: named, tested byte-exact by
test/compiler/native-stream.f, and scheduled to die.
