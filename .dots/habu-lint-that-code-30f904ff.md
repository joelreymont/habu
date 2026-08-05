---
title: Lint that code rewinds go through CODE-RECLAIM
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T22:01:26.527219+02:00"
---

src/habu/xref.f CODE-RECLAIM:TRUNCATE is now the one checked word that lowers the code pointer, and everything keyed to a code address (src/compiler/native/publish.f CLAIMED, clobber.f rows, inline.f rows) is dropped from its notice. Nothing yet stops a new checked caller writing a bare cp! rewind instead. The consequence is bounded rather than silent - NPUB:SLOT-CK refuses the next publication with E-NPUB-SLOT instead of letting a slot be claimed twice under a stale row - but the refusal is downstream of the mistake. Add a lint over src/ and lib/ that rejects the cp! token outside CODE-RECLAIM:TRUNCATE and the forward bumps that own their own span (publish.f WRITE, lib/task.f TASK-ENTRY-BUILD), lexed through tools/lint/source-lex.f so a mention in a comment or a string is not a hit, with hostile fixtures for both. Depends on habu-tie-native-rows-2103f90f.

GROOMED 2026-08-04 (dot-groom). Dangling blocker repointed. habu-tie-native-rows-2103f90f is
no longer in the graph: it was closed and archived by commit f85977ac3 "Close the row-lifetime
and clobber dots", whose code commit 48d4fa01d "Tie native rows to code lifetime" is the
landing this lint was to protect. The dependency is satisfied; nothing blocks this dot now.
