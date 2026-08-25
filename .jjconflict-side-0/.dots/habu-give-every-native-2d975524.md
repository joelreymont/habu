---
title: Give every native suite a bare-callee crossing fixture
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T20:23:22.315959+02:00"
---

Found by the locals-scope landing: forcing CROSS-L to return 0 passes native-rstack, native-loop, native-do, native-leave and native-inline UNCHANGED - every staged callee in those fixtures has a clobber row, so CALL-KEEPS? keeps registers and nothing ever travels; the whole crossing carrier was dead-tested. native-locals-scope.f NLS-SLOT closes it for locals by staging the ENGINE's own routine as callee (no clobber row, bare, locals travel) - the real mixed state of a partly-migrated tree. Give the other suites the same shape for their carriers (counters, parked values). Files: test/compiler/native-{rstack,loop,do,leave,inline}.f. Depends: none.

ADDITION (calls audit 2026-08-14): E-A64RAV-DKEEP has no standing
NEGATIVE fixture in a registered suite - native-dstack-alias.f pins
only the accept side; the refusal's firing is recorded historically
(header prose, LESSONS mutation runs). Same class as this dot's
carriers: a guard whose firing no suite binds. Add the negative
when doing the carrier fixtures.
