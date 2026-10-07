\ checker-owner-descriptor-subject.f - test/checker-owner-descriptor.f's
\ SOURCE-EXTENT subject, run as a process of its own.
\
\ A source-loaded compiler binds its owner calls once, at load
\ (src/compiler/native/checker-owner.f BIND-SOURCE-CALLS). This shortens the
\ live owner's recorded extent to end where the declared-row field begins, then
\ loads that source again: the binding refuses with E-NCOMP-OWNER before it
\ reads the field, and the last line never runs. The core-prefix rewind
\ discards the compiler this engine carries, so its source loads afresh; it
\ runs at the process entry because what loads after it compiles over what it
\ discarded.
include src/habu/prefix-rewind.f
\ The extent is the cell before the record (src/core/checker-owner-guard.f VALIDATE).
CHECKER-OWNER-ABI:DECLARED-ROW-OFF
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CELL - CELL-VIEW !
require src/compiler/native/checker-owner.f
s" checker-owner-descriptor-subject: bound past the recorded extent" type cr
