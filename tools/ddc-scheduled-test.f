\ ddc-scheduled-test.f - focused tests for the change-triggered DDC gate.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/content-key.f lib/process.f lib/process-argv.f
\ lib/process-env.f tools/ddc-verify.f tools/ddc-scheduled.f tools/ddc-scheduled-test.f
\
\ Exercises the trigger DECISION and marker lifecycle with an injected fake
\ audit seam and a temp marker, so no real gforth chain runs here (the real DDC
\ chain is the manual/scheduled audit, covered by tools/ddc-verify-test.f).

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require tools/ddc-verify.f
require tools/ddc-scheduled.f

package DDCST

7 constant FAKE-DIVERGENT-RC                  \ an arbitrary nonzero audit rc

create ROOT-BUF FS-PATH-CAP allot
variable ROOT-U
create MK-BUF FS-PATH-CAP allot
variable MK-U
create RD-BUF $80 allot
variable AUDIT-N
variable AUDIT-RC

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: MK$ ( -- ptr u8 n ) MK-BUF MK-U @ ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U ! ;

: FAKE-AUDIT ( -- n )
   AUDIT-N @ 1+ AUDIT-N !
   AUDIT-RC @ ;

: INSTALL-FAKE ( -- )
   [: FAKE-AUDIT ;] DDCS:AUDIT! ;

: PREPARE ( -- )
   s" habu-ddcs-test" TMPDIR-MKDIR ROOT!
   ROOT$ s" ddc-marker.txt" MK-BUF JOIN-PATH MK-U !
   MK$ DDCS:MARKER-PATH!
   INSTALL-FAKE ;

\ A 64-char hex the current tree can never produce.
: WRONG-KEY$ ( -- ptr u8 n )
   s" 0000000000000000000000000000000000000000000000000000000000000000" ;

: SEED-MARKER ( ptr u8 n -- )                \ write the given key to the temp marker
   MK$ 2swap WRITE-ALL ;

: MARKER-NOW$ ( -- ptr u8 n )                \ read the temp marker back, trimmed
   MK$ RD-BUF $80 READ-ALL {: got:n :}
   RD-BUF got TRIM ;

: RESET-FAKE ( n -- )                        \ arm the fake with a chosen rc
   AUDIT-RC !
   0 AUDIT-N ! ;

\ marker == current key -> unchanged -> RUN passes without touching the audit.
: TEST-UNCHANGED ( -- )
   0 RESET-FAKE
   DDCS:COMPUTE
   DDCS:KEY$ SEED-MARKER
   s" unchanged: CHANGED? false" T-LABEL
   DDCS:CHANGED? TFALSE
   s" unchanged: RUN rc 0" T-LABEL
   DDCS:RUN 0 T=
   s" unchanged: audit not run" T-LABEL
   AUDIT-N @ 0 T= ;

\ marker stale + audit converges -> RUN passes and refreshes the marker to the key.
: TEST-CHANGED-CONVERGE ( -- )
   0 RESET-FAKE
   WRONG-KEY$ SEED-MARKER
   s" converge: CHANGED? true" T-LABEL
   DDCS:CHANGED? TTRUE
   s" converge: RUN rc 0" T-LABEL
   DDCS:RUN 0 T=
   s" converge: audit ran once" T-LABEL
   AUDIT-N @ 1 T=
   s" converge: marker refreshed to key" T-LABEL
   MARKER-NOW$ DDCS:KEY$ STR= TTRUE ;

\ marker stale + audit diverges -> RUN fails with the audit rc, marker untouched.
: TEST-CHANGED-DIVERGE ( -- )
   FAKE-DIVERGENT-RC RESET-FAKE
   WRONG-KEY$ SEED-MARKER
   s" diverge: RUN rc = audit rc" T-LABEL
   DDCS:RUN FAKE-DIVERGENT-RC T=
   s" diverge: audit ran once" T-LABEL
   AUDIT-N @ 1 T=
   s" diverge: marker NOT refreshed" T-LABEL
   MARKER-NOW$ WRONG-KEY$ STR= TTRUE ;

: MAIN ( -- )
   T-RESET
   PREPARE
   TEST-UNCHANGED
   TEST-CHANGED-CONVERGE
   TEST-CHANGED-DIVERGE
   ROOT$ REMOVE-TREE
   T-REPORT
   s" ddc-scheduled-test: ok" type cr ;

MAIN

;package
