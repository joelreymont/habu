\ checker-verify-pkg-scope.f - the checker's verifier package scope hands the
\ caller's package back byte for byte. VERIFY:SOURCE-BUF-IN-SCOPE saves the
\ caller's package name, length and mode and restores them on leaving; entering
\ again rejects with E-PKG-CONTEXT unless the mirror equals the live record, so
\ a second replay from the same open package is the exact test of the restore.

\ Run: bin/hb --load src/lib/error.f src/lib/string.f
\   src/habu/verify-source.f test/checker-verify-pkg-scope.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/test/eval.f
require src/habu/verify-source.f

package CVPS
public

\ Replay one source span in the caller's live package context; 0 means the
\ verifier accepted both the package context and the source.
: REPLAY ( str -- n ) {: s:str :}
   [: s VERIFY:SOURCE-BUF-IN-SCOPE ;] catch ;

\ Replay in a neutral scope, which starts at top level with no imports.
: REPLAY-NEUTRAL ( str -- n ) {: s:str :}
   [: [: s VERIFY:SOURCE-BUF-IN-SCOPE ;] CHECK:SCOPED ;] catch ;

\ Diagnostics are collected in prose, not printed, so the rejecting case reports
\ its own text instead of leaving a rejection on the gate's stderr.
: REPLAY-QUIET ( str -- n ) {: s:str :}
   REJECTION:CAPTURE
   REJECTION:JSON @ {: was:n :}
   false REJECTION:JSON !
   s REPLAY {: rc:n :}
   was REJECTION:JSON !
   REJECTION:BUFFER-OFF
   rc ;

: DIAG$ ( -- str )
   REJECTION:BUFFER$ ;

;package

T-RESET

\ Three clean replays in a row, a rejecting one, and a clean one after it. The
\ rejecting step leaves the verifier window by the throwing path, so the replay
\ after it proves that path restores the package too.
package CVPS-FIRST
private

s" CVPS-FIRST: first replay certifies" T-LABEL
s" : CVPS-F1 ( n -- n ) ;" CVPS:REPLAY 0 T=
s" CVPS-FIRST: second replay certifies, so the package name came back" T-LABEL
s" : CVPS-F2 ( n -- n ) ;" CVPS:REPLAY 0 T=
s" CVPS-FIRST: third replay certifies" T-LABEL
s" : CVPS-F3 ( n -- n ) ;" CVPS:REPLAY 0 T=
s" CVPS-FIRST: a rejecting replay is rejected, not a context error" T-LABEL
s" : CVPS-FBAD ( n -- n ) dup ;" CVPS:REPLAY-QUIET 0 T<>
s" CVPS-FIRST: the rejection is the word's stack mismatch, so it was a real source reject" T-LABEL
CVPS:DIAG$ s" error[E-MISMATCH]" STR:CONTAINS? TTRUE
CVPS:DIAG$ s" : CVPS-FBAD" STR:CONTAINS? TTRUE
s" CVPS-FIRST: replay after the throwing one certifies" T-LABEL
s" : CVPS-F4 ( n -- n ) ;" CVPS:REPLAY 0 T=

;package

\ A name of a different length: a stale length leaves the mirror disagreeing
\ with the live record even when the bytes are right.
package CVPS-SECOND-PACKAGE-WITH-A-MUCH-LONGER-NAME
private

s" long name: first replay certifies" T-LABEL
s" : CVPS-S1 ( n -- n ) ;" CVPS:REPLAY 0 T=
s" long name: second replay certifies" T-LABEL
s" : CVPS-S2 ( n -- n ) ;" CVPS:REPLAY 0 T=

;package

\ Back inside the first package: reopening resumes the same scope, and the
\ replays must still certify after the longer name has been through the mirror.
package CVPS-FIRST
private

s" reopened CVPS-FIRST: replay certifies" T-LABEL
s" : CVPS-F5 ( n -- n ) ;" CVPS:REPLAY 0 T=
s" reopened CVPS-FIRST: replay certifies again" T-LABEL
s" : CVPS-F6 ( n -- n ) ;" CVPS:REPLAY 0 T=

;package

\ Top level is the no-package context: repeated replays must certify there too,
\ which pins that the empty-name case round-trips instead of only appearing to.
s" top level: first replay certifies" T-LABEL
s" : CVPS-T1 ( n -- n ) ;" CVPS:REPLAY 0 T=
s" top level: second replay certifies" T-LABEL
s" : CVPS-T2 ( n -- n ) ;" CVPS:REPLAY 0 T=

\ ---- parts ------------------------------------------------------------------
\ `package B` inside A opens the part A:B, and its `;package` returns to A's
\ private section, so a word defined after it is A's private one.
s" package CVPS-PA package CVPS-PB ;package : CVPS-PP ( -- n ) 1 ; ;package : CVPS-PQ ( -- n ) CVPS-PP ;"
CVPS:REPLAY-QUIET drop
s" a part's ;package: the package's private word is undefined after it closes" T-LABEL
CVPS:DIAG$ s" error[E-UNDEFINED]" STR:CONTAINS? TTRUE
CVPS:DIAG$ s" `CVPS-PP`" STR:CONTAINS? TTRUE

\ A replay reopening a part the engine holds binds the part's words.
package CVPS-HOST package CVPS-PART public : CVPS-PW ( -- n ) 42 ; ;package ;package
s" a neutral replay reopening an engine-held part binds its words" T-LABEL
s" package CVPS-HOST package CVPS-PART : CVPS-X2 ( -- n ) CVPS-PW ; ;package ;package"
CVPS:REPLAY-NEUTRAL 0 T=

\ ---- sections inside a structure -------------------------------------------
\ `public` inside an open structure switches the section at the run, so the
\ fields and the constant after it are public.
s" package CVPS-SE private BEGIN-STRUCTURE REC-SIZE public CELL +FIELD REC.A PTR-FIELD: REC.B 40 +FIELD REC.C CELL +FIELD REC.D END-STRUCTURE REC-SIZE constant REC-BYTES NULL-PTR REC.D NULL-PTR - constant D-OFF ;package package CVPS-SF private : SHOW ( -- ) CVPS-SE:D-OFF . CVPS-SE:REC-BYTES . NULL-PTR CVPS-SE:REC.B NULL-PTR - . cr ; ;package"
CVPS:REPLAY-QUIET drop
s" a section switch inside a structure moves the words after it" T-LABEL
CVPS:DIAG$ s" E-UNDEFINED" STR:CONTAINS? TFALSE

\ A structure closes in the package scope it opened in.
s" package CVPS-SG private BEGIN-STRUCTURE REC-SIZE CELL +FIELD REC.A ;package END-STRUCTURE"
CVPS:REPLAY-QUIET drop
s" a package closing inside a structure rejects the structure" T-LABEL
CVPS:DIAG$ s" error[E-UNCLOSED]: `BEGIN-STRUCTURE` is still open at `;package`" STR:CONTAINS? TTRUE

\ ---- nested neutral scopes ---------------------------------------------------
\ The inner scope's replay starts at using depth 0, so its `using` fills the slot
\ the outer source's `using` holds; leaving the inner scope puts that slot back.
package CVPS-NPA public : CVPS-NAW ( -- n ) 1 ; ;package
package CVPS-NPB public : CVPS-NAW ( -- n ) 2 ; : CVPS-NBW ( -- n ) 3 ; ;package
package CVPS-NEST
public
variable SEEN
: INNER ( -- )
   [: s" using CVPS-NPB CVPS-NBW drop ;using" VERIFY:SOURCE-BUF-IN-SCOPE ;] CHECK:SCOPED ;
: OUTER ( -- )
   [: s" using CVPS-NPA CVPS-NEST:INNER CVPS-NAW CVPS-NEST:SEEN ! ;using" evaluate-closed ;]
   CHECK:SCOPED ;
;package
CVPS-NEST:OUTER
s" nested neutral scopes: the outer using resolves after the inner scope" T-LABEL
CVPS-NEST:SEEN @ 1 T=

\ ---- the two scope questions a name query can ask --------------------------
\ A name resolves where a candidate calling it is accepted, and is defined
\ where a new one is a duplicate: a global only at top level, which a package
\ may shadow.

\ The sentinel is an existing global (src/lib/error.f's codes), so the test
\ adds no unpackaged global.

s" top level: a candidate binds a global" T-LABEL
s" CVPS-R1 ( -- n ) E-STR-BOUNDS" CHECK-QUIET-CANDIDATE! -1 T=
s" top level: redefining a global is a duplicate" T-LABEL
s" : E-STR-BOUNDS ( -- n ) 1 ;" TEST-EVAL:RC 70 T=

package CVPS-SCOPE
private

\ The word is not defined in THIS package, and the engine still binds it here.
s" in a package: a candidate still binds the global" T-LABEL
s" CVPS-R2 ( -- n ) E-STR-BOUNDS" CHECK-QUIET-CANDIDATE! -1 T=
s" in a package: the engine binds and runs it" T-LABEL
E-STR-BOUNDS -2200 T=
\ After the engine row: the shadow is this package's own E-STR-BOUNDS.
s" in a package: a definition may shadow a global" T-LABEL
s" : E-STR-BOUNDS ( -- n ) 1 ;" TEST-EVAL:RC 0 T=

\ A name defined nowhere binds no candidate and is no duplicate, so the
\ probes above do not pass on an answer that is always yes.
s" in a package: a candidate binds no absent name" T-LABEL
s" CVPS-R3 ( -- n ) CVPS-NO-SUCH-WORD" CHECK-QUIET-CANDIDATE! 0 T=
s" in a package: defining an absent name is no duplicate" T-LABEL
s" : CVPS-NO-SUCH-WORD ( -- n ) 3 ;" TEST-EVAL:RC 0 T=

\ A tail this package owns is a duplicate and binds a candidate, so the
\ duplicate probes do not pass on an answer that is always no.
: CVPS-OWN-TAIL ( -- n ) 9 ;
s" in a package: redefining its own tail is a duplicate" T-LABEL
s" : CVPS-OWN-TAIL ( -- n ) 2 ;" TEST-EVAL:RC 70 T=
s" in a package: a candidate binds its own tail" T-LABEL
s" CVPS-R4 ( -- n ) CVPS-OWN-TAIL" CHECK-QUIET-CANDIDATE! -1 T=

;package

T-REPORT
s" checker-verify-pkg-scope: ok" type cr
