\ checker-verify-pkg-scope.f - the checker's verifier package scope hands the
\ caller's package back byte for byte. VERIFY:SOURCE-BUF-IN-SCOPE saves the
\ caller's package name, length and mode and restores them on leaving; entering
\ again refuses with E-PKG-CONTEXT unless the mirror equals the live record, so
\ a second replay from the same open package is the exact test of the restore.

\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f
\   src/habu/verify-source.f test/checker-verify-pkg-scope.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/habu/verify-source.f

package CVPS
private

TYPED-VARIABLE SRC-A ptr u8
variable SRC-U

\ A quotation cannot read the enclosing word's locals, so the source span
\ travels through these two cells to the caught body.
: ACT ( -- )
   SRC-A @ SRC-U @ VERIFY:SOURCE-BUF-IN-SCOPE ;

create DIAG-BUF $1000 allot
variable DIAG-U

public

\ Replay one source span in the caller's live package context; 0 means the
\ verifier accepted both the package context and the source.
: REPLAY ( ptr u8 n -- n ) {: a:ptr u:n :}
   a SRC-A !
   u SRC-U !
   [: ACT ;] catch ;

\ Diagnostics are collected, not printed, so the rejecting case reports its own
\ text instead of leaving a rejection on the gate's stderr.
: REPLAY-QUIET ( ptr u8 n -- n ) {: a:ptr u:n :}
   DIAG-BUF $1000 DIAG-BUFFER!
   a u REPLAY {: rc:n :}
   DIAG-BUFFER$ nip DIAG-U !
   DIAG-BUFFER-OFF
   rc ;

: DIAG$ ( -- ptr u8 n )
   DIAG-BUF DIAG-U @ ;

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
s" CVPS-FIRST: the rejection names the word, so it was a real source reject" T-LABEL
CVPS:DIAG$ s" cvps-fbad" CONTAINS? TTRUE
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

\ ---- the two scope questions a name query can ask --------------------------
\ CHECKER-DEFINED-HERE? asks whether a name is defined in the scope a new
\ definition would land in (duplicate guards); CHECKER-RESOLVES? asks whether
\ the checker knows an effect for the word the token binds to here (load guards,
\ probes, absence assertions). RESOLVES? answers the same inside a package as
\ outside for a global; DEFINED-HERE? keeps the narrow answer, or the duplicate
\ guards would refuse legal shadowing.

\ The sentinel is an existing global (src/lib/errors.f's codes), so the test
\ adds no unpackaged global.

s" top level: RESOLVES? sees a global" T-LABEL
s" E-STR-BOUNDS" CHECKER-RESOLVES? TTRUE
s" top level: DEFINED-HERE? sees a global" T-LABEL
s" E-STR-BOUNDS" CHECKER-DEFINED-HERE? TTRUE

package CVPS-SCOPE
private

\ The word is not defined in THIS package, and the engine still binds it here.
s" in a package: RESOLVES? still sees the global" T-LABEL
s" E-STR-BOUNDS" CHECKER-RESOLVES? TTRUE
s" in a package: DEFINED-HERE? correctly does not" T-LABEL
s" E-STR-BOUNDS" CHECKER-DEFINED-HERE? TFALSE
s" in a package: the engine binds and runs it" T-LABEL
E-STR-BOUNDS -2200 T=

\ A name that is genuinely nowhere answers no to both, so the two rows above
\ are not passing on a query that always says yes.
s" in a package: RESOLVES? says no to an absent name" T-LABEL
s" CVPS-NO-SUCH-WORD" CHECKER-RESOLVES? TFALSE
s" in a package: DEFINED-HERE? says no to an absent name" T-LABEL
s" CVPS-NO-SUCH-WORD" CHECKER-DEFINED-HERE? TFALSE

\ A tail this package DOES own answers yes to both, which pins that
\ DEFINED-HERE? is answering about this scope rather than always saying no.
: CVPS-OWN-TAIL ( -- n ) 9 ;
s" in a package: DEFINED-HERE? sees its own tail" T-LABEL
s" CVPS-OWN-TAIL" CHECKER-DEFINED-HERE? TTRUE
s" in a package: RESOLVES? sees its own tail" T-LABEL
s" CVPS-OWN-TAIL" CHECKER-RESOLVES? TTRUE

;package

T-REPORT
s" checker-verify-pkg-scope: ok" type cr
