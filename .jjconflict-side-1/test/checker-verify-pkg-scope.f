\ checker-verify-pkg-scope.f - the checker's verifier package scope must hand
\ the caller's package back byte for byte (dot habu-neutralize-checker-pkg-b9a250c8).
\
\ VERIFY:SOURCE-BUF-IN-SCOPE replays recorded source with the checker's own
\ package mirror as the authority. Entering that window saves the caller's
\ package name, length and mode; leaving it puts them back. Entering it again
\ first PROVES the mirror still equals the engine's live package record - same
\ mode, same length, same name bytes - and refuses with E-PKG-CONTEXT if it does
\ not. So a second replay from the same open package is the exact test of the
\ restore: it cannot pass on a scrambled name, and no substring check is
\ involved.
\
\ It used to refuse. The save and restore loops kept their index on the data
\ stack and read it back with `over` after the destination address was already
\ pushed, so `over` reached the fetched byte instead of the index and each byte
\ landed at an offset equal to its own character code. One round trip looked
\ healthy because the mirror already held the right name; the second replay from
\ an open package saw a scrambled name and threw 7136 - which is how
\ test/xt-cell-test.f failed, on its second in-scope registration.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f
\   src/habu/verify-source.f test/checker-verify-pkg-scope.f

require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/verify-source.f

package CVPS
private

variable SRC-A
variable SRC-U

: SRC-A-FIELD ( -- ptr ptr u8 )
   SRC-A 0 ptr-field ;

\ A quotation cannot read the enclosing word's locals, so the source span
\ travels through these two cells to the caught body.
: ACT ( -- )
   SRC-A-FIELD @ SRC-U @ VERIFY:SOURCE-BUF-IN-SCOPE ;

create DIAG-BUF $1000 allot
variable DIAG-U

public

\ Replay one source span in the caller's live package context; 0 means the
\ verifier accepted both the package context and the source.
: REPLAY ( ptr u8 n -- n ) {: a:ptr u:n :}
   a SRC-A-FIELD !
   u SRC-U !
   [: ACT ;] catch ;

\ Same replay with the checker's diagnostics collected instead of printed, so
\ the deliberately rejecting case reports its own text rather than leaving a
\ stray rejection on the gate's stderr.
: REPLAY-QUIET ( ptr u8 n -- n ) {: a:ptr u:n :}
   DIAG-BUF $1000 DIAG-BUFFER!
   a u REPLAY {: rc:n :}
   DIAG-BUFFER$ nip DIAG-U !
   DIAG-BUFFER-OFF
   rc ;

\ The text the last quiet replay collected.
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

\ A second package whose name is a different length: the restore has to put back
\ the length as well as the bytes, and a stale length would leave the mirror
\ disagreeing with the live record here even if the bytes were right.
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
\ A query about a name has TWO readings, and they are not the same question:
\   CHECKER-DEFINED-HERE?  is this name already defined in the scope a new
\                          definition would land in?  (duplicate-definition
\                          guards want this, and only this)
\   CHECKER-RESOLVES?      does the checker know an effect for the word this
\                          token would BIND to here?  (load-discipline guards,
\                          vocabulary probes and absence assertions want this)
\
\ They used to share one spelling wired to the first reading, so a guard asking
\ the second got the first's answer: a global the engine resolves and RUNS in
\ the open package was reported absent, and the report inverted the moment its
\ file gained a package (dot habu-checker-defined-answers-1504bbde). The pin is
\ that the second question gives the SAME answer inside a package as outside,
\ for a word that is global - and that the first question still gives the
\ narrow answer, or the duplicate guards would start refusing legal shadowing.

\ The sentinel is an EXISTING global - lib/errors.f's error codes are the
\ repository's documented global vocabulary - so this pins the real case
\ without adding an unpackaged global of its own.

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
