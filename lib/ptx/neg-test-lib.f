\ neg-test-lib.f - in-process PTX negative checker assertions.
\
\ Package PTXN publishes exactly two words: REJECTS, a candidate the checker
\ must refuse, named by a needle in the RENDERED diagnostic, and ACCEPTS, its
\ resolving control. The candidate plumbing underneath stays private on purpose.
\ Three call sites had already hand-rolled their own control off the raw verdict
\ and two of them got it wrong, so reaching the verdict is the thing to seal:
\ with CHECK private, a positive control can only be the shared one.
\
\ CHECK-CANDIDATE! answers -1 certified / 0 rejected / 1 uncheckable
\ (MODEL-CAD-V2-PLAN.md; src/core/checker.f). An UNRESOLVABLE candidate - a
\ typo, a sealed private mint referenced cross-package - answers 1, which is
\ neither of the other two. Both assertions therefore test the EXACT verdict and
\ never "not the other one": `verdict 0 <>` reads an unresolvable candidate as
\ certified, which is how a control can silently stop controlling anything.

require lib/ptx/test-prelude.f

package PTXN
private

$10000 constant BUF-CAP

\ CHECK-CANDIDATE!'s three answers, plus a pre-state that is none of them: a
\ throw before the store cannot then leave a readable verdict behind.
-1 constant CERT
 0 constant REJ
 2 constant NO-VERD

variable SRC-A
variable SRC-U
variable RC
variable VERD

\ The assertions below hold the rendered diagnostic's LENGTH in a local named
\ `dlen`, never `diag`: names are case-insensitive here, so a local spelled
\ `diag` shadows this buffer inside that definition and pushes a length where a
\ `ptr u8` is meant. The shadow is silent whenever the two effects happen to
\ agree; here it surfaced as an arity mismatch at CONTAINS?.
create DIAG BUF-CAP allot

: SRC-A-FIELD ( -- ptr ptr u8 )
   SRC-A 0 ptr-field ;

: SRC-A@ ( -- ptr u8 )
   SRC-A-FIELD @ ;

: SRC-A! ( ptr u8 -- )
   SRC-A-FIELD ! ;

: SRC! ( ptr u8 n -- ) {: a:ptr u:n :}
   a SRC-A!
   u SRC-U ! ;

: SRC$ ( -- ptr u8 n )
   SRC-A@ SRC-U @ ;

: ACT ( -- )
   SRC$ CHECK-CANDIDATE! VERD ! ;

\ ( candidate -- diaglen verdict rc ) : run one candidate through the real
\ checker with the diagnostic buffer armed.
: CHECK ( ptr u8 n -- n n n ) {: src:ptr srcu:n :}
   src srcu SRC!
   NO-VERD VERD !
   DIAG BUF-CAP DIAG-BUFFER!
   [: ACT ;] catch RC !
   DIAG-BUFFER$ nip
   DIAG-BUFFER-OFF
   VERD @ RC @ ;

public

\ The needle is matched against the RENDERED diagnostic, never against the
\ candidate source, so needle text sitting in the candidate's own comments or
\ strings cannot satisfy it. Prefer a needle that names the rule and the thing
\ it named - "'t' is minted into family 'acc'" - over a bare word name, which an
\ undefined-word error at the same call site would also match.
: REJECTS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n needle:ptr needleu:n label:ptr labelu:n :}
   src srcu CHECK {: dlen:n verdict:n rc:n :}
   label labelu T-LABEL
   rc 0 T=
   label labelu T-LABEL
   verdict REJ T=
   label labelu T-LABEL
   DIAG dlen needle needleu CONTAINS? TTRUE ;

\ The resolving control for a REJECTS case: the same premise minus the one
\ property under test. An EMPTY diagnostic is asserted alongside the verdict -
\ that is what proves a needle matched in the paired negative is the CHECKER's
\ text and not the candidate's own source echoed back.
: ACCEPTS ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n label:ptr labelu:n :}
   src srcu CHECK {: dlen:n verdict:n rc:n :}
   label labelu T-LABEL
   rc 0 T=
   label labelu T-LABEL
   verdict CERT T=
   label labelu T-LABEL
   dlen 0 T= ;

;package
