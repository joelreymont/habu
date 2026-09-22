\ native-colon.f - the ordinary colon path owns native compilation.
\
\ This loads the production compiler and then writes ordinary definitions. The
\ engine's own token reader captures them; no test entry evaluates source or
\ calls the compiler directly. Execution and the live dictionary record show
\ that the ordinary path publishes a runnable native routine.
\ Tier 1 first: the ordinary `:` handler dispatches to the optimizing compiler
\ only at tier 1, and a native routine published by that path is the subject.
1 set-tier

require lib/test.f
require src/compiler/native/compiler.f

package NST-AWAY
public
: NST-K ( -- n ) 40 ;
: SHARED ( -- n ) 1 ;
;package

package NCOMP-COLON-USED-B
public
: SHARED ( -- n ) 2 ;
;package

package NCOMP-COLON-PRIOR
public
: PRIOR-BARE ( n -- )
   drop E-A-EMPTY throw ;

: PRIOR-RECURSE ( n -- )
   drop E-A-BOUNDS throw ;

: PRIOR-QUAL ( n -- )
   drop E-A-BOUNDS throw ;
;package

using NST-AWAY
: NST-USED ( -- n )
   NST-K 2 + ;
;using

package NCOMP-COLON-TEST

private

get-current constant TEST-WID

: ADD3 ( n -- n )
   3 + ;

: REVIEW-PI ( -- ) ; immediate
s" REVIEW-PI" 0 parse-imm

: REVIEW-RUN ( -- n )
   REVIEW-PI 73 ;

\ The pending package member is not findable until `;`: its body still resolves
\ the case-folded spelling to the used-package word.
using NCOMP-COLON-PRIOR
: PRIOR-BARE ( n n -- n )
   0= if prior-bare then ;

\ `RECURSE` names this pending definition, never the prior same-tail word.
: PRIOR-RECURSE ( n -- n )
   dup 1 <= if drop 1 exit then
   1- RECURSE ;

\ A qualified spelling reaches the same used-package record as the bare name
\ captured before checking this pending member.
: PRIOR-QUAL ( n n -- n )
   0= if ncomp-colon-prior:PrIoR-qUaL then ;
;using

: PLAIN$ ( -- ptr u8 n )
   s" alpha; beta" ;

: ESCAPED$ ( -- ptr u8 n )
   S\" alpha\\;beta" ;

TRUSTED: TRUSTED-DBASE ( -- ptr a )
   dbase@ ;

TRUSTED: TRUSTED-ADD1 ( n -- n )
   1 + ;

4096 constant MAP-BYTES
$1002 constant MAP-FLAGS
variable MAP-CELL

: FULL-CLOBBER ( -- n )
   0 MAP-BYTES 3 MAP-FLAGS -1 0 mmap ;

: REPEATED-DATA-ADDRESS ( -- n )
   MAP-CELL @ 0= if FULL-CLOBBER MAP-CELL ! then
   MAP-CELL @ ;

: DK-CALLEE ( n -- n )
   1 + 2 + 3 + 4 + 5 + 6 + ;

: DK-DEAD ( n -- )
   7 DK-CALLEE 2drop ;

: DK-LIVE ( n -- n )
   7 DK-CALLEE drop 1+ ;

: ASK-AMBIGUOUS ( -- )
   s" SHARED" NDICT:CALL-TARGET drop ;

variable AMBIGUOUS-RC

using NST-AWAY
using NCOMP-COLON-USED-B
' ASK-AMBIGUOUS catch AMBIGUOUS-RC !
;using
;using

: USED-CASE ( -- )
   s" a bare used-public call follows the engine binding" T-LABEL
   NST-USED 42 T=

   s" distinct used-public records are ambiguous" T-LABEL
   AMBIGUOUS-RC @ E-USING-AMBIGUOUS T= ;

: DEAD-RESULT-CASE ( -- )
   s" dead call results are not reloaded" T-LABEL
   10 DK-DEAD

   s" live kept results still cross the call" T-LABEL
   10 DK-LIVE 11 T= ;

: PRIOR-BINDING-CASE ( -- )
   s" a case-folded pending-name call reaches the prior word" T-LABEL
   7 -1 PRIOR-BARE 7 T=
   [: 7 0 PRIOR-BARE drop ;] E-A-EMPTY TTHROWSQ

   s" a qualified pending-name call reaches the same prior record" T-LABEL
   8 -1 PRIOR-QUAL 8 T=
   [: 8 0 PRIOR-QUAL drop ;] E-A-BOUNDS TTHROWSQ

   s" recurse still names the pending definition" T-LABEL
   4 PRIOR-RECURSE 1 T= ;

public

: RUN ( -- )
   s" ordinary colon executes the native routine" T-LABEL
   4 ADD3 7 T=

   s" a neutral parsing immediate executes during capture" T-LABEL
   REVIEW-RUN 73 T=

   USED-CASE

   s" its record points at native code" T-LABEL
   s" ADD3" TEST-WID XREF-FIND-WL XREF-START 0 T<>

   s" its record has native code bytes" T-LABEL
   s" ADD3" TEST-WID XREF-FIND-WL XREF-LEN 0 T<>

   s" a semicolon inside a plain string is captured as data" T-LABEL
   PLAIN$ s" alpha; beta" T$=

   s" a semicolon inside an escaped string is captured as data" T-LABEL
   ESCAPED$ s" alpha\;beta" T$=

   s" a DATA address repeated across a full-clobber call is rematerialized" T-LABEL
   REPEATED-DATA-ADDRESS
   dup 0<> TTRUE
   REPEATED-DATA-ADDRESS T=

   DEAD-RESULT-CASE
   PRIOR-BINDING-CASE

   s" a real trusted cast uses the same native compiler" T-LABEL
   TRUSTED-DBASE drop
   4 TRUSTED-ADD1 5 T=

   T-REPORT ;

;package

NCOMP-COLON-TEST:RUN
