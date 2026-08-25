\ native-colon.f - the ordinary colon path owns native compilation.
\
\ This loads the production compiler and then writes ordinary definitions. The
\ engine's own token reader captures them; no test entry evaluates source or
\ calls the compiler directly. The dictionary record and publication row show
\ that the reachable body is the native routine and no old body was published.

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

using NST-AWAY
: NST-USED ( -- n )
   NST-K 2 + ;
;using

package NCOMP-COLON-TEST

private

get-current constant TEST-WID

: ADD3 ( n -- n )
   3 + ;

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

public

: RUN ( -- )
   s" ordinary colon executes the native routine" T-LABEL
   4 ADD3 7 T=

   USED-CASE

   s" its record points at the native publication" T-LABEL
   s" ADD3" TEST-WID XREF-FIND-WL XREF-START
   s" ADD3" TEST-WID NPUB:NEW-START T=

   s" its record length is the native publication length" T-LABEL
   s" ADD3" TEST-WID XREF-FIND-WL XREF-LEN
   s" ADD3" TEST-WID NPUB:NEW-LEN T=

   s" no old compiler bytes were published" T-LABEL
   s" ADD3" TEST-WID NPUB:OLD-START 0 T=
   s" ADD3" TEST-WID NPUB:OLD-LEN 0 T=

   s" a semicolon inside a plain string is captured as data" T-LABEL
   PLAIN$ s" alpha; beta" T$=

   s" a semicolon inside an escaped string is captured as data" T-LABEL
   ESCAPED$ s" alpha\;beta" T$=

   s" a DATA address repeated across a full-clobber call is rematerialized" T-LABEL
   REPEATED-DATA-ADDRESS
   dup 0<> TTRUE
   REPEATED-DATA-ADDRESS T=

   DEAD-RESULT-CASE

   s" a real trusted cast uses the same native compiler" T-LABEL
   TRUSTED-DBASE drop
   4 TRUSTED-ADD1 5 T=

   T-REPORT ;

;package

NCOMP-COLON-TEST:RUN
