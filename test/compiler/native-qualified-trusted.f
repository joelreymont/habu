\ Native publication of a qualified TRUSTED: word must keep the source
\ spelling for its declared family effect. The bare private twin exercises
\ the same multi-cell effect through the same compiler path.
1 set-tier

require lib/test.f
require src/compiler/native/compiler.f

package NCOMP-QTRUST-LEFT ;package
package NCOMP-QTRUST-RIGHT ;package

package NCOMP-QTRUST

public

STRUCTURE shelf 2
   FIELD source read-view<a,b,u8>
   FIELD count n
;STRUCTURE

private

TRUSTED: LOCAL-EMPTY ( mut-view<c,d,e,shelf<a,b>> -- mut-view<c,d,e,shelf<a,b>> read-view<a,b,u8> )
   0 0 ;

public

TRUSTED: NCOMP-QTRUST-SHELF:EMPTY ( mut-view<c,d,e,shelf<a,b>> -- mut-view<c,d,e,shelf<a,b>> read-view<a,b,u8> )
   0 0 ;

TRUSTED: NCOMP-QTRUST-LEFT:SAME ( n -- n )
   1+ ;

TRUSTED: NCOMP-QTRUST-RIGHT:SAME ( n n -- n )
   + ;

TRUSTED: NCOMP-QTRUST-LEFT:DOWN ( n -- n )
   dup 0= if exit then 1- recurse ;

PTR-VARIABLE SRC-A
variable SRC-U

: SOURCE-GO ( -- )
   SRC-A @ SRC-U @ INCLUDE-EVALUATE ;

: SOURCE-RC ( ptr u8 n -- n )
   SRC-U ! SRC-A ! [: SOURCE-GO ;] catch ;

s" TRUSTED: NCOMP-QTRUST-LEFT:RETRY ( n -- n ) NO-SUCH-WORD ;" SOURCE-RC
   0<> TTRUE

TRUSTED: NCOMP-QTRUST-LEFT:RETRY ( n -- n )
   2 + ;

: CHECK ( -- )
   5 NCOMP-QTRUST-LEFT:SAME 6 T=
   6 7 NCOMP-QTRUST-RIGHT:SAME 13 T=
   4 NCOMP-QTRUST-LEFT:DOWN 0 T=
   5 NCOMP-QTRUST-LEFT:RETRY 7 T= ;

;package

NCOMP-QTRUST:CHECK
s" native qualified trusted: ok" type cr
