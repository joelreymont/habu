\ A mutable view owns one linear capability even when its element type is
\ linear. A record also owns each separate linear field it contains.
\ Run through the native source load path: bin/hb --load test/c2-mut-records.f

require lib/test.f
require lib/test/subject.f
require lib/adt/option.f

package MREC
public

DEFLINEAR MREC:tok

STRUCTURE one 4
   FIELD view mut-view<a,b,c,d>
;STRUCTURE

STRUCTURE mixed 5
   FIELD view mut-view<a,b,c,d>
   FIELD owned e
;STRUCTURE

STRUCTURE two 6
   FIELD left mut-view<a,b,c,d>
   FIELD right mut-view<a,b,e,g>
;STRUCTURE

STRUCTURE borrowed 4
   FIELD view read-view<a,b,d>
;STRUCTURE


private

: ONE-MAKE ( mut-view<p,q,a,u8> -- one<p,q,a,u8> ) MREC-ONE:MAKE ;
: ONE-UNMAKE ( one<p,q,a,u8> -- mut-view<p,q,a,u8> ) MREC-ONE:UNMAKE ;
: ONE-MOVE ( one<p,q,a,u8> n -- n one<p,q,a,u8> ) swap ;
: ONE-PHANTOM ( mut-view<p,q,a,tok> -- one<p,q,a,tok> ) MREC-ONE:MAKE ;
: ONE-PHANTOM-UNMAKE ( one<p,q,a,tok> -- mut-view<p,q,a,tok> ) MREC-ONE:UNMAKE ;

: MIXED-MAKE ( mut-view<p,q,a,u8> tok -- mixed<p,q,a,u8,tok> ) MREC-MIXED:MAKE ;
: MIXED-UNMAKE ( mixed<p,q,a,u8,tok> -- mut-view<p,q,a,u8> tok ) MREC-MIXED:UNMAKE ;
: MIXED-PHANTOM ( mut-view<p,q,a,tok> tok -- mixed<p,q,a,tok,tok> ) MREC-MIXED:MAKE ;
: MIXED-PHANTOM-UNMAKE ( mixed<p,q,a,tok,tok> -- mut-view<p,q,a,tok> tok ) MREC-MIXED:UNMAKE ;
: MIXED-MOVE ( mixed<p,q,a,tok,tok> n -- n mixed<p,q,a,tok,tok> ) swap ;

: TWO-MAKE ( mut-view<p,q,a,u8> mut-view<p,q,b,u8> -- two<p,q,a,u8,b,u8> ) MREC-TWO:MAKE ;
: TWO-UNMAKE ( two<p,q,a,u8,b,u8> -- mut-view<p,q,a,u8> mut-view<p,q,b,u8> ) MREC-TWO:UNMAKE ;
: TWO-MOVE ( two<p,q,a,u8,b,u8> n -- n two<p,q,a,u8,b,u8> ) swap ;

: READ-MAKE ( read-view<p,q,tok> -- borrowed<p,q,a,tok> ) MREC-BORROWED:MAKE ;
: READ-UNMAKE ( borrowed<p,q,a,tok> -- read-view<p,q,tok> ) MREC-BORROWED:UNMAKE ;
: READ-COPY ( borrowed<p,q,a,tok> -- borrowed<p,q,a,tok> borrowed<p,q,a,tok> ) dup ;
: READ-DROP ( borrowed<p,q,a,tok> -- ) drop ;

: NEST-MOVE ( option<one<p,q,a,u8>> n -- n option<one<p,q,a,u8>> ) swap ;


$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

: REJECT ( ptr u8 n n -- bool ) {: expected:n :}
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

public

: CHECK ( -- )
   s" one view in a record cannot be copied" T-LABEL
   s" : MREC-ONE-DUP ( one<p,q,a,u8> -- one<p,q,a,u8> one<p,q,a,u8> ) dup ;" 70 REJECT TTRUE
   s" one view in a record cannot be dropped" T-LABEL
   s" : MREC-ONE-DROP ( one<p,q,a,u8> -- ) drop ;" 70 REJECT TTRUE
   s" phantom linear element does not permit record copying" T-LABEL
   s" : MREC-PHANTOM-DUP ( one<p,q,a,tok> -- one<p,q,a,tok> one<p,q,a,tok> ) dup ;" 70 REJECT TTRUE
   s" view and owned field remain separate obligations" T-LABEL
   s" : MREC-MIXED-DUP ( mixed<p,q,a,tok,tok> -- mixed<p,q,a,tok,tok> mixed<p,q,a,tok,tok> ) dup ;" 70 REJECT TTRUE
   s" both independent views remain owned" T-LABEL
   s" : MREC-TWO-DROP ( two<p,q,a,u8,b,u8> -- ) drop ;" 70 REJECT TTRUE
   s" independent views cannot be copied together" T-LABEL
   s" : MREC-TWO-DUP ( two<p,q,a,u8,b,u8> -- two<p,q,a,u8,b,u8> two<p,q,a,u8,b,u8> ) dup ;" 70 REJECT TTRUE
   s" two views remain separate after UNMAKE" T-LABEL
   s" : MREC-TWO-LOSE ( two<p,q,a,u8,b,u8> -- mut-view<p,q,a,u8> ) MREC-TWO:UNMAKE drop ;" 70 REJECT TTRUE
   s" two views cannot be duplicated after UNMAKE" T-LABEL
   s" : MREC-TWO-COPY ( two<p,q,a,u8,b,u8> -- mut-view<p,q,a,u8> mut-view<p,q,b,u8> mut-view<p,q,b,u8> ) MREC-TWO:UNMAKE dup ;" 70 REJECT TTRUE
   s" nested record cannot be copied" T-LABEL
   s" : MREC-NEST-DUP ( option<one<p,q,a,u8>> -- option<one<p,q,a,u8>> option<one<p,q,a,u8>> ) dup ;" 70 REJECT TTRUE
   s" nested record cannot be dropped" T-LABEL
   s" : MREC-NEST-DROP ( option<one<p,q,a,u8>> -- ) drop ;" 70 REJECT TTRUE
   s" an empty sum branch cannot absorb an owned record" T-LABEL
   s" : MREC-NEST-LOSE ( one<p,q,a,u8> -- option<one<p,q,a,u8>> ) OPTION:NONE ;" 70 REJECT TTRUE
   s" matching cannot discard a view held by a sum" T-LABEL
   s" : MREC-NEST-ARM-DROP ( option<one<p,q,a,u8>> -- n ) MATCH option none OF 0 ENDOF some OF drop 1 ENDOF ;MATCH ;" 70 REJECT TTRUE ;

;package

T-RESET
MREC:CHECK
T-REPORT
s" c2-mut-records: ok" type cr
