\ capture.f - a capture taken while a Wasm shadow is open carries the shadow's
\ routines (src/arch/wasm/capture.f into src/habu/aot-decl.f AOT-SHADOW), as
\ test/aot-shadow-capture.f proves for x86-64.
\ Registered as `SUITE wasm-capture`.

package WSHCAP
public
ndict@ here BYTE-VIEW NULL-PTR BYTE-VIEW - variable PRE-R  variable PRE-D  PRE-D !  PRE-R !
variable PRIVATE-WID
;package

require src/lib/string.f
require src/lib/le.f
require src/lib/test.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/xref.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/compiler/native/string.f
require src/compiler/native/shadow.f
require src/arch/wasm/leb.f
require src/arch/wasm/encode.f
require src/arch/wasm/backend.f
require src/arch/wasm/capture.f

\ A binding is a multi-cell value, which only a compiled body may hold.
package WSHCAP
public
: OPEN-WASM ( -- ) WBACK:BINDING NSHADOW:OPEN ;
;package
WBACK:INSTALL
WSHCAP:OPEN-WASM
AOT-ARM:WINDOW-OPEN
NSTR:WINDOW-OPEN

package WSHCAP-WINDOW
public

create WCELL 8 allot
7 WCELL !

1 set-tier
: PEEK ( -- n ) WCELL @ ;
private : HIDDEN ( n -- ) . ;
get-current WSHCAP:PRIVATE-WID !
: HELPER ( n -- n ) 3 * ; public
: USER ( n -- n ) HELPER 1+ ;
: GONE ( -- n ) PEEK 1+ ;
: GONER ( -- n ) GONE 1+ ;
undefine GONE
undefine GONER
: LEAF ( n n -- n ) + ;
: CALLER ( n -- n ) dup LEAF 1 + ;
: TICK ( -- [ n n -- n ] ) ['] LEAF ;
: SHOW ( n -- ) . ;
: FACT ( n -- n ) dup 1 > if dup 1 - RECURSE * then ;
: CONST ( n -- ) create , does> ( -- n ) @ ;
: QUOT ( -- [ n -- n ] ) [: 1 + ;] ;
0 set-tier

7 CONST SEVEN

defer HOOK ( n n -- n )
: ARM-HOOK ( -- ) ['] LEAF is HOOK ;
ARM-HOOK

;package

AOT-ARM:WINDOW-CLOSE

package WSHCAP
using AOT-BUF
using AOT-SHADOW

\ Wasm's own opcodes for the bytes a field sits after, and a body's last.
$10 constant OP-CALL
$42 constant OP-I64-CONST
$0B constant OP-END

\ ---- the records, as the capture ships them -------------------------------------
\ The row of the capture's compact record table that carries `name`, or -1 when
\ the capture strips the record.
: CREC ( n -- ptr u8 ) {: k:n :} AOT-REC-BUF@ AOT-REC-MAX 48 * + k AOT-CREC-ROW * + ;
: SHIPPED ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1
   AOT-REC-N @ 0 ?do
      AOT-NAMES-BUF@ i CREC 8 + LE:U32@ + {: e:ptr :}
      e 1+ e c@ a u STR= if drop i leave then
   loop ;
: W-PEEK ( -- n ) s" PEEK" SHIPPED ;
: W-HELPER ( -- n ) s" HELPER" SHIPPED ;
: W-USER ( -- n ) s" USER" SHIPPED ;
: W-LEAF ( -- n ) s" LEAF" SHIPPED ;
: W-CALLER ( -- n ) s" CALLER" SHIPPED ;
: W-TICK ( -- n ) s" TICK" SHIPPED ;
: W-SHOW ( -- n ) s" SHOW" SHIPPED ;
: W-FACT ( -- n ) s" FACT" SHIPPED ;
: W-CONST ( -- n ) s" CONST" SHIPPED ;
: W-DOES ( -- n ) s" CONST;does" SHIPPED ;
: W-QUOT ( -- n ) s" QUOT" SHIPPED ;
: W-CELL ( -- n ) s" WCELL" SHIPPED ;
: W-HOOK ( -- n ) s" HOOK" SHIPPED ;

\ ---- the tables ---------------------------------------------------------------
: REC@ ( n n -- n ) {: r:n f:n :}
   REC-BUF@ r REC-ROW * + f + LE:U32@ ;
: SITE@ ( n n -- n ) {: s:n f:n :}
   SITE-BUF@ s AOT-SHADOW:SITE-ROW * + f + LE:U32@ ;
: XT@ ( n n -- n ) {: x:n f:n :}
   XT-BUF@ x XT-ROW * + f + LE:U32@ ;
: CODE-BYTE@ ( n -- n ) CODE-BUF@ + c@ ;
: CODE-LE32@ ( n -- n ) CODE-BUF@ + LE:U32@ ;

: INDEX@ ( n -- n ) {: at:n :} CODE-BUF@ CODE-LEN @ at WLEB:U32-PAD@ ;
: ADDR@ ( n -- n ) {: at:n :} CODE-BUF@ CODE-LEN @ at WLEB:S64-PAD@ ;

\ The shadow's record row keyed by shipped record w, or -1.
: ROW-OF ( n -- n ) {: w:n :}
   -1
   REC-N @ 0 ?do
      i 0 REC@ w = if drop i leave then
   loop ;

: AT-OF ( n -- n ) ROW-OF 4 REC@ ;
: LEN-OF ( n -- n ) ROW-OF 8 REC@ ;

\ The first site of `kind` inside shipped record w's routine, or -1.
: SITE-IN ( n n -- n ) {: w:n kind:n :}
   w AT-OF {: at:n :}
   w LEN-OF {: len:n :}
   -1
   SITE-N @ 0 ?do
      i 0 SITE@ {: s:n :}
      s at >=  s at len + < and  i 4 SITE@ kind = and if drop i leave then
   loop ;

: SITES-IN ( n -- n ) {: w:n :}
   w AT-OF {: at:n :}
   w LEN-OF {: len:n :}
   0
   SITE-N @ 0 ?do
      i 0 SITE@ {: s:n :}
      s at >= s at len + < and if 1+ then
   loop ;

: REC-TARGET ( n -- n ) SITE-REC-TAG or ;

\ The window offset of WCELL, as the DATA literal must now hold it.
: WCELL-OFF ( -- n )
   WSHCAP-WINDOW:WCELL BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N +  AOT-ARM:D0 @ - ;

\ ---- the capture -------------------------------------------------------------
\ The Wasm backend (src/arch/wasm/backend.f) loads at run time and a window
\ compiles at tier 1 with a Wasm shadow open, so each definition is also an
\ emission in NSHADOW's map. The window holds a leaf, calls (window, recursive,
\ prefix, private helper), code and DATA literals, a `does>` definer, a
\ quotation and a code cell; a created word (WCELL) and a defer (HOOK), both
\ tier 0, carry routines; a `does>` child (SEVEN) ships no routine, its Wasm
\ body having no slot for the clause. A dead private word and two dead retired
\ ones (the second calling the first) ship no record between two shadowed
\ records, so past them a record's window index and its shipped row differ.
: CAPTURE ( -- )
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:WASM-TARGET-CAPTURE ;

: RECORDS-CASE ( -- )
   s" every shipped tier-1 definition and fixed word but a does> child whose clause replaced its body is one shadow record in window order, a does> companion beside its definer" T-LABEL
   REC-N @ 13 T=
   W-CELL ROW-OF 0 T=
   W-PEEK ROW-OF 1 T=
   W-HELPER ROW-OF 2 T=
   W-USER ROW-OF 3 T=
   W-LEAF ROW-OF 4 T=
   W-CALLER ROW-OF 5 T=
   W-TICK ROW-OF 6 T=
   W-SHOW ROW-OF 7 T=
   W-FACT ROW-OF 8 T=
   W-CONST ROW-OF 9 T=
   W-DOES ROW-OF 10 T=
   W-QUOT ROW-OF 11 T=
   W-HOOK ROW-OF 12 T= ;

: STRIP-CASE ( -- )
   s" dead private and retired words ship no record; the live helper keeps its private WID, and LEAF follows USER past six window records" T-LABEL
   s" HIDDEN" SHIPPED -1 T=
   s" GONE" SHIPPED -1 T=
   s" GONER" SHIPPED -1 T=
   W-HELPER CREC 16 + LE:U32@  PRIVATE-WID @ AOT-ARM:W0 @ - 1+ T=
   W-LEAF W-USER 1+ T=
   s" WSHCAP-WINDOW:LEAF" XREF-FIND-INDEX  s" WSHCAP-WINDOW:PEEK" XREF-FIND-INDEX 6 +  T= ;

: SPAN-CASE ( -- )
   s" a leaf's span is its whole Wasm emission, entered at its first byte, from the magic to the body's end, with no site" T-LABEL
   W-LEAF ROW-OF 12 REC@ 0 T=
   W-LEAF AT-OF CODE-LE32@ WENC:MAGIC T=
   W-LEAF AT-OF W-LEAF LEN-OF + 1- CODE-BYTE@ OP-END T=
   W-LEAF SITES-IN 0 T=
   CODE-LEN @ W-HOOK AT-OF W-HOOK LEN-OF + T= ;

\ Shipped record w's routine holds one site, a call row naming shipped record t
\ over a padded zero index.
: CALLS ( n n -- ) {: w:n t:n :}
   w SITES-IN 1 T=
   w AOT-SHADOW:CALL SITE-IN {: s:n :}
   s 0 >= TTRUE
   s 8 SITE@ t REC-TARGET T=
   s 0 SITE@ {: at:n :}
   at 1- CODE-BYTE@ OP-CALL T=
   at INDEX@ 0 T= ;

: CALL-CASE ( -- )
   s" a call to a window word is a call row naming that word's shipped record, over a padded zero index" T-LABEL
   W-CALLER W-LEAF CALLS
   W-USER W-HELPER CALLS ;

: RECURSE-CASE ( -- )
   s" a self call is the call row a call to another window word is, naming its own word's shipped record" T-LABEL
   W-FACT W-FACT CALLS ;

: LITERAL-CASE ( -- )
   s" a code literal names its word's record over a zeroed padded field, a DATA literal holds its window offset" T-LABEL
   W-TICK AOT-SHADOW:CODE SITE-IN {: t:n :}
   t 0 >= TTRUE
   t 8 SITE@ W-LEAF REC-TARGET T=
   t 0 SITE@ {: tat:n :}
   tat 1- CODE-BYTE@ OP-I64-CONST T=
   tat ADDR@ 0 T=
   W-PEEK AOT-SHADOW:DATA SITE-IN {: d:n :}
   d 0 >= TTRUE
   d 0 SITE@ {: dat:n :}
   dat 1- CODE-BYTE@ OP-I64-CONST T=
   dat ADDR@ WCELL-OFF T= ;

\ Shipped record w's routine holds a FUN site whose padded field after an
\ `i64.const` holds off. A does> companion enters its definer's routine at the
\ clause offset the definer's function address keeps; a quotation's function
\ address keeps its function's offset.
: FUN-AT ( n n -- ) {: w:n off:n :}
   w AOT-SHADOW:FUN SITE-IN {: s:n :}
   s 0 >= TTRUE
   s 8 SITE@ 0 T=
   s 0 SITE@ {: at:n :}
   at 1- CODE-BYTE@ OP-I64-CONST T=
   at ADDR@ off T= ;

: DOES-CASE ( -- )
   s" a does> companion shares its definer's routine and enters at the clause, the offset its definer's function address keeps" T-LABEL
   W-CONST ROW-OF {: r:n :}
   W-DOES ROW-OF {: c:n :}
   c 4 REC@ r 4 REC@ T=
   c 8 REC@ r 8 REC@ T=
   r 12 REC@ 0 T=
   c 12 REC@ {: entry:n :}
   entry 0 > TTRUE
   W-CONST entry FUN-AT ;

: QUOT-CASE ( -- )
   s" a quotation's function address keeps its function's offset, its emission's second header row" T-LABEL
   W-QUOT AT-OF 4 + CODE-LE32@ 2 T=
   W-QUOT  W-QUOT AT-OF 20 + CODE-LE32@  FUN-AT ;

: PREFIX-CASE ( -- )
   s" a call to a word of the engine's own prefix travels by name" T-LABEL
   W-SHOW AOT-SHADOW:CALL SITE-IN {: s:n :}
   s 0 >= TTRUE
   s 8 SITE@ {: t:n :}
   t SITE-TARGET-MASK invert and SITE-NAME-TAG T=
   AOT-NAMES-BUF@ t SITE-TARGET-MASK and + {: e:ptr :}
   e 1+ e c@ s" ." T$= ;

: CELL-CASE ( -- )
   s" a declared code cell holding a window word's entry is keyed by that word's record" T-LABEL
   XT-N @ 1 T=
   0 4 XT@ W-LEAF T=
   0 0 XT@ AOT-WINDOW:XTOFF-N @ < TTRUE ;

: FIXED-CASE ( -- )
   s" a created word's routine holds its window DATA literal and nothing else; a defer's loads its cell and calls execute by name; a does> child ships with no routine" T-LABEL
   W-CELL SITES-IN 1 T=
   W-CELL AOT-SHADOW:DATA SITE-IN {: d:n :}
   d 0 >= TTRUE
   d 0 SITE@ {: dat:n :}
   dat 1- CODE-BYTE@ OP-I64-CONST T=
   dat ADDR@ WCELL-OFF T=
   W-HOOK AOT-SHADOW:DATA SITE-IN 0 >= TTRUE
   W-HOOK AOT-SHADOW:CALL SITE-IN {: s:n :}
   s 0 >= TTRUE
   s 8 SITE@ {: t:n :}
   t SITE-TARGET-MASK invert and SITE-NAME-TAG T=
   AOT-NAMES-BUF@ t SITE-TARGET-MASK and + {: e:ptr :}
   e 1+ e c@ s" execute" T$=
   s" SEVEN" SHIPPED {: w:n :}
   w 0 >= TTRUE
   w ROW-OF -1 T=
   WSHCAP-WINDOW:SEVEN 7 T= ;

public

: RUN ( -- )
   CAPTURE
   NSHADOW:CLOSE
   T-RESET
   RECORDS-CASE
   STRIP-CASE
   SPAN-CASE
   CALL-CASE
   RECURSE-CASE
   LITERAL-CASE
   DOES-CASE
   QUOT-CASE
   PREFIX-CASE
   CELL-CASE
   FIXED-CASE
   T-REPORT ;

;using
;using
;package

WSHCAP:RUN
