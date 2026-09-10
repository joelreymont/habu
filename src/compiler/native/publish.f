\ publish.f - commit one sealed native emission to its pending dictionary record.
\ Every refusal precedes the code window; the commit phase only copies accepted
\ bytes, records relocation sites, and publishes the record.

require lib/prelude.f
require lib/errors.f
require src/compiler/native/branch.f
require src/compiler/native/dict.f
require src/compiler/native/emit.f

package NPUB

private

$4000 constant CODE-RESERVE
4 constant INSN-BYTES

TRUSTED: CODE-WINDOW ( ptr u8 n n -- )
   code-publish ;

TRUSTED: RELOC-EXTERNAL ( n -- )
   callmap-set ;

TRUSTED: RELOC-ADDR ( n -- )
   addrmap-set ;

TRUSTED: PUBLISH-REC ( n n n -- )
   xref-retarget ;

TRUSTED: MIN-IN-REC ( n n -- )
   min-in-mark ;

TRUSTED: DOES-RECORD ( n n -- )
   does-record ;

: SIZE-CK ( -- n )
   A64EMIT:SIZE {: n:n :}
   A64EMIT:INSNS 0 <= if E-NPUB-SIZE throw then
   n INSN-BYTES < if E-NPUB-SIZE throw then
   n INSN-BYTES mod 0<> if E-NPUB-SIZE throw then
   n ;

: OFFSET-CK ( n n -- n ) {: off:n size:n :}
   off 0 < if E-NPUB-OFFSET throw then
   off INSN-BYTES + size > if E-NPUB-OFFSET throw then
   off INSN-BYTES mod 0<> if E-NPUB-OFFSET throw then
   off ;

: CODE-CEILING ( -- n )
   dbase@ REGION + CODE-RESERVE - ;

: ROOM-CK ( n -- n ) {: size:n :}
   cp@ {: fn:n :}
   fn size + CODE-CEILING > if E-NPUB-ROOM throw then
   fn ;

: PLACE-CK ( n -- ) {: fn:n :}
   A64EMIT:PLACED? 0= if exit then
   A64EMIT:PLACEMENT fn <> if E-NPUB-PLACE throw then ;

: INSN-ADDR ( n n n -- n ) {: fn:n size:n k:n :}
   k A64EMIT:MAP-OFFSET@ size OFFSET-CK fn + ;

: MAP-CK ( n -- ) {: size:n :}
   A64EMIT:INSNS 0 ?do
      i A64EMIT:MAP-OFFSET@ size OFFSET-CK  i INSN-BYTES * <> if
         E-NPUB-OFFSET throw
      then
   loop ;

: EXTERNAL? ( n -- bool ) {: t:n :}
   t dbase@ < if true exit then
   t dbase@ REGION + >= ;

: LEAVES? ( n n n -- bool ) {: fn:n size:n t:n :}
   t fn < if true exit then
   t fn size + >= ;

\ A tail branch out of the region cannot be relocated by the call map.
: TAIL-RELOC-CK ( n n -- ) {: fn:n size:n :}
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ {: w:n :}
      w NBR:B? if
         fn size i INSN-ADDR  w  NBR:B-TARGET {: t:n :}
         fn size t LEAVES? t EXTERNAL? and if E-NPUB-RELOC throw then
      then
   loop ;

: RELOC-CALLS ( n -- ) {: fn:n :}
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ NBR:BL? if
         fn i INSN-BYTES * +  i A64EMIT:WORD@  NBR:BL-TARGET EXTERNAL? if
            fn i INSN-BYTES * + RELOC-EXTERNAL
         then
      then
   loop ;

: RELOC-ADDRS ( n -- ) {: fn:n :}
   A64EMIT:ADDR-SITES 0 ?do
      fn  i A64EMIT:ADDR-SITE@ INSN-BYTES * +  RELOC-ADDR
   loop ;

\ Dictionary records omit a trailing return and retain the whole span otherwise.
: RECORDED-LEN ( n -- n ) {: size:n :}
   A64EMIT:TRAILING-RETURN? 0= if size exit then
   size INSN-BYTES - ;

: VALIDATE-EMISSION ( n -- n ) {: size:n :}
   size ROOM-CK {: fn:n :}
   fn PLACE-CK
   fn size TAIL-RELOC-CK
   fn ;

: COMMIT ( n n n -- ) {: idx:n fn:n size:n :}
   A64EMIT:BYTES fn size CODE-WINDOW
   fn RELOC-CALLS
   fn RELOC-ADDRS
   fn  size RECORDED-LEN  idx PUBLISH-REC ;

: PENDING-IDX ( -- n )
   ndict@ ;

: PENDING-CK ( n -- ) {: idx:n :}
   idx PENDING-IDX <> if E-NPUB-PENDING throw then
   idx XREF-REC XREF-WORDLIST {: wid:n :}
   wid XREF-RETIRED-WL = if E-NPUB-PENDING throw then
   wid XREF-NAMESPACE-WL = if E-NPUB-PENDING throw then
   idx XREF-REC XREF-FLAGS {: f:n :}
   f DNAME-INT and 0<> if E-NPUB-PENDING throw then
   f DNAME-IMM and 0<> if E-NPUB-PENDING throw then ;

: PENDING-PROVE ( -- n n n )
   SIZE-CK {: size:n :}
   size MAP-CK
   PENDING-IDX {: idx:n :}
   idx PENDING-CK
   size VALIDATE-EMISSION {: fn:n :}
   idx fn size ;

\ Publishing the checker's one-shot minimum-input latch is engine authority.
\ Keep the boundary at the native publisher that consumes it for this record.
TRUSTED: PENDING-FACTS ( n -- ) {: idx:n :}
   REC-WIDE-PUBLISH
   REC-MIN-IN@ {: mi:n :}
   mi 0<> if idx mi MIN-IN-REC then ;

5 constant DOES-SUFFIX-BYTES

: PAD-INSTRUCTION ( n -- n )
   3 + -4 and ;

: DOES-NAME-PAD ( n -- n )
   XREF-REC XREF-NAME$ nip DOES-SUFFIX-BYTES + PAD-INSTRUCTION ;

: DOES-PROVE ( n -- n n n n ) {: fun:n :}
   PENDING-PROVE {: idx:n fn:n size:n :}
   idx 1+ DICT-CAP >= if E-NPUB-PENDING throw then
   idx DOES-NAME-PAD {: pad:n :}
   fn size + pad + CODE-CEILING > if E-NPUB-ROOM throw then
   fun A64EMIT:FUNCTION-OFFSET@ {: off:n :}
   off 0 <= if E-NPUB-OFFSET throw then
   off size OFFSET-CK drop
   idx fn size off ;

public

: PUBLISH-PENDING ( -- )
   PENDING-PROVE {: idx:n fn:n size:n :}
   idx fn size COMMIT
   ndict@ 1+ ndict!
   idx PENDING-FACTS ;

: PUBLISH-PENDING-DOES ( n -- ) {: fun:n :}
   fun DOES-PROVE {: idx:n fn:n size:n off:n :}
   idx fn size COMMIT
   fn off +  size off - RECORDED-LEN  DOES-RECORD
   ndict@ 1+ ndict!
   idx PENDING-FACTS
   ndict@ 1+ ndict! ;

: NEXT-SLOT ( -- n )
   cp@ ;

: IN-REGION? ( n -- bool )
   EXTERNAL? 0= ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
