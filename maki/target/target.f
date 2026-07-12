\ maki/target/target.f - immutable target descriptors and nominal identities.
\
\ Semantic facts own identity; labels are presentation only. Equal descriptors
\ intern to one CAD-KIND:target-id even when callers supply another label. The
\ registry is append-only, and every public lookup either returns a validated
\ nominal id or throws. Raw representation conversions remain private.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require maki/cad-kinds.f

-5252 constant E-TARGET-CAP
-5253 constant E-TARGET-FACT
-5254 constant E-TARGET-ID
-5255 constant E-TARGET-LABEL
-5256 constant E-TARGET-UNKNOWN

package TARGET
public

1 constant ISA-PTX

1  constant CAP-PTX
2  constant CAP-FP16
4  constant CAP-BF16
8  constant CAP-TF32
16 constant CAP-MMA
32 constant CAP-ASYNC
64 constant CAP-BARRIER
127 constant CAP-ALL

SUMTYPE descriptor 0
   VARIANT value n n n n n n ;VARIANT
;SUMTYPE

;package

package TARGET
private

16 constant TGT-CAP
32 constant TGT-LABEL-MAX
TGT-CAP TGT-LABEL-MAX * constant TGT-LABEL-CAP

TGT-CAP LAYOUT-BUFFER TGT-DESCS descriptor
create TGT-LABELS TGT-LABEL-CAP allot
create TGT-LO TGT-CAP cells allot
create TGT-LL TGT-CAP cells allot
variable TGT-LABEL-U
variable TGT-N
variable TGT-SM87

TRUSTED: RAW>TARGET-ID ( n -- CAD-KIND:target-id ) ;
TRUSTED: TARGET-ID>RAW ( CAD-KIND:target-id -- n ) ;

$cbf29ce484222325 constant TGT-HASH-BASIS
$100000001b3 constant TGT-HASH-PRIME

: DESC-UN ( descriptor -- n n n n n n )
   MATCH descriptor
      value OF ENDOF
   ;MATCH ;

: HASH-MIX ( n n -- n )
   xor TGT-HASH-PRIME * ;

: DESC-HASH ( descriptor -- n )
   DESC-UN {: isa:n arch:n warp:n threads:n shared:n caps:n :}
   TGT-HASH-BASIS
   isa HASH-MIX arch HASH-MIX warp HASH-MIX
   threads HASH-MIX shared HASH-MIX caps HASH-MIX ;

: DESC-EQ? ( descriptor descriptor -- bool )
   \ typed-local-lint: allow-bare-local - closed descriptor bundles preserve roles.
   {: a b :}
   a DESC-UN {: ai:n aa:n aw:n at:n as:n ac:n :}
   b DESC-UN {: bi:n ba:n bw:n bt:n bs:n bc:n :}
   ai bi = aa ba = and aw bw = and
   at bt = and as bs = and ac bc = and ;

: POW2? ( n -- bool ) {: v:n :}
   v 0 > v v 1- and 0= and ;

: LABEL-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= u TGT-LABEL-MAX > or if E-TARGET-LABEL throw then
   u 0 ?do
      a i + c@ {: c:n :}
      c 32 <= c 127 >= or if E-TARGET-LABEL throw then
   loop ;

: DESC-CK ( descriptor -- )
   DESC-UN
   {: isa:n arch:n warp:n threads:n shared:n caps:n :}
   isa ISA-PTX <> if E-TARGET-FACT throw then
   arch 0 <= arch 999 > or if E-TARGET-FACT throw then
   warp POW2? 0= warp 64 > or if E-TARGET-FACT throw then
   threads warp < threads 2048 > or if E-TARGET-FACT throw then
   threads warp mod 0<> if E-TARGET-FACT throw then
   shared 0 <= if E-TARGET-FACT throw then
   caps CAP-ALL invert and 0<> if E-TARGET-FACT throw then
   caps CAP-PTX and 0= if E-TARGET-FACT throw then ;

: ID-CK ( CAD-KIND:target-id -- n )
   TARGET-ID>RAW dup 0 < over TGT-N @ >= or if E-TARGET-ID throw then ;

: DESC-RAW@ ( n -- descriptor )
   TGT-DESCS @ ;

: DESC-RAW! ( descriptor n -- )
   TGT-DESCS ! ;

: DESC-FIND ( descriptor -- n )
   \ typed-local-lint: allow-bare-local - closed descriptor bundle preserves its role.
   {: d :}
   TGT-N @ 0 ?do
      d i DESC-RAW@ DESC-EQ? if i unloop exit then
   loop
   -1 ;

: LABEL-PUT ( ptr u8 n n -- ) {: a:ptr u:n raw:n :}
   TGT-LABEL-U @ u + TGT-LABEL-CAP > if E-TARGET-CAP throw then
   TGT-LABEL-U @ {: off:n :}
   a TGT-LABELS off + u BYTE-COPY
   off raw cells TGT-LO + !
   u raw cells TGT-LL + !
   off u + TGT-LABEL-U ! ;

: SM87! ( CAD-KIND:target-id -- )
   TGT-SM87 ! ;

public

: DESCRIPTOR ( n n n n n n -- descriptor )
   construct descriptor value dup DESC-CK ;

: REGISTER ( ptr u8 n descriptor -- CAD-KIND:target-id )
   \ typed-local-lint: allow-bare-local - d is a closed descriptor bundle.
   {: label:ptr labelu:n d :}
   label labelu LABEL-CK
   d DESC-CK
   d DESC-FIND {: found:n :}
   found 0 >= if found RAW>TARGET-ID exit then
   TGT-N @ TGT-CAP >= if E-TARGET-CAP throw then
   TGT-LABEL-U @ labelu + TGT-LABEL-CAP > if E-TARGET-CAP throw then
   TGT-N @ {: raw:n :}
   d raw DESC-RAW!
   label labelu raw LABEL-PUT
   raw 1+ TGT-N !
   raw RAW>TARGET-ID ;

: RESOLVE ( descriptor -- CAD-KIND:target-id )
   dup DESC-CK
   DESC-FIND dup 0 < if drop E-TARGET-UNKNOWN throw then
   RAW>TARGET-ID ;

: VALIDATE ( CAD-KIND:target-id -- CAD-KIND:target-id )
   dup ID-CK drop ;

: DESCRIPTOR@ ( CAD-KIND:target-id -- descriptor )
   ID-CK DESC-RAW@ ;

: LABEL$ ( CAD-KIND:target-id -- ptr u8 n )
   ID-CK {: raw:n :}
   TGT-LABELS raw cells TGT-LO + @ +
   raw cells TGT-LL + @ ;

: DIGEST@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-HASH ;

: EQUAL? ( CAD-KIND:target-id CAD-KIND:target-id -- bool )
   {: a:CAD-KIND:target-id b:CAD-KIND:target-id :}
   a DESCRIPTOR@ b DESCRIPTOR@ DESC-EQ? ;

: ISA@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-UN drop drop drop drop drop ;

: ARCH@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-UN drop drop drop drop nip ;

: WARP@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-UN drop drop drop nip nip ;

: THREADS@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-UN drop drop nip nip nip ;

: SHARED@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-UN drop nip nip nip nip ;

: CAPS@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-UN nip nip nip nip nip ;

: FACTS$ ( CAD-KIND:target-id -- ptr u8 n )
   DESCRIPTOR@ DESC-UN
   {: isa:n arch:n warp:n threads:n shared:n caps:n :}
   SB-RESET
   s" isa=" SB-APPEND isa SB-INT
   s" |arch=" SB-APPEND arch SB-INT
   s" |warp=" SB-APPEND warp SB-INT
   s" |threads=" SB-APPEND threads SB-INT
   s" |shared=" SB-APPEND shared SB-INT
   s" |caps=" SB-APPEND caps SB-INT
   SB$ ;

: COUNT ( -- n )
   TGT-N @ ;

: SM87 ( -- CAD-KIND:target-id )
   TGT-SM87 @ VALIDATE ;

private

: SM87-DESC ( -- descriptor )
   ISA-PTX 87 32 1024 49152
   CAP-PTX CAP-FP16 or CAP-BF16 or CAP-TF32 or CAP-MMA or CAP-ASYNC or CAP-BARRIER or
   DESCRIPTOR ;

: INIT ( -- )
   s" sm_87" SM87-DESC REGISTER SM87! ;

INIT

;package
