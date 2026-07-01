\ engine-suite.f — the behavior suite run BY THE ENGINE ITSELF (bin/hb), no
\ gforth. A failure prints F<index>, assertion detail, and exits 1 via report.

require test/checker-assert.f

variable #FAIL
variable #CASE

256 constant T-LABEL-CAP
create T-LABEL-BUF T-LABEL-CAP allot
variable T-LABEL-U

: T-LABEL-CLEAR ( -- )
   0 T-LABEL-U ! ;

: T-LABEL$ ( -- ptr u8 n )
   T-LABEL-BUF T-LABEL-U @ ;

: T-LABEL ( ptr u8 n -- ) {: a:ptr u:n :}
   u T-LABEL-CAP > if s" engine-suite: label too long" 1 die then
   0 begin dup u < while
      dup a + c@  over T-LABEL-BUF + c!
      1+
   repeat drop
   u T-LABEL-U ! ;

: T-LABEL. ( -- )
   T-LABEL-U @ 0 > if s" case: " type T-LABEL$ type cr then ;

: T-FAIL+ ( -- )
   #FAIL @ 1 + #FAIL ! ;

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   T-LABEL.
   T-FAIL+ ;

: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL
      s" assert: expected " type want .
      s" got " type got .
   then
   T-LABEL-CLEAR ;

: T$= ( ptr u8 n ptr u8 n -- ) {: ga:ptr gu:n wa:ptr wu:n :}
   #CASE @ 1 + #CASE !
   gu wu <> if
      T-FAIL
      s" assert string len: expected " type wu .
      s" got " type gu .
      T-LABEL-CLEAR exit
   then
   0 begin dup gu < while
      dup ga + c@  over wa + c@ <> if
         drop T-FAIL
         s" assert string byte mismatch" type cr
         T-LABEL-CLEAR exit
      then
      1 +
   repeat drop
   T-LABEL-CLEAR ;

\ arithmetic, stack, comparisons
5 dup * 25 T=
1 2 3 rot + + 6 T=
10 3 - 7 T=
-1 $FF and 255 T=
$AF 175 T=
-$10 -16 T=
: TMOVK-LITS ( -- n n n n ) 0 -1 $123456789ABCDEF0 $FFFFFFFF0000FFFF ;
TMOVK-LITS $FFFFFFFF0000FFFF T= $123456789ABCDEF0 T= -1 T= 0 T=
7 2 mod 1 T=
7 2 /mod 3 T= 1 T=
$10 4 lshift $100 T=
5 3 > -1 T=
5 3 <= 0 T=
-7 abs 7 T=
3 7 min 3 T=
3 7 max 7 T=
1 2 3 4 2swap 2 T= 1 T= 4 T= 3 T=
1 2 3 4 2over 2 T= 1 T= 4 T= 3 T= 2 T= 1 T=
0 ?dup 0 T=
5 ?dup + 10 T=
depth 0 T=
1 2 depth 2 T= 2 T= 1 T=
: TDEPTH ( i64 i64 -- i64 i64 i64 ) depth ;
3 4 TDEPTH 2 T= 4 T= 3 T=
8 cell+ 16 T=
8 char+ 9 T=
8 chars 8 T=
depth 0 T=
1 2 3 depth nip nip nip 3 T=

\ control flow
: TIF ( n -- n ) dup 5 > if drop 99 else 1 + then ;
3 TIF 4 T=
9 TIF 99 T=
: TLOOP ( -- n ) 0 begin 1 + dup 10 >= until ;
TLOOP 10 T=
: TDO ( -- n ) 0 5 0 do i + loop ;
TDO 10 T=
: TQDO ( -- n ) 0 3 3 ?do 1 + loop ;
TQDO 0 T=
: TPLOOP ( -- n ) 0 10 0 do 1 + 2 +loop ;
TPLOOP 5 T=
: TJ ( -- n ) 0 3 0 do 4 0 do j + loop loop ;
TJ 12 T=
: TLEAVE ( -- n ) 0 10 0 do 1 + dup 4 = if leave then loop ;
TLEAVE 4 T=
: TCASE ( n -- n )
   case
      1 of 10 endof
      2 of 20 endof
      30 swap
   endcase ;
1 TCASE 10 T=
2 TCASE 20 T=
9 TCASE 30 T=
: TNESTCASE ( n n -- n ) {: inner:n outer:n :}
   outer case
      1 of inner case 5 of 15 endof 16 swap endcase endof
      2 of 20 endof
      99 swap
   endcase ;
5 1 TNESTCASE 15 T=
4 1 TNESTCASE 16 T=
0 2 TNESTCASE 20 T=
9 3 TNESTCASE 99 T=
KERNEL: TKERNEL-INC ( n -- n ) 1+ ;
8 TKERNEL-INC 9 T=

\ target predicates must be real executable booleans, not trusted signature stubs
: TTARGET-KNOWN ( -- n )
   HB-TARGET-KNOWN? if 1 else 0 then ;
: TTARGET-COUNT ( -- n )
   0
   HB-TARGET-LINUX? if 1 + then
   HB-TARGET-MACOS? if 1 + then ;
TTARGET-KNOWN 1 T=
TTARGET-COUNT 1 T=

\ return stack, exit, recurse
: TRS ( -- n ) 1 2 >r 10 + r> + ;
TRS 13 T=
: TRS2 ( -- n ) 1 2 2>r 10 2r> + + ;
TRS2 13 T=
: TRS2@ ( -- n ) 1 2 2>r 2r@ + 2r> + + ;
TRS2@ 6 T=
: TEXIT ( n -- n ) dup 5 > if drop 99 exit then 1 + ;
3 TEXIT 4 T=
7 TEXIT 99 T=
: FIB ( i64 -- i64 ) dup 2 < if drop 1 exit then dup 1- recurse swap 2 - recurse + ;
10 FIB 89 T=

\ locals (typed)
: TLOC ( n n -- n ) {: a:n b:n :} a b + ;
3 4 TLOC 7 T=
: TLOC-IF ( n bool -- n )
   if {: x:n :} x else drop 0 then ;
7 0 0= TLOC-IF 7 T=
7 0 0= 0= TLOC-IF 0 T=
: TLOC-GUARD ( n -- n )
   dup 0 < if drop 99 exit then
   {: x:n :} x 1 + ;
4 TLOC-GUARD 5 T=
-1 TLOC-GUARD 99 T=
: TLOC-DO ( -- n )
   0 4 0 do i {: x:n :} x + loop ;
TLOC-DO 6 T=
: TLOC-LEAVE ( n -- n )
   {: base:n :}
   0 5 0 do
      i {: x:n :}
      x 2 = if leave then
      x +
   loop
   base + ;
10 TLOC-LEAVE 11 T=
: TLOC-CASE ( n -- n )
   case
      1 of 10 {: x:n :} x 1 + endof
      2 of 20 {: y:n :} y 1 + endof
      30 swap
   endcase ;
1 TLOC-CASE 11 T=
2 TLOC-CASE 21 T=
3 TLOC-CASE 30 T=
: TLOC-SHADOW ( n bool -- n )
   if {: drop:n :} drop else drop 0 then ;
7 0 0= TLOC-SHADOW 7 T=
7 0 0= 0= TLOC-SHADOW 0 T=

\ create/does>
: CONST ( n -- ) create , does> ( -- n ) @ ;
5 CONST FIVE
FIVE 5 T=
: ARR ( n -- ) create cells allot does> ( n -- ptr a ) swap cells + ;
4 ARR A4
7 2 A4 !
2 A4 @ 7 T=
here 3 over c! 65 over 1 + c! 66 over 2 + c! count 3 T= drop
here 10 over ! 5 over +! @ 15 T=

\ SwiftForth-style escaped string words
create TESC-S-WANT
   $41 c, $5C c, $42 c, $22 c, $0A c, $09 c, $0D c, $00 c, $41 c,
: TESC-S-WANT$ ( -- ptr u8 n )
   TESC-S-WANT 9 ;
: TESC-S-COMPILED$ ( -- ptr u8 n )
   S\" A\\B\"\n\t\r\z\x41" ;
S\" A\\B\"\n\t\r\z\x41" TESC-S-WANT$ T$=
TESC-S-COMPILED$ TESC-S-WANT$ T$=

create TESC-S-MULTILINE-WANT
   $7B c, $0A c, $20 c, $22 c, $6B c, $22 c, $3A c,
   $20 c, $22 c, $76 c, $22 c, $0A c, $7D c, $0A c,
: TESC-S-MULTILINE-WANT$ ( -- ptr u8 n )
   TESC-S-MULTILINE-WANT 14 ;
: TESC-S-MULTILINE$ ( -- ptr u8 n )
   S\" {
 \"k\": \"v\"
}
" ;
TESC-S-MULTILINE$ TESC-S-MULTILINE-WANT$ T$=

create TESC-C-WANT
   $41 c, $22 c, $0A c, $42 c,
: TESC-C-WANT$ ( -- ptr u8 n )
   TESC-C-WANT 4 ;
: TESC-C-COMPILED$ ( -- ptr u8 n )
   C\" A\q\n\x42" count ;
C\" A\q\n\x42" count TESC-C-WANT$ T$=
TESC-C-COMPILED$ TESC-C-WANT$ T$=

\ quotations + combinators
: TQ1 ( -- n ) 5 [: 1 + ;] execute ;
TQ1 6 T=
: RUN-R> ( [ -- i64 | i64 -- ] -- i64 ) 7 >r execute ;
: TQRIN ( -- n ) [: r> ;] RUN-R> ;
TQRIN 7 T=
: RUN->R ( [ -- | -- i64 ] -- i64 ) execute r> ;
: TQROUT ( -- n ) [: 9 >r ;] RUN->R ;
TQROUT 9 T=
: TDIP ( -- n ) 10 3 [: 2 * ;] DIP + ;
TDIP 23 T=
: TKEEP ( -- n ) 7 [: 1+ ;] KEEP + ;
TKEEP 15 T=
: TBI ( -- n ) 5 [: 1+ ;] [: 2 * ;] BI + ;
TBI 16 T=
: TTRI ( -- n ) 3 [: 1+ ;] [: 2 * ;] [: 3 + ;] TRI + + ;
TTRI 16 T=
: TTIMES ( -- n ) 0 5 [: 1+ ;] TIMES ;
TTIMES 5 T=
create IARR 3 cells allot
1 IARR !  2 IARR cell+ !  3 IARR cell+ cell+ !
: TEACH ( -- n ) 0 IARR 3 [: + ;] EACH ;
TEACH 6 T=
: TFOLD ( -- n ) IARR 3 0 [: + ;] FOLD ;
TFOLD 6 T=
: TMAP ( -- ) IARR 3 [: 1+ ;] MAP ;
TMAP
TFOLD 9 T=
: T-CHECK-REJECTS ( ptr u8 n -- )
   2dup T-LABEL
   CHECK-QUIET-CANDIDATE! 0 T= ;

TRUSTED: T-CHECK-PASSES ( ptr u8 n -- )
   2dup T-LABEL
   CHECK! -1 T= ;
variable TC-UEND
variable TC-NEND
variable TC-SYMN
variable TC-SYMU
variable TC-DIAG
UEND @ TC-UEND !
NORET-END @ TC-NEND !
SYM-N @ TC-SYMN !
SYM-STR-U @ TC-SYMU !
s" A ( n -- n ) 1+" CHECK-CANDIDATE! -1 T=
s" A ( n -- n ) dup drop" CHECK-CANDIDATE! -1 T=
DIAGXT @ TC-DIAG !
0 DIAGXT !
s" A ( n -- n n ) drop" CHECK-CANDIDATE! 0 T=
TC-DIAG @ DIAGXT !
s" T-CAND-THROW ( n -- n ) dup 0 < if 1 throw then" CHECK-CANDIDATE! -1 T=
UEND @ TC-UEND @ = -1 T=
NORET-END @ TC-NEND @ = -1 T=
SYM-N @ TC-SYMN @ = -1 T=
SYM-STR-U @ TC-SYMU @ = -1 T=
variable TG-UEND
variable TG-CAP
variable TG-USIGS-P
variable TG-GROW-CAP
variable TG-GROW-NEXT
variable TG-UOFF
UEND @ TG-UEND !
USIGS-CAP-U @ TG-CAP !
USIGS-P @ TG-USIGS-P !
USIGS-GROW-CAP @ TG-GROW-CAP !
USIGS-GROW-NEXT @ TG-GROW-NEXT !
USIGS-USER-OFF @ TG-UOFF !
USIGS-P @
USIGS-CAP-U @
USIGS-RESET
USIGS-CAP-U @ T=
USIGS-P @ T=
UEND @ 0 T=
USIGS @ 0 T=
USIGS-GROW-CAP @ 0 T=
USIGS-GROW-NEXT @ 0 T=
USIGS-INIT-CAP 2 / USIGS-CAP-U !
USIGS-P @
USIGS-RESET
USIGS-P @ = 0 T=
USIGS-CAP-U @ USIGS-INIT-CAP T=
UEND @ 0 T=
USIGS @ 0 T=
UEND @ 128 + USIGS-CAP-U !
s" T-GROW-PAIR" s" ptr u8 n ptr u8 n -- ptr u8 n" TRUST
s" COK-GROW-PAIR ( ptr u8 n ptr u8 n -- ptr u8 n ) T-GROW-PAIR" T-CHECK-PASSES
TG-USIGS-P @ USIGS-P !
TG-CAP @ USIGS-CAP-U !
TG-GROW-CAP @ USIGS-GROW-CAP !
TG-GROW-NEXT @ USIGS-GROW-NEXT !
TG-UOFF @ USIGS-USER-OFF !
TG-UEND @ USIGS-RESTORE-END
s" T-PHASE-ID" s" img -- img" TRUST
s" COK-PHASE-ID ( img -- img ) T-PHASE-ID" T-CHECK-PASSES
s" CBAD-PHASE-BORROW ( -- ) T-PHASE-ID" T-CHECK-REJECTS
s" T-ASM-CODE" s" -- asm" TRUST
s" T-BUILD-IMAGE" s" asm -- img" TRUST
s" T-CODESIG2" s" img -- img" TRUST
s" T-BUILD-SNAP-HDR" s" n -- snap n" TRUST
s" T-SNAP-EXTRA-PTR" s" -- ptr u8" TRUST
s" T-SNAP-EXTRA-SIZE" s" -- n" TRUST
s" COK-BUILD-IMAGE ( -- img ) T-ASM-CODE T-BUILD-IMAGE" T-CHECK-PASSES
s" COK-CODESIG2 ( -- img ) T-ASM-CODE T-BUILD-IMAGE T-CODESIG2" T-CHECK-PASSES
s" COK-SNAP-HDR ( n -- snap n ) T-BUILD-SNAP-HDR" T-CHECK-PASSES
s" COK-SNAP-EXTRA ( -- ptr u8 n ) T-SNAP-EXTRA-PTR T-SNAP-EXTRA-SIZE" T-CHECK-PASSES
s" COK-THROW-GUARD ( i64 -- i64 ) dup 0 < if 1 throw then 1 +" T-CHECK-PASSES
s" COK-DIE-GUARD ( i64 -- i64 ) dup 0 < if here 0 1 die then 1 +" T-CHECK-PASSES
s" T-PTX-LOAD" s" span<space-global,f32,extent-n> gridctx<block-256,extent-n,mask-live> -- tile<f32,block-256,mask-live>" TRUST
s" T-PTX-ADD" s" tile<f32,block-256,mask-live> tile<f32,block-256,mask-live> -- tile<f32,block-256,mask-live>" TRUST
s" T-PTX-GRID" s" span<space-global,f32,e> -- gridctx<block-256,e,fresh-mask-live>" TRUST
s" T-PTX-MLOAD" s" span<space-global,f32,e> gridctx<block-256,e,m> -- tile<f32,block-256,m>" TRUST
s" T-PTX-MADD" s" tile<f32,block-256,m> tile<f32,block-256,m> -- tile<f32,block-256,m>" TRUST
s" T-MK-SPAN" s" n -- span<space-global,f32,fresh-extent-n>" TRUST
s" T-MK-SPAN=" s" n -- span<space-global,f32,fresh-extent-n> span<space-global,f32,fresh-extent-n>" TRUST
s" T-PTX-SAME-EXTENT" s" span<space-global,f32,e> span<space-global,f32,e> --" TRUST
s" COK-PTX-LOAD ( span<space-global,f32,extent-n> gridctx<block-256,extent-n,mask-live> -- tile<f32,block-256,mask-live> ) T-PTX-LOAD" T-CHECK-PASSES
s" COK-PTX-ID ( span<space-global,f32,extent-n> -- span<space-global,f32,extent-n> )" T-CHECK-PASSES
s" COK-PTX-ID-CALL ( span<space-global,f32,extent-n> -- span<space-global,f32,extent-n> ) COK-PTX-ID" T-CHECK-PASSES
s" COK-PTX-RIGID-SHARED ( n -- ) T-MK-SPAN= T-PTX-SAME-EXTENT" T-CHECK-PASSES
s" CBAD-PTX-RIGID-LONE ( n n -- ) T-MK-SPAN swap T-MK-SPAN T-PTX-SAME-EXTENT" T-CHECK-REJECTS
s" COK-PTX-RET-SHARED T-MK-SPAN=" T-CHECK-PASSES
s" COK-PTX-RET-SHARED-CALL ( n -- ) COK-PTX-RET-SHARED T-PTX-SAME-EXTENT" T-CHECK-PASSES
s" COK-PTX-RET-LONE T-MK-SPAN swap T-MK-SPAN" T-CHECK-PASSES
s" CBAD-PTX-RET-LONE-CALL ( n n -- ) COK-PTX-RET-LONE T-PTX-SAME-EXTENT" T-CHECK-REJECTS
s" COK-PTX-MASK-SHARED {: s :} s T-PTX-GRID {: g :} s g T-PTX-MLOAD s g T-PTX-MLOAD T-PTX-MADD" T-CHECK-PASSES
s" CBAD-PTX-MASK-DISTINCT {: s :} s T-PTX-GRID {: g1 :} s T-PTX-GRID {: g2 :} s g1 T-PTX-MLOAD s g2 T-PTX-MLOAD T-PTX-MADD" T-CHECK-REJECTS
variable TSHOW-XT
variable TSHOW-N
: TSHOW-HOOK ( ptr u8 n n -- )
   drop
   s" x" CORE-STR= 0= if T-FAIL then
   TSHOW-N @ 1 + TSHOW-N ! ;
LOCSHOWXT @ TSHOW-XT !
' TSHOW-HOOK LOCSHOWXT !
0 TSHOW-N !
s" COK-SHOW-INFERRED ( i64 -- i64 ) {: x:? :} x" T-CHECK-PASSES
s" CBAD-SHOW-INFERRED ( i64 -- ) {: x:? :} x x" T-CHECK-REJECTS
TSHOW-N @ 2 T=
TSHOW-XT @ LOCSHOWXT !
s" T-NEED-I64" s" i64 --" TRUST
s" T-NEED-U32" s" u32 --" TRUST
s" T-NEED-U16" s" u16 --" TRUST
s" T-NEED-U8" s" u8 --" TRUST
s" T-GIVE-U16" s" -- u16" TRUST
s" T-GIVE-U8" s" -- u8" TRUST
s" T-GIVE-I64" s" -- i64" TRUST
s" COK-U8-WIDEN-IN ( u8 -- ) T-NEED-I64" T-CHECK-PASSES
s" COK-U8-WIDEN-OUT ( -- i64 ) T-GIVE-U8" T-CHECK-PASSES
s" COK-U16-WIDEN-IN ( u16 -- ) T-NEED-U32" T-CHECK-PASSES
s" COK-U16-WIDEN-OUT ( -- u32 ) T-GIVE-U16" T-CHECK-PASSES
s" CBAD-I64-NARROW-IN ( i64 -- ) T-NEED-U8" T-CHECK-REJECTS
s" CBAD-I64-NARROW-OUT ( -- u8 ) T-GIVE-I64" T-CHECK-REJECTS
s" CBAD-U32-NARROW-IN ( u32 -- ) T-NEED-U16" T-CHECK-REJECTS
DEFTYPE node
s" T->NODE" s" n -- node" TRUST
s" T-NODE>N" s" node -- n" TRUST
s" T-NEED-NODE" s" node --" TRUST
s" COK-NODE-ROLE ( n -- n ) T->NODE T-NODE>N" T-CHECK-PASSES
s" CBAD-NODE-LEN ( n -- len ) T->NODE" T-CHECK-REJECTS
s" CBAD-NODE-IDX ( n -- ) >IDX T-NEED-NODE" T-CHECK-REJECTS
s" CBAD-UNKNOWN-ROLE ( n -- track ) T->NODE" T-CHECK-REJECTS
DEFLINEAR own
s" T-MAKE-OWN" s" -- own" TRUST
s" T-FREE-OWN" s" own --" TRUST
s" COK-OWN-PASS ( own -- own )" T-CHECK-PASSES
s" COK-OWN-MAKE ( -- own ) T-MAKE-OWN" T-CHECK-PASSES
s" COK-OWN-FREE ( own -- ) T-FREE-OWN" T-CHECK-PASSES
s" CBAD-OWN-DUP ( own -- own own ) dup" T-CHECK-REJECTS
s" CBAD-OWN-DROP ( own -- ) drop" T-CHECK-REJECTS
s" CBAD-OWN-OVER ( own n -- own n own ) over" T-CHECK-REJECTS
s" CBAD-OWN-FETCH ( ptr own -- own ) @" T-CHECK-REJECTS
s" CBAD-OWN-STORE ( own ptr own -- ) !" T-CHECK-REJECTS
VALUE-RECORD point x n y n END-VALUE-RECORD
VALUE-RECORD rect w n h n END-VALUE-RECORD
VALUE-RECORD box value a END-VALUE-RECORD
VALUE-RECORD hdl owner own raw ptr u8 END-VALUE-RECORD
: T->POINT ( n n -- point ) ;
: T-POINT> ( point -- n n ) ;
: T-POINT-DUP ( point -- point point ) over over ;
: T-POINT-X ( point -- n ) drop ;
: T-POINT-Y ( point -- n ) nip ;
: T-POINT-X! ( n point -- point ) swap drop ;
: T-POINT-Y! ( point n -- point ) >r drop r> ;
: T->BOX ( a -- box ) ;
: T-BOX> ( box -- a ) ;
3 4 T->POINT T-POINT> 4 T= 3 T=
3 4 T->POINT T-POINT-DUP T-POINT> 4 T= 3 T= T-POINT> 4 T= 3 T=
3 4 T->POINT T-POINT-X 3 T=
3 4 T->POINT T-POINT-Y 4 T=
9 3 4 T->POINT T-POINT-X! T-POINT> 4 T= 9 T=
3 4 T->POINT 8 T-POINT-Y! T-POINT> 8 T= 3 T=
55 T->BOX T-BOX> 55 T=
s" COK-POINT-ID ( point -- point )" T-CHECK-PASSES
s" COK-POINT-DUP ( point -- point point ) over over" T-CHECK-PASSES
s" COK-POINT-ROUNDTRIP ( n n -- n n ) T->POINT T-POINT>" T-CHECK-PASSES
s" COK-POINT-X ( point -- n ) drop" T-CHECK-PASSES
s" COK-POINT-Y ( point -- n ) nip" T-CHECK-PASSES
s" COK-POINT-X-SET ( n point -- point ) swap drop" T-CHECK-PASSES
s" COK-POINT-Y-SET ( point n -- point ) >r drop r>" T-CHECK-PASSES
s" COK-BOX-ROUNDTRIP ( n -- n ) T->BOX T-BOX>" T-CHECK-PASSES
s" COK-HDL-PASS ( hdl -- hdl )" T-CHECK-PASSES
s" CBAD-POINT-RECT ( point -- rect )" T-CHECK-REJECTS
s" CBAD-POINT-DUP ( point -- point point ) dup" T-CHECK-REJECTS
s" CBAD-POINT-PARTIAL ( n -- point )" T-CHECK-REJECTS
s" CBAD-BOX-RECT ( box -- rect )" T-CHECK-REJECTS
s" CBAD-HDL-DUP ( hdl -- hdl hdl ) over over" T-CHECK-REJECTS
s" CBAD-DIP ( i64 i64 -- i64 ) [: 1+ ;] DIP" T-CHECK-REJECTS
s" CBAD-KEEP ( i64 -- i64 ) [: 1+ ;] KEEP" T-CHECK-REJECTS
s" CBAD-BI ( i64 -- i64 ) [: 1+ ;] [: drop ;] BI" T-CHECK-REJECTS
s" CBAD-TIMES ( i64 -- i64 i64 ) 5 [: 1+ ;] TIMES" T-CHECK-REJECTS
s" CBAD-MAP ( ptr i64 i64 -- i64 ) [: 1+ ;] MAP" T-CHECK-REJECTS
s" CBAD-QLOCAL ( i64 -- i64 ) {: x:n :} [: x ;] execute" T-CHECK-REJECTS
s" COK-CASE ( i64 -- i64 ) case 1 of 10 endof 2 of 20 endof 30 swap endcase" T-CHECK-PASSES
s" CBAD-CASE-ARM ( i64 -- i64 ) case 1 of 10 11 endof 20 swap endcase" T-CHECK-REJECTS
s" CBAD-CASE-MISSING ( i64 -- i64 ) case 1 of 10 endof" T-CHECK-REJECTS
s" CBAD-CASE-ORPHAN ( i64 -- i64 ) 1 of 2 endof" T-CHECK-REJECTS
s" CBAD-IF-MISSING ( i64 -- i64 ) dup 0 > if 1" T-CHECK-REJECTS
s" CBAD-THEN-ORPHAN ( i64 -- i64 ) 1 then" T-CHECK-REJECTS
s" CBAD-I-ORPHAN ( -- i64 ) i" T-CHECK-REJECTS
s" CBAD-LEAVE-ORPHAN ( -- ) leave" T-CHECK-REJECTS
s" CBAD-PTX-SPACE ( span<space-shared,f32,extent-n> gridctx<block-256,extent-n,mask-live> -- tile<f32,block-256,mask-live> ) T-PTX-LOAD" T-CHECK-REJECTS
s" CBAD-PTX-EXTENT ( span<space-global,f32,extent-m> gridctx<block-256,extent-n,mask-live> -- tile<f32,block-256,mask-live> ) T-PTX-LOAD" T-CHECK-REJECTS
s" CBAD-PTX-MASK ( tile<f32,block-256,mask-a> tile<f32,block-256,mask-b> -- tile<f32,block-256,mask-a> ) T-PTX-ADD" T-CHECK-REJECTS
s" CBAD-PTX-ID-SPACE ( span<space-shared,f32,extent-n> -- span<space-global,f32,extent-n> ) COK-PTX-ID" T-CHECK-REJECTS
: TROLE-REG ( n -- n ) >REG REG>N ;
7 TROLE-REG 7 T=
: TROLE-LABEL ( n -- n ) >LABEL LABEL>N ;
8 TROLE-LABEL 8 T=
: TROLE-VA ( n -- n ) >VA VA>N ;
9 TROLE-VA 9 T=
: TROLE-SYMIDX ( n -- n ) >SYMIDX SYMIDX>N ;
10 TROLE-SYMIDX 10 T=
s" CBAD-REG-LABEL ( reg label -- reg ) nip" T-CHECK-REJECTS
s" CBAD-VA-SYMIDX ( va symidx -- va ) nip" T-CHECK-REJECTS
s" CBAD-BUILD-IMAGE ( -- ) T-BUILD-IMAGE" T-CHECK-REJECTS
s" CBAD-BUILD-IMAGE-STALE ( -- img ) T-ASM-CODE T-BUILD-IMAGE T-BUILD-IMAGE" T-CHECK-REJECTS
s" CBAD-CODESIG2 ( -- ) T-CODESIG2" T-CHECK-REJECTS
s" CBAD-SNAP-HDR ( n -- n ) T-BUILD-SNAP-HDR" T-CHECK-REJECTS
s" CBAD-THROW-DUMMY ( i64 -- i64 ) dup 0 < if 1 throw 0 then 1 +" T-CHECK-REJECTS
s" CBAD-DIE-DUMMY ( i64 -- i64 ) dup 0 < if here 0 1 die 0 then 1 +" T-CHECK-REJECTS
s" CBAD-EXIT-DUMMY ( i64 -- i64 ) exit 0" T-CHECK-REJECTS
s" T-LINUX-DUP2-FD" s" reg fd reg --" TRUST
s" T-LINUX-SPAWN" s" reg reg reg reg reg reg reg --" TRUST
s" T-SPAWN-DUP2-ACTION" s" reg fd --" TRUST
s" T-SPAWN-DARWIN-FINISH" s" label label --" TRUST
s" TROLE-LINUX-DUP2 ( reg fd reg -- ) T-LINUX-DUP2-FD" T-CHECK-PASSES
s" CBAD-LINUX-DUP2-FD ( reg reg reg -- ) T-LINUX-DUP2-FD" T-CHECK-REJECTS
s" CBAD-LINUX-SPAWN ( reg reg reg fd reg reg reg -- ) T-LINUX-SPAWN" T-CHECK-REJECTS
s" TROLE-DARWIN-DUP2 ( reg fd -- ) T-SPAWN-DUP2-ACTION" T-CHECK-PASSES
s" TROLE-DARWIN-FINISH ( label label -- ) T-SPAWN-DARWIN-FINISH" T-CHECK-PASSES
s" CBAD-DARWIN-DUP2 ( reg reg -- ) T-SPAWN-DUP2-ACTION" T-CHECK-REJECTS
s" CBAD-DARWIN-FINISH ( reg label -- ) T-SPAWN-DARWIN-FINISH" T-CHECK-REJECTS
: ES-BYTE-FIELD ( ptr n -- ptr ptr u8 ) 0 ptr-field ;
s" CBAD-FIELD ( ptr n n -- ) swap ES-BYTE-FIELD !" T-CHECK-REJECTS
s" CBAD-LOCAL-SCOPE ( i64 bool -- i64 ) if {: drop:i64 :} drop else drop 0 then drop" T-CHECK-REJECTS
s" CBAD-LOCAL-DEAD ( i64 -- i64 ) exit {: x:i64 :} x" T-CHECK-REJECTS

\ immediate / postpone / compile,
: IM5 ( -- n ) 5 ; immediate
: ES-TI ( -- n ) IM5 ;
ES-TI 5 T=
\ POSTPONE is compiler-manipulating; this fixture tests the runtime primitive,
\ not checked user code. TP must compile through P5 while the trusted immediate
\ boundary is active.
TRUSTED: P5 ( -- i64 ) postpone IM5 ; immediate
: TP ( -- n ) P5 ;
TP 5 T=

\ child processes: run-rc spawns + waits (paths need a NUL)
create ES-PZB 64 allot
: ES-PATHZ ( ptr u8 n -- ptr u8 ) {: a:ptr u:n :}
   0 begin dup u < while  dup a + c@  over ES-PZB + c!  1 + repeat drop
   0 ES-PZB u + c!  ES-PZB ;
s" /usr/bin/true" ES-PATHZ run-rc 0 T=
s" /usr/bin/false" ES-PATHZ run-rc 1 T=

\ filesystem syscalls
create ES-STB 256 allot
create DBUF 4096 allot
create RLB 64 allot
create ES-TARGETZ
   65 c, 71 c, 69 c, 78 c, 84 c, 83 c, 46 c, 109 c, 100 c, 0 c,
create ES-LINKZ
   47 c, 116 c, 109 c, 112 c, 47 c, 104 c, 97 c, 98 c, 117 c, 45 c,
   101 c, 110 c, 103 c, 105 c, 110 c, 101 c, 45 c, 115 c, 117 c,
   105 c, 116 c, 101 c, 45 c, 108 c, 105 c, 110 c, 107 c, 0 c,
create DIRBASE 8 allot
variable DFD
: U16@ ( ptr u8 -- n ) {: a:ptr :} a c@ a 1 + c@ 8 lshift or ;
: MODE@ ( -- n ) ES-STB 4 + U16@ ;
s" AGENTS.md" ES-PATHZ 0 access 0 T=
s" /nonexistent-habu-fs" ES-PATHZ 0 access -1 T=
s" AGENTS.md" ES-PATHZ ES-STB stat64 0 T=
MODE@ $F000 and $8000 = -1 T=
s" src" ES-PATHZ ES-STB stat64 0 T=
MODE@ $F000 and $4000 = -1 T=
s" src/os/macos" ES-PATHZ open-rd DFD !
DFD @ 0 >= -1 T=
0 DIRBASE !
DFD @ DBUF 4096 DIRBASE getdirentries64 0 > -1 T=
DFD @ close
s" /tmp/habu-engine-suite-mkdir" ES-PATHZ rmdir drop
s" /tmp/habu-engine-suite-mkdir" ES-PATHZ 493 mkdir 0 T=
s" /tmp/habu-engine-suite-mkdir" ES-PATHZ ES-STB stat64 0 T=
MODE@ $F000 and $4000 = -1 T=
s" /tmp/habu-engine-suite-mkdir" ES-PATHZ rmdir 0 T=
ES-LINKZ unlink drop
ES-TARGETZ ES-LINKZ symlink 0 T=
ES-LINKZ ES-STB lstat64 0 T=
MODE@ $F000 and $A000 = -1 T=
ES-LINKZ RLB 64 readlink 9 T=
RLB c@ 65 T=
ES-LINKZ unlink 0 T=
0 4096 3 $1002 -1 0 mmap dup 0 < 0 T= dup 65 swap c! c@ 65 T=

\ floats (the f+ prim must be the FLOAT op — it was once shadowed by a
\ jit fold helper named f+)
: TFP ( -- bool ) 1.5 2.5 f+ 4.0 f= ;
TFP -1 T=
: TFL ( -- bool ) 1.5 2.5 f< ;
TFL -1 T=
: TFNEG ( -- bool ) -1.5 -2.5 f+ -4.0 f= ;
TFNEG -1 T=

\ exit inside a quotation targets the QUOTATION's epilogue (scoped chain)
: TQX ( n -- n ) [: dup 0 > if exit then drop 99 ;] execute ;
5 TQX 5 T=
0 TQX 99 T=

\ empty interpret string
s" " nip 0 T=

\ run-rc spawn failure -> -1 (not a hang or garbage status)
s" /nonexistent-habu-x" ES-PATHZ run-rc -1 T=

\ snapshot-writer intrinsics are sane
$340000000 constant ES-LINUX-DATA-VA
$44000000000 constant ES-MACOS-DATA-VA
: ES-TARGET-UNKNOWN ( -- )
   s" engine-suite: unknown target" 76 die ;

: ES-DATA-VA ( -- n )
   HB-TARGET-LINUX? if ES-LINUX-DATA-VA exit then
   HB-TARGET-MACOS? if ES-MACOS-DATA-VA exit then
   ES-TARGET-UNKNOWN ;
dbase@ $300000000 = -1 T=
data-base ES-DATA-VA = -1 T=
cp@ dbase@ - 0 > -1 T=
ndict@ 0 > -1 T=

\ time primitives: deterministic shape/range only, never exact wall time
epoch-seconds 1600000000 > -1 T=
: TEPOCH ( -- bool ) epoch-seconds 1600000000 > ;
TEPOCH -1 T=
: TEPOCH-DEPTH ( -- bool ) depth >r epoch-seconds drop depth r> = ;
TEPOCH-DEPTH -1 T=
create TEPOCH-BYTE 120 c,
: TEPOCH-AFTER-WRITEERR ( -- bool )
   99 TEPOCH-BYTE 1 write drop epoch-seconds 1600000000 > ;
TEPOCH-AFTER-WRITEERR -1 T=
mono-ns mono-ns <= -1 T=
: TMONO-ELAPSED ( -- n ) mono-ns 0 100000 0 do i + loop drop mono-ns swap - ;
TMONO-ELAPSED 0 > -1 T=

\ register pool stress: 14 live VS values exceed the 13-reg pool (x9..x15,
\ x29, x25, x23, x24, x21, x22) mid-expression -> the 14th allocation takes
\ the spill path; sum proves no value was lost or aliased (1+..+14 = 105)
: TRP ( -- n )
   1 2 3 4 5 6 7 8 9 10 11 12 13 14  + + + + + + + + + + + + + ;
TRP 105 T=

\ loop-resident registers: 12 loop-carried values + the counter (13 = the
\ full pool) survive a BEGIN/UNTIL back edge via the two-cell packed snapshot
: TLR ( -- n )
   1 2 3 4 5 6 7 8 9 10 11 12  0 begin 1 + dup 3 = until drop
   + + + + + + + + + + + ;
TLR 78 T=

\ locals register cache: repeat refs reuse the cached reg (one ldr total)...
: KL ( n -- n ) {: a:n :} a a + a + ;
5 KL 15 T=
\ ...a call spills and invalidates (the ref after must reload from the frame)
: KC ( n -- n ) {: a:n :} P5 drop a ;
3 KC 3 T=
\ ...and the cache claim survives a BEGIN back edge (loop-resident local)
: KR ( n -- n ) {: a:n :} 0 begin a + dup 15 < 0= until ;
5 KR 15 T=
: KW ( n -- n ) {: a:n :} 0 begin dup 12 < while a + repeat ;
4 KW 12 T=

\ Public parser primitive: runtime and immediate paths both use the native
\ tokenizer and advance the caller's input cursor.
: TPN1 ( -- ) parse-name 5 T= c@ 97 T= ;
TPN1 alpha
: TPNI ( -- ) parse-name 4 T= c@ 98 T= ; immediate
: TPN2 ( -- n ) TPNI beta 7 ;
TPN2 7 T=

\ evaluate frame storage must not overlap the JIT virtual value stack.
: TEVAL-FRAME-END ( -- n )
   EVAL-FRAME EVAL-MAX-DEPTH EVAL-FRAME-SIZE * + ;
EVAL-FRAME VVAL-OFF VSMAX cells + >= -1 T=
TEVAL-FRAME-END DATA-START <= -1 T=
s" : TEVAL-HI-CALLEE ( n n n n n n n -- ) {: a:n b:n c:n d:n e:n f:n g:n :} a drop b drop c drop d drop e drop f drop g drop ; : TEVAL-HI-CALL ( -- ) 1 2 3 4 5 6 7 TEVAL-HI-CALLEE ;" evaluate
: TEVAL-PARSE ( -- )
   parse-name 5 T= c@ 103 T= ;
TEVAL-PARSE gamma

\ float VS: d-reg binops (FADD path), dup of a float constant, and a
\ loop-resident float accumulator surviving BEGIN back edges in a d-reg
: TFD ( -- bool ) 2.0 dup f+ 4.0 f= ;
TFD -1 T=
: ES-TFA ( n -- bool ) {: n:n :} 0.0 0 begin 1 + swap 1.5 f+ swap dup n = until drop 6.0 f= ;
4 ES-TFA -1 T=
\ a call spills the float (bits to the memory stack); the prim path finishes
: TF5 ( -- n ) 5 ;
: TFC ( -- bool ) 0.5 TF5 drop 0.5 f+ 1.0 f= ;
TFC -1 T=

\ float pool: deep expression spills past d8..d15 (10 live floats), the other
\ binops, and a float carried through a quotation
: TFS ( -- bool )
   1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0
   f+ f+ f+ f+ f+ f+ f+ f+ f+ 55.0 f= ;
TFS -1 T=
: TFM ( -- bool ) 3.0 4.0 f* 12.0 f= ;
TFM -1 T=
: TFV ( -- bool ) 10.0 4.0 f/ 2.5 f= ;
TFV -1 T=
: TFQ ( -- bool ) 2.0 [: 3.0 f+ ;] execute 5.0 f= ;
TFQ -1 T=
: TFG ( -- bool ) 5.0 3.0 f> ;
TFG -1 T=

\ FFI: AAPCS64 trampoline runtime proof. Inside a compiled word (so cp@ is the
\ stable free code slot, not a transient top-level line buffer) emit a C-ABI
\ leaf `add x0,x0,x1; ret` at cp@ via patch32, then call it through ffi-call
\ with an 8-cell arg buffer [3,4,..]. Proves x0..x7 marshalling, blr, the x0
\ return, and that XDS (x19) survives the C call. Trusted boundary (raw code +
\ foreign call).
create FFI-ARGS 8 cells allot
: TFFI ( -- n )
   3 FFI-ARGS !  4 FFI-ARGS 8 + !
   cp@ {: fn:n :}
   $8B010000 fn patch32            \ add x0, x0, x1   at fn
   $D65F03C0 fn 4 + patch32        \ ret             at fn+4
   FFI-ARGS fn ffi-call ;
TFFI 7 T=

\ report: count + nonzero exit on failure
: REPORT ( -- )
   #FAIL @ 0 = if [char] o emit [char] k emit cr exit then
   #FAIL @ . s" engine-suite: failures" 1 die ;
REPORT
