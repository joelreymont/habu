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
\ later-wins redefinition through the audited TRUST override path: the second
\ row must replace the first for all later callers (in-place index update).
s" T-RDF" s" n -- n" TRUST
s" caller of first TRUST effect certifies" T-LABEL
s" COK-RDF-V1 ( n -- n ) T-RDF" CHECK-QUIET-CANDIDATE! -1 T=
s" T-RDF" s" -- n" TRUST
s" caller of overriding TRUST effect certifies" T-LABEL
s" COK-RDF-V2 ( -- n ) T-RDF" CHECK-QUIET-CANDIDATE! -1 T=
s" caller of stale first effect rejects" T-LABEL
s" CBAD-RDF-V1 ( n -- n ) T-RDF" CHECK-QUIET-CANDIDATE! 0 T=
\ candidate rollback restores the pre-candidate effect: a candidate may shadow
\ T-RDF, but after its scope ends the shadow must be gone for later checks.
s" T-SCV" s" n -- n" TRUST
s" candidate shadow of TRUSTed word certifies" T-LABEL
s" T-SCV ( -- n ) 5" CHECK-QUIET-CANDIDATE! -1 T=
s" pre-candidate effect restored after scope" T-LABEL
s" COK-SCV-BACK ( n -- n ) T-SCV" CHECK-QUIET-CANDIDATE! -1 T=
s" candidate shadow effect does not leak" T-LABEL
s" CBAD-SCV-LEAK ( -- n ) T-SCV" CHECK-QUIET-CANDIDATE! 0 T=
\ control-flag rollback: a candidate that turns T-CTV into a no-return thrower
\ records CTL flags inside its scope only; after the scope the caller's code
\ after T-CTV is live again.
s" T-CTV" s" --" TRUST
s" candidate no-return redefinition certifies" T-LABEL
s" T-CTV ( -- ) 1 throw" CHECK-QUIET-CANDIDATE! -1 T=
s" ctl flags rolled back with the scope" T-LABEL
s" COK-CTV-LIVE ( -- n ) T-CTV 5" CHECK-QUIET-CANDIDATE! -1 T=
\ checker scope: a name interned inside the scope stops resolving after it.
CHECKER-SCOPE-START
s" T-SCOPED-W" s" -- n" TRUST
s" scoped TRUST resolves inside the scope" T-LABEL
s" COK-SCOPED-IN ( -- n ) T-SCOPED-W" CHECK-QUIET-CANDIDATE! -1 T=
CHECKER-SCOPE-DONE
s" scoped TRUST retired with the scope" T-LABEL
s" CUNK-SCOPED-OUT ( -- n ) T-SCOPED-W" CHECK-QUIET-CANDIDATE! 1 T=
\ package resolution order: private wins over public and global inside the
\ open package; the private row never leaks as qualified or global outside.
s" T-PRESO" s" -- n" TRUST
package ES-PRES
: T-PRESO ( -- n n ) 1 2 ;
public
: ES-PRES-PUB ( -- n ) 7 ;
private
s" private shadows global inside the package" T-LABEL
s" COK-PRES-PRIV ( -- n n ) T-PRESO" CHECK-QUIET-CANDIDATE! -1 T=
s" global effect hidden inside the package" T-LABEL
s" CBAD-PRES-GLOB ( -- n ) T-PRESO" CHECK-QUIET-CANDIDATE! 0 T=
s" public resolves unqualified inside the package" T-LABEL
s" COK-PRES-PUB-IN ( -- n ) ES-PRES-PUB" CHECK-QUIET-CANDIDATE! -1 T=
end-package
s" global effect restored outside the package" T-LABEL
s" COK-PRES-GLOB ( -- n ) T-PRESO" CHECK-QUIET-CANDIDATE! -1 T=
s" private row does not resolve qualified" T-LABEL
s" CUNK-PRES-QUAL-PRIV ( -- n n ) ES-PRES:T-PRESO" CHECK-QUIET-CANDIDATE! 1 T=
s" public resolves qualified outside the package" T-LABEL
s" COK-PRES-QUAL ( -- n ) ES-PRES:ES-PRES-PUB" CHECK-QUIET-CANDIDATE! -1 T=
s" public does not resolve bare outside the package" T-LABEL
s" CUNK-PRES-BARE ( -- n ) ES-PRES-PUB" CHECK-QUIET-CANDIDATE! 1 T=
\ mid-body ( ... ) comments are comments (EM-COMMENT parity), never a second
\ signature: they must not clobber declared-sig state or flip the verdict.
s" CBAD-SIG-TRAIL ( n -- n ) dup dup ( n -- n )" T-CHECK-REJECTS
s" CBAD-SIG-MID ( n -- n ) dup ( n -- n ) dup" T-CHECK-REJECTS
s" COK-SIG-MID-COMMENT ( n -- n ) dup ( scratch note ) drop" T-CHECK-PASSES
s" sigless-mid-comment certifies" T-LABEL
s" COK-SIGLESS-MID-COMMENT dup ( n -- n ) drop" CHECK-QUIET-CANDIDATE! -1 T=
\ escaped-string payloads: the checker accepts exactly the engine's escape set
\ (C-ESC-DECODE-BASIC) and rejects what the load path rejects.
s\" COK-ESC-GOOD ( -- ptr u8 n ) s\\\" a\\n b\\\\ c\\\" d\\x41 e\\XaF f\\q g\\z\" " T-CHECK-PASSES
s\" CBAD-ESC-LETTER ( -- ptr u8 n ) s\\\" bad\\g\" " T-CHECK-REJECTS
s\" CBAD-ESC-HEX ( -- ptr u8 n ) s\\\" bad\\xZZ\" " T-CHECK-REJECTS
s\" CBAD-ESC-HEX-SHORT ( -- ptr u8 n ) s\\\" bad\\x4\" " T-CHECK-REJECTS
s\" CBAD-ESC-UNTERMINATED ( -- ptr u8 n ) s\\\" no end" T-CHECK-REJECTS
\ pointer arithmetic + byte view: pointee-polymorphic ptr+n arithmetic, ptr-ptr
\ difference, and an explicit ( ptr a -- ptr u8 ) byte view all certify. A
\ pointer's pointee element type is invariant: the u8->cell/u32 integer widening
\ that applies to top-level scalar cells must NOT apply inside a ptr, so a
\ concrete ptr u8 never satisfies ptr cell/ptr u32 and cross-pointee unification
\ is rejected. ptr+ptr and cell @/! on a byte span stay rejected.
s" COK-PTR-ADD ( ptr a n -- ptr a ) +" T-CHECK-PASSES
s" COK-PTR-ADD-REV ( n ptr a -- ptr a ) +" T-CHECK-PASSES
s" COK-PTR-SUB ( ptr a n -- ptr a ) -" T-CHECK-PASSES
s" COK-PTR-DIFF ( ptr a ptr a -- n ) -" T-CHECK-PASSES
s" COK-PTR-CELLPLUS ( ptr a -- ptr a ) cell+" T-CHECK-PASSES
s" COK-PTR-CHARPLUS ( ptr a -- ptr a ) char+" T-CHECK-PASSES
s" COK-PTR-BYTEADD ( ptr u8 n -- ptr u8 ) +" T-CHECK-PASSES
s" COK-PTR-VIEW ( ptr a -- ptr u8 )" T-CHECK-PASSES
s" COK-PTR-SAME-EQ ( ptr u8 ptr u8 -- bool ) =" T-CHECK-PASSES
s" COK-PTR-SCALAR-WIDEN ( u8 -- cell )" T-CHECK-PASSES
s" CBAD-PTR-ADD-PP ( ptr a ptr a -- ptr a ) +" T-CHECK-REJECTS
s" CBAD-PTR-WIDEN-CELL ( ptr u8 -- ptr cell )" T-CHECK-REJECTS
s" CBAD-PTR-WIDEN-U32 ( ptr u8 -- ptr u32 )" T-CHECK-REJECTS
s" CBAD-PTR-WIDEN-NEST ( ptr ptr u8 -- ptr ptr cell )" T-CHECK-REJECTS
s" CBAD-PTR-UNIFY-EQ ( ptr u8 ptr cell -- bool ) =" T-CHECK-REJECTS
s" CBAD-PTR-UNIFY-EQ-REV ( ptr cell ptr u8 -- bool ) =" T-CHECK-REJECTS
s" CBAD-PTR-CELL-ON-BYTE ( ptr u8 -- n ) @" T-CHECK-REJECTS
\ Cell store `!` on a byte span is rejected exactly like cell load `@`. This is
\ the miss class the fixpoint certify caught in checker.f USIGS-CLEAR (dot
\ habu-fix-0-usigs): a head accessor declared ( -- ptr u8 ) whose caller stores a
\ cell with `0 WORD !` must reject, because `!` requires a ptr a target.
s" CBAD-PTR-CELL-STORE-ON-BYTE ( ptr u8 -- ) 0 swap !" T-CHECK-REJECTS
s" COK-PTR-CELL-STORE ( ptr a -- ) 0 swap !" T-CHECK-PASSES
variable ESB-BYTE-P
: ESB-BYTE-HEAD ( -- ptr u8 ) ESB-BYTE-P @ ;
s" CBAD-USIGS-BYTE-STORE ( -- ) 0 ESB-BYTE-HEAD !" T-CHECK-REJECTS
\ REC-SIG refusal is certified-but-unrecorded and must say which word and why.
512 constant RSD-CAP
create RSD-BUF RSD-CAP allot
: T-HAS? ( ptr u8 n ptr u8 n -- n ) {: ha:ptr hu:n na:ptr nu:n :}
   hu nu < IF 0 EXIT THEN
   0 BEGIN dup hu nu - <= WHILE
      ha over + nu na nu CORE-STR= IF drop -1 EXIT THEN
      1 +
   REPEAT drop 0 ;
\ Quotation effects render recursively to QDEPTH-MAX levels: a triple-nested
\ quot now renders fully and RECORDS (no '?' -> RQM stays 0), while a quot nested
\ past the budget still caps at '?' and refuses recording via the unmodeled tag.
RSD-BUF RSD-CAP DIAG-BUFFER!
s" triple-nested quot certifies" T-LABEL
s" RSQ3 [: [: [: ;] ;] ;]" CHECK-QUIET-CANDIDATE! -1 T=
s" triple-nested quot renders fully and records (no unmodeled-tag refusal)" T-LABEL
DIAG-BUFFER$ s" unmodeled type tag" T-HAS? 0 T=
RSD-BUF RSD-CAP DIAG-BUFFER!
s" quot nested past the render budget still certifies" T-LABEL
s" RSQ7 [: [: [: [: [: [: [: ;] ;] ;] ;] ;] ;] ;]" CHECK-QUIET-CANDIDATE! -1 T=
s" over-budget quot refuses recording and names the word" T-LABEL
DIAG-BUFFER$ s" rsq7" T-HAS? -1 T=
s" over-budget quot refusal names the unmodeled-tag reason" T-LABEL
DIAG-BUFFER$ s" unmodeled type tag" T-HAS? -1 T=
s" T-V14" s" -- a b c d e g h i j k l m o p" TRUST
RSD-BUF RSD-CAP DIAG-BUFFER!
s" rec-refuse var overflow still certifies" T-LABEL
s" RSV28 T-V14 T-V14" CHECK-QUIET-CANDIDATE! -1 T=
s" rec-refuse diag names var-count word" T-LABEL
DIAG-BUFFER$ s" rsv28" T-HAS? -1 T=
s" rec-refuse diag names var-count reason" T-LABEL
DIAG-BUFFER$ s" more than 26 type variables" T-HAS? -1 T=
DIAG-BUFFER-OFF
\ recurse checks against the cached declared sig (fresh instance per site)
s" recurse against declared sig certifies" T-LABEL
s" CREC-OK ( n -- n ) dup 2 < if drop 1 exit then 1 - recurse 1 +" CHECK-QUIET-CANDIDATE! -1 T=
s" wrong-effect recursion rejects" T-LABEL
s" CREC-BAD ( n -- n ) dup recurse" CHECK-QUIET-CANDIDATE! 0 T=
s" sig-less recursion stays uncheckable" T-LABEL
s" CREC-SIGLESS dup recurse" CHECK-QUIET-CANDIDATE! 1 T=
\ engine FIND parity for colon tokens: a non-edge first colon plus a second
\ colon never resolves (FIND-QBAD); edge colons stay ordinary names.
s" a:b:c" s" -- n" TRUST
s" a:b:" s" -- n" TRUST
s" x:" s" -- n" TRUST
s" ::x" s" -- n" TRUST
s" tq:tail" s" -- n" TRUST
s" double-colon token rejects" T-LABEL
s" CBAD-QUAL-DOUBLE ( -- n ) a:b:c" CHECK-QUIET-CANDIDATE! 1 T=
s" trailing-second-colon token rejects" T-LABEL
s" CBAD-QUAL-TRAIL ( -- n ) a:b:" CHECK-QUIET-CANDIDATE! 1 T=
s" edge-colon names stay ordinary" T-LABEL
s" COK-QUAL-EDGE ( -- n n ) x: ::x" CHECK-QUIET-CANDIDATE! -1 T=
s" single-colon qualified resolves" T-LABEL
s" COK-QUAL-ONE ( -- n ) tq:tail" CHECK-QUIET-CANDIDATE! -1 T=
RSD-BUF RSD-CAP DIAG-BUFFER!
-1 JSON-DIAGS !
s" qualified diag verdict" T-LABEL
s" CBAD-QUAL-DIAG ( -- n ) a:b:c" CHECK-CANDIDATE! 1 T=
0 JSON-DIAGS !
s" qualified diag code" T-LABEL
DIAG-BUFFER$ s" E-BAD-QUALIFIED" T-HAS? -1 T=
s" qualified diag token" T-LABEL
DIAG-BUFFER$ s" a:b:c" T-HAS? -1 T=
DIAG-BUFFER-OFF
variable TG-UEND
variable TG-CAP
variable TG-USIGS-P
variable TG-GROW-CAP
variable TG-GROW-NEXT
variable TG-UOFF
variable TG-SMALL-CAP
UEND @ TG-UEND !
USIGS-CAP-U @ TG-CAP !
USIGS-P @ TG-USIGS-P !
USIGS-GROW-CAP @ TG-GROW-CAP !
USIGS-GROW-NEXT @ TG-GROW-NEXT !
USIGS-USER-OFF @ TG-UOFF !
\ normalize first: a restored snapshot boots with a persisted (smaller)
\ store; one reset guarantees the runtime-sized arena the asserts assume
USIGS-RESET
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
USIGS-CAP-U @ TG-SMALL-CAP !
s" T-GROW-PAIR" s" ptr u8 n ptr u8 n -- ptr u8 n" TRUST
s" COK-GROW-PAIR ( ptr u8 n ptr u8 n -- ptr u8 n ) T-GROW-PAIR" T-CHECK-PASSES
\ growth is geometric: one forced grow at least doubles the old cap
s" usigs-grow-geometric" T-LABEL
USIGS-CAP-U @ TG-SMALL-CAP @ 2 * >= -1 T=
TG-USIGS-P @ USIGS-P !
TG-CAP @ USIGS-CAP-U !
TG-GROW-CAP @ USIGS-GROW-CAP !
TG-GROW-NEXT @ USIGS-GROW-NEXT !
TG-UOFF @ USIGS-USER-OFF !
TG-UEND @ USIGS-RESTORE-END
\ snapshot cap policy: smallest power-of-2 grain multiple >= size
s" pow2-cap floor" T-LABEL
1 USIGS-POW2-CAP USIGS-GRAIN T=
s" pow2-cap exact grain" T-LABEL
USIGS-GRAIN USIGS-POW2-CAP USIGS-GRAIN T=
s" pow2-cap rounds up" T-LABEL
USIGS-GRAIN 1 + USIGS-POW2-CAP USIGS-GRAIN 2 * T=
s" pow2-cap next power" T-LABEL
USIGS-GRAIN 3 * USIGS-POW2-CAP USIGS-GRAIN 4 * T=
\ cell-wise USIGS-COPY preserves odd-length byte spans (body + tail)
create TG-CPY-SRC
   $11 c, $22 c, $33 c, $44 c, $55 c, $66 c, $77 c, $88 c,
   $99 c, $AA c, $BB c,
create TG-CPY-DST 11 allot
TG-CPY-SRC TG-CPY-DST 11 USIGS-COPY
s" usigs-copy bytes" T-LABEL
TG-CPY-DST 11 TG-CPY-SRC 11 T$=
\ --- growable typevar arena: a body needing > MAXTV-INIT vars no longer dies
\ and a mid-check grow must not corrupt the var-id maps (relocatable arena).
variable TG-TV-CAP
TV-CAP @ TG-TV-CAP !
8 TV-CAP !                       \ shrink shared cap so a small body forces a grow
s" COK-TVGROW ( a -- a ) dup drop dup drop dup drop dup drop dup drop" T-CHECK-PASSES
s" tv-arena-grow-geometric" T-LABEL
TV-CAP @ 16 >= -1 T=             \ one forced grow at least doubles the shrunk cap
TV-SNAP-RESET                    \ repoint every var-id store back at its boot buffer
s" tv-arena-restored-cap" T-LABEL
TV-CAP @ MAXTV-INIT T=
s" tv-arena-restored-boot" T-LABEL
TVT TVT-BOOT = -1 T=
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
\ --- growable decoupled scratch arenas: crossing each init cap mid-check no
\ longer dies and does not corrupt (each grow relocates its mmap store). Reuses
\ T-MK-SPAN above so no new TRUST site is introduced.
2 SPA-CAP !                      \ push records
s" COK-SPAG ( -- ) 0 0 0 0 0 0 0 0 drop drop drop drop drop drop drop drop" T-CHECK-PASSES
s" spa-arena-grow" T-LABEL
SPA-CAP @ 2 > -1 T=
2 PTR-CAP !                      \ ptr terms
s" COK-PTRG ( ptr a ptr a ptr a ptr a -- ) drop drop drop drop" T-CHECK-PASSES
s" ptr-arena-grow" T-LABEL
PTR-CAP @ 2 > -1 T=
2 QE-CAP !                       \ quotation effects
s" COK-QEG ( -- ) [: ;] drop [: ;] drop [: ;] drop [: ;] drop" T-CHECK-PASSES
s" qe-arena-grow" T-LABEL
QE-CAP @ 2 > -1 T=
2 PARAM-CAP !                    \ parametric terms
2 ATOM-CAP !                     \ atom terms
s" COK-PARMG ( n n n -- ) T-MK-SPAN drop T-MK-SPAN drop T-MK-SPAN drop" T-CHECK-PASSES
s" param-arena-grow" T-LABEL
PARAM-CAP @ 2 > -1 T=
s" atom-arena-grow" T-LABEL
ATOM-CAP @ 2 > -1 T=
DECOUPLED-ARENA-SNAP-RESET       \ repoint every scratch store back at its boot buffer
s" decoupled-arena-restored" T-LABEL
SPA-CAP @ MAXPUSH-INIT =  PTR-CAP @ MAXPTR-INIT =  and
QE-CAP @ MAXQE-INIT =  and  ATOM-CAP @ MAXATOM-INIT =  and
PARAM-CAP @ MAXPARAM-INIT =  and  -1 T=
\ --- growable registries (CT / VREC / SYMS): crossing each init cap mid-run no
\ longer dies, a relocating grow preserves lookups (SYMS rehashes its HIDX index),
\ and a grown store persists into fresh image DATA with its string pointers
\ rebased. Each test lowers the live cap to force a grow, then restores the store
\ to its baked boot buffer (record arrays grow to mmap first, so string-pool
\ rebases never touch the pristine boot buffer).
variable TR-HERE
\ ---- CT registry ----
variable TR-CTN  variable TR-CTU  variable TR-CTC  variable TR-CTSC
variable TR-CT-NAP variable TR-CT-NUP variable TR-CT-CLP variable TR-CT-WDP
variable TR-CT-SGP variable TR-CT-STP variable TR-CTCODE variable TR-CT-LC
variable TR-CT-STRLC
CTN @ TR-CTN !  CT-STR-U @ TR-CTU !  CT-CAP-V @ TR-CTC !  CT-STR-CAP-V @ TR-CTSC !
CT-NAME-A-P @ TR-CT-NAP !  CT-NAME-U-P @ TR-CT-NUP !  CT-CLASS-P @ TR-CT-CLP !
CT-WIDTH-P @ TR-CT-WDP !  CT-SIGN-P @ TR-CT-SGP !  CT-STR-P @ TR-CT-STP !
CTN @ TR-CTCODE !
s" CTGROWPROBE" CTN @ CT-ROLE 64 CS-NONE CT-SET
CTN @ dup CT-CAP-V !  TR-CT-LC !         \ next CT-SET crosses the record cap
s" CTGROWTWO" CTN @ CT-ROLE 64 CS-NONE CT-SET
s" ct-record-grow" T-LABEL
CT-CAP-V @ TR-CT-LC @ > -1 T=
s" ct-record-grow-find" T-LABEL
s" CTGROWPROBE" CT-FIND TR-CTCODE @ T=
CT-STR-U @ dup CT-STR-CAP-V !  TR-CT-STRLC !   \ next name copy crosses the string cap
s" CTGROWTHREE" CTN @ CT-ROLE 64 CS-NONE CT-SET
s" ct-str-grow" T-LABEL
CT-STR-CAP-V @ TR-CT-STRLC @ > -1 T=
s" ct-str-grow-rebase-find" T-LABEL
s" CTGROWPROBE" CT-FIND TR-CTCODE @ T=
here TR-HERE !
CT-SNAPSHOT-PERSIST
s" ct-persist-moved" T-LABEL
CT-NAME-A-P @ TR-HERE @ >= -1 T=
s" ct-persist-find" T-LABEL
s" CTGROWPROBE" CT-FIND TR-CTCODE @ T=
TR-CTN @ CTN !  TR-CTU @ CT-STR-U !  TR-CTC @ CT-CAP-V !  TR-CTSC @ CT-STR-CAP-V !
TR-CT-NAP @ CT-NAME-A-P !  TR-CT-NUP @ CT-NAME-U-P !  TR-CT-CLP @ CT-CLASS-P !
TR-CT-WDP @ CT-WIDTH-P !  TR-CT-SGP @ CT-SIGN-P !  TR-CT-STP @ CT-STR-P !
\ ---- VREC registry: records, nodes, fields, string pool ----
\ Grow the record AND node arrays to mmap BEFORE the string pool, so the string
\ rebase (which touches record names and VR-ATOM/VR-PARAM node VN.A cells) never
\ mutates the pristine boot buffers.
variable TR-VN  variable TR-VU  variable TR-VC  variable TR-VSC  variable TR-VRID
variable TR-V-LC  variable TR-V-STRLC
variable TR-VNODEN variable TR-VNODEC variable TR-V-NODE-LC
variable TR-VFN variable TR-VFC variable TR-V-FIELD-LC
VREC-N @ TR-VN !  VREC-STR-U @ TR-VU !  VREC-CAP-V @ TR-VC !  VREC-STR-CAP-V @ TR-VSC !
VREC-NODE-N @ TR-VNODEN !  VREC-NODE-CAP-V @ TR-VNODEC !
VREC-FIELD-N @ TR-VFN !  VREC-FIELD-CAP-V @ TR-VFC !
VREC-N @ TR-VRID !
s" VRGROWPROBE" VREC-BEGIN drop
VREC-N @ dup VREC-CAP-V !  TR-V-LC !     \ next VREC-BEGIN crosses the record cap
s" VRGROWTWO" VREC-BEGIN drop
s" vrec-record-grow" T-LABEL
VREC-CAP-V @ TR-V-LC @ > -1 T=
s" vrec-record-grow-find" T-LABEL
s" VRGROWPROBE" VREC-FIND -1 T= TR-VRID @ T=
VREC-NODE-N @ dup VREC-NODE-CAP-V !  TR-V-NODE-LC !   \ next node crosses the node cap
VR-CON VREC-NODE-NEW drop
s" vrec-node-grow" T-LABEL
VREC-NODE-CAP-V @ TR-V-NODE-LC @ > -1 T=
VREC-FIELD-N @ dup VREC-FIELD-CAP-V !  TR-V-FIELD-LC !   \ next field crosses the field cap
0 VREC-FIELD!
s" vrec-field-grow" T-LABEL
VREC-FIELD-CAP-V @ TR-V-FIELD-LC @ > -1 T=
VREC-STR-U @ dup VREC-STR-CAP-V !  TR-V-STRLC !   \ next name crosses the string cap
s" VRGROWTHREE" VREC-BEGIN drop
s" vrec-str-grow" T-LABEL
VREC-STR-CAP-V @ TR-V-STRLC @ > -1 T=
s" vrec-str-grow-rebase-find" T-LABEL
s" VRGROWPROBE" VREC-FIND -1 T= TR-VRID @ T=
s" vrec-str-grow-name-intact" T-LABEL
TR-VRID @ VREC-NAME$ s" VRGROWPROBE" T$=
here TR-HERE !
VREC-SNAPSHOT-PERSIST
s" vrec-persist-moved" T-LABEL
VREC-NAME-A-P @ TR-HERE @ >= -1 T=
s" vrec-persist-find" T-LABEL
s" VRGROWPROBE" VREC-FIND -1 T= TR-VRID @ T=
s" vrec-persist-name-intact" T-LABEL
TR-VRID @ VREC-NAME$ s" VRGROWPROBE" T$=
VREC-ARENA-BOOT                          \ every VREC store P back to its boot buffer
TR-VN @ VREC-N !  TR-VU @ VREC-STR-U !  TR-VC @ VREC-CAP-V !  TR-VSC @ VREC-STR-CAP-V !
TR-VNODEN @ VREC-NODE-N !  TR-VNODEC @ VREC-NODE-CAP-V !
TR-VFN @ VREC-FIELD-N !  TR-VFC @ VREC-FIELD-CAP-V !
\ ---- SYMS registry: record array grow rehashes HIDX; string pool grow rebases ----
variable TR-SC  variable TR-SSC  variable TR-SN  variable TR-SSU
variable TR-SP  variable TR-SSP  variable TR-SID0 variable TR-SID1
variable TR-S-LC variable TR-S-STRLC
SYM-CAP-V @ TR-SC !  SYM-STR-CAP-V @ TR-SSC !  SYM-N @ TR-SN !  SYM-STR-U @ TR-SSU !
SYMS-P @ TR-SP !  SYM-STR-P @ TR-SSP !
s" tgpkg" SYM-GLOBAL s" SYMGROWPROBE" SYM-INTERN TR-SID0 !
SYM-N @ dup SYM-CAP-V !  TR-S-LC !       \ next intern crosses the record cap
0 HIDX-MEM !  0 HIDX-VALID !             \ drop the full-cap index; rebuild at TR-S-LC
s" syms-rehash-before-grow" T-LABEL
s" tgpkg" SYM-GLOBAL s" SYMGROWPROBE" SYM-FIND -1 T= TR-SID0 @ T=
s" tgpkg" SYM-GLOBAL s" SYMGROWTWO" SYM-INTERN TR-SID1 !
s" syms-record-grow" T-LABEL
SYM-CAP-V @ TR-S-LC @ > -1 T=
s" syms-rehash-preserves-probe" T-LABEL
s" tgpkg" SYM-GLOBAL s" SYMGROWPROBE" SYM-FIND -1 T= TR-SID0 @ T=
s" syms-new-findable" T-LABEL
s" tgpkg" SYM-GLOBAL s" SYMGROWTWO" SYM-FIND -1 T= TR-SID1 @ T=
SYM-STR-U @ dup SYM-STR-CAP-V !  TR-S-STRLC !   \ next name crosses the string cap
s" tgpkg" SYM-GLOBAL s" SYMGROWTHREE" SYM-INTERN drop
s" syms-str-grow" T-LABEL
SYM-STR-CAP-V @ TR-S-STRLC @ > -1 T=
s" syms-str-grow-rebase-find" T-LABEL
s" tgpkg" SYM-GLOBAL s" SYMGROWPROBE" SYM-FIND -1 T= TR-SID0 @ T=
here TR-HERE !
SYM-SNAPSHOT-PERSIST
s" syms-persist-moved" T-LABEL
SYMS-P @ TR-HERE @ >= -1 T=
s" syms-persist-find" T-LABEL
s" tgpkg" SYM-GLOBAL s" SYMGROWPROBE" SYM-FIND -1 T= TR-SID0 @ T=
TR-SC @ SYM-CAP-V !  TR-SSC @ SYM-STR-CAP-V !  TR-SN @ SYM-N !  TR-SSU @ SYM-STR-U !
TR-SP @ SYMS-P !  TR-SSP @ SYM-STR-P !
0 HIDX-MEM !  0 HIDX-VALID !             \ rebuild a fresh index from the restored SYMS
\ --- unification trail: prim-overload trials (TRY-EFF) undo speculative binds by
\ popping the trail, not by copying the whole TVT/RVT pool. Crossing the trail
\ init cap mid-check grows it without corrupting undo; a backtracking body still
\ certifies. Lower TRAIL-CAP to force a grow, then repoint to the boot pool.
variable TR-TRAIL-CAP
TRAIL-CAP @ TR-TRAIL-CAP !
2 TRAIL-CAP !                            \ shrink so a few binds force a trail grow
s" COK-TRAIL ( n n n n -- n ) + + + " T-CHECK-PASSES
s" trail-arena-grow" T-LABEL
TRAIL-CAP @ 2 > -1 T=
TR-TRAIL-CAP @ TRAIL-CAP !  TRAIL-BOOT TRAIL-P !  TRAIL-RESET
\ --- path compression in T-RES: at trial depth 0 a resolved var chain is
\ compressed so intermediate vars point directly at the root; inside a trial
\ (depth>0) compression is disabled, so a rolled-back trial can never leave a
\ permanent var pointing at a cleared trial var. Build a v0->v1->v2->CON chain by
\ hand and observe both behaviors, then restore the var pool.
variable TC-V0  variable TC-V1  variable TC-V2  variable TC-CON  variable TC-FV
FV @ TC-FV !
FRESH MK-VAR TC-V0 !   FRESH MK-VAR TC-V1 !   FRESH MK-VAR TC-V2 !   7 MK-CON TC-CON !
TC-V1 @ TC-V0 @ PAY cells TVT + !        \ v0 -> v1
TC-V2 @ TC-V1 @ PAY cells TVT + !        \ v1 -> v2
TC-CON @ TC-V2 @ PAY cells TVT + !       \ v2 -> CON
0 TRIAL-DEPTH !
s" pathcomp-resolves" T-LABEL
TC-V0 @ T-RES TC-CON @ T=
s" pathcomp-compressed" T-LABEL
TC-V0 @ PAY cells TVT + @ TC-CON @ T=     \ v0 now points directly at CON
TC-V1 @ TC-V0 @ PAY cells TVT + !         \ rebuild the chain
TC-V2 @ TC-V1 @ PAY cells TVT + !
TC-CON @ TC-V2 @ PAY cells TVT + !
1 TRIAL-DEPTH !
s" pathcomp-trial-resolves" T-LABEL
TC-V0 @ T-RES TC-CON @ T=
s" pathcomp-trial-not-compressed" T-LABEL
TC-V0 @ PAY cells TVT + @ TC-V1 @ T=      \ inside a trial: still -> v1, not compressed
0 TRIAL-DEPTH !
UNBOUND TC-V0 @ PAY cells TVT + !  UNBOUND TC-V1 @ PAY cells TVT + !
UNBOUND TC-V2 @ PAY cells TVT + !  TC-FV @ FV !
s" COK-POSTDEC ( a -- a ) dup drop" T-CHECK-PASSES
\ --- locals over the compiler-matched cap fail CLOSED, not silently uncheckable
s" COK-LOC16 ( n -- n ) {: xxxxxxxxxxxxxxxx :} xxxxxxxxxxxxxxxx" T-CHECK-PASSES
s" CBAD-LOC17 ( n -- n ) {: xxxxxxxxxxxxxxxxx :} xxxxxxxxxxxxxxxxx" T-CHECK-REJECTS
\ --- multi-error load mode: rejects do not abort the load; the declared sig is
\ trusted so later definitions keep checking, and the count drives a fail-closed
\ exit. MEA3 calls the rejected MEA1 and certifies against its trusted n->n sig.
MULTI-ERR-BEGIN
s" : MEA1 ( n -- n ) drop ; : MEA2 ( n -- n ) drop drop ; : MEA3 ( n -- n ) MEA1 ;" evaluate
s" multi-err collects both rejects" T-LABEL
MULTI-ERR-END 2 T=
s" multi-err mode cleared after end" T-LABEL
MULTI-ERR? 0= -1 T=
\ --- MULTI-ERR file-relative diagnostic origin (dot habu-native-file-relative).
\ The driver evaluates a whole source buffer in one MULTI-ERR run and passes the
\ buffer base plus the ABSOLUTE address of the compiler's def name-token cell
\ (data-base DEF-TKA-CELL +) to MULTI-ERR-ORIGIN!; the checker then reports each
\ rejected def's FILE position instead of a def-buffer-relative one. With the
\ origin set, the JSON positions are byte-for-byte identical to tools/check.f
\ --all-errors (golden test/golden/diag-all-errors.err). Off by default: without
\ MULTI-ERR-ORIGIN! a def on file line 3 still reports line 1.
create MEO-CAP 8192 allot
variable MEO-SA  variable MEO-SU
: MEO-AT? ( ptr u8 n ptr u8 n -- bool ) {: h:ptr hu:n n:ptr nu:n :}
   hu nu < if 0 0= 0= exit then
   0 begin dup nu < while
      dup n + c@  over h + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;
variable MEO-CI
: MEO-CONTAINS? ( ptr u8 n ptr u8 n -- bool ) {: h:ptr hu:n n:ptr nu:n :}
   0 MEO-CI !
   begin MEO-CI @ nu + hu <= while
      h MEO-CI @ +  nu  n nu MEO-AT? if 0 0= exit then
      MEO-CI @ 1+ MEO-CI !
   repeat 0 0= 0= ;
\ negative: no origin set -> a rejected def on file line 3 reports def-relative line 1
MEO-CAP 8192 DIAG-BUFFER!  -1 DIAG-JSON!
MULTI-ERR-BEGIN
s\" : MEOFF-A ( -- ) ;\n: MEOFF-B ( -- ) ;\n: MEOFF-BAD ( i64 -- i64 ) dup ;\n" evaluate
MULTI-ERR-END drop
s" multi-err without origin stays def-relative (line 1)" T-LABEL
DIAG-BUFFER$ s\" \"line\":1," MEO-CONTAINS? -1 T=
s" multi-err without origin reports no file line 3" T-LABEL
DIAG-BUFFER$ s\" \"line\":3," MEO-CONTAINS? 0 T=
DIAG-BUFFER-OFF  0 DIAG-JSON!
\ positive: origin set -> file-relative positions match the all-errors golden
s\" : GDX-AE-OK ( i64 -- i64 ) dup * ;\n: GDX-AE-SEMI ( -- i64 ) [char] ; ;\n: GDX-AE-BAD1 ( i64 -- i64 ) dup ;\n: GDX-AE-BAD2 ( i64 -- ) >r ;\n" MEO-SU !  MEO-SA !
MEO-CAP 8192 DIAG-BUFFER!  -1 DIAG-JSON!
MULTI-ERR-BEGIN
MEO-SA @  data-base DEF-TKA-CELL +  1 1 0  MULTI-ERR-ORIGIN!
MEO-SA @ MEO-SU @ evaluate
s" multi-err origin collects both rejects" T-LABEL
MULTI-ERR-END 2 T=
s" multi-err origin BAD1 file-relative position matches golden" T-LABEL
DIAG-BUFFER$ s\" \"line\":3,\"column\":30,\"byte_start\":100,\"byte_end\":103" MEO-CONTAINS? -1 T=
s" multi-err origin BAD2 file-relative position matches golden" T-LABEL
DIAG-BUFFER$ s\" \"line\":4,\"column\":26,\"byte_start\":131,\"byte_end\":133" MEO-CONTAINS? -1 T=
DIAG-BUFFER-OFF  0 DIAG-JSON!
\ --- user-declarable nominal integer types (dot habu-declarable-nominal-int).
\ `deftype NAME` registers a fresh nominal AND auto-derives its explicit no-op
\ converter pair >NAME ( n -- NAME ) / NAME>N ( NAME -- n ). The nominal is
\ distinct from n and from every other nominal (no widening); the only way across
\ the boundary is the generated cast.
deftype frame-idx
deftype exposure-us
s" DNI-MK ( n -- frame-idx ) >frame-idx" T-CHECK-PASSES
s" DNI-UN ( frame-idx -- n ) frame-idx>N" T-CHECK-PASSES
s" DNI-RT ( n -- n ) >frame-idx frame-idx>N" T-CHECK-PASSES
s" DNI-KEEP ( frame-idx -- frame-idx )" T-CHECK-PASSES
s" DNI-NOWIDEN ( frame-idx -- n )" T-CHECK-REJECTS
s" DNI-NOCAST ( n -- frame-idx )" T-CHECK-REJECTS
s" DNI-DISTINCT ( frame-idx -- exposure-us )" T-CHECK-REJECTS
s" DNI-XCAST ( n -- exposure-us ) >frame-idx" T-CHECK-REJECTS
\ user nominal types render by NAME in diagnostics, not as '?'
MEO-CAP 8192 DIAG-BUFFER!  -1 DIAG-JSON!
s" DNI-RENDER ( frame-idx -- n )" CHECK! drop
s" nominal type renders by name in diagnostic" T-LABEL
DIAG-BUFFER$ s\" frame-idx" MEO-CONTAINS? -1 T=
DIAG-BUFFER-OFF  0 DIAG-JSON!
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
\ --- parametric type-family (TFAM) signature parsing (PLAN item 4) -----------
\ Registry-driven `family<...>` parsing replacing the old PARAM-CTOR? whitelist:
\ correct arity, reentrant nested parse, dual `ptr`, and family-id identity.
s" COK-TFAM-SPAN ( span<space-global,f32,extent-n> -- span<space-global,f32,extent-n> )" T-CHECK-PASSES
s" COK-TFAM-PTRPLAIN ( ptr u8 -- ) drop" T-CHECK-PASSES
s" COK-TFAM-PTRP ( ptr<space-global,f32> u32 -- ) drop drop" T-CHECK-PASSES
s" COK-TFAM-NEST ( acc<t,tile<t,b,m>,b> -- acc<t,tile<t,b,m>,b> )" T-CHECK-PASSES
s" COK-TFAM-NEST4 ( matrix<tile<x,y,z>,a,b,c> -- matrix<tile<x,y,z>,a,b,c> )" T-CHECK-PASSES
s" CBAD-TFAM-ARITY ( span<a,b> -- ) drop" T-CHECK-REJECTS
s" CBAD-TFAM-ARITY4 ( tile<a,b,c,d> -- ) drop" T-CHECK-REJECTS
s" CBAD-TFAM-UNKNOWN ( nope<n> -- ) drop" T-CHECK-REJECTS
s" CBAD-TFAM-PTRARITY ( ptr<a> -- ) drop" T-CHECK-REJECTS
\ Referencing a STORED nested-param sig used to crash the checker (native stack
\ overflow) once enough arena state accumulated: LIN-TYPE-COUNT descended a bound
\ VAR through FIELD-INNER without resolving it, reading an unrelated param-arena
\ slot that pointed back at the var. Fixed by resolving before the field descent
\ (src/core/checker.f LIN-TYPE-COUNT); dot habu-tfam-nested-param-09fa2004.
s" COK-TFAM-NEST-CALL ( acc<t,tile<t,b,m>,b> -- acc<t,tile<t,b,m>,b> ) COK-TFAM-NEST" T-CHECK-PASSES
s" COK-TFAM-NEST4-CALL ( matrix<tile<x,y,z>,a,b,c> -- matrix<tile<x,y,z>,a,b,c> ) COK-TFAM-NEST4" T-CHECK-PASSES
\ Family-specific arity diagnostics (PLAN item 4 acceptance): assert the verdict
\ AND the diagnostic KIND (SGBAD-ARITY?), not merely rejection, so a regression
\ swapping the arity reason for a generic syntax error is caught. These read a
\ checker-internal predicate, so they run at top level (not inside a `:` body).
s" CBAD-TFAM-ARITY-DIAG ( span<a,b> -- ) drop" 2dup T-LABEL CHECK-QUIET-CANDIDATE! 0 T=  SGBAD-ARITY? -1 T=
s" CBAD-TFAM-ARITY4-DIAG ( tile<a,b,c,d> -- ) drop" 2dup T-LABEL CHECK-QUIET-CANDIDATE! 0 T=  SGBAD-ARITY? -1 T=
\ `ptr` duality: proper `ptr<space,elem>` resolves as a family (incl. nested in
\ another family's args); over-arity rejects via the arity diagnostic; a bare
\ `ptr` with no element rejects via the bare-ptr diagnostic; a bare `ptr` inside
\ another family's args also rejects.
s" COK-PTR-IN-FAM ( span<space-global,ptr<space-global,f32>,extent-n> -- ) drop" T-CHECK-PASSES
s" CBAD-PTR-OVERARITY ( ptr<a,b,c> -- ) drop" 2dup T-LABEL CHECK-QUIET-CANDIDATE! 0 T=  SGBAD-ARITY? -1 T=
s" CBAD-PTR-BARE-ROWEND ( a ptr -- ) drop" 2dup T-LABEL CHECK-QUIET-CANDIDATE! 0 T=  SGBAD-BAREPTR? -1 T=
s" CBAD-PTR-BARE-IN-FAM ( span<space-global,ptr,extent-n> -- ) drop" T-CHECK-REJECTS
\ Uncapped per-param arity (dot habu-tfam-4-remainder): a family with arity > 4
\ (old PARAM-MAX-ARGS SoA-row cap) parses, checks, persists, instantiates through
\ the flat PARGP/VNARG/EN arg pools, and renders. Register a test-only arity-6
\ family, then prove: bare parse; a STORED-sig reference (E-COPY + E-INST round
\ trip); a nested arity-6 arg; a value-record field of the arity-6 family (VNARG
\ pool); and wrong-arity rejection (5 and 7 args) with the arity diagnostic.
s" tfam6r-big" 6 TFAM-REG-CELL
s" COK-BIG6 ( tfam6r-big<a,b,c,d,e,f> -- tfam6r-big<a,b,c,d,e,f> )" T-CHECK-PASSES
s" COK-BIG6-CALL ( tfam6r-big<a,b,c,d,e,f> -- tfam6r-big<a,b,c,d,e,f> ) COK-BIG6" T-CHECK-PASSES
s" COK-BIG6-NEST ( tfam6r-big<a,tfam6r-big<t,u,v,w,x,y>,c,d,e,f> -- tfam6r-big<a,tfam6r-big<t,u,v,w,x,y>,c,d,e,f> )" T-CHECK-PASSES
s" CBAD-BIG6-A5-DIAG ( tfam6r-big<a,b,c,d,e> -- ) drop" 2dup T-LABEL CHECK-QUIET-CANDIDATE! 0 T=  SGBAD-ARITY? -1 T=
s" CBAD-BIG6-A7-DIAG ( tfam6r-big<a,b,c,d,e,f,g> -- ) drop" 2dup T-LABEL CHECK-QUIET-CANDIDATE! 0 T=  SGBAD-ARITY? -1 T=
\ SC-QUOT: a quotation as a family argument (dot habu-tfam-4-remainder). SIG-TYPE
\ parses [ in -- out | rin -- rout ] as one param arg (a T-QUOT term), threaded
\ through parse, persist (E-COPY/VREC-COPY), instantiate (E-INST), and render.
\ Prove: bare parse; a STORED-sig reference (E-COPY + REND-SIG record + E-INST);
\ an explicit return-stack clause; a quotation nested inside the quot arg's stack;
\ and malformed effect rows (missing '--' or ']') reject.
s" scq-fam" 2 TFAM-REG-CELL
s" COK-SCQ ( scq-fam<[ n -- n ],f32> -- scq-fam<[ n -- n ],f32> )" T-CHECK-PASSES
s" COK-SCQ-CALL ( scq-fam<[ n -- n ],f32> -- scq-fam<[ n -- n ],f32> ) COK-SCQ" T-CHECK-PASSES
s" COK-SCQ-RET ( scq-fam<[ n -- n | a -- a ],f32> -- scq-fam<[ n -- n | a -- a ],f32> )" T-CHECK-PASSES
s" COK-SCQ-QNEST ( scq-fam<[ [ n -- n ] -- n ],f32> -- scq-fam<[ [ n -- n ] -- n ],f32> )" T-CHECK-PASSES
\ Malformed-row first causes are asserted by KIND (destruction review): a missing
\ '--' hits SIG-PARSE-QUOT's EXPECT-SIG -> SGBAD-SYNTAX; a missing ']' after the
\ data rows is first seen as the stray ',' by SIG-TYPE -> SGBAD-UNKNOWN (the ']'
\ EXPECT never runs); the extra '--' after a full return clause is the fixture
\ that genuinely reaches the return-branch s" ]" EXPECT-SIG -> SGBAD-SYNTAX.
s" CBAD-SCQ-NODASH ( scq-fam<[ n n ],f32> -- ) drop" 2dup T-LABEL CHECK-QUIET-CANDIDATE! 0 T=  SGBAD-SYNTAX? -1 T=
s" CBAD-SCQ-NOCLOSE ( scq-fam<[ n -- n ,f32> -- ) drop" 2dup T-LABEL CHECK-QUIET-CANDIDATE! 0 T=  SGBAD-UNKNOWN? -1 T=
s" CBAD-SCQ-RETCLOSE ( scq-fam<[ n -- n | a -- a -- ],f32> -- ) drop" 2dup T-LABEL CHECK-QUIET-CANDIDATE! 0 T=  SGBAD-SYNTAX? -1 T=
\ Render acceptance (destruction review): a mismatch diagnostic must render the
\ full parametric type — all six args of an arity-6 application, and an SC-QUOT
\ arg's din/dout rows plus the return clause — never a collapsed string or '?'.
s" T-BIG6-MK" s" -- tfam6r-big<n,f32,u8,u16,i64,bool>" TRUST
s" T-SCQ-MK" s" -- scq-fam<[ n -- n | a -- a ],f32>" TRUST
RSD-BUF RSD-CAP DIAG-BUFFER!
s" arity-6 mismatch diagnostic rejects" T-LABEL
s" CBAD-BIG6-REND ( -- n ) T-BIG6-MK" CHECK-CANDIDATE! 0 T=
s" arity-6 diagnostic renders all six args" T-LABEL
DIAG-BUFFER$ s" tfam6r-big<n,f32,u8,u16,i64,bool>" T-HAS? -1 T=
RSD-BUF RSD-CAP DIAG-BUFFER!
s" SC-QUOT mismatch diagnostic rejects" T-LABEL
s" CBAD-SCQ-REND ( -- n ) T-SCQ-MK" CHECK-CANDIDATE! 0 T=
s" SC-QUOT diagnostic renders quot rows and return clause" T-LABEL
DIAG-BUFFER$ s" scq-fam<[ n-- n | a-- a],f32>" T-HAS? -1 T=
DIAG-BUFFER-OFF
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
\ execute (quotation application) enforces the same linear conservation as a
\ direct step: a polymorphic quotation applied to a linear that copies or drops
\ it is rejected; an explicit linear consumer/producer and a passthrough certify.
s" COK-OWN-EXEC-FREE ( own -- ) [: T-FREE-OWN ;] execute" T-CHECK-PASSES
s" COK-OWN-EXEC-ID ( own -- own ) [: ;] execute" T-CHECK-PASSES
s" COK-OWN-EXEC-MAKE ( -- own ) [: T-MAKE-OWN ;] execute" T-CHECK-PASSES
s" COK-N-EXEC-DUP ( n -- n n ) [: dup ;] execute" T-CHECK-PASSES
s" CBAD-OWN-EXEC-DUP ( own -- own own ) [: dup ;] execute" T-CHECK-REJECTS
s" CBAD-OWN-EXEC-DROP ( own -- ) [: drop ;] execute" T-CHECK-REJECTS
s" CBAD-OWN-EXEC-NEST ( own -- own own ) [: [: dup ;] execute ;] execute" T-CHECK-REJECTS
\ acquire/release pairing proven through a work quotation: balanced certifies;
\ leak (missing release) and double release are rejected.
s" COK-OWN-FRAME-WORK ( -- ) T-MAKE-OWN [: ;] execute T-FREE-OWN" T-CHECK-PASSES
s" CBAD-OWN-FRAME-LEAK ( -- ) T-MAKE-OWN [: ;] execute" T-CHECK-REJECTS
s" CBAD-OWN-FRAME-DOUBLE ( -- ) T-MAKE-OWN [: ;] execute T-FREE-OWN T-FREE-OWN" T-CHECK-REJECTS
\ Polymorphic linear laundering (habu-linear-kind-inference): a linear copied or
\ dropped while its type is still a polymorphic var — through KEEP/BI's `over` or
\ an intra-quotation `dup`/`over`, before it binds linear — is rejected by the
\ linear kind discipline, even though concrete-count conservation stays neutral
\ at the call site. KEEP/BI copy `a` into a consumer quotation and also return it;
\ the intra-quot cases copy the var, then FREE binds it linear (deferred taint).
s" CBAD-OWN-KEEP-LAUNDER ( own -- own ) [: T-FREE-OWN ;] KEEP" T-CHECK-REJECTS
s" CBAD-OWN-BI-LAUNDER ( own -- own ) [: ;] [: T-FREE-OWN ;] BI" T-CHECK-REJECTS
s" CBAD-OWN-QUOT-DUP-FREE ( own -- own ) [: dup T-FREE-OWN ;] execute" T-CHECK-REJECTS
s" CBAD-OWN-QUOT-OVER-FREE ( own n -- own n ) [: over T-FREE-OWN ;] execute" T-CHECK-REJECTS
\ Positives the discipline must not touch: KEEP over non-linear data still
\ certifies, and a sound DIP that only MOVES the linear (1-in / 1-out) certifies.
s" COK-N-KEEP ( n -- n ) [: 1+ ;] KEEP drop" T-CHECK-PASSES
s" COK-OWN-DIP-PASS ( n own -- n own ) [: 1+ ;] DIP" T-CHECK-PASSES
\ Linear values may not launder through {: :} locals (dot
\ habu-checker-linear-values-a5745699). A local reference re-pushes its binding
\ outside the LIN-SNAPSHOT/LIN-CHECK count discipline, so binding a linear into a
\ local hid double-reference (duplication), an unreferenced local (leak), and a
\ referenced-then-freed-twice double-consume. Binding a value that concretely
\ resolves linear is rejected outright with E-LINEAR-LOCAL; the value must stay on
\ the stack and be factored. Direct-stack linear discipline (COK-OWN-*/CBAD-OWN-*
\ above) is unchanged, and non-linear locals are untouched.
s" CBAD-OWN-LOCAL-DUP ( own -- own own ) {: x:own :} x x" T-CHECK-REJECTS
s" CBAD-OWN-LOCAL-LEAK ( own -- ) {: x:own :}" T-CHECK-REJECTS
s" CBAD-OWN-LOCAL-DOUBLE-FREE ( own -- ) {: x:own :} x T-FREE-OWN x T-FREE-OWN" T-CHECK-REJECTS
s" CBAD-OWN-LOCAL-ONCE ( own -- ) {: x:own :} x T-FREE-OWN" T-CHECK-REJECTS
s" CBAD-OWN-LOCAL-UNTYPED ( own -- own own ) {: x :} x x" T-CHECK-REJECTS
s" CBAD-OWN-LOCAL-BRANCH ( bool own -- ) {: x:own :} if x T-FREE-OWN then" T-CHECK-REJECTS
s" CBAD-OWN-LOCAL-MAKE ( -- ) T-MAKE-OWN {: x:own :} x T-FREE-OWN" T-CHECK-REJECTS
\ Deferred laundering: a local bound to a still-polymorphic var referenced twice,
\ that only later resolves linear, must reject through the taint discipline.
s" CBAD-OWN-LOCAL-POLY-DUP ( a -- ) {: x :} x x T-FREE-OWN T-FREE-OWN" T-CHECK-REJECTS
\ Positive controls: the linear kept on the stack still certifies, a non-linear
\ local still binds/references (single AND duplicate), and a poly local that never
\ resolves linear is untouched.
s" COK-OWN-STACK-KEEP ( own -- ) T-FREE-OWN" T-CHECK-PASSES
s" COK-N-LOCAL-DUP ( n -- n n ) {: x:n :} x x" T-CHECK-PASSES
s" COK-POLY-LOCAL-DUP ( a -- a a ) {: x :} x x" T-CHECK-PASSES
\ The reject carries the dedicated E-LINEAR-LOCAL code and factor_linear_local class.
RSD-BUF RSD-CAP DIAG-BUFFER!
s" linear-local reject carries E-LINEAR-LOCAL" T-LABEL
s" CBAD-OWN-LOCAL-DIAG ( own -- own own ) {: x:own :} x x" CHECK-CANDIDATE! 0 T=
s" linear-local diagnostic names E-LINEAR-LOCAL" T-LABEL
DIAG-BUFFER$ s" E-LINEAR-LOCAL" T-HAS? -1 T=
DIAG-BUFFER-OFF
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
\ Value-record with an arity-6 family field type (dot habu-tfam-4-remainder):
\ the field wrapper field<rec,q,tfam6r-big<6>> stores its 6-arg inner param in the
\ VNARG pool; the roundtrip forces VREC-PUSH-FIELDS/VREC-INST to read it back.
VALUE-RECORD tfam6r-vr q tfam6r-big<a,b,c,d,e,f> END-VALUE-RECORD
: T->BIG6VR ( tfam6r-big<a,b,c,d,e,f> -- tfam6r-vr ) ;
: T-BIG6VR> ( tfam6r-vr -- tfam6r-big<a,b,c,d,e,f> ) ;
s" COK-BIG6VR-ID ( tfam6r-vr -- tfam6r-vr )" T-CHECK-PASSES
s" COK-BIG6VR-ROUNDTRIP ( tfam6r-big<a,b,c,d,e,f> -- tfam6r-big<a,b,c,d,e,f> ) T->BIG6VR T-BIG6VR>" T-CHECK-PASSES
\ Value-record with a quotation-family (SC-QUOT) field type: VREC-COPY persists the
\ field's T-QUOT arg subtree (VR-QUOT node) and VREC-INST reads it back on roundtrip.
VALUE-RECORD scq-vr q scq-fam<[ n -- n ],f32> END-VALUE-RECORD
: T->SCQVR ( scq-fam<[ n -- n ],f32> -- scq-vr ) ;
: T-SCQVR> ( scq-vr -- scq-fam<[ n -- n ],f32> ) ;
s" COK-SCQVR-ID ( scq-vr -- scq-vr )" T-CHECK-PASSES
s" COK-SCQVR-ROUNDTRIP ( scq-fam<[ n -- n ],f32> -- scq-fam<[ n -- n ],f32> ) T->SCQVR T-SCQVR>" T-CHECK-PASSES
\ VNARG rollback parity (destruction review): a VALUE-RECORD defined inside a
\ rolled-back scope allocates VR-PARAM nodes AND their VNARG runs; RBF-POP must
\ rewind VNARG-N in lockstep with VREC-NODE-N, and a record defined BEFORE the
\ scope must still instantiate from its (surviving, below-the-mark) VNARG run.
variable TR-VNARG-RB   variable TR-VNODE-RB
VNARG-N @ TR-VNARG-RB !   VREC-NODE-N @ TR-VNODE-RB !
CHECKER-SCOPE-START
VALUE-RECORD tfam6r-rb q tfam6r-big<a,b,c,d,e,f> END-VALUE-RECORD
s" vnarg-scope-grew" T-LABEL   VNARG-N @ TR-VNARG-RB @ > -1 T=
s" vrec-node-scope-grew" T-LABEL   VREC-NODE-N @ TR-VNODE-RB @ > -1 T=
CHECKER-SCOPE-DONE
s" vnarg-rollback-rewinds" T-LABEL   VNARG-N @ TR-VNARG-RB @ T=
s" vrec-node-rollback-parity" T-LABEL   VREC-NODE-N @ TR-VNODE-RB @ T=
s" surviving record instantiates after rollback" T-LABEL
s" COK-BIG6VR-POSTRB ( tfam6r-vr -- tfam6r-vr )" CHECK-QUIET-CANDIDATE! -1 T=
\ VNARG persist read-back (destruction review): force the arg pool onto a grown
\ mmap store, bake it with VREC-SNAPSHOT-PERSIST, and prove a real >4-arg run
\ still instantiates from the persisted buffer (VNARG-N stable across the bake).
variable TR-VNARG-P0
VNARG-N @ VNARG-CAP-V !          \ next reserve crosses the cap -> VNARG grows to mmap
VALUE-RECORD tfam6r-vrp q tfam6r-big<a,b,c,d,e,f> END-VALUE-RECORD
VNARG-N @ TR-VNARG-P0 !
s" vnarg-grow" T-LABEL   VNARG-P @ VNARG-BOOT = 0 T=   \ pool left the boot buffer -> persist is a real copy
VREC-SNAPSHOT-PERSIST
s" vnarg-persist-stable" T-LABEL   VNARG-N @ TR-VNARG-P0 @ T=
s" vnarg-persist-readback" T-LABEL
s" COK-BIG6VRP-RT ( tfam6r-vrp -- tfam6r-vrp )" CHECK-QUIET-CANDIDATE! -1 T=
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

\ rejected definitions must not leak code space: the hooked publish path rolls
\ CP back to the definition start (the pre-name CP for >16-char names) when
\ the hook verdict is 0. ES-VERDICT-HOOK is the raw-verdict boundary: unlike
\ HOOK it returns the checker verdict instead of throwing, so the engine's
\ reject-continue branch runs in-process.
TRUSTED: ES-VERDICT-HOOK ( ptr u8 n -- n ) CHECK! ;
variable ES-CPX-DIAG
variable ES-CPX-CP
variable ES-CPX-ND
\ unsigned body with an unsafe token: checker verdict 0, engine rejects
: ES-CPX-BAD$ ( -- ptr u8 n ) s" : ES-CPX-BAD 0 set-check ;" ;
\ >16-char name: C-STORE-NAME copies it into code space before the entry, so
\ the reject rollback must reclaim the name bytes too (CP := pre-name CP)
: ES-CPX-BAD-EXT$ ( -- ptr u8 n ) s" : ES-CPX-BAD-EXTENDED-NAME 0 set-check ;" ;
DIAGXT @ ES-CPX-DIAG !  0 DIAGXT !
' ES-VERDICT-HOOK set-check
cp@ ES-CPX-CP !  ndict@ ES-CPX-ND !
ES-CPX-BAD$ evaluate
s" cpx-reject-1" T-LABEL cp@ ES-CPX-CP @ T=
ES-CPX-BAD$ evaluate
ES-CPX-BAD$ evaluate
ES-CPX-BAD$ evaluate
s" cpx-reject-4" T-LABEL cp@ ES-CPX-CP @ T=
s" cpx-reject-ndict" T-LABEL ndict@ ES-CPX-ND @ T=
ES-CPX-BAD-EXT$ evaluate
s" cpx-reject-ext-1" T-LABEL cp@ ES-CPX-CP @ T=
ES-CPX-BAD-EXT$ evaluate
ES-CPX-BAD-EXT$ evaluate
s" cpx-reject-ext-3" T-LABEL cp@ ES-CPX-CP @ T=
s" cpx-reject-ext-ndict" T-LABEL ndict@ ES-CPX-ND @ T=
' HOOK set-check
ES-CPX-DIAG @ DIAGXT !
\ retry after rejects: the accepted definition lands on the reclaimed entry
: ES-CPX-GOOD ( n -- n ) 1 + ;
6 ES-CPX-GOOD 7 T=
s" cpx-retry-entry" T-LABEL ' ES-CPX-GOOD ES-CPX-CP @ T=
\ ext-named retry: name bytes land at cp@, entry follows the aligned name
cp@ ES-CPX-CP !
: ES-CPX-GOOD-EXTENDED-NAME ( n -- n ) 2 + ;
6 ES-CPX-GOOD-EXTENDED-NAME 8 T=
s" cpx-retry-ext-entry" T-LABEL ' ES-CPX-GOOD-EXTENDED-NAME ES-CPX-CP @ 28 + T=

\ Hash-index rollback churn must terminate. A rejected/evaluated candidate can
\ roll NDICT and CP back while leaving stale HIDX slots. Inserts must reuse
\ those stale slots instead of probing forever once the fixed table fills.
variable ES-HIDX-ND
variable ES-HIDX-CP
: ES-HIDX-SRC$ ( -- ptr u8 n )
   s" : ES-HIDX-CHURNED 1 ;" ;
0 set-check
ndict@ ES-HIDX-ND !  cp@ ES-HIDX-CP !
: ES-HIDX-ROLLBACK-CHURN ( -- )
   20000 0 ?do
      ES-HIDX-SRC$ evaluate
      ES-HIDX-ND @ ndict!
      ES-HIDX-CP @ cp!
   loop ;
ES-HIDX-ROLLBACK-CHURN
' HOOK set-check
s" hidx rollback churn terminates" T-LABEL
7 7 T=

\ Candidate dictionary/hook smoke, folded from the former standalone GE-CAND-SMOKE
\ candidate launch into this engine-suite run so the batch shares one HABU_UNDER_TEST
\ spawn instead of two. Each check is an independent T= probe (no entangled stdout
\ contract): the boot check hook is installed, a checked def compiles and runs, and a
\ representative baked process primitive resolves. Runs on both the candidate and bin/hb.
s" smoke-hook-installed" T-LABEL data-base HOOK-CELL + @ 0= 0 T=
: ES-SMOKE-SQ ( i64 -- i64 ) dup * ;
s" smoke-checked-compile-run" T-LABEL 7 ES-SMOKE-SQ 49 T=
s" smoke-baked-word-resolves" T-LABEL ' spawn-argv-env-cwd-io 0 <> -1 T=

\ x18 Darwin-reserved regressions (dot habu-rca-engine-sigsegv-ba81a08c):
\ XNU zeroes x18 on any trap return; pre-fix, interpret-mode escaped
\ literals pushed base 0 once a copy crossed a fresh DATA page, and
\ compile-mode literals crashed LPAT when the copy crossed a code page.
\ Direct top-level fixtures (evaluate cannot carry escaped literals yet:
\ dot habu-interpret-mode-escaped-d8dad34b).
: TX18-CHK ( ptr u8 n -- ) 150 T=  c@ [char] A T= ;
: TX18-QCHK ( ptr u8 n -- ) 180 T=  c@ [char] B T= ;
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" TX18-CHK
: TX18-DA ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DB ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DC ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DD ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DE ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DF ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DG ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DH ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DI ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DJ ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DK ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DL ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DM ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DN ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DO ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DP ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DQ ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DR ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DS ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DT ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DU ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DV ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DW ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
: TX18-DX ( -- )
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   S\" BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" TX18-QCHK
   ;
TX18-DA TX18-DB TX18-DC TX18-DD TX18-DE TX18-DF TX18-DG TX18-DH TX18-DI TX18-DJ TX18-DK TX18-DL
TX18-DM TX18-DN TX18-DO TX18-DP TX18-DQ TX18-DR TX18-DS TX18-DT TX18-DU TX18-DV TX18-DW TX18-DX

\ report: count + nonzero exit on failure
: REPORT ( -- )
   #FAIL @ 0 = if [char] o emit [char] k emit cr exit then
   #FAIL @ . s" engine-suite: failures" 1 die ;
REPORT
