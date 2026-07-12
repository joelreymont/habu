\ gate-dictionary.f - checked runner for dictionary and checker contracts.
\
\ Load after test/gate-common.f and tools/check-all-errors-core.f.

$41 constant GD-A
$2E constant GD-DOT
$63 constant GD-C-LOWER
600 constant GD-LONG-NAME-LEN

create GD-INC-CORE FS-PATH-CAP allot
create GD-INC-API FS-PATH-CAP allot
create GD-INC-MAIN FS-PATH-CAP allot
create GD-INC-DUP FS-PATH-CAP allot

variable GD-INC-CORE-U
variable GD-INC-API-U
variable GD-INC-MAIN-U
variable GD-INC-DUP-U
variable GD-CHECK-LABEL-A
variable GD-CHECK-LABEL-U
variable GD-CANDIDATE-A
variable GD-CANDIDATE-U
variable GD-CANDIDATE-VERDICT
variable GD-START-NS

: GD-CHECK-LABEL-A-FIELD ( -- ptr ptr u8 )
   GD-CHECK-LABEL-A 0 ptr-field ;

: GD-CANDIDATE-A-FIELD ( -- ptr ptr u8 )
   GD-CANDIDATE-A 0 ptr-field ;

: GD-CHECK-LABEL$ ( -- ptr u8 n )
   GD-CHECK-LABEL-A-FIELD @ GD-CHECK-LABEL-U @ ;

: GD-CANDIDATE$ ( -- ptr u8 n )
   GD-CANDIDATE-A-FIELD @ GD-CANDIDATE-U @ ;

: GD-CHECK-LABEL! ( ptr u8 n -- ) {: a:ptr u:n :}
   u GD-CHECK-LABEL-U !
   a GD-CHECK-LABEL-A-FIELD ! ;

: GD-CANDIDATE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u GD-CANDIDATE-U !
   a GD-CANDIDATE-A-FIELD ! ;

\ typed-local-lint: allow-bare-local - q is the contract action quotation.
: GD-RUN ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   mono-ns GD-START-NS !
   q execute
   label labelu mono-ns GD-START-NS @ - PROC-NS-PER-MS / GS-SPAN ;

: GD-EMIT-LONG-NAME ( -- )
   GD-LONG-NAME-LEN GD-A GE-SRC-REPEAT-C ;

: GD-SRC-LONG-NAME-S" ( -- )
   s" s" GE-SRC+
   GE-DQ GE-SRC-C
   GE-SRC-SP
   GD-EMIT-LONG-NAME
   GE-DQ GE-SRC-C ;

: GD-SRC-DOTQ ( ptr u8 n -- ) {: a:ptr u:n :}
   GD-DOT GE-SRC-C
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: GD-SRC-CQ ( ptr u8 n -- ) {: a:ptr u:n :}
   GD-C-LOWER GE-SRC-C
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: GD-INC-CORE$ ( -- ptr u8 n )
   GD-INC-CORE GD-INC-CORE-U @ ;

: GD-INC-API$ ( -- ptr u8 n )
   GD-INC-API GD-INC-API-U @ ;

: GD-INC-MAIN$ ( -- ptr u8 n )
   GD-INC-MAIN GD-INC-MAIN-U @ ;

: GD-INC-DUP$ ( -- ptr u8 n )
   GD-INC-DUP GD-INC-DUP-U @ ;

: GD-INC-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu:n dst:ptr up:ptr :}
   GT-ROOT name nameu dst JOIN-PATH up ! ;

: GD-INC-PATHS ( -- )
   s" app-core.f" GD-INC-CORE GD-INC-CORE-U GD-INC-PATH!
   s" app-api.f" GD-INC-API GD-INC-API-U GD-INC-PATH!
   s" app-main.f" GD-INC-MAIN GD-INC-MAIN-U GD-INC-PATH!
   s" app-dup.f" GD-INC-DUP GD-INC-DUP-U GD-INC-PATH! ;

: GD-SB-LINE ( ptr u8 n -- )
   SB-APPEND
   GE-LF SB-APPEND-C ;

: GD-SB-INCLUDE-LINE ( ptr u8 n -- )
   s" include " SB-APPEND
   SB-APPEND
   GE-LF SB-APPEND-C ;

: GD-SB-S" ( ptr u8 n -- )
   s" s" SB-APPEND
   GE-DQ SB-APPEND-C
   GE-SP SB-APPEND-C
   SB-APPEND
   GE-DQ SB-APPEND-C ;

: GD-WRITE-CORE ( -- )
   SB-RESET
   s" package APP" GD-SB-LINE
   s" : H ( -- n ) 9 ;" GD-SB-LINE
   s" public" GD-SB-LINE
   s" : CORE ( -- n ) H ;" GD-SB-LINE
   s" end-package" GD-SB-LINE
   GD-INC-CORE$ SB$ WRITE-ALL ;

: GD-WRITE-API-BODY ( -- )
   s" package APP" GD-SB-LINE
   s" public" GD-SB-LINE
   s" : GET ( -- n ) H ;" GD-SB-LINE
   s" end-package" GD-SB-LINE ;

: GD-WRITE-API-NOINC ( -- )
   SB-RESET
   GD-WRITE-API-BODY
   GD-INC-API$ SB$ WRITE-ALL ;

: GD-WRITE-API-INCLUDE ( -- )
   SB-RESET
   GD-INC-CORE$ GD-SB-INCLUDE-LINE
   GD-WRITE-API-BODY
   GD-INC-API$ SB$ WRITE-ALL ;

: GD-WRITE-MAIN-RUN ( -- )
   SB-RESET
   s" APP:GET ." GD-SB-LINE
   GD-INC-MAIN$ SB$ WRITE-ALL ;

: GD-WRITE-MAIN-INCLUDE ( -- )
   SB-RESET
   GD-INC-API$ GD-SB-INCLUDE-LINE
   s" APP:GET ." GD-SB-LINE
   GD-INC-MAIN$ SB$ WRITE-ALL ;

: GD-WRITE-MAIN-INCLUDED ( -- )
   SB-RESET
   GD-INC-API$ GD-SB-S"
   s"  included" GD-SB-LINE
   s" APP:GET ." GD-SB-LINE
   GD-INC-MAIN$ SB$ WRITE-ALL ;

: GD-WRITE-DUP ( -- )
   SB-RESET
   s" package APP" GD-SB-LINE
   s" : H ( -- n ) 1 ;" GD-SB-LINE
   s" end-package" GD-SB-LINE
   GD-INC-DUP$ SB$ WRITE-ALL ;

: GD-LONG-DICTIONARY-SOURCE ( -- )
   GE-SRC-RESET
   s" : LONG-DICTIONARY-NAME-ADDONE ( i64 -- i64 ) 1 + ;" GE-SRC-LINE
   s" 41 LONG-DICTIONARY-NAME-ADDONE ." GE-SRC-LINE
   s" 123 constant LONG-DICTIONARY-CONSTANT" GE-SRC-LINE
   s" LONG-DICTIONARY-CONSTANT ." GE-SRC-LINE
   s" variable LONG-DICTIONARY-VARIABLE" GE-SRC-LINE
   s" 77 LONG-DICTIONARY-VARIABLE !" GE-SRC-LINE
   s" LONG-DICTIONARY-VARIABLE @ ." GE-SRC-LINE
   s" LONG-DICTIONARY-NAME-ADDONE" GE-SRC-S"
   s"  get-current search-wl 0= ." GE-SRC-LINE
   s" long-dictionary-name-addone" GE-SRC-S"
   s"  get-current search-wl 0= ." GE-SRC-LINE
   s" TRUSTED: LONG-DICTIONARY-TRUSTED ( n -- n ) dup ;" GE-SRC-LINE
   s" USE ( n -- n ) LONG-DICTIONARY-TRUSTED" GE-SRC-CHECK-LINE
   s" BAD ( n -- n n ) LONG-DICTIONARY-TRUSTED" GE-SRC-CHECK-LINE
   s" 9 LONG-DICTIONARY-TRUSTED . ." GE-SRC-LINE ;

: GD-LONG-DICTIONARY ( -- )
   GE-HB-RESET
   GD-LONG-DICTIONARY-SOURCE
   s" hb long dictionary names" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 42" GE-OUT-LINE
   s" 123" GE-OUT-LINE
   s" 77" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   s" 9" GE-OUT-LINE
   s" 9" GE-OUT-LINE
   SB$ s" hb long dictionary names output" GE-EXPECT-OUT ;

: GD-WORDLIST-SOURCE ( -- )
   GE-SRC-RESET
   s" wordlist constant LONG-WL" GE-SRC-LINE
   s" LONG-WL set-current" GE-SRC-LINE
   s" : LONG-WORDLIST-ONLY-NAME ( -- i64 ) 8 ;" GE-SRC-LINE
   s" 0 set-current" GE-SRC-LINE
   s" LONG-WORDLIST-ONLY-NAME" GE-SRC-S"
   s"  0 search-wl 0= ." GE-SRC-LINE
   s" LONG-WORDLIST-ONLY-NAME" GE-SRC-S"
   s"  LONG-WL search-wl 0= ." GE-SRC-LINE ;

: GD-WORDLIST ( -- )
   GE-HB-RESET
   GD-WORDLIST-SOURCE
   s" hb long dictionary wordlist isolation" GE-EVAL-RUN-STDIN
   SB-RESET
   s" -1" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   SB$ s" hb long dictionary wordlist isolation output" GE-EXPECT-OUT ;

: GD-LONG-NAME-SOURCE ( -- )
   GE-SRC-RESET
   s" : " GE-SRC+
   GD-EMIT-LONG-NAME
   s"  ( -- n ) 1 ;" GE-SRC-LINE
   GD-EMIT-LONG-NAME
   s"  ." GE-SRC-LINE
   GD-SRC-LONG-NAME-S"
   s"  get-current search-wl 0= ." GE-SRC-LINE ;

: GD-LONG-NAME ( -- )
   GE-HB-RESET
   GD-LONG-NAME-SOURCE
   s" hb dictionary name over 255 bytes" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 1" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   SB$ s" hb dictionary name over 255 bytes output" GE-EXPECT-OUT ;

: GD-TRUSTED-DOES-SOURCE ( -- )
   GE-SRC-RESET
   s" TRUSTED: ARR ( n -- ) create cells allot does> ( n -- ptr a ) swap 0 ?do cell+ loop ;" GE-SRC-LINE
   s" 4 ARR A4" GE-SRC-LINE
   s" USE ( n -- ptr a ) A4" GE-SRC-CHECK-LINE
   s" 7 2 A4 !" GE-SRC-LINE
   s" 2 A4 @ ." GE-SRC-LINE ;

: GD-TRUSTED-DOES ( -- )
   GE-HB-RESET
   GD-TRUSTED-DOES-SOURCE
   s" hb trusted CREATE...DOES> effect recording" GE-EVAL-RUN-STDIN
   SB-RESET
   s" -1" GE-OUT-LINE
   s" 7" GE-OUT-LINE
   SB$ s" hb trusted CREATE...DOES> effect recording output" GE-EXPECT-OUT ;

: GD-BAD-DOES ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" TRUSTED: BADARR ( n -- ) create cells allot does> ( n -- ptr a ) drop ;" GE-SRC-LINE
   70 s" does>" s" hb bad trusted DOES> diagnostic" GE-EVAL-FORK-BAD
   GE-HB-RESET
   GE-SRC-RESET
   s" TRUSTED: BADDEF ( n -- ) create cells allot does> drop ;" GE-SRC-LINE
   76 s" does>" s" hb trusted DOES> without created signature diagnostic" GE-EVAL-FORK-BAD ;

: GD-ROW-QUOT-CHECKS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" V1 ( R -- R i64 ) 5" GE-SRC-CHECK-LINE
   s" V2 ( i64 [ i64 -- i64 ] -- i64 ) execute" GE-SRC-CHECK-LINE
   s" V3 ( R -- R i64 ) 5 5" GE-SRC-CHECK-LINE
   s" hb rows/quot sig verify" GE-EVAL-RUN-STDIN
   SB-RESET s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE s" 0" GE-OUT-LINE
   SB$ s" hb rows/quot sig verify output" GE-EXPECT-OUT ;

: GD-PRIMITIVE-CHECKS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" P1 ( i64 i64 i64 i64 -- i64 i64 i64 i64 i64 i64 ) 2over" GE-SRC-CHECK-LINE
   s" P2 ( i64 i64 -- i64 i64 ) 2>r 2r>" GE-SRC-CHECK-LINE
   s" P3 ( i64 -- i64 ) abs" GE-SRC-CHECK-LINE
   s" P4 ( i64 i64 -- i64 i64 ) /mod" GE-SRC-CHECK-LINE
   s" P5 ( ptr u8 -- ptr u8 i64 ) count" GE-SRC-CHECK-LINE
   s" P6 ( i64 i64 -- i64 i64 i64 ) depth" GE-SRC-CHECK-LINE
   s" P7 ( -- n ) 0 4096 3 $1002 -1 0 mmap" GE-SRC-CHECK-LINE
   s" P8 ( ptr u8 ptr a -- ptr u8 ) swap over 0 ptr-field ! 0 ptr-field @" GE-SRC-CHECK-LINE
   s" hb primitive checklist signatures" GE-EVAL-RUN-STDIN
   SB-RESET
   s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE
   s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE
   s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE
   SB$ s" hb primitive checklist signatures output" GE-EXPECT-OUT ;

: GD-RETURN-CHECKS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" RBAD1 ( i64 i64 -- ) 2>r" GE-SRC-CHECK-LINE
   s" RBAD2 ( -- i64 i64 ) 2r>" GE-SRC-CHECK-LINE
   s" RPEEK ( i64 i64 -- i64 i64 i64 i64 ) 2>r 2r@ 2r>" GE-SRC-CHECK-LINE
   s" QD ( i64 -- i64 i64 ) ?dup" GE-SRC-CHECK-LINE
   s" hb return-stack/?dup primitive verdicts" GE-EVAL-RUN-STDIN
   SB-RESET s" 0" GE-OUT-LINE s" 0" GE-OUT-LINE s" -1" GE-OUT-LINE s" 1" GE-OUT-LINE
   SB$ s" hb return-stack/?dup primitive verdicts output" GE-EXPECT-OUT ;

: GD-COMBINATOR-CHECKS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" CDIP ( i64 i64 -- i64 i64 ) [: 1+ ;] DIP" GE-SRC-CHECK-LINE
   s" CKEEP ( i64 -- i64 i64 ) [: 1+ ;] KEEP" GE-SRC-CHECK-LINE
   s" CBI ( i64 -- i64 i64 ) [: 1+ ;] [: 2 * ;] BI" GE-SRC-CHECK-LINE
   s" CTRI ( i64 -- i64 i64 i64 ) [: 1+ ;] [: 2 * ;] [: 3 + ;] TRI" GE-SRC-CHECK-LINE
   s" CTIMES ( i64 -- i64 ) 5 [: 1+ ;] TIMES" GE-SRC-CHECK-LINE
   s" CEACH ( i64 ptr i64 i64 -- i64 ) [: + ;] EACH" GE-SRC-CHECK-LINE
   s" CMAP ( ptr i64 i64 -- ) [: 1+ ;] MAP" GE-SRC-CHECK-LINE
   s" CFOLD ( ptr i64 i64 i64 -- i64 ) [: + ;] FOLD" GE-SRC-CHECK-LINE
   s" hb combinator/iterator verdicts" GE-EVAL-RUN-STDIN
   SB-RESET
   s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE
   s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE
   SB$ s" hb combinator/iterator verdicts output" GE-EXPECT-OUT ;

: GD-LOCAL-QUOT-CHECKS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" QLOCAL ( i64 -- i64 ) {: x:n :} [: x ;] execute" GE-SRC-CHECK-LINE
   s" hb rejects local capture in quotation" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 0" GE-OUT-LINE
   SB$ s" hb rejects local capture in quotation output" GE-EXPECT-OUT ;

: GD-LOCAL-QUOT-COMPILE-FAIL ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" 0 set-check" GE-SRC-LINE
   s" : QLOCAL {: x:n :} [: x ;] execute ;" GE-SRC-LINE
   75 s" x" s" hb compiler rejects local capture in quotation diagnostic" GE-EVAL-FORK-BAD ;

: GD-LOCAL-FIRST ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : LDUP ( n -- n ) {: dup:n :} dup ;" GE-SRC-LINE
   s" 7 LDUP ." GE-SRC-LINE
   s" : GD-LOCAL-I ( n -- n ) {: i:n :} i ;" GE-SRC-LINE
   s" 8 GD-LOCAL-I ." GE-SRC-LINE
   s" : LMASK ( n -- n ) $FF and ;" GE-SRC-LINE
   s" : LRD ( n n n -- n ) {: RD:n imm:n hw:n :} RD LMASK ;" GE-SRC-LINE
   s" 7 7 7 LRD ." GE-SRC-LINE
   s" hb locals resolve before dictionary and loop words" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 7" GE-OUT-LINE
   s" 8" GE-OUT-LINE
   s" 7" GE-OUT-LINE
   SB$ s" hb local-first output" GE-EXPECT-OUT ;

: GD-LITERAL-FIRST ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : 10 ( -- ) ;" GE-SRC-LINE
   s" : CALL10 ( -- n ) 10 ;" GE-SRC-LINE
   s" CALL10 ." GE-SRC-LINE
   s" hb numeric literals before dictionary lookup" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 10" GE-OUT-LINE
   SB$ s" hb numeric literal-first output" GE-EXPECT-OUT ;

: GD-NAMESPACE-QUALIFIED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GD-COUNT ( -- n ) 1 ;" GE-SRC-LINE
   s" : HB:GD-COUNT ( -- n ) 2 ;" GE-SRC-LINE
   s" package HBT" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" TRUSTED: GD-TRUSTED ( -- n ) 7 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" GD-COUNT ." GE-SRC-LINE
   s" HB:GD-COUNT ." GE-SRC-LINE
   s" USE-HB ( -- n ) HB:GD-COUNT" GE-SRC-CHECK-LINE
   s" USE-HBT-TRUSTED ( -- n ) HBT:GD-TRUSTED" GE-SRC-CHECK-LINE
   s" : HBCALL ( -- n ) HB:GD-COUNT ;" GE-SRC-LINE
   s" : HBTRUSTCALL ( -- n ) HBT:GD-TRUSTED ;" GE-SRC-LINE
   s" HBCALL ." GE-SRC-LINE
   s" HBTRUSTCALL ." GE-SRC-LINE
   s" hb:gd-count ." GE-SRC-LINE
   s" HB:GD-COUNT" GE-SRC-S"
   s"  0 search-wl 0= ." GE-SRC-LINE
   s" GD-COUNT" GE-SRC-S"
   s"  0 search-wl 0= ." GE-SRC-LINE
   s" : GD-EDGE: ( -- n ) 3 ;" GE-SRC-LINE
   s" GD-EDGE: ." GE-SRC-LINE
   s" GD-EDGE:" GE-SRC-S"
   s"  0 search-wl 0= ." GE-SRC-LINE
   s" hb wordlist namespace qualification" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 1" GE-OUT-LINE
   s" 2" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" 2" GE-OUT-LINE
   s" 7" GE-OUT-LINE
   s" 2" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   s" 3" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   SB$ s" hb wordlist namespace qualification output" GE-EXPECT-OUT ;

: GD-PACKAGE-SOURCE ( -- )
   GE-SRC-RESET
   s" package HB" GE-SRC-LINE
   s" : HIDDEN ( -- n ) 3 ;" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : EXPOSED ( -- n ) HIDDEN 4 + ;" GE-SRC-LINE
   s" private" GE-SRC-LINE
   s" : HIDDEN2 ( -- n ) EXPOSED 5 + ;" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : EXPOSED2 ( -- n ) HIDDEN2 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" HB:EXPOSED ." GE-SRC-LINE
   s" HB:EXPOSED2 ." GE-SRC-LINE
   s" : CALL-HB ( -- n ) HB:EXPOSED ;" GE-SRC-LINE
   s" CALL-HB ." GE-SRC-LINE
   s" hb:exposed ." GE-SRC-LINE
   s" hB:eXpOsEd ." GE-SRC-LINE
   s" EXPOSED" GE-SRC-S"
   s"  0 search-wl 0= ." GE-SRC-LINE
   s" HIDDEN" GE-SRC-S"
   s"  0 search-wl 0= ." GE-SRC-LINE
   s" package hb" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : MORE ( -- n ) EXPOSED 10 + ;" GE-SRC-LINE
   s" : AGAIN ( -- n ) HIDDEN 7 + ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" HB:MORE ." GE-SRC-LINE
   s" HB:AGAIN ." GE-SRC-LINE ;

: GD-PACKAGE-RUNTIME ( -- )
   GE-HB-RESET
   GD-PACKAGE-SOURCE
   s" hb package public/private/reopen" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 7" GE-OUT-LINE
   s" 12" GE-OUT-LINE
   s" 7" GE-OUT-LINE
   s" 7" GE-OUT-LINE
   s" 7" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" 17" GE-OUT-LINE
   s" 10" GE-OUT-LINE
   SB$ s" hb package public/private/reopen output" GE-EXPECT-OUT ;

: GD-PACKAGE-SEMICOLON ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" package GD-SPA" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : VALUE ( -- n ) 41 ;" GE-SRC-LINE
   s" ;PaCkAgE" GE-SRC-LINE
   s" : GD-SPA-AFTER ( -- n ) 1 ;" GE-SRC-LINE
   s" GD-SPA:VALUE GD-SPA-AFTER + ." GE-SRC-LINE
   s" hb semicolon package alias" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 42" GE-OUT-LINE
   SB$ s" hb semicolon package alias output" GE-EXPECT-OUT ;

: GD-PACKAGE-JIT-STACK-ISOLATION ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : FIVE-LITS ( -- n n n n n ) 10 10 10 10 10 ;" GE-SRC-LINE
   s" FIVE-LITS + + + + ." GE-SRC-LINE
   s" hb package cells do not overlap jit stack" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 50" GE-OUT-LINE
   SB$ s" hb package cells do not overlap jit stack output" GE-EXPECT-OUT ;

: GD-PACKAGE-CHECK-SOURCE ( -- )
   GE-SRC-RESET
   s" package CK" GE-SRC-LINE
   s" : HELP ( -- n ) 2 ;" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : EXPORTED ( -- n ) HELP 5 + ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" USE-CK ( -- n ) CK:EXPORTED" GE-SRC-CHECK-LINE
   s" BAD-CK ( -- n ) HELP" GE-SRC-CHECK-LINE ;

: GD-PACKAGE-CHECK-GOOD-BODY ( -- )
   s" package CK" GE-SRC-LINE
   s" : HELP ( -- n ) 2 ;" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : EXPORTED ( -- n ) HELP 5 + ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" : USE-CK ( -- n ) CK:EXPORTED ;" GE-SRC-LINE ;

: GD-CHECK-BUF-ACT ( -- )
   GD-CHECK-LABEL$ GE-SRC-BUF GE-SRC-U @ CHECK-ALL-ERRORS-BUF ;

: GD-CHECK-BUF-STORE ( n -- ) {: rc:n :}
   rc OUTCOME:EXITED GT-OUTCOME!
   0 GT-OUT-U !
   CHECK-ALL-ERRORS-OUT$ nip GT-ERR-U ! ;

: GD-CHECK-BUF-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   s" inprocess-check" GS-EVENT
   label labelu GD-CHECK-LABEL!
   GT-ERR-BUF GT-ERR-CAP GT-OUT-BUF GT-OUT-CAP CHECK-ALL-ERRORS-BUFFERS!
   0 0= 0= CHECK-ALL-ERRORS-JSON!
   [: GD-CHECK-BUF-ACT ;] catch GD-CHECK-BUF-STORE ;

: GD-CHECK-BUF-BAD ( n ptr u8 n ptr u8 n -- )
   {: rc:n needle:ptr needleu:n label:ptr labelu:n :}
   label labelu GD-CHECK-BUF-RUN
   rc label labelu GE-EXPECT-RC
   needle needleu label labelu GE-EXPECT-ERR-HAS
   label labelu GT-PROGRESS-PASS ;

: GD-CANDIDATE-ACT ( -- )
   GD-CANDIDATE$ CHECK-CANDIDATE! GD-CANDIDATE-VERDICT ! ;

: GD-CANDIDATE-RC ( n -- n ) {: rc:n :}
   rc 0 <> if rc exit then
   GD-CANDIDATE-VERDICT @ 0= if $46 exit then
   0 ;

: GD-CANDIDATE-STORE ( n -- ) {: rc:n :}
   rc GD-CANDIDATE-RC OUTCOME:EXITED GT-OUTCOME!
   0 GT-OUT-U !
   DIAG-BUFFER$ nip GT-ERR-U !
   DIAG-BUFFER-OFF ;

: GD-CANDIDATE-RUN ( ptr u8 n ptr u8 n -- ) {: body:ptr bodyu:n label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   s" inprocess-candidate" GS-EVENT
   body bodyu GD-CANDIDATE!
   -1 GD-CANDIDATE-VERDICT !
   GT-ERR-BUF GT-ERR-CAP DIAG-BUFFER!
   [: GD-CANDIDATE-ACT ;] catch GD-CANDIDATE-STORE ;

: GD-CANDIDATE-BAD ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: body:ptr bodyu:n rc:n needle:ptr needleu:n label:ptr labelu:n :}
   body bodyu label labelu GD-CANDIDATE-RUN
   rc label labelu GE-EXPECT-RC
   needle needleu label labelu GE-EXPECT-ERR-HAS
   label labelu GT-PROGRESS-PASS ;

: GD-CHECK-BAD-ALL ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GD-CHECK-BUF-RUN
   70 label labelu GE-EXPECT-RC
   label labelu GT-PROGRESS-PASS ;

: GD-CHECK-BUF-GOOD ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GD-CHECK-BUF-RUN
   label labelu GE-EXPECT-OK
   label labelu GE-EXPECT-SILENT
   label labelu GT-PROGRESS-PASS ;

: GD-PACKAGE-NORET-GOOD-BODY ( -- )
   s" package GD-NR" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : STOP ( -- ) 1 throw ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" : GD-NR-OK ( n -- n ) dup 0 < if GD-NR:STOP then 1 + ;" GE-SRC-LINE ;

: GD-PACKAGE-CHECK ( -- )
   GE-HB-RESET
   GD-PACKAGE-CHECK-SOURCE
   s" hb package checker scope" GE-EVAL-RUN-STDIN
   SB-RESET
   s" -1" GE-OUT-LINE
   s" 1" GE-OUT-LINE
   SB$ s" hb package checker scope output" GE-EXPECT-OUT ;

: GD-PACKAGE-NORET ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" package GD-NR" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : STOP ( -- ) 1 throw ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" : GD-NR-BAD ( n -- n ) dup 0 < if GD-NR:STOP 0 then 1 + ;" GE-SRC-LINE
   $46 s" gd-nr-bad" s" checker package no-return rejects live tail" GD-CHECK-BUF-BAD
   GE-SRC-RESET
   s" package GD-NRA" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : STOP ( -- ) 1 throw ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" package GD-NRB" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : STOP ( -- ) 2 throw ;" GE-SRC-LINE
   s" undefine STOP" GE-SRC-LINE
   s" : STOP ( -- n ) 7 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" : GD-NRA-BAD ( n -- n ) dup 0 < if GD-NRA:STOP 0 then 1 + ;" GE-SRC-LINE
   $46 s" gd-nra-bad" s" package undefine keeps other no-return symbol" GD-CHECK-BUF-BAD ;

: GD-RUN-BAD-SOURCE ( n ptr u8 n ptr u8 n -- )
   GE-EVAL-FORK-BAD ;

: GD-RUN-BAD-CHILD ( n ptr u8 n ptr u8 n -- )
   {: rc:n needle:ptr needleu:n label:ptr labelu:n :}
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   rc label labelu GE-EXPECT-RC
   needle needleu label labelu GE-EXPECT-ERR-HAS ;

: GD-DUPLICATE-DEFINITION-REJECTS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" package APP" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 1 ;" GE-SRC-LINE
   s" : RESET ( -- n ) 2 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   $4E s" RESET" s" package rejects duplicate public word" GD-RUN-BAD-SOURCE
   GE-SRC-RESET
   s" package APP" GE-SRC-LINE
   s" : H ( -- n ) 1 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" package APP" GE-SRC-LINE
   s" : H ( -- n ) 2 ;" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : GET ( -- n ) H ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   $4E s" duplicate definition" s" package rejects duplicate private word across reopen" GD-CHECK-BUF-BAD
   GE-SRC-RESET
   s" package APP" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 1 ;" GE-SRC-LINE
   s" : reset ( -- n ) 2 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   $4E s" duplicate definition" s" package rejects case-variant duplicate word" GD-CHECK-BUF-BAD
   GE-SRC-RESET
   s" package APP" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" variable CELL" GE-SRC-LINE
   s" variable cell" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   $4E s" cell" s" package rejects duplicate variable word" GD-RUN-BAD-SOURCE
   GE-SRC-RESET
   s" : RESET ( -- n ) 1 ;" GE-SRC-LINE
   s" : RESET ( -- n ) 2 ;" GE-SRC-LINE
   $4E s" duplicate definition" s" global wordlist rejects duplicate word" GD-CHECK-BUF-BAD
   GE-SRC-RESET
   s" : GD-HDUP ( -- n ) 1 ;" GE-SRC-LINE
   s" undefine GD-HDUP" GE-SRC-LINE
   s" : GD-HDUP ( -- n ) 2 ;" GE-SRC-LINE
   s" : GD-HDUP ( -- n ) 3 ;" GE-SRC-LINE
   $4E s" GD-HDUP" s" hash dup-probe rejects duplicate behind a retired slot" GD-RUN-BAD-SOURCE
   GE-SRC-RESET
   s" : dup ( n -- n n ) dup ;" GE-SRC-LINE
   $4E s" dup" s" primitive shadow attempt rejects without undefine" GD-RUN-BAD-SOURCE ;

: GD-EXPLICIT-REDEFINITION ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GD-RDEF ( -- n ) 1 ;" GE-SRC-LINE
   s" undefine GD-RDEF" GE-SRC-LINE
   s" : GD-RDEF ( -- n ) 2 ;" GE-SRC-LINE
   s" GD-RDEF ." GE-SRC-LINE
   s" package GD-RPKG" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 3 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" package GD-RPKG-B" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 6 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" package GD-RPKG" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" undefine RESET" GE-SRC-LINE
   s" : RESET ( -- n ) 4 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" GD-RPKG:RESET ." GE-SRC-LINE
   s" GD-RPKG-B:RESET ." GE-SRC-LINE
   s" defer GD-RV ( -- n )" GE-SRC-LINE
   s" undefine GD-RV" GE-SRC-LINE
   s" defer GD-RV ( -- n )" GE-SRC-LINE
   s" : GD-RV-FIVE ( -- n ) 5 ;" GE-SRC-LINE
   s" : GD-RV-INSTALL ( -- ) [: GD-RV-FIVE ;] is GD-RV ;" GE-SRC-LINE
   s" GD-RV-INSTALL GD-RV ." GE-SRC-LINE
   s" hb explicit undefine redefinition" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 2" GE-OUT-LINE
   s" 4" GE-OUT-LINE
   s" 6" GE-OUT-LINE
   s" 5" GE-OUT-LINE
   SB$ s" hb explicit undefine redefinition output" GE-EXPECT-OUT ;

: GD-EXPLICIT-REDEF-CHECK-BODY ( -- )
   s" : GD-CRED ( -- n ) 1 ;" GE-SRC-LINE
   s" undefine GD-CRED" GE-SRC-LINE
   s" : GD-CRED ( -- n ) 2 ;" GE-SRC-LINE
   s" : GD-CUSE ( -- n ) GD-CRED ;" GE-SRC-LINE
   s" defer GD-CDV ( -- n )" GE-SRC-LINE
   s" undefine GD-CDV" GE-SRC-LINE
   s" defer GD-CDV ( -- n )" GE-SRC-LINE
   s" : GD-CDV-FIVE ( -- n ) 5 ;" GE-SRC-LINE
   s" : GD-CDV-INSTALL ( -- ) [: GD-CDV-FIVE ;] is GD-CDV ;" GE-SRC-LINE
   s" : GD-CDV-CALL ( -- n ) GD-CDV-INSTALL GD-CDV ;" GE-SRC-LINE ;

: GD-PACKAGE-SHADOW-POSITIVES ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" package APP" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : COUNT ( -- n ) 7 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" package MK" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 2 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" package APP" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 1 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" APP:COUNT ." GE-SRC-LINE
   s" APP:RESET ." GE-SRC-LINE
   s" MK:RESET ." GE-SRC-LINE
   s" 3 dup + ." GE-SRC-LINE
   s" hb package duplicate positives" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 7" GE-OUT-LINE
   s" 1" GE-OUT-LINE
   s" 2" GE-OUT-LINE
   s" 6" GE-OUT-LINE
   SB$ s" hb package duplicate positives output" GE-EXPECT-OUT ;

: GD-PACKAGE-DUPLICATE-CHECK ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" package APP" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 1 ;" GE-SRC-LINE
   s" : RESET ( -- n ) 2 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   $4E s" duplicate definition" s" checker rejects package duplicate definition" GD-CHECK-BUF-BAD ;

: GD-RUN-LOAD-ONE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n label:ptr labelu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
   path pathu GE-ARG+
   s" --" GE-ARG+
   label labelu GT-PROGRESS-RUN
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: GD-RUN-LOAD-THREE ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
   GD-INC-CORE$ GE-ARG+
   GD-INC-API$ GE-ARG+
   GD-INC-MAIN$ GE-ARG+
   s" --" GE-ARG+
   label labelu GT-PROGRESS-RUN
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: GD-RUN-LOAD-BAD ( n ptr u8 n ptr u8 n -- )
   {: rc:n needle:ptr needleu:n label:ptr labelu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
   GD-INC-MAIN$ GE-ARG+
   s" --" GE-ARG+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   rc label labelu GE-EXPECT-RC
   needle needleu label labelu GE-EXPECT-ERR-HAS ;

: GD-EXPECT-GET9 ( ptr u8 n -- ) {: label:ptr labelu:n :}
   SB-RESET
   s" 9" GE-OUT-LINE
   SB$ label labelu GE-EXPECT-OUT ;

: GD-PACKAGE-MULTIFILE-LOAD ( -- )
   GD-INC-PATHS
   GD-WRITE-CORE
   GD-WRITE-API-NOINC
   GD-WRITE-MAIN-RUN
   s" hb package reopen across load files" GD-RUN-LOAD-THREE
   s" hb package reopen across load files output" GD-EXPECT-GET9 ;

: GD-PACKAGE-INCLUDE ( -- )
   GD-INC-PATHS
   GD-WRITE-CORE
   GD-WRITE-API-INCLUDE
   GD-WRITE-MAIN-INCLUDE
   GD-INC-MAIN$ s" hb include nested package source" GD-RUN-LOAD-ONE
   s" hb include nested package source output" GD-EXPECT-GET9
   GD-WRITE-MAIN-INCLUDED
   GD-INC-MAIN$ s" hb included string package source" GD-RUN-LOAD-ONE
   s" hb included string package source output" GD-EXPECT-GET9
   GD-WRITE-DUP
   SB-RESET
   GD-INC-DUP$ GD-SB-INCLUDE-LINE
   s" package APP" GD-SB-LINE
   s" : H ( -- n ) 2 ;" GD-SB-LINE
   s" end-package" GD-SB-LINE
   GD-INC-MAIN$ SB$ WRITE-ALL
   $4E s" duplicate definition" s" hb include rejects duplicate package reopen" GD-RUN-LOAD-BAD ;

: GD-PACKAGE-MISUSE ( -- )
   GE-HB-RESET
   GE-SRC-RESET  s" public" GE-SRC-LINE
   $4B s" public" s" package rejects public outside" GD-RUN-BAD-SOURCE
   GE-SRC-RESET  s" private" GE-SRC-LINE
   $4B s" private" s" package rejects private outside" GD-RUN-BAD-SOURCE
   GE-SRC-RESET  s" end-package" GE-SRC-LINE
   $4B s" end-package" s" package rejects end outside" GD-RUN-BAD-SOURCE
   GE-SRC-RESET  s" ;package" GE-SRC-LINE
   $4B s" ;package" s" package rejects semicolon closer outside" GD-RUN-BAD-SOURCE
   GE-SRC-RESET  s" package A" GE-SRC-LINE  s" package B" GE-SRC-LINE
   $4B s" package" s" package rejects nesting" GD-RUN-BAD-SOURCE
   GE-SRC-RESET  s" package" GE-SRC-LINE
   $4A s" package" s" package rejects missing name" GD-RUN-BAD-SOURCE
   GE-SRC-RESET  s" package A:B" GE-SRC-LINE
   $4B s" A:B" s" package rejects qualified name" GD-RUN-BAD-SOURCE
   GE-SRC-RESET
   s" package P" GE-SRC-LINE
   s" : H ( -- n ) 1 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" P:H ." GE-SRC-LINE
   $46 s" P:H" s" package hides private qualified word" GD-RUN-BAD-CHILD
   GE-SRC-RESET
   s" package P" GE-SRC-LINE
   s" : H ( -- n ) 1 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" : BAD ( -- n ) H ;" GE-SRC-LINE
   $46 s" at 'H'" s" package rejects private checked call" GD-RUN-BAD-CHILD
   GE-SRC-RESET
   s" package P" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : E ( -- n ) 1 ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" E ." GE-SRC-LINE
   $46 s" E" s" package hides public word from global lookup" GD-RUN-BAD-CHILD ;

: GD-STRUCTURE-SOURCE ( -- )
   GE-SRC-RESET
   s" BEGIN-STRUCTURE POINT" GE-SRC-LINE
   s" CELL +FIELD POINT.X" GE-SRC-LINE
   s" CELL +FIELD POINT.Y" GE-SRC-LINE
   s" PTR-FIELD: POINT.NAME" GE-SRC-LINE
   s" CFIELD: POINT.FLAGS" GE-SRC-LINE
   s" END-STRUCTURE" GE-SRC-LINE
   s" POINT ." GE-SRC-LINE
   s" create GD-POINT POINT allot" GE-SRC-LINE
   s" PTR-VARIABLE GD-PTR" GE-SRC-LINE
   s" 11 GD-POINT POINT.X !" GE-SRC-LINE
   s" 22 GD-POINT POINT.Y !" GE-SRC-LINE
   s" create GD-NAME $67 c, $64 c," GE-SRC-LINE
   s" GD-NAME GD-POINT POINT.NAME !" GE-SRC-LINE
   s" GD-NAME GD-PTR !" GE-SRC-LINE
   s" 123 GD-POINT POINT.FLAGS c!" GE-SRC-LINE
   s" GD-POINT POINT.X @ ." GE-SRC-LINE
   s" GD-POINT POINT.Y @ ." GE-SRC-LINE
   s" GD-POINT POINT.FLAGS c@ ." GE-SRC-LINE
   s" GD-POINT POINT.NAME @ 2 type cr" GE-SRC-LINE
   s" GD-PTR @ 2 type cr" GE-SRC-LINE
   s" : GD-USE-X ( ptr a -- ptr a ) POINT.X ;" GE-SRC-LINE
   s" : GD-USE-NAME ( ptr a -- ptr ptr u8 ) POINT.NAME ;" GE-SRC-LINE
   s" : GD-USE-PTR ( -- ptr ptr u8 ) GD-PTR ;" GE-SRC-LINE
   s" : GD-USE-FLAGS ( ptr a -- ptr u8 ) POINT.FLAGS ;" GE-SRC-LINE ;

: GD-STRUCTURES ( -- )
   GE-HB-RESET
   GD-STRUCTURE-SOURCE
   s" hb structures field layout and typing" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 25" GE-OUT-LINE
   s" 11" GE-OUT-LINE
   s" 22" GE-OUT-LINE
   s" 123" GE-OUT-LINE
   s" gd" GE-OUT-LINE
   s" gd" GE-OUT-LINE
   SB$ s" hb structures output" GE-EXPECT-OUT ;

: GD-STRUCTURE-CANDIDATE-SETUP ( -- )
   GE-EVAL-MARK
   GE-EVAL-CAPTURE
   s" structures byte-field checker setup" GE-EXPECT-OK
   s" structures byte-field checker setup" GE-EXPECT-SILENT ;

: GD-STRUCTURE-BAD-CANDIDATE ( -- )
   s" GD-BAD-FLAGS ( ptr a -- n ) POINT.FLAGS @"
   $46 s" gd-bad-flags" s" structures reject byte field cell load" GD-CANDIDATE-BAD ;

: GD-STRUCTURE-MISUSE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" CELL +FIELD GD-NO-STRUCT.FIELD" GE-SRC-LINE
   $4C s" structure: no active structure" s" structures reject field outside begin" GD-RUN-BAD-SOURCE
   GE-SRC-RESET
   s" BEGIN-STRUCTURE OUTER" GE-SRC-LINE
   s" BEGIN-STRUCTURE INNER" GE-SRC-LINE
   $4C s" structure: nested begin" s" structures reject nesting" GD-RUN-BAD-SOURCE
   GE-SRC-RESET
   s" BEGIN-STRUCTURE POINT" GE-SRC-LINE
   s" CFIELD: POINT.FLAGS" GE-SRC-LINE
   s" END-STRUCTURE" GE-SRC-LINE
   GD-STRUCTURE-CANDIDATE-SETUP
   GD-STRUCTURE-BAD-CANDIDATE
   GE-EVAL-FORGET ;

: GD-ENUMS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" 0 ENUM+ GD-E0 ENUM+ GD-E1 ENUM4+ GD-E4 ENUM+ GD-E8 drop" GE-SRC-LINE
   s" GD-E0 ." GE-SRC-LINE
   s" GD-E1 ." GE-SRC-LINE
   s" GD-E4 ." GE-SRC-LINE
   s" GD-E8 ." GE-SRC-LINE
   s" : GD-USE-E4 ( -- n ) GD-E4 ;" GE-SRC-LINE
   s" GD-USE-E4 ." GE-SRC-LINE
   s" package GD-EV" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" 100 ENUM+ CODE-A ENUM4+ CODE-B drop" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" GD-EV:CODE-A ." GE-SRC-LINE
   s" GD-EV:CODE-B ." GE-SRC-LINE
   s" hb enums sequence and package scope" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 0" GE-OUT-LINE
   s" 1" GE-OUT-LINE
   s" 2" GE-OUT-LINE
   s" 6" GE-OUT-LINE
   s" 2" GE-OUT-LINE
   s" 100" GE-OUT-LINE
   s" 101" GE-OUT-LINE
   SB$ s" hb enums output" GE-EXPECT-OUT
   GE-SRC-RESET
   s" 0 ENUM+ GD-EDUP ENUM+ GD-EDUP drop" GE-SRC-LINE
   $4E s" GD-EDUP" s" enums reject duplicate constant names" GD-RUN-BAD-SOURCE ;

: GD-EXEC-VECTOR-SOURCE ( -- )
   GE-SRC-RESET
   s" defer GD-XV-ACTION ( -- i64 )" GE-SRC-LINE
   s" : GD-XV-FIVE ( -- i64 ) 5 ;" GE-SRC-LINE
   s" : GD-XV-SEVEN ( -- i64 ) 7 ;" GE-SRC-LINE
   s" : GD-XV-INSTALL-FIVE ( -- ) [: GD-XV-FIVE ;] is GD-XV-ACTION ;" GE-SRC-LINE
   s" : GD-XV-INSTALL-SEVEN ( -- ) [: GD-XV-SEVEN ;] is GD-XV-ACTION ;" GE-SRC-LINE
   s" GD-XV-INSTALL-FIVE" GE-SRC-LINE
   s" GD-XV-ACTION ." GE-SRC-LINE
   s" GD-XV-INSTALL-SEVEN" GE-SRC-LINE
   s" GD-XV-ACTION ." GE-SRC-LINE ;

: GD-EXEC-VECTOR-CHECK-BODY ( -- )
   s" defer GD-XV-ACTION ( -- i64 )" GE-SRC-LINE
   s" : GD-XV-FIVE ( -- i64 ) 5 ;" GE-SRC-LINE
   s" : GD-XV-INSTALL-FIVE ( -- ) [: GD-XV-FIVE ;] is GD-XV-ACTION ;" GE-SRC-LINE
   s" : GD-XV-CALL ( -- i64 ) GD-XV-INSTALL-FIVE GD-XV-ACTION ;" GE-SRC-LINE ;

: GD-EXEC-VECTOR-READER-CHECK-BODY ( -- )
   s" defer GD-XV-READER ( -- ptr u8 n )" GE-SRC-LINE
   s" : GD-XV-LINE ( -- ptr u8 n ) 0 script-argv$ ;" GE-SRC-LINE
   s" : GD-XV-ENABLE ( -- ) [: GD-XV-LINE ;] is GD-XV-READER ;" GE-SRC-LINE
   s" : GD-XV-READ ( -- ptr u8 n ) GD-XV-ENABLE GD-XV-READER ;" GE-SRC-LINE ;

: GD-EXEC-VECTORS ( -- )
   GE-HB-RESET
   GD-EXEC-VECTOR-SOURCE
   s" hb checked execution vectors" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 5" GE-OUT-LINE
   s" 7" GE-OUT-LINE
   SB$ s" hb checked execution vectors output" GE-EXPECT-OUT ;

: GD-EXEC-VECTOR-PACKAGE-CHECK-BODY ( -- )
   s" package GDXV" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" defer RUN ( -- i64 )" GE-SRC-LINE
   s" : FIVE ( -- i64 ) 5 ;" GE-SRC-LINE
   s" : INSTALL ( -- ) [: FIVE ;] is RUN ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" : GD-XV-PKG-CALL ( -- i64 ) GDXV:INSTALL GDXV:RUN ;" GE-SRC-LINE ;

: GD-EXEC-VECTOR-PACKAGE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" package GDXV" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" defer RUN ( -- i64 )" GE-SRC-LINE
   s" : FIVE ( -- i64 ) 5 ;" GE-SRC-LINE
   s" : INSTALL ( -- ) [: FIVE ;] is RUN ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" GDXV:INSTALL" GE-SRC-LINE
   s" GDXV:RUN ." GE-SRC-LINE
   s" hb package execution vector" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 5" GE-OUT-LINE
   SB$ s" hb package execution vector output" GE-EXPECT-OUT ;

: GD-EXEC-VECTOR-MISUSE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" defer GD-XV-UNSET ( -- i64 )" GE-SRC-LINE
   s" GD-XV-UNSET ." GE-SRC-LINE
   $4C s" defer: unset execution vector" s" execution vector rejects unset call" GD-RUN-BAD-SOURCE
   GE-SRC-RESET
   s" defer GD-XV-ACTION-A ( -- i64 )" GE-SRC-LINE
   s" : GD-XV-BAD-A ( -- ) [: 1 2 ;] is GD-XV-ACTION-A ;" GE-SRC-LINE
   s" defer GD-XV-ACTION-B ( -- i64 )" GE-SRC-LINE
   s" : GD-XV-FIVE-B ( -- i64 ) 5 ;" GE-SRC-LINE
   s" : GD-XV-BAD-TICK-B ( -- ) ['] GD-XV-FIVE-B is GD-XV-ACTION-B ;" GE-SRC-LINE
   s" : GD-XV-NOT-DEFER-C ( -- ) ;" GE-SRC-LINE
   s" : GD-XV-BAD-TARGET-C ( -- ) [: ;] is GD-XV-NOT-DEFER-C ;" GE-SRC-LINE
   s" defer GD-XV-READER-D ( -- ptr u8 n )" GE-SRC-LINE
   s" : GD-XV-BAD-READER-D ( -- ) [: 1 ;] is GD-XV-READER-D ;" GE-SRC-LINE
   s" check.f rejects execution vector misuse batch" GD-CHECK-BAD-ALL
   s" gd-xv-bad-a" s" check.f rejects effect-mismatched execution vector assignment" GE-EXPECT-ERR-HAS
   s" gd-xv-bad-tick-b" s" check.f rejects raw xt execution vector assignment" GE-EXPECT-ERR-HAS
   s" gd-xv-bad-target-c" s" check.f rejects non-defer execution vector target" GE-EXPECT-ERR-HAS
   s" gd-xv-bad-reader-d" s" check.f rejects reader-shaped effect mismatch" GE-EXPECT-ERR-HAS
   GE-SRC-RESET
   s" : GD-XV-NOT-DEFER ( -- ) ;" GE-SRC-LINE
   s" : GD-XV-BAD-TARGET ( -- ) [: ;] is GD-XV-NOT-DEFER ;" GE-SRC-LINE
   $4C s" GD-XV-NOT-DEFER" s" execution vector rejects non-defer target" GD-RUN-BAD-SOURCE ;

: GD-CASE-BODY ( -- )
   s" : GD-CASE-PICK ( n -- n ) case 1 of 10 endof 2 of 20 endof 30 swap endcase ;" GE-SRC-LINE
   s" : GD-CASE-NEST ( n n -- n ) {: inner:n outer:n :}" GE-SRC-LINE
   s"    outer case" GE-SRC-LINE
   s"       1 of inner case 5 of 15 endof 16 swap endcase endof" GE-SRC-LINE
   s"       2 of 20 endof" GE-SRC-LINE
   s"       99 swap" GE-SRC-LINE
   s"    endcase ;" GE-SRC-LINE
   s" package GDCASE" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : PICK ( n -- n ) case 7 of 70 endof 80 swap endcase ;" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" : GD-CASE-PKG ( n -- n ) GDCASE:PICK ;" GE-SRC-LINE ;

: GD-CASE-SOURCE ( -- )
   GE-SRC-RESET
   GD-CASE-BODY ;

: GD-CASES ( -- )
   GE-HB-RESET
   GD-CASE-SOURCE
   s" 1 GD-CASE-PICK ." GE-SRC-LINE
   s" 2 GD-CASE-PICK ." GE-SRC-LINE
   s" 9 GD-CASE-PICK ." GE-SRC-LINE
   s" 5 1 GD-CASE-NEST ." GE-SRC-LINE
   s" 4 1 GD-CASE-NEST ." GE-SRC-LINE
   s" 0 2 GD-CASE-NEST ." GE-SRC-LINE
   s" 9 3 GD-CASE-NEST ." GE-SRC-LINE
   s" 7 GD-CASE-PKG ." GE-SRC-LINE
   s" 8 GD-CASE-PKG ." GE-SRC-LINE
   s" hb checked case control" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 10" GE-OUT-LINE
   s" 20" GE-OUT-LINE
   s" 30" GE-OUT-LINE
   s" 15" GE-OUT-LINE
   s" 16" GE-OUT-LINE
   s" 20" GE-OUT-LINE
   s" 99" GE-OUT-LINE
   s" 70" GE-OUT-LINE
   s" 80" GE-OUT-LINE
   SB$ s" hb checked case control output" GE-EXPECT-OUT ;

: GD-CASE-MISUSE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GD-CASE-BAD-ARM ( n -- n ) case 1 of 10 11 endof 20 swap endcase ;" GE-SRC-LINE
   s" : GD-CASE-MISSING-END ( n -- n ) case 1 of 10 endof ;" GE-SRC-LINE
   s" : GD-CASE-ORPHAN-OF ( n -- n ) 1 of 2 endof ;" GE-SRC-LINE
   s" check.f rejects case misuse batch" GD-CHECK-BAD-ALL
   s" gd-case-bad-arm" s" check.f rejects case effect mismatch" GE-EXPECT-ERR-HAS
   s" gd-case-missing-end" s" check.f rejects unterminated case" GE-EXPECT-ERR-HAS
   s" gd-case-orphan-of" s" check.f rejects of outside case" GE-EXPECT-ERR-HAS ;

: GD-PARSING-RUNTIME-SOURCE ( -- )
   GE-SRC-RESET
   s" hi" GD-SRC-DOTQ s"  cr" GE-SRC-LINE
   s" ok" GD-SRC-CQ s"  count type cr" GE-SRC-LINE
   s" : DQ ( -- ) " GE-SRC+ s" bye" GD-SRC-DOTQ s"  ;" GE-SRC-LINE
   s" DQ cr" GE-SRC-LINE
   s" : CQ ( -- ptr u8 n ) " GE-SRC+ s" yo" GD-SRC-CQ s"  count ;" GE-SRC-LINE
   s" CQ type cr" GE-SRC-LINE ;

: GD-PARSING-RUNTIME ( -- )
   GE-HB-RESET
   GD-PARSING-RUNTIME-SOURCE
   s" hb parsing-word runtime surface" GE-EVAL-RUN-STDIN
   SB-RESET
   s" hi" GE-OUT-LINE s" ok" GE-OUT-LINE s" bye" GE-OUT-LINE s" yo" GE-OUT-LINE
   SB$ s" hb parsing-word runtime surface output" GE-EXPECT-OUT ;

: GD-PARSING-CHECK-BODY ( -- )
   s" : DQ ( -- ) " GE-SRC+ s" ok" GD-SRC-DOTQ s"  ;" GE-SRC-LINE
   s" : CQ ( -- ptr u8 n ) " GE-SRC+ s" ok" GD-SRC-CQ s"  count ;" GE-SRC-LINE ;

: GD-CHECK-POSITIVE-BATCH ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   GD-PACKAGE-CHECK-GOOD-BODY
   GD-PACKAGE-NORET-GOOD-BODY
   GD-EXPLICIT-REDEF-CHECK-BODY
   GD-EXEC-VECTOR-CHECK-BODY
   GD-EXEC-VECTOR-READER-CHECK-BODY
   GD-EXEC-VECTOR-PACKAGE-CHECK-BODY
   GD-CASE-BODY
   GD-PARSING-CHECK-BODY
   s" check.f dictionary positive certification batch" GD-CHECK-BUF-GOOD ;

: GD-DATA-OVERFLOW ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" $10000000001 allot" GE-SRC-LINE
   76 s" " s" data-space overflow rc" GE-EVAL-FORK-BAD ;

: GD-NAMED-ROW-RUN ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : PSH ( R -- R i64 ) 5 ;" GE-SRC-LINE
   s" PSH ." GE-SRC-LINE
   s" hb named-row sig run" GE-EVAL-RUN-STDIN
   SB-RESET s" 5" GE-OUT-LINE
   SB$ s" hb named-row sig run output" GE-EXPECT-OUT ;

: GD-XREF ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" lib/test.f" GE-SRC-FILE+
   s" tools/xref-test.f" GE-SRC-FILE+
   s" hb native xref words" GE-EVAL-RUN-STDIN
   s" xref-test: ok" s" hb native xref words output" GE-EXPECT-OUT-HAS ;

: GD-MAIN ( -- )
   s" hb-gate-dictionary" GT-START
   s" dictionary/long-dictionary" [: GD-LONG-DICTIONARY ;] GD-RUN
   s" dictionary/wordlist" [: GD-WORDLIST ;] GD-RUN
   s" dictionary/long-name" [: GD-LONG-NAME ;] GD-RUN
   s" dictionary/trusted-does" [: GD-TRUSTED-DOES ;] GD-RUN
   s" dictionary/bad-does" [: GD-BAD-DOES ;] GD-RUN
   s" dictionary/row-quot" [: GD-ROW-QUOT-CHECKS ;] GD-RUN
   s" dictionary/primitives" [: GD-PRIMITIVE-CHECKS ;] GD-RUN
   s" dictionary/return" [: GD-RETURN-CHECKS ;] GD-RUN
   s" dictionary/combinators" [: GD-COMBINATOR-CHECKS ;] GD-RUN
   s" dictionary/local-quot" [: GD-LOCAL-QUOT-CHECKS ;] GD-RUN
   s" dictionary/local-quot-compile" [: GD-LOCAL-QUOT-COMPILE-FAIL ;] GD-RUN
   s" dictionary/local-first" [: GD-LOCAL-FIRST ;] GD-RUN
   s" dictionary/literal-first" [: GD-LITERAL-FIRST ;] GD-RUN
   s" dictionary/namespace" [: GD-NAMESPACE-QUALIFIED ;] GD-RUN
   s" dictionary/package-runtime" [: GD-PACKAGE-RUNTIME ;] GD-RUN
   s" dictionary/package-semicolon" [: GD-PACKAGE-SEMICOLON ;] GD-RUN
   s" dictionary/package-jit-stack" [: GD-PACKAGE-JIT-STACK-ISOLATION ;] GD-RUN
   s" dictionary/package-check" [: GD-PACKAGE-CHECK ;] GD-RUN
   s" dictionary/package-noret" [: GD-PACKAGE-NORET ;] GD-RUN
   s" dictionary/duplicate" [: GD-DUPLICATE-DEFINITION-REJECTS ;] GD-RUN
   s" dictionary/redefine" [: GD-EXPLICIT-REDEFINITION ;] GD-RUN
   s" dictionary/package-shadow" [: GD-PACKAGE-SHADOW-POSITIVES ;] GD-RUN
   s" dictionary/package-duplicate-check" [: GD-PACKAGE-DUPLICATE-CHECK ;] GD-RUN
   s" dictionary/package-multifile" [: GD-PACKAGE-MULTIFILE-LOAD ;] GD-RUN
   s" dictionary/package-include" [: GD-PACKAGE-INCLUDE ;] GD-RUN
   s" dictionary/package-misuse" [: GD-PACKAGE-MISUSE ;] GD-RUN
   s" dictionary/structures" [: GD-STRUCTURES ;] GD-RUN
   s" dictionary/structure-misuse" [: GD-STRUCTURE-MISUSE ;] GD-RUN
   s" dictionary/enums" [: GD-ENUMS ;] GD-RUN
   s" dictionary/exec-vectors" [: GD-EXEC-VECTORS ;] GD-RUN
   s" dictionary/exec-vector-package" [: GD-EXEC-VECTOR-PACKAGE ;] GD-RUN
   s" dictionary/exec-vector-misuse" [: GD-EXEC-VECTOR-MISUSE ;] GD-RUN
   s" dictionary/case" [: GD-CASES ;] GD-RUN
   s" dictionary/case-misuse" [: GD-CASE-MISUSE ;] GD-RUN
   s" dictionary/parsing-runtime" [: GD-PARSING-RUNTIME ;] GD-RUN
   s" dictionary/check-positive-batch" [: GD-CHECK-POSITIVE-BATCH ;] GD-RUN
   s" dictionary/data-overflow" [: GD-DATA-OVERFLOW ;] GD-RUN
   s" dictionary/named-row" [: GD-NAMED-ROW-RUN ;] GD-RUN
   s" dictionary/xref" [: GD-XREF ;] GD-RUN
   GT-CLEANUP
   s" PASS: native dictionary/checker gate phase" type cr ;
