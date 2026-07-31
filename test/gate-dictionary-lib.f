\ gate-dictionary.f - checked runner for dictionary and checker contracts.
\
\ Load after test/gate-common.f and tools/check-all-errors-core.f.

package GATE-DICTIONARY

$41 constant NAME-CHAR
$2E constant DOT-CHAR
$63 constant C-LOWER-CHAR
600 constant LONG-NAME-LEN

create INC-CORE FS-PATH-CAP allot
create INC-API FS-PATH-CAP allot
create INC-MAIN FS-PATH-CAP allot
create INC-DUP FS-PATH-CAP allot

variable INC-CORE-U
variable INC-API-U
variable INC-MAIN-U
variable INC-DUP-U
variable LABEL-A
variable LABEL-U
variable CANDIDATE-A
variable CANDIDATE-U
variable CANDIDATE-VERDICT
variable START-NS

: LABEL-A-FIELD ( -- ptr ptr u8 )
   LABEL-A 0 ptr-field ;

: CANDIDATE-A-FIELD ( -- ptr ptr u8 )
   CANDIDATE-A 0 ptr-field ;

: LABEL$ ( -- ptr u8 n )
   LABEL-A-FIELD @ LABEL-U @ ;

: CANDIDATE$ ( -- ptr u8 n )
   CANDIDATE-A-FIELD @ CANDIDATE-U @ ;

: LABEL! ( ptr u8 n -- ) {: a:ptr u:n :}
   u LABEL-U !
   a LABEL-A-FIELD ! ;

: CANDIDATE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CANDIDATE-U !
   a CANDIDATE-A-FIELD ! ;

\ typed-local-lint: allow-bare-local - q carries the case action effect.
: CASE-RUN ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   mono-ns START-NS !
   q execute
   label labelu mono-ns START-NS @ - PROC-NS-PER-MS / GS-SPAN ;

: EMIT-LONG-NAME ( -- )
   LONG-NAME-LEN NAME-CHAR GE-SRC-REPEAT-C ;

: LONG-NAME-S" ( -- )
   s" s" GE-SRC+
   GE-DQ GE-SRC-C
   GE-SRC-SP
   EMIT-LONG-NAME
   GE-DQ GE-SRC-C ;

: SRC-DOTQ ( ptr u8 n -- ) {: a:ptr u:n :}
   DOT-CHAR GE-SRC-C
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: SRC-CQ ( ptr u8 n -- ) {: a:ptr u:n :}
   C-LOWER-CHAR GE-SRC-C
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: INC-CORE$ ( -- ptr u8 n )
   INC-CORE INC-CORE-U @ ;

: INC-API$ ( -- ptr u8 n )
   INC-API INC-API-U @ ;

: INC-MAIN$ ( -- ptr u8 n )
   INC-MAIN INC-MAIN-U @ ;

: INC-DUP$ ( -- ptr u8 n )
   INC-DUP INC-DUP-U @ ;

: INC-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu:n dst:ptr up:ptr :}
   GT-ROOT name nameu dst JOIN-PATH up ! ;

: INC-PATHS ( -- )
   s" app-core.f" INC-CORE INC-CORE-U INC-PATH!
   s" app-api.f" INC-API INC-API-U INC-PATH!
   s" app-main.f" INC-MAIN INC-MAIN-U INC-PATH!
   s" app-dup.f" INC-DUP INC-DUP-U INC-PATH! ;

: SB-LINE ( ptr u8 n -- )
   SB-APPEND
   GE-LF SB-APPEND-C ;

: SB-INCLUDE-LINE ( ptr u8 n -- )
   s" include " SB-APPEND
   SB-APPEND
   GE-LF SB-APPEND-C ;

: SB-S" ( ptr u8 n -- )
   s" s" SB-APPEND
   GE-DQ SB-APPEND-C
   GE-SP SB-APPEND-C
   SB-APPEND
   GE-DQ SB-APPEND-C ;

: WRITE-CORE ( -- )
   SB-RESET
   s" package APP" SB-LINE
   s" : H ( -- n ) 9 ;" SB-LINE
   s" public" SB-LINE
   s" : CORE ( -- n ) H ;" SB-LINE
   s" ;package" SB-LINE
   INC-CORE$ SB$ WRITE-ALL ;

: WRITE-API-BODY ( -- )
   s" package APP" SB-LINE
   s" public" SB-LINE
   s" : GET ( -- n ) H ;" SB-LINE
   s" ;package" SB-LINE ;

: WRITE-API-NOINC ( -- )
   SB-RESET
   WRITE-API-BODY
   INC-API$ SB$ WRITE-ALL ;

: WRITE-API-INCLUDE ( -- )
   SB-RESET
   INC-CORE$ SB-INCLUDE-LINE
   WRITE-API-BODY
   INC-API$ SB$ WRITE-ALL ;

: WRITE-MAIN-RUN ( -- )
   SB-RESET
   s" APP:GET ." SB-LINE
   INC-MAIN$ SB$ WRITE-ALL ;

: WRITE-MAIN-INCLUDE ( -- )
   SB-RESET
   INC-API$ SB-INCLUDE-LINE
   s" APP:GET ." SB-LINE
   INC-MAIN$ SB$ WRITE-ALL ;

: WRITE-MAIN-INCLUDED ( -- )
   SB-RESET
   INC-API$ SB-S"
   s"  included" SB-LINE
   s" APP:GET ." SB-LINE
   INC-MAIN$ SB$ WRITE-ALL ;

: WRITE-DUP ( -- )
   SB-RESET
   s" package APP" SB-LINE
   s" : H ( -- n ) 1 ;" SB-LINE
   s" ;package" SB-LINE
   INC-DUP$ SB$ WRITE-ALL ;

: LONG-DICTIONARY-SOURCE ( -- )
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

: LONG-DICTIONARY ( -- )
   GE-HB-RESET
   LONG-DICTIONARY-SOURCE
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

: WORDLIST-SOURCE ( -- )
   GE-SRC-RESET
   s" wordlist constant LONG-WL" GE-SRC-LINE
   s" LONG-WL set-current" GE-SRC-LINE
   s" : LONG-WORDLIST-ONLY-NAME ( -- i64 ) 8 ;" GE-SRC-LINE
   s" 0 set-current" GE-SRC-LINE
   s" LONG-WORDLIST-ONLY-NAME" GE-SRC-S"
   s"  0 search-wl 0= ." GE-SRC-LINE
   s" LONG-WORDLIST-ONLY-NAME" GE-SRC-S"
   s"  LONG-WL search-wl 0= ." GE-SRC-LINE ;

: WORDLIST ( -- )
   GE-HB-RESET
   WORDLIST-SOURCE
   s" hb long dictionary wordlist isolation" GE-EVAL-RUN-STDIN
   SB-RESET
   s" -1" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   SB$ s" hb long dictionary wordlist isolation output" GE-EXPECT-OUT ;

: LONG-NAME-SOURCE ( -- )
   GE-SRC-RESET
   s" : " GE-SRC+
   EMIT-LONG-NAME
   s"  ( -- n ) 1 ;" GE-SRC-LINE
   EMIT-LONG-NAME
   s"  ." GE-SRC-LINE
   LONG-NAME-S"
   s"  get-current search-wl 0= ." GE-SRC-LINE ;

: LONG-NAME ( -- )
   GE-HB-RESET
   LONG-NAME-SOURCE
   s" hb dictionary name over 255 bytes" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 1" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   SB$ s" hb dictionary name over 255 bytes output" GE-EXPECT-OUT ;

: TRUSTED-DOES-SOURCE ( -- )
   GE-SRC-RESET
   s" TRUSTED: ARR ( n -- ) create cells allot does> ( n -- ptr a ) swap 0 ?do cell+ loop ;" GE-SRC-LINE
   s" 4 ARR A4" GE-SRC-LINE
   s" USE ( n -- ptr a ) A4" GE-SRC-CHECK-LINE
   s" 7 2 A4 !" GE-SRC-LINE
   s" 2 A4 @ ." GE-SRC-LINE ;

: TRUSTED-DOES ( -- )
   GE-HB-RESET
   TRUSTED-DOES-SOURCE
   s" hb trusted CREATE...DOES> effect recording" GE-EVAL-RUN-STDIN
   SB-RESET
   s" -1" GE-OUT-LINE
   s" 7" GE-OUT-LINE
   SB$ s" hb trusted CREATE...DOES> effect recording output" GE-EXPECT-OUT ;

: BAD-DOES ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" TRUSTED: BADARR ( n -- ) create cells allot does> ( n -- ptr a ) drop ;" GE-SRC-LINE
   70 s" does>" s" hb bad trusted DOES> diagnostic" GE-EVAL-FORK-BAD
   GE-HB-RESET
   GE-SRC-RESET
   s" TRUSTED: BADDEF ( n -- ) create cells allot does> drop ;" GE-SRC-LINE
   76 s" does>" s" hb trusted DOES> without created signature diagnostic" GE-EVAL-FORK-BAD ;

: ROW-QUOT-CHECKS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" V1 ( R -- R i64 ) 5" GE-SRC-CHECK-LINE
   s" V2 ( i64 [ i64 -- i64 ] -- i64 ) execute" GE-SRC-CHECK-LINE
   s" V3 ( R -- R i64 ) 5 5" GE-SRC-CHECK-LINE
   s" hb rows/quot sig verify" GE-EVAL-RUN-STDIN
   SB-RESET s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE s" 0" GE-OUT-LINE
   SB$ s" hb rows/quot sig verify output" GE-EXPECT-OUT ;

: PRIMITIVE-CHECKS ( -- )
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

: RETURN-CHECKS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" RBAD1 ( i64 i64 -- ) 2>r" GE-SRC-CHECK-LINE
   s" RBAD2 ( -- i64 i64 ) 2r>" GE-SRC-CHECK-LINE
   s" RPEEK ( i64 i64 -- i64 i64 i64 i64 ) 2>r 2r@ 2r>" GE-SRC-CHECK-LINE
   s" QD ( i64 -- i64 i64 ) ?dup" GE-SRC-CHECK-LINE
   s" hb return-stack/?dup primitive verdicts" GE-EVAL-RUN-STDIN
   SB-RESET s" 0" GE-OUT-LINE s" 0" GE-OUT-LINE s" -1" GE-OUT-LINE s" 1" GE-OUT-LINE
   SB$ s" hb return-stack/?dup primitive verdicts output" GE-EXPECT-OUT ;

: COMBINATOR-CHECKS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" CDIP ( i64 i64 -- i64 i64 ) [: 1+ ;] DIP" GE-SRC-CHECK-LINE
   s" CKEEP ( i64 -- i64 i64 ) [: 1+ ;] KEEP" GE-SRC-CHECK-LINE
   s" CBI ( i64 -- i64 i64 ) [: 1+ ;] [: 2 * ;] BI" GE-SRC-CHECK-LINE
   s" CTRI ( i64 -- i64 i64 i64 ) [: 1+ ;] [: 2 * ;] [: 3 + ;] TRI" GE-SRC-CHECK-LINE
   s" CTIMES ( i64 -- i64 ) 5 [: 1+ ;] TIMES" GE-SRC-CHECK-LINE
   \ The array's element and the quotation's operand are ONE type in EACH/MAP/FOLD
   \ (`ptr a` with `[ .. a .. ]`). `[: + ;]` and `[: 1+ ;]` are `n` arithmetic, so
   \ the array is `ptr n`; `ptr i64` named a different element and only certified
   \ while a pointee still admitted one integer type for another.
   s" CEACH ( i64 ptr n i64 -- i64 ) [: + ;] EACH" GE-SRC-CHECK-LINE
   s" CMAP ( ptr n i64 -- ) [: 1+ ;] MAP" GE-SRC-CHECK-LINE
   s" CFOLD ( ptr n i64 i64 -- i64 ) [: + ;] FOLD" GE-SRC-CHECK-LINE
   s" hb combinator/iterator verdicts" GE-EVAL-RUN-STDIN
   SB-RESET
   s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE
   s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE
   SB$ s" hb combinator/iterator verdicts output" GE-EXPECT-OUT ;

: LOCAL-QUOT-CHECKS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" QLOCAL ( i64 -- i64 ) {: x:n :} [: x ;] execute" GE-SRC-CHECK-LINE
   s" hb rejects local capture in quotation" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 0" GE-OUT-LINE
   SB$ s" hb rejects local capture in quotation output" GE-EXPECT-OUT ;

: LOCAL-QUOT-FAIL ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" 0 set-check" GE-SRC-LINE
   s" : QLOCAL {: x:n :} [: x ;] execute ;" GE-SRC-LINE
   75 s" x" s" hb compiler rejects local capture in quotation diagnostic" GE-EVAL-FORK-BAD ;

: LOCAL-FIRST ( -- )
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

: LITERAL-FIRST ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : 10 ( -- ) ;" GE-SRC-LINE
   s" : CALL10 ( -- n ) 10 ;" GE-SRC-LINE
   s" CALL10 ." GE-SRC-LINE
   s" hb numeric literals before dictionary lookup" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 10" GE-OUT-LINE
   SB$ s" hb numeric literal-first output" GE-EXPECT-OUT ;

: NAMESPACE-QUALIFIED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GD-COUNT ( -- n ) 1 ;" GE-SRC-LINE
   s" : HB:GD-COUNT ( -- n ) 2 ;" GE-SRC-LINE
   s" package hb" GE-SRC-LINE
   s" : GD-HIDDEN ( -- n ) 5 ;" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : GD-PUBLIC ( -- n ) GD-HIDDEN HB:GD-COUNT + ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" package HBT" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" TRUSTED: GD-TRUSTED ( -- n ) 7 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" GD-COUNT ." GE-SRC-LINE
   s" HB:GD-COUNT ." GE-SRC-LINE
   s" HB:GD-PUBLIC ." GE-SRC-LINE
   s" GD-HIDDEN" GE-SRC-S"
   s"  0 search-wl 0= ." GE-SRC-LINE
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
   s" hb wordlist namespace qualification" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 1" GE-OUT-LINE
   s" 2" GE-OUT-LINE
   s" 7" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" 2" GE-OUT-LINE
   s" 7" GE-OUT-LINE
   s" 2" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   SB$ s" hb wordlist namespace qualification output" GE-EXPECT-OUT ;

: PACKAGE-SOURCE ( -- )
   GE-SRC-RESET
   s" package HB" GE-SRC-LINE
   s" : HIDDEN ( -- n ) 3 ;" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : EXPOSED ( -- n ) HIDDEN 4 + ;" GE-SRC-LINE
   s" private" GE-SRC-LINE
   s" : HIDDEN2 ( -- n ) EXPOSED 5 + ;" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : EXPOSED2 ( -- n ) HIDDEN2 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
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
   s" ;package" GE-SRC-LINE
   s" HB:MORE ." GE-SRC-LINE
   s" HB:AGAIN ." GE-SRC-LINE ;

: PACKAGE-RUNTIME ( -- )
   GE-HB-RESET
   PACKAGE-SOURCE
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

: PACKAGE-ABSOLUTE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" package GD-ABSOLUTE-LONG:A:B" GE-SRC-LINE
   s" : SECRET ( -- n ) 41 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" package gd-absolute-long:a:b" GE-SRC-LINE
   s" : CALL-SECRET ( -- n ) SECRET 1+ ;" GE-SRC-LINE
   s" CALL-SECRET ." GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" cp@ ndict@ wordlist" GE-SRC-LINE
   s" package gd-absolute-long ;package" GE-SRC-LINE
   s" package GD-ABSOLUTE-LONG:a ;package" GE-SRC-LINE
   s" package gd-absolute-long:A:b ;package" GE-SRC-LINE
   s" wordlist swap 1+ = ." GE-SRC-LINE
   s" ndict@ = ." GE-SRC-LINE
   s" cp@ = ." GE-SRC-LINE
   s" hb absolute package prefix/reopen" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 42" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   SB$ s" hb absolute package prefix/reopen output" GE-EXPECT-OUT ;

: FULL-QUALIFIED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" TRUSTED: GD-QTRY ( ptr u8 n -- n ) ['] evaluate catch dup if >r 2drop r> then ;" GE-SRC-LINE
   s" : GD-QDEF:A:B:C:VALUE ( -- n ) 42 ;" GE-SRC-LINE
   s" wordlist cp@ ndict@" GE-SRC-LINE
   s" package GD-QDEF ;package" GE-SRC-LINE
   s" package GD-QDEF:A ;package" GE-SRC-LINE
   s" ndict@ = ." GE-SRC-LINE
   s" cp@ = ." GE-SRC-LINE
   s" wordlist swap 1+ = ." GE-SRC-LINE
   s" GD-QDEF:A:B:C:VALUE ." GE-SRC-LINE
   s" gd-qdef:a:b:c:value ." GE-SRC-LINE
   s" package GD-QPKG:A:B" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : VALUE ( -- n ) 43 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" GD-QPKG:A:B:VALUE ." GE-SRC-LINE
   \ Each native EXPORT refusal pins rc, NDICT, CP, and absent alias publication.
   s" package GD-QEXPORT" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" cp@ ndict@" GE-SRC-LINE
   s" EXPORT :GD-QPKG:A:B:VALUE" GE-SRC-S"  s"  GD-QTRY 70 = ." GE-SRC-LINE
   s" ndict@ = ." GE-SRC-LINE
   s" cp@ = ." GE-SRC-LINE
   s" VALUE" GE-SRC-S"  s"  get-current search-wl 0= ." GE-SRC-LINE
   s" cp@ ndict@" GE-SRC-LINE
   s" EXPORT GD-QPKG:A:B:VALUE:" GE-SRC-S"  s"  GD-QTRY 70 = ." GE-SRC-LINE
   s" ndict@ = ." GE-SRC-LINE
   s" cp@ = ." GE-SRC-LINE
   s" VALUE" GE-SRC-S"  s"  get-current search-wl 0= ." GE-SRC-LINE
   s" cp@ ndict@" GE-SRC-LINE
   s" EXPORT GD-QPKG:A::B:VALUE" GE-SRC-S"  s"  GD-QTRY 70 = ." GE-SRC-LINE
   s" ndict@ = ." GE-SRC-LINE
   s" cp@ = ." GE-SRC-LINE
   s" VALUE" GE-SRC-S"  s"  get-current search-wl 0= ." GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : GD-QGHOST ( -- n ) 99 ;" GE-SRC-LINE
   s" package GD-QSCOPE" GE-SRC-LINE
   s" GD-QPKG:A:B:GD-QGHOST" GE-SRC-S"  s"  GD-QTRY 70 = ." GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" package GD-QTYPE:A:B" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" ENUM state ready ;ENUM" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" GD-QTYPE:A:B:STATE:READY drop" GE-SRC-S"  s"  GD-QTRY 0= ." GE-SRC-LINE
   s" wordlist cp@ ndict@" GE-SRC-LINE
   s" : GD-QTYPE:A:B:STATE:BAD ( -- n ) 1 ;" GE-SRC-S"  s"  GD-QTRY 75 = ." GE-SRC-LINE
   s" ndict@ = ." GE-SRC-LINE
   s" cp@ = ." GE-SRC-LINE
   s" wordlist swap 1+ = ." GE-SRC-LINE
   s" GD-QTYPE:A:B:STATE:BAD" GE-SRC-S"  s"  GD-QTRY 70 = ." GE-SRC-LINE
   s" wordlist cp@ ndict@" GE-SRC-LINE
   s" : :GD-QBAD ( -- n ) 1 ;" GE-SRC-S"  s"  GD-QTRY 75 = ." GE-SRC-LINE
   s" : GD-QBAD: ( -- n ) 1 ;" GE-SRC-S"  s"  GD-QTRY 75 = ." GE-SRC-LINE
   s" : GD-QBAD::WORD ( -- n ) 1 ;" GE-SRC-S"  s"  GD-QTRY 75 = ." GE-SRC-LINE
   s" ndict@ = ." GE-SRC-LINE
   s" cp@ = ." GE-SRC-LINE
   s" wordlist swap 1+ = ." GE-SRC-LINE
   s" wordlist cp@ ndict@" GE-SRC-LINE
   s" :GD-QMISS" GE-SRC-S"  s"  GD-QTRY 70 = ." GE-SRC-LINE
   s" GD-QMISS:" GE-SRC-S"  s"  GD-QTRY 70 = ." GE-SRC-LINE
   s" GD-QMISS::WORD" GE-SRC-S"  s"  GD-QTRY 70 = ." GE-SRC-LINE
   s" ndict@ = ." GE-SRC-LINE
   s" cp@ = ." GE-SRC-LINE
   s" wordlist swap 1+ = ." GE-SRC-LINE
   s" hb full qualified paths" GE-EVAL-RUN-STDIN
   SB-RESET
   3 0 ?do s" -1" GE-OUT-LINE loop
   s" 42" GE-OUT-LINE
   s" 42" GE-OUT-LINE
   s" 43" GE-OUT-LINE
   31 0 ?do s" -1" GE-OUT-LINE loop
   SB$ s" hb full qualified paths output" GE-EXPECT-OUT ;

: PACKAGE-ROLLBACK-SOURCE ( -- )
   GE-SRC-RESET
   s" TRUSTED: GD-PKG-TRY ( ptr u8 n -- n ) ['] evaluate catch ;" GE-SRC-LINE
   s" cp@ ndict@" GE-SRC-LINE
   s" package :GD-PKG-BAD" GE-SRC-S"  s"  GD-PKG-TRY 75 = ." GE-SRC-LINE
   s" ndict@ = ." GE-SRC-LINE
   s" cp@ = ." GE-SRC-LINE
   s" cp@ ndict@" GE-SRC-LINE
   s" package GD-PKG-BAD:" GE-SRC-S"  s"  GD-PKG-TRY 75 = ." GE-SRC-LINE
   s" ndict@ = ." GE-SRC-LINE
   s" cp@ = ." GE-SRC-LINE
   s" cp@ ndict@" GE-SRC-LINE
   s" package GD-PKG::BAD" GE-SRC-S"  s"  GD-PKG-TRY 75 = ." GE-SRC-LINE
   s" ndict@ = ." GE-SRC-LINE
   s" cp@ = ." GE-SRC-LINE
   s" wordlist cp@ ndict@" GE-SRC-LINE
   s" dbase@ REGION + $4000 - cp!" GE-SRC-LINE
   s" package GD-PACKAGE-CAPACITY-LONG:A" GE-SRC-S"  s"  GD-PKG-TRY 76 = ." GE-SRC-LINE
   s" ndict@ = ." GE-SRC-LINE
   s" cp!" GE-SRC-LINE
   s" wordlist swap 1+ = ." GE-SRC-LINE
   s" wordlist cp@ ndict@" GE-SRC-LINE
   s" package GD-PKG-ROLLBACK-LONG:A:B GD-PKG-MISSING" GE-SRC-S"  s"  GD-PKG-TRY 70 = ." GE-SRC-LINE
   s" ndict@ = ." GE-SRC-LINE
   s" cp@ = ." GE-SRC-LINE
   s" wordlist swap - ." GE-SRC-LINE
   s" wordlist" GE-SRC-LINE
   s" package GD-PKG-ROLLBACK-LONG:A:B ;package" GE-SRC-LINE
   s" wordlist swap - ." GE-SRC-LINE ;

: PACKAGE-ROLLBACK ( -- )
   GE-HB-RESET
   PACKAGE-ROLLBACK-SOURCE
   s" hb absolute package transactional publication" GE-EVAL-RUN-STDIN
   SB-RESET
   12 0 ?do s" -1" GE-OUT-LINE loop
   s" -1" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" 7" GE-OUT-LINE
   s" 7" GE-OUT-LINE
   SB$ s" hb absolute package transactional publication output" GE-EXPECT-OUT ;

: PACKAGE-SEMICOLON ( -- )
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

: PACKAGE-JIT-STACK ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : FIVE-LITS ( -- n n n n n ) 10 10 10 10 10 ;" GE-SRC-LINE
   s" FIVE-LITS + + + + ." GE-SRC-LINE
   s" hb package cells do not overlap jit stack" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 50" GE-OUT-LINE
   SB$ s" hb package cells do not overlap jit stack output" GE-EXPECT-OUT ;

: PACKAGE-CHECK-SOURCE ( -- )
   GE-SRC-RESET
   s" package CK" GE-SRC-LINE
   s" : HELP ( -- n ) 2 ;" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : EXPORTED ( -- n ) HELP 5 + ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" USE-CK ( -- n ) CK:EXPORTED" GE-SRC-CHECK-LINE
   s" BAD-CK ( -- n ) HELP" GE-SRC-CHECK-LINE ;

: PACKAGE-GOOD-BODY ( -- )
   s" package CK" GE-SRC-LINE
   s" : HELP ( -- n ) 2 ;" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : EXPORTED ( -- n ) HELP 5 + ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : USE-CK ( -- n ) CK:EXPORTED ;" GE-SRC-LINE ;

: CHECK-BUF-ACT ( -- )
   LABEL$ GE-SRC-BUF GE-SRC-U @ CHECK-ALL-ERRORS:BUF ;

: CHECK-BUF-STORE ( n -- ) {: rc:n :}
   rc OUTCOME:EXITED GT-OUTCOME!
   0 GT-OUT-U !
   CHECK-ALL-ERRORS:OUT$ nip GT-ERR-U ! ;

: CHECK-BUF-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   s" inprocess-check" GS-EVENT
   label labelu LABEL!
   GT-ERR-BUF GT-ERR-CAP GT-OUT-BUF GT-OUT-CAP CHECK-ALL-ERRORS:BUFFERS!
   0 0= 0= CHECK-ALL-ERRORS:JSON!
   [: CHECK-BUF-ACT ;] catch CHECK-BUF-STORE ;

: CHECK-BUF-BAD ( n ptr u8 n ptr u8 n -- )
   {: rc:n needle:ptr needleu:n label:ptr labelu:n :}
   label labelu CHECK-BUF-RUN
   rc label labelu GE-EXPECT-RC
   needle needleu label labelu GE-EXPECT-ERR-HAS
   label labelu GT-PROGRESS-PASS ;

: CANDIDATE-ACT ( -- )
   CANDIDATE$ CHECK-CANDIDATE! CANDIDATE-VERDICT ! ;

: CANDIDATE-RC ( n -- n ) {: rc:n :}
   rc 0 <> if rc exit then
   CANDIDATE-VERDICT @ 0= if $46 exit then
   0 ;

: CANDIDATE-STORE ( n -- ) {: rc:n :}
   rc CANDIDATE-RC OUTCOME:EXITED GT-OUTCOME!
   0 GT-OUT-U !
   DIAG-BUFFER$ nip GT-ERR-U !
   DIAG-BUFFER-OFF ;

: CANDIDATE-RUN ( ptr u8 n ptr u8 n -- ) {: body:ptr bodyu:n label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   s" inprocess-candidate" GS-EVENT
   body bodyu CANDIDATE!
   -1 CANDIDATE-VERDICT !
   GT-ERR-BUF GT-ERR-CAP DIAG-BUFFER!
   [: CANDIDATE-ACT ;] catch CANDIDATE-STORE ;

: CANDIDATE-BAD ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: body:ptr bodyu:n rc:n needle:ptr needleu:n label:ptr labelu:n :}
   body bodyu label labelu CANDIDATE-RUN
   rc label labelu GE-EXPECT-RC
   needle needleu label labelu GE-EXPECT-ERR-HAS
   label labelu GT-PROGRESS-PASS ;

: CHECK-BAD-ALL ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu CHECK-BUF-RUN
   70 label labelu GE-EXPECT-RC
   label labelu GT-PROGRESS-PASS ;

: CHECK-BUF-GOOD ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu CHECK-BUF-RUN
   label labelu GE-EXPECT-OK
   label labelu GE-EXPECT-SILENT
   label labelu GT-PROGRESS-PASS ;

: NORET-GOOD-BODY ( -- )
   s" package GD-NR" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : STOP ( -- ) 1 throw ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : GD-NR-OK ( n -- n ) dup 0 < if GD-NR:STOP then 1 + ;" GE-SRC-LINE ;

: PACKAGE-CHECK ( -- )
   GE-HB-RESET
   PACKAGE-CHECK-SOURCE
   s" hb package checker scope" GE-EVAL-RUN-STDIN
   SB-RESET
   s" -1" GE-OUT-LINE
   s" 1" GE-OUT-LINE
   SB$ s" hb package checker scope output" GE-EXPECT-OUT
   GE-SRC-RESET
   s" package GD-QCHECK" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : TARGET ( -- n ) 41 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" undefine GD-QCHECK:TARGET" GE-SRC-LINE
   s" : GD-QCHECK-CALL ( -- n ) GD-QCHECK:TARGET ;" GE-SRC-LINE
   $46 s" GD-QCHECK:TARGET" s" checker forgets qualified undefined word" CHECK-BUF-BAD ;

: PACKAGE-NORET ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" package GD-NR" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : STOP ( -- ) 1 throw ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : GD-NR-BAD ( n -- n ) dup 0 < if GD-NR:STOP 0 then 1 + ;" GE-SRC-LINE
   $46 s" gd-nr-bad" s" checker package no-return rejects live tail" CHECK-BUF-BAD
   GE-SRC-RESET
   s" package GD-NRA" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : STOP ( -- ) 1 throw ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" package GD-NRB" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : STOP ( -- ) 2 throw ;" GE-SRC-LINE
   s" undefine STOP" GE-SRC-LINE
   s" : STOP ( -- n ) 7 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : GD-NRA-BAD ( n -- n ) dup 0 < if GD-NRA:STOP 0 then 1 + ;" GE-SRC-LINE
   $46 s" gd-nra-bad" s" package undefine keeps other no-return symbol" CHECK-BUF-BAD ;

: RUN-BAD-SOURCE ( n ptr u8 n ptr u8 n -- )
   GE-EVAL-FORK-BAD ;

: RUN-BAD-CHILD ( n ptr u8 n ptr u8 n -- )
   {: rc:n needle:ptr needleu:n label:ptr labelu:n :}
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   rc label labelu GE-EXPECT-RC
   needle needleu label labelu GE-EXPECT-ERR-HAS ;

\ One literal grammar everywhere (engine + checker): int -?d+ | -?$h+, float
\ -?d*.d+ — dot-leading spellings (.5 -.5 .0) ARE float literals; 5. and ..5
\ are words. The engine number parser claims literals before dictionary lookup
\ (LITERAL-FIRST), so the checker must claim exactly the same token set or a
\ number-shaped word certifies with an effect the runtime never executes (the
\ lib/fmt.f .0 incident). Number-shaped definitions appear only inside child
\ source strings here; reserved-name-lint forbids them in committed code.
: LITERAL-FLOAT-SRC ( -- )
   GE-SRC-RESET
   s" : FRUN ( -- r ) .5 ;" GE-SRC-LINE
   s" FRUN ." GE-SRC-LINE
   s" .0 ." GE-SRC-LINE
   s" -.5 ." GE-SRC-LINE
   s" 1.5 ." GE-SRC-S"  s"  evaluate" GE-SRC-LINE
   s" -$FF ." GE-SRC-LINE
   s" FLOK1 ( -- r ) .5" GE-SRC-CHECK-LINE
   s" FLOK2 ( -- r ) -.5" GE-SRC-CHECK-LINE
   s" FLOK3 ( -- r ) .0" GE-SRC-CHECK-LINE
   s" FLOK4 ( -- r ) 1.5" GE-SRC-CHECK-LINE
   s" FLOK5 ( -- n ) -$FF" GE-SRC-CHECK-LINE
   s" FLBAD1 ( -- r ) 5." GE-SRC-CHECK-LINE
   s" FLBAD2 ( -- r ) ..5" GE-SRC-CHECK-LINE
   s" FLBAD3 ( -- n ) .5" GE-SRC-CHECK-LINE
   s" : .0 ( n -- ) drop ;" GE-SRC-LINE
   s" 60 .0 . ." GE-SRC-LINE
   s" SHOK ( -- r ) .0" GE-SRC-CHECK-LINE
   s" SHBAD ( n -- ) .0" GE-SRC-CHECK-LINE ;

: LITERAL-FLOAT-OUT ( -- )
   SB-RESET
   s" 4602678819172646912" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   s" -4620693217682128896" GE-OUT-LINE
   s" 4609434218613702656" GE-OUT-LINE
   s" -255" GE-OUT-LINE
   s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE
   s" -1" GE-OUT-LINE s" -1" GE-OUT-LINE
   s" 1" GE-OUT-LINE s" 1" GE-OUT-LINE s" 0" GE-OUT-LINE
   s" 0" GE-OUT-LINE s" 60" GE-OUT-LINE
   s" -1" GE-OUT-LINE s" 0" GE-OUT-LINE ;

: LITERAL-FLOAT-FIRST ( -- )
   GE-HB-RESET
   LITERAL-FLOAT-SRC
   s" hb float literal grammar engine+checker" GE-HB-RUN-STDIN
   LITERAL-FLOAT-OUT
   SB$ s" hb float literal-first output" GE-EXPECT-OUT
   GE-HB-RESET
   GE-SRC-RESET
   s" : .0 ( n -- ) drop ;" GE-SRC-LINE
   s" : SHRUN ( n -- ) .0 ;" GE-SRC-LINE
   70 s" at '.0'" s" hb rejects call to number-shaped word" RUN-BAD-CHILD
   GE-HB-RESET
   GE-SRC-RESET
   s" 5. ." GE-SRC-LINE
   70 s" E-UNDEFINED: 5." s" hb engine keeps trailing-dot token a word" RUN-BAD-CHILD
   GE-HB-RESET
   GE-SRC-RESET
   s" ..5 ." GE-SRC-LINE
   70 s" E-UNDEFINED: ..5" s" hb engine keeps double-dot token a word" RUN-BAD-CHILD ;

: LITERAL-FLOAT-EVAL ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : FRUN2 ( -- r ) -.5 ;" GE-SRC-LINE
   s" FRUN2 ." GE-SRC-LINE
   s" FLEV1 ( -- r ) .5" GE-SRC-CHECK-LINE
   s" FLEV2 ( -- n ) .5" GE-SRC-CHECK-LINE
   s" hb resident evaluate float literal-first" GE-EVAL-RUN-STDIN
   SB-RESET
   s" -4620693217682128896" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   SB$ s" hb resident evaluate float literal-first output" GE-EXPECT-OUT ;

: DUPLICATE-DEFINITION-REJECTS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" package APP" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 1 ;" GE-SRC-LINE
   s" : RESET ( -- n ) 2 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   $4E s" RESET" s" package rejects duplicate public word" RUN-BAD-SOURCE
   GE-SRC-RESET
   s" package APP" GE-SRC-LINE
   s" : H ( -- n ) 1 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" package APP" GE-SRC-LINE
   s" : H ( -- n ) 2 ;" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : GET ( -- n ) H ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   $4E s" duplicate definition" s" package rejects duplicate private word across reopen" CHECK-BUF-BAD
   GE-SRC-RESET
   s" package APP" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 1 ;" GE-SRC-LINE
   s" : reset ( -- n ) 2 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   $4E s" duplicate definition" s" package rejects case-variant duplicate word" CHECK-BUF-BAD
   GE-SRC-RESET
   s" package APP" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" variable CELL" GE-SRC-LINE
   s" variable cell" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   $4E s" cell" s" package rejects duplicate variable word" RUN-BAD-SOURCE
   GE-SRC-RESET
   s" : RESET ( -- n ) 1 ;" GE-SRC-LINE
   s" : RESET ( -- n ) 2 ;" GE-SRC-LINE
   $4E s" duplicate definition" s" global wordlist rejects duplicate word" CHECK-BUF-BAD
   GE-SRC-RESET
   s" : GD-HDUP ( -- n ) 1 ;" GE-SRC-LINE
   s" undefine GD-HDUP" GE-SRC-LINE
   s" : GD-HDUP ( -- n ) 2 ;" GE-SRC-LINE
   s" : GD-HDUP ( -- n ) 3 ;" GE-SRC-LINE
   $4E s" GD-HDUP" s" hash dup-probe rejects duplicate behind a retired slot" RUN-BAD-SOURCE
   GE-SRC-RESET
   s" : dup ( n -- n n ) dup ;" GE-SRC-LINE
   $4E s" dup" s" primitive shadow attempt rejects without undefine" RUN-BAD-SOURCE ;

: EXPLICIT-REDEFINITION ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GD-RDEF ( -- n ) 1 ;" GE-SRC-LINE
   s" undefine GD-RDEF" GE-SRC-LINE
   s" : GD-RDEF ( -- n ) 2 ;" GE-SRC-LINE
   s" GD-RDEF ." GE-SRC-LINE
   s" package GD-RPKG" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 3 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" package GD-RPKG-B" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 6 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" package GD-RPKG" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" undefine RESET" GE-SRC-LINE
   s" : RESET ( -- n ) 4 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" GD-RPKG:RESET ." GE-SRC-LINE
   s" GD-RPKG-B:RESET ." GE-SRC-LINE
   s" defer GD-RV ( -- n )" GE-SRC-LINE
   s" undefine GD-RV" GE-SRC-LINE
   s" defer GD-RV ( -- n )" GE-SRC-LINE
   s" : GD-RV-FIVE ( -- n ) 5 ;" GE-SRC-LINE
   s" : GD-RV-INSTALL ( -- ) [: GD-RV-FIVE ;] is GD-RV ;" GE-SRC-LINE
   s" GD-RV-INSTALL GD-RV ." GE-SRC-LINE
   s" package GD-QUNDEF" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : TARGET ( -- n ) 41 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" undefine GD-QUNDEF:TARGET" GE-SRC-LINE
   s" s" GE-SRC+ GE-DQ GE-SRC-C s"  GD-QUNDEF:TARGET" GE-SRC+ GE-DQ GE-SRC-C
   s"  XREF-FIND XREF-FOUND? ." GE-SRC-LINE
   s" package GD-QUNDEF" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : TARGET ( -- n ) 42 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" GD-QUNDEF:TARGET ." GE-SRC-LINE
   s" variable GD-UQ-A   variable GD-UQ-U" GE-SRC-LINE
   s" : GD-UQ-GO ( -- ) GD-UQ-A @ GD-UQ-U @ INCLUDE-EVALUATE ;" GE-SRC-LINE
   s" : GD-UQ-CATCH ( ptr u8 n -- n ) GD-UQ-U ! GD-UQ-A ! [: GD-UQ-GO ;] catch ;" GE-SRC-LINE
   s" s" GE-SRC+ GE-DQ GE-SRC-C s"  undefine PCAP-CAPTURED:MAKE" GE-SRC+ GE-DQ GE-SRC-C
   s"  GD-UQ-CATCH ." GE-SRC-LINE
   s" s" GE-SRC+ GE-DQ GE-SRC-C s"  PCAP-CAPTURED:MAKE" GE-SRC+ GE-DQ GE-SRC-C
   s"  XREF-FIND XREF-FOUND? ." GE-SRC-LINE
   s" : GD-QROLL-CALL ( len len -- pcap:captured ) PCAP-CAPTURED:MAKE ;" GE-SRC-LINE
   s" s" GE-SRC+ GE-DQ GE-SRC-C s"  checker-kept" GE-SRC+ GE-DQ GE-SRC-C
   s"  type cr" GE-SRC-LINE
   s" hb explicit undefine redefinition" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 2" GE-OUT-LINE
   s" 4" GE-OUT-LINE
   s" 6" GE-OUT-LINE
   s" 5" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   s" 42" GE-OUT-LINE
   s" 7111" GE-OUT-LINE
   s" -1" GE-OUT-LINE
   s" checker-kept" GE-OUT-LINE
   SB$ s" hb explicit undefine redefinition output" GE-EXPECT-OUT ;

: REDEF-CHECK-BODY ( -- )
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

: PACKAGE-SHADOW-POSITIVES ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" package APP" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : COUNT ( -- n ) 7 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" package MK" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 2 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" package APP" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 1 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
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

: PACKAGE-DUPLICATE-CHECK ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" package APP" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : RESET ( -- n ) 1 ;" GE-SRC-LINE
   s" : RESET ( -- n ) 2 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   $4E s" duplicate definition" s" checker rejects package duplicate definition" CHECK-BUF-BAD ;

: RUN-LOAD-ONE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n label:ptr labelu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
   path pathu GE-ARG+
   s" --" GE-ARG+
   label labelu GT-PROGRESS-RUN
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: RUN-LOAD-THREE ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
   INC-CORE$ GE-ARG+
   INC-API$ GE-ARG+
   INC-MAIN$ GE-ARG+
   s" --" GE-ARG+
   label labelu GT-PROGRESS-RUN
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: RUN-LOAD-BAD ( n ptr u8 n ptr u8 n -- )
   {: rc:n needle:ptr needleu:n label:ptr labelu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
   INC-MAIN$ GE-ARG+
   s" --" GE-ARG+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   rc label labelu GE-EXPECT-RC
   needle needleu label labelu GE-EXPECT-ERR-HAS ;

: EXPECT-GET9 ( ptr u8 n -- ) {: label:ptr labelu:n :}
   SB-RESET
   s" 9" GE-OUT-LINE
   SB$ label labelu GE-EXPECT-OUT ;

: PACKAGE-MULTIFILE-LOAD ( -- )
   INC-PATHS
   WRITE-CORE
   WRITE-API-NOINC
   WRITE-MAIN-RUN
   s" hb package reopen across load files" RUN-LOAD-THREE
   s" hb package reopen across load files output" EXPECT-GET9 ;

: PACKAGE-INCLUDE ( -- )
   INC-PATHS
   WRITE-CORE
   WRITE-API-INCLUDE
   WRITE-MAIN-INCLUDE
   INC-MAIN$ s" hb include nested package source" RUN-LOAD-ONE
   s" hb include nested package source output" EXPECT-GET9
   WRITE-MAIN-INCLUDED
   INC-MAIN$ s" hb included string package source" RUN-LOAD-ONE
   s" hb included string package source output" EXPECT-GET9
   WRITE-DUP
   SB-RESET
   INC-DUP$ SB-INCLUDE-LINE
   s" package APP" SB-LINE
   s" : H ( -- n ) 2 ;" SB-LINE
   s" ;package" SB-LINE
   INC-MAIN$ SB$ WRITE-ALL
   $4E s" duplicate definition" s" hb include rejects duplicate package reopen" RUN-LOAD-BAD ;

: PACKAGE-MISUSE ( -- )
   GE-HB-RESET
   GE-SRC-RESET  s" public" GE-SRC-LINE
   $4B s" public" s" package rejects public outside" RUN-BAD-SOURCE
   GE-SRC-RESET  s" private" GE-SRC-LINE
   $4B s" private" s" package rejects private outside" RUN-BAD-SOURCE
   GE-SRC-RESET  s" end-package" GE-SRC-LINE
   $46 s" E-UNDEFINED: end-package" s" package rejects legacy closer" RUN-BAD-CHILD
   GE-SRC-RESET  s" ;package" GE-SRC-LINE
   $4B s" ;package" s" package rejects semicolon closer outside" RUN-BAD-SOURCE
   GE-SRC-RESET  s" package A" GE-SRC-LINE  s" package B" GE-SRC-LINE
   $4B s" package" s" package rejects nesting" RUN-BAD-SOURCE
   GE-SRC-RESET  s" package" GE-SRC-LINE
   $4A s" package" s" package rejects missing name" RUN-BAD-SOURCE
   GE-SRC-RESET  s" package A::B" GE-SRC-LINE
   $4B s" A::B" s" package rejects doubled separator" RUN-BAD-SOURCE
   GE-SRC-RESET
   s" package P" GE-SRC-LINE
   s" : H ( -- n ) 1 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" P:H ." GE-SRC-LINE
   $46 s" P:H" s" package hides private qualified word" RUN-BAD-CHILD
   GE-SRC-RESET
   s" package P" GE-SRC-LINE
   s" : H ( -- n ) 1 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : BAD ( -- n ) H ;" GE-SRC-LINE
   $46 s" at 'H'" s" package rejects private checked call" RUN-BAD-CHILD
   GE-SRC-RESET
   s" package P" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : E ( -- n ) 1 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" E ." GE-SRC-LINE
   $46 s" E" s" package hides public word from global lookup" RUN-BAD-CHILD
   GE-SRC-RESET
   s" package GD-UQ" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : KEEP ( -- n ) 73 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" undefine GD-UQ:MISSING" GE-SRC-LINE
   70 s" undefine: word not found" s" unknown qualified undefine fails closed" RUN-BAD-SOURCE ;

: STRUCTURE-SOURCE ( -- )
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

: STRUCTURES ( -- )
   GE-HB-RESET
   STRUCTURE-SOURCE
   s" hb structures field layout and typing" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 25" GE-OUT-LINE
   s" 11" GE-OUT-LINE
   s" 22" GE-OUT-LINE
   s" 123" GE-OUT-LINE
   s" gd" GE-OUT-LINE
   s" gd" GE-OUT-LINE
   SB$ s" hb structures output" GE-EXPECT-OUT ;

: STRUCTURE-SETUP ( -- )
   GE-EVAL-MARK
   GE-EVAL-CAPTURE
   s" structures byte-field checker setup" GE-EXPECT-OK
   s" structures byte-field checker setup" GE-EXPECT-SILENT ;

: STRUCTURE-BAD ( -- )
   s" GD-BAD-FLAGS ( ptr a -- n ) POINT.FLAGS @"
   $46 s" gd-bad-flags" s" structures reject byte field cell load" CANDIDATE-BAD ;

: STRUCTURE-MISUSE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   \ Depth satisfies +FIELD's declared inputs ( ptr a n n ) so the body's own
   \ state check is reached: an underdepth bare call now fails closed earlier
   \ with `hb: interpret stack underdepth:` (test/underdepth-gate.f).
   s" 0 0 CELL +FIELD GD-NO-STRUCT.FIELD" GE-SRC-LINE
   $4C s" structure: no active structure" s" structures reject field outside begin" RUN-BAD-SOURCE
   GE-SRC-RESET
   s" BEGIN-STRUCTURE OUTER" GE-SRC-LINE
   s" BEGIN-STRUCTURE INNER" GE-SRC-LINE
   $4C s" structure: nested begin" s" structures reject nesting" RUN-BAD-SOURCE
   GE-SRC-RESET
   s" BEGIN-STRUCTURE POINT" GE-SRC-LINE
   s" CFIELD: POINT.FLAGS" GE-SRC-LINE
   s" END-STRUCTURE" GE-SRC-LINE
   STRUCTURE-SETUP
   STRUCTURE-BAD
   GE-EVAL-FORGET ;

: ENUMS ( -- )
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
   s" ;package" GE-SRC-LINE
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
   $4E s" GD-EDUP" s" enums reject duplicate constant names" RUN-BAD-SOURCE ;

: EXEC-VECTOR-SOURCE ( -- )
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

: VECTOR-CHECK-BODY ( -- )
   s" defer GD-XV-ACTION ( -- i64 )" GE-SRC-LINE
   s" : GD-XV-FIVE ( -- i64 ) 5 ;" GE-SRC-LINE
   s" : GD-XV-INSTALL-FIVE ( -- ) [: GD-XV-FIVE ;] is GD-XV-ACTION ;" GE-SRC-LINE
   s" : GD-XV-CALL ( -- i64 ) GD-XV-INSTALL-FIVE GD-XV-ACTION ;" GE-SRC-LINE ;

: VECTOR-READER-BODY ( -- )
   s" defer GD-XV-READER ( -- ptr u8 n )" GE-SRC-LINE
   s" : GD-XV-LINE ( -- ptr u8 n ) 0 script-argv$ ;" GE-SRC-LINE
   s" : GD-XV-ENABLE ( -- ) [: GD-XV-LINE ;] is GD-XV-READER ;" GE-SRC-LINE
   s" : GD-XV-READ ( -- ptr u8 n ) GD-XV-ENABLE GD-XV-READER ;" GE-SRC-LINE ;

: EXEC-VECTORS ( -- )
   GE-HB-RESET
   EXEC-VECTOR-SOURCE
   s" hb checked execution vectors" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 5" GE-OUT-LINE
   s" 7" GE-OUT-LINE
   SB$ s" hb checked execution vectors output" GE-EXPECT-OUT ;

: VECTOR-PACKAGE-BODY ( -- )
   s" package GDXV" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" defer RUN ( -- i64 )" GE-SRC-LINE
   s" : FIVE ( -- i64 ) 5 ;" GE-SRC-LINE
   s" : INSTALL ( -- ) [: FIVE ;] is RUN ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : GD-XV-PKG-CALL ( -- i64 ) GDXV:INSTALL GDXV:RUN ;" GE-SRC-LINE ;

: EXEC-VECTOR-PACKAGE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" package GDXV" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" defer RUN ( -- i64 )" GE-SRC-LINE
   s" : FIVE ( -- i64 ) 5 ;" GE-SRC-LINE
   s" : INSTALL ( -- ) [: FIVE ;] is RUN ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" GDXV:INSTALL" GE-SRC-LINE
   s" GDXV:RUN ." GE-SRC-LINE
   s" hb package execution vector" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 5" GE-OUT-LINE
   SB$ s" hb package execution vector output" GE-EXPECT-OUT ;

: EXEC-VECTOR-MISUSE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" defer GD-XV-UNSET ( -- i64 )" GE-SRC-LINE
   s" GD-XV-UNSET ." GE-SRC-LINE
   $4C s" defer: unset execution vector" s" execution vector rejects unset call" RUN-BAD-SOURCE
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
   s" check.f rejects execution vector misuse batch" CHECK-BAD-ALL
   s" gd-xv-bad-a" s" check.f rejects effect-mismatched execution vector assignment" GE-EXPECT-ERR-HAS
   s" gd-xv-bad-tick-b" s" check.f rejects raw xt execution vector assignment" GE-EXPECT-ERR-HAS
   s" gd-xv-bad-target-c" s" check.f rejects non-defer execution vector target" GE-EXPECT-ERR-HAS
   s" gd-xv-bad-reader-d" s" check.f rejects reader-shaped effect mismatch" GE-EXPECT-ERR-HAS
   GE-SRC-RESET
   s" : GD-XV-NOT-DEFER ( -- ) ;" GE-SRC-LINE
   s" : GD-XV-BAD-TARGET ( -- ) [: ;] is GD-XV-NOT-DEFER ;" GE-SRC-LINE
   $4C s" GD-XV-NOT-DEFER" s" execution vector rejects non-defer target" RUN-BAD-SOURCE ;

: CASE-BODY ( -- )
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
   s" ;package" GE-SRC-LINE
   s" : GD-CASE-PKG ( n -- n ) GDCASE:PICK ;" GE-SRC-LINE ;

: CASE-SOURCE ( -- )
   GE-SRC-RESET
   CASE-BODY ;

: CASES ( -- )
   GE-HB-RESET
   CASE-SOURCE
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

: CASE-MISUSE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GD-CASE-BAD-ARM ( n -- n ) case 1 of 10 11 endof 20 swap endcase ;" GE-SRC-LINE
   s" : GD-CASE-MISSING-END ( n -- n ) case 1 of 10 endof ;" GE-SRC-LINE
   s" : GD-CASE-ORPHAN-OF ( n -- n ) 1 of 2 endof ;" GE-SRC-LINE
   s" check.f rejects case misuse batch" CHECK-BAD-ALL
   s" gd-case-bad-arm" s" check.f rejects case effect mismatch" GE-EXPECT-ERR-HAS
   s" gd-case-missing-end" s" check.f rejects unterminated case" GE-EXPECT-ERR-HAS
   s" gd-case-orphan-of" s" check.f rejects of outside case" GE-EXPECT-ERR-HAS ;

: PARSING-SOURCE ( -- )
   GE-SRC-RESET
   s" hi" SRC-DOTQ s"  cr" GE-SRC-LINE
   s" ok" SRC-CQ s"  count type cr" GE-SRC-LINE
   s" : DQ ( -- ) " GE-SRC+ s" bye" SRC-DOTQ s"  ;" GE-SRC-LINE
   s" DQ cr" GE-SRC-LINE
   s" : CQ ( -- ptr u8 n ) " GE-SRC+ s" yo" SRC-CQ s"  count ;" GE-SRC-LINE
   s" CQ type cr" GE-SRC-LINE ;

: PARSING-RUNTIME ( -- )
   GE-HB-RESET
   PARSING-SOURCE
   s" hb parsing-word runtime surface" GE-EVAL-RUN-STDIN
   SB-RESET
   s" hi" GE-OUT-LINE s" ok" GE-OUT-LINE s" bye" GE-OUT-LINE s" yo" GE-OUT-LINE
   SB$ s" hb parsing-word runtime surface output" GE-EXPECT-OUT ;

: PARSING-CHECK-BODY ( -- )
   s" : DQ ( -- ) " GE-SRC+ s" ok" SRC-DOTQ s"  ;" GE-SRC-LINE
   s" : CQ ( -- ptr u8 n ) " GE-SRC+ s" ok" SRC-CQ s"  count ;" GE-SRC-LINE ;

: CHECK-POSITIVE-BATCH ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   PACKAGE-GOOD-BODY
   NORET-GOOD-BODY
   REDEF-CHECK-BODY
   VECTOR-CHECK-BODY
   VECTOR-READER-BODY
   VECTOR-PACKAGE-BODY
   CASE-BODY
   PARSING-CHECK-BODY
   s" check.f dictionary positive certification batch" CHECK-BUF-GOOD ;

: DATA-OVERFLOW ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" $10000000001 allot" GE-SRC-LINE
   76 s" " s" data-space overflow rc" GE-EVAL-FORK-BAD ;

: NAMED-ROW-RUN ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : PSH ( R -- R i64 ) 5 ;" GE-SRC-LINE
   s" PSH ." GE-SRC-LINE
   s" hb named-row sig run" GE-EVAL-RUN-STDIN
   SB-RESET s" 5" GE-OUT-LINE
   SB$ s" hb named-row sig run output" GE-EXPECT-OUT ;

: XREF ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" lib/test.f" GE-SRC-FILE+
   s" tools/xref-test.f" GE-SRC-FILE+
   s" hb native xref words" GE-EVAL-RUN-STDIN
   s" xref-test: ok" s" hb native xref words output" GE-EXPECT-OUT-HAS ;

public

: RUN ( -- )
   s" hb-gate-dictionary" GT-START
   s" dictionary/long-dictionary" [: LONG-DICTIONARY ;] CASE-RUN
   s" dictionary/wordlist" [: WORDLIST ;] CASE-RUN
   s" dictionary/long-name" [: LONG-NAME ;] CASE-RUN
   s" dictionary/trusted-does" [: TRUSTED-DOES ;] CASE-RUN
   s" dictionary/bad-does" [: BAD-DOES ;] CASE-RUN
   s" dictionary/row-quot" [: ROW-QUOT-CHECKS ;] CASE-RUN
   s" dictionary/primitives" [: PRIMITIVE-CHECKS ;] CASE-RUN
   s" dictionary/return" [: RETURN-CHECKS ;] CASE-RUN
   s" dictionary/combinators" [: COMBINATOR-CHECKS ;] CASE-RUN
   s" dictionary/local-quot" [: LOCAL-QUOT-CHECKS ;] CASE-RUN
   s" dictionary/local-quot-compile" [: LOCAL-QUOT-FAIL ;] CASE-RUN
   s" dictionary/local-first" [: LOCAL-FIRST ;] CASE-RUN
   s" dictionary/literal-first" [: LITERAL-FIRST ;] CASE-RUN
   s" dictionary/literal-float" [: LITERAL-FLOAT-FIRST ;] CASE-RUN
   s" dictionary/literal-float-eval" [: LITERAL-FLOAT-EVAL ;] CASE-RUN
   s" dictionary/namespace" [: NAMESPACE-QUALIFIED ;] CASE-RUN
   s" dictionary/package-runtime" [: PACKAGE-RUNTIME ;] CASE-RUN
   s" dictionary/package-absolute" [: PACKAGE-ABSOLUTE ;] CASE-RUN
   s" dictionary/full-qualified" [: FULL-QUALIFIED ;] CASE-RUN
   s" dictionary/package-rollback" [: PACKAGE-ROLLBACK ;] CASE-RUN
   s" dictionary/package-semicolon" [: PACKAGE-SEMICOLON ;] CASE-RUN
   s" dictionary/package-jit-stack" [: PACKAGE-JIT-STACK ;] CASE-RUN
   s" dictionary/package-check" [: PACKAGE-CHECK ;] CASE-RUN
   s" dictionary/package-noret" [: PACKAGE-NORET ;] CASE-RUN
   s" dictionary/duplicate" [: DUPLICATE-DEFINITION-REJECTS ;] CASE-RUN
   s" dictionary/redefine" [: EXPLICIT-REDEFINITION ;] CASE-RUN
   s" dictionary/package-shadow" [: PACKAGE-SHADOW-POSITIVES ;] CASE-RUN
   s" dictionary/package-duplicate-check" [: PACKAGE-DUPLICATE-CHECK ;] CASE-RUN
   s" dictionary/package-multifile" [: PACKAGE-MULTIFILE-LOAD ;] CASE-RUN
   s" dictionary/package-include" [: PACKAGE-INCLUDE ;] CASE-RUN
   s" dictionary/package-misuse" [: PACKAGE-MISUSE ;] CASE-RUN
   s" dictionary/structures" [: STRUCTURES ;] CASE-RUN
   s" dictionary/structure-misuse" [: STRUCTURE-MISUSE ;] CASE-RUN
   s" dictionary/enums" [: ENUMS ;] CASE-RUN
   s" dictionary/exec-vectors" [: EXEC-VECTORS ;] CASE-RUN
   s" dictionary/exec-vector-package" [: EXEC-VECTOR-PACKAGE ;] CASE-RUN
   s" dictionary/exec-vector-misuse" [: EXEC-VECTOR-MISUSE ;] CASE-RUN
   s" dictionary/case" [: CASES ;] CASE-RUN
   s" dictionary/case-misuse" [: CASE-MISUSE ;] CASE-RUN
   s" dictionary/parsing-runtime" [: PARSING-RUNTIME ;] CASE-RUN
   s" dictionary/check-positive-batch" [: CHECK-POSITIVE-BATCH ;] CASE-RUN
   s" dictionary/data-overflow" [: DATA-OVERFLOW ;] CASE-RUN
   s" dictionary/named-row" [: NAMED-ROW-RUN ;] CASE-RUN
   s" dictionary/xref" [: XREF ;] CASE-RUN
   GT-CLEANUP
   s" PASS: native dictionary/checker gate phase" type cr ;

;package
