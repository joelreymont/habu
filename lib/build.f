\ build.f - checked helpers for Habu build scripts.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, and lib/process.f.

65536 constant BUILD-SOURCE-CAP
10 constant BUILD-LF
13 constant BUILD-CR
32 constant BUILD-SP
58 constant BUILD-COLON
59 constant BUILD-SEMI

0 constant BUILD-STEP-NAME-A
1 constant BUILD-STEP-NAME-U
2 constant BUILD-STEP-CMD-A
3 constant BUILD-STEP-CMD-U
4 constant BUILD-STEP-ARGV-A
5 constant BUILD-STEP-ARGV-U
6 constant BUILD-STEP-TMP-A
7 constant BUILD-STEP-TMP-U
8 constant BUILD-STEP-ART-A
9 constant BUILD-STEP-ART-U
10 constant BUILD-STEP-RC-OFF
11 constant BUILD-STEP-CELLS

create BUILD-SOURCE-BUF BUILD-SOURCE-CAP allot
create BUILD-PATH-BUF FS-PATH-CAP allot

variable BUILD-SOURCE-LEN
variable BUILD-I
variable BUILD-DEFS
variable BUILD-START
variable BUILD-END

: BUILD-FALSE ( -- bool )
   0 0= 0= ;

: BUILD-TRUE ( -- bool )
   0 0= ;

: BUILD-WHITE? ( n -- bool ) {: c :}
   c BUILD-SP = if BUILD-TRUE exit then
   c BUILD-LF = if BUILD-TRUE exit then
   c BUILD-CR = ;

: BUILD-STEP-CHECK-OFF ( n -- ) {: off :}
   off 0 < if E-BUILD-PATH throw then
   off BUILD-STEP-CELLS >= if E-BUILD-PATH throw then ;

: BUILD-STEP-FIELD ( ptr a n -- ptr a ) {: rec:ptr off :}
   off BUILD-STEP-CHECK-OFF
   rec off cells + ;

: BUILD-STEP-A! ( ptr u8 ptr a n -- ) {: a:ptr rec:ptr off :}
   a rec off BUILD-STEP-FIELD ! ;

: BUILD-STEP-N! ( n ptr a n -- ) {: n rec:ptr off :}
   n rec off BUILD-STEP-FIELD ! ;

: BUILD-STEP-A@ ( ptr a n -- ptr u8 )
   BUILD-STEP-FIELD @ ;

: BUILD-STEP-N@ ( ptr a n -- n )
   BUILD-STEP-FIELD @ ;

: BUILD-STEP-PAIR! ( ptr u8 n ptr a n -- ) {: a:ptr u rec:ptr off :}
   u 0 < if E-BUILD-PATH throw then
   a rec off BUILD-STEP-A!
   u rec off 1 + BUILD-STEP-N! ;

: BUILD-STEP-PAIR$ ( ptr a n -- ptr u8 n ) {: rec:ptr off :}
   rec off BUILD-STEP-A@
   rec off 1 + BUILD-STEP-N@ ;

: BUILD-STEP-EMPTY! ( ptr a n -- ) {: rec:ptr off :}
   BUILD-SOURCE-BUF 0 rec off BUILD-STEP-PAIR! ;

: BUILD-STEP-CLEAR ( ptr a -- ) {: rec:ptr :}
   rec BUILD-STEP-NAME-A BUILD-STEP-EMPTY!
   rec BUILD-STEP-CMD-A BUILD-STEP-EMPTY!
   rec BUILD-STEP-ARGV-A BUILD-STEP-EMPTY!
   rec BUILD-STEP-TMP-A BUILD-STEP-EMPTY!
   rec BUILD-STEP-ART-A BUILD-STEP-EMPTY!
   -1 rec BUILD-STEP-RC-OFF BUILD-STEP-FIELD ! ;

: BUILD-STEP-NAME! ( ptr u8 n ptr a -- ) {: a:ptr u rec:ptr :}
   u 0 <= if E-BUILD-COMMAND throw then
   a u rec BUILD-STEP-NAME-A BUILD-STEP-PAIR! ;

: BUILD-STEP-COMMAND! ( ptr u8 n ptr a -- ) {: a:ptr u rec:ptr :}
   u 0 <= if E-BUILD-COMMAND throw then
   a u rec BUILD-STEP-CMD-A BUILD-STEP-PAIR! ;

: BUILD-STEP-ARGV! ( ptr u8 n ptr a -- ) {: a:ptr u rec:ptr :}
   a u rec BUILD-STEP-ARGV-A BUILD-STEP-PAIR! ;

: BUILD-STEP-TMP! ( ptr u8 n ptr a -- ) {: a:ptr u rec:ptr :}
   u 0 <= if E-BUILD-PATH throw then
   a u rec BUILD-STEP-TMP-A BUILD-STEP-PAIR! ;

: BUILD-STEP-ARTIFACT! ( ptr u8 n ptr a -- ) {: a:ptr u rec:ptr :}
   u 0 <= if E-BUILD-PATH throw then
   a u rec BUILD-STEP-ART-A BUILD-STEP-PAIR! ;

: BUILD-STEP-NAME$ ( ptr a -- ptr u8 n )
   BUILD-STEP-NAME-A BUILD-STEP-PAIR$ ;

: BUILD-STEP-COMMAND$ ( ptr a -- ptr u8 n )
   BUILD-STEP-CMD-A BUILD-STEP-PAIR$ ;

: BUILD-STEP-ARGV$ ( ptr a -- ptr u8 n )
   BUILD-STEP-ARGV-A BUILD-STEP-PAIR$ ;

: BUILD-STEP-TMP$ ( ptr a -- ptr u8 n )
   BUILD-STEP-TMP-A BUILD-STEP-PAIR$ ;

: BUILD-STEP-ARTIFACT$ ( ptr a -- ptr u8 n )
   BUILD-STEP-ART-A BUILD-STEP-PAIR$ ;

: BUILD-STEP-RC@ ( ptr a -- n )
   BUILD-STEP-RC-OFF BUILD-STEP-FIELD @ ;

: BUILD-STEP-RC! ( n ptr a -- ) {: rc rec:ptr :}
   rc rec BUILD-STEP-RC-OFF BUILD-STEP-N! ;

: BUILD-FIND-CHAR ( n n -- n ) {: start ch :}
   start begin dup BUILD-SOURCE-LEN @ < while
      dup BUILD-SOURCE-BUF + c@ ch = if exit then
      1+
   repeat drop -1 ;

: BUILD-SKIP-WHITE ( n -- n )
   begin dup BUILD-SOURCE-LEN @ < while
      dup BUILD-SOURCE-BUF + c@ BUILD-WHITE? if
         1+
      else
         exit
      then
   repeat ;

TRUSTED: BUILD-CHECK-RAW ( ptr u8 n -- n )
   CHECK! ;

: BUILD-CHECK-ONE ( n n -- ) {: start finish :}
   finish start <= if E-BUILD-SOURCE throw then
   BUILD-SOURCE-BUF start + finish start - BUILD-CHECK-RAW -1 <> if
      E-BUILD-SOURCE throw
   then ;

: BUILD-READ-SOURCE ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-BUILD-SOURCE throw then
   a u FILE? 0= if E-BUILD-SOURCE throw then
   a u BUILD-SOURCE-BUF BUILD-SOURCE-CAP READ-ALL
   dup 0 <= if E-BUILD-SOURCE throw then
   BUILD-SOURCE-LEN ! ;

: BUILD-CHECK-NEXT ( n -- n ) {: start :}
   start BUILD-COLON BUILD-FIND-CHAR dup 0 < if exit then
   1+ BUILD-SKIP-WHITE BUILD-START !
   BUILD-START @ BUILD-SEMI BUILD-FIND-CHAR dup 0 < if E-BUILD-SOURCE throw then
   BUILD-END !
   BUILD-START @ BUILD-END @ BUILD-CHECK-ONE
   BUILD-DEFS @ 1+ BUILD-DEFS !
   BUILD-END @ 1+ ;

: BUILD-CHECK ( ptr u8 n -- )
   BUILD-READ-SOURCE
   0 BUILD-DEFS !
   0 BUILD-I !
   begin BUILD-I @ BUILD-SOURCE-LEN @ < while
      BUILD-I @ BUILD-CHECK-NEXT dup 0 < if
         drop BUILD-SOURCE-LEN @ BUILD-I !
      else
         BUILD-I !
      then
   repeat
   BUILD-DEFS @ 0= if E-BUILD-SOURCE throw then ;

: BUILD-EXPECT ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-BUILD-PATH throw then
   a u FILE? 0= if E-BUILD-PATH throw then ;

: BUILD-ARTIFACT ( ptr u8 n ptr u8 n -- ptr u8 n ) {: root:ptr rootu name:ptr nameu :}
   rootu 0 <= if E-BUILD-PATH throw then
   nameu 0 <= if E-BUILD-PATH throw then
   rootu 1 + nameu + FS-PATH-CAP > if E-BUILD-PATH throw then
   root rootu name nameu BUILD-PATH-BUF JOIN-PATH
   BUILD-PATH-BUF swap ;

: BUILD-STEP ( ptr u8 n [ -- n ] -- ) {: name:ptr nameu q :}
   nameu 0 <= if E-BUILD-COMMAND throw then
   q execute {: rc :}
   rc 0 <> if E-BUILD-STATUS throw then ;

: BUILD-RUN ( ptr u8 n ptr u8 n -- n ) {: cmd:ptr cmdu artifact:ptr artifactu :}
   cmdu 0 <= if E-BUILD-COMMAND throw then
   cmd cmdu FILE? 0= if E-BUILD-COMMAND throw then
   cmd cmdu >LEN PROC-RUN-RC RC>N {: rc :}
   rc 0 <> if E-BUILD-STATUS throw then
   artifact artifactu BUILD-EXPECT
   rc ;

: BUILD-STEP-VALIDATE ( ptr a -- ) {: rec:ptr :}
   rec BUILD-STEP-NAME$ nip 0 <= if E-BUILD-COMMAND throw then
   rec BUILD-STEP-COMMAND$ FILE? 0= if E-BUILD-COMMAND throw then
   rec BUILD-STEP-TMP$ DIR? 0= if E-BUILD-PATH throw then
   rec BUILD-STEP-ARTIFACT$ nip 0 <= if E-BUILD-PATH throw then ;

: BUILD-STEP-RUN ( ptr a -- n ) {: rec:ptr :}
   rec BUILD-STEP-VALIDATE
   rec BUILD-STEP-COMMAND$ rec BUILD-STEP-ARTIFACT$ BUILD-RUN {: rc :}
   rc rec BUILD-STEP-RC!
   rc ;
