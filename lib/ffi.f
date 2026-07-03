\ ffi.f - checked C-ABI foreign calls with dynamic loading.

s" lib/errors.f" required
s" lib/string.f" required
s" lib/ffi-abi.f" required

2 constant RTLD-NOW                       \ dlopen flag: resolve all symbols now

package FFI

$800 constant FDEF-CAP
$400 constant FDEF-TOK-CAP
0 constant FDEF-N
1 constant FDEF-PTR
2 constant FDEF-NOM
3 constant FDEF-VOID

create FDEF-BUF FDEF-CAP allot
create FDEF-TOK-BUF FDEF-TOK-CAP allot
create FDEF-TAG FFI-MAX-ARGS cells allot
create FDEF-OFF FFI-MAX-ARGS cells allot
create FDEF-LEN FFI-MAX-ARGS cells allot
variable FDEF-U
variable FDEF-TOK-U
variable FDEF-ARGC
variable FDEF-OUT
variable FDEF-OUT-OFF
variable FDEF-OUT-LEN

: FDEF-CLEAR ( -- )
   0 FDEF-U !
   0 FDEF-TOK-U !
   0 FDEF-ARGC !
   FDEF-VOID FDEF-OUT !
   0 FDEF-OUT-OFF !
   0 FDEF-OUT-LEN ! ;

: FDEF-ROOM ( n -- )
   FDEF-U @ + FDEF-CAP > if E-FFI-ARITY throw then ;

: FDEF-TOK-ROOM ( n -- )
   FDEF-TOK-U @ + FDEF-TOK-CAP > if E-FFI-ARITY throw then ;

: FDEF+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u FDEF-ROOM
   0 begin dup u < while
      dup a + c@ FDEF-BUF FDEF-U @ + c!
      FDEF-U @ 1+ FDEF-U !
      1+
   repeat drop ;

: FDEF-C+ ( n -- ) {: c:n :}
   1 FDEF-ROOM
   c FDEF-BUF FDEF-U @ + c!
   FDEF-U @ 1+ FDEF-U ! ;

: FDEF-SP ( -- )
   32 FDEF-C+ ;

: FDEF-NL ( -- )
   10 FDEF-C+ ;

: FDEF-TOK+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   u FDEF-TOK-ROOM
   FDEF-TOK-U @ {: off:n :}
   0 begin dup u < while
      dup a + c@ FDEF-TOK-BUF FDEF-TOK-U @ + c!
      FDEF-TOK-U @ 1+ FDEF-TOK-U !
      1+
   repeat drop
   off ;

: FDEF-CHECK-ARG ( n -- )
   FFI-MAX-ARGS FFI-CHECK-INDEX ;

: FDEF-TAG! ( n n -- )
   {: tag:n idx:n :}
   idx FDEF-CHECK-ARG
   tag FDEF-TAG idx cells + ! ;

: FDEF-OFF! ( n n -- )
   {: off:n idx:n :}
   idx FDEF-CHECK-ARG
   off FDEF-OFF idx cells + ! ;

: FDEF-LEN! ( n n -- )
   {: len:n idx:n :}
   idx FDEF-CHECK-ARG
   len FDEF-LEN idx cells + ! ;

: FDEF-TAG@ ( n -- n )
   dup FDEF-CHECK-ARG
   FDEF-TAG swap cells + @ ;

: FDEF-OFF@ ( n -- n )
   dup FDEF-CHECK-ARG
   FDEF-OFF swap cells + @ ;

: FDEF-LEN@ ( n -- n )
   dup FDEF-CHECK-ARG
   FDEF-LEN swap cells + @ ;

: FDEF-TOK$ ( n -- ptr u8 n ) {: idx:n :}
   FDEF-TOK-BUF idx FDEF-OFF@ + idx FDEF-LEN@ ;

: FDEF-ARG+ ( n ptr u8 n -- )
   {: tag:n a:ptr u:n :}
   FDEF-ARGC @ dup FFI-MAX-ARGS >= if E-FFI-ARITY throw then {: idx:n :}
   tag idx FDEF-TAG!
   a u FDEF-TOK+ idx FDEF-OFF!
   u idx FDEF-LEN!
   idx 1+ FDEF-ARGC ! ;

: FDEF-OUT! ( n ptr u8 n -- )
   {: tag:n a:ptr u:n :}
   tag FDEF-OUT !
   a u FDEF-TOK+ FDEF-OUT-OFF !
   u FDEF-OUT-LEN ! ;

: FDEF-OUT$ ( -- ptr u8 n )
   FDEF-TOK-BUF FDEF-OUT-OFF @ + FDEF-OUT-LEN @ ;

: FDEF-NEXT ( -- ptr u8 n )
   parse-name dup 0= if E-FFI-SYNTAX throw then ;

: FDEF-EXPECT ( ptr u8 n -- ) {: want:ptr wantu:n :}
   FDEF-NEXT want wantu STR= 0= if E-FFI-SYNTAX throw then ;

: FDEF-EFF+ ( ptr u8 n -- )
   FDEF+ FDEF-SP ;

: FDEF-CELL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" n" STR= if STR-TRUE exit then
   a u s" i64" STR= ;

: FDEF-PTR-TAIL ( -- )
   FDEF-NEXT FDEF-EFF+ ;

: FDEF-IN-TYPE ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u FDEF-EFF+
   a u s" ptr" STR= if
      FDEF-PTR a u FDEF-ARG+
      FDEF-PTR-TAIL
      exit
   then
   a u FDEF-CELL? if FDEF-N a u FDEF-ARG+ exit then
   FDEF-NOM a u FDEF-ARG+ ;

: FDEF-OUT-TYPE ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u FDEF-EFF+
   a u s" ptr" STR= if
      FDEF-PTR a u FDEF-OUT!
      FDEF-PTR-TAIL
      exit
   then
   a u FDEF-CELL? if FDEF-N a u FDEF-OUT! exit then
   FDEF-NOM a u FDEF-OUT! ;

: FDEF-INPUTS ( -- )
   begin
      FDEF-NEXT 2dup s" --" STR= if
         FDEF-EFF+ exit
      then
      FDEF-IN-TYPE
   again ;

: FDEF-OUTPUTS ( -- )
   begin
      FDEF-NEXT 2dup s" )" STR= if 2drop s" )" FDEF+ exit then
      FDEF-OUT-TYPE
   again ;

: FDEF-EFFECT ( -- )
   s" (" FDEF-EXPECT
   s" ( " FDEF+
   FDEF-INPUTS
   FDEF-OUTPUTS ;

: FDEF-NAME-XT ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" variable " FDEF+
   a u FDEF+
   s" -XT" FDEF+
   FDEF-NL ;

: FDEF-SYM$ ( ptr u8 n -- )
   115 FDEF-C+ 34 FDEF-C+ FDEF-SP
   FDEF+
   34 FDEF-C+ FDEF-SP ;

: FDEF-RESOLVE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n sym:ptr symu:n res:ptr resu:n :}
   s"    " FDEF+
   name nameu FDEF+ s" -XT @ 0= if " FDEF+
   sym symu FDEF-SYM$
   res resu FDEF+
   s"  dup 0= if E-FFI-DLSYM throw then " FDEF+
   name nameu FDEF+ s" -XT ! then" FDEF+ FDEF-NL ;

: FDEF-ERASE-NOM ( n -- ) {: idx:n :}
   idx FDEF-TOK$ FDEF+
   s" >N" FDEF+ ;

: FDEF-ERASE ( n -- ) {: idx:n :}
   idx FDEF-TAG@ case
      FDEF-N of endof
      FDEF-PTR of s" P>N" FDEF+ FDEF-SP endof
      FDEF-NOM of idx FDEF-ERASE-NOM FDEF-SP endof
      E-FFI-ARITY throw
   endcase
   idx 10 >= if idx 10 / 48 + FDEF-C+ then
   idx 10 mod 48 + FDEF-C+
   s"  FFI-ARG!" FDEF+ FDEF-NL ;

: FDEF-MARSHAL ( -- )
   FDEF-ARGC @ begin dup 0 > while
      1- dup FDEF-ERASE
   repeat drop ;

: FDEF-REFINE-NOM ( -- )
   s" >" FDEF+
   FDEF-OUT$ FDEF+ ;

: FDEF-REFINE ( -- )
   FDEF-OUT @ case
      FDEF-VOID of s"  drop" FDEF+ endof
      FDEF-N of endof
      FDEF-PTR of s"  N>P" FDEF+ endof
      FDEF-NOM of s"  " FDEF+ FDEF-REFINE-NOM endof
      E-FFI-ARITY throw
   endcase
   FDEF-NL ;

: FDEF-BODY ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n sym:ptr symu:n res:ptr resu:n :}
   name nameu sym symu res resu FDEF-RESOLVE
   FDEF-MARSHAL
   s"    0 " FDEF+  name nameu FDEF+  s" -XT @ FFI-CALLABI" FDEF+
   FDEF-REFINE
   s" ;" FDEF+ FDEF-NL ;

TRUSTED: FDEF-EVAL ( -- )
   FDEF-BUF FDEF-U @ evaluate ;

public

: DEFINE ( -- )
   FDEF-CLEAR
   FDEF-NEXT {: name:ptr nameu:n :}
   name nameu FDEF-NAME-XT
   s" : " FDEF+ name nameu FDEF+
   FDEF-SP
   FDEF-EFFECT
   FDEF-NL
   FDEF-NEXT {: res:ptr resu:n :}
   FDEF-NEXT {: sym:ptr symu:n :}
   s" FFI;" FDEF-EXPECT
   name nameu sym symu res resu FDEF-BODY
   FDEF-EVAL ;

end-package

: FFI: ( -- )
   FFI:DEFINE ;

\ ---- dynamic loading ------------------------------------------------------
\ DLOPEN returns 0 on failure; DLSYM returns 0 for a missing symbol. Callers
\ that must not proceed on failure check the handle/fn against 0 themselves.
: DLOPEN ( ptr u8 n -- n ) {: path:ptr flags :}
   path P>N FFI-DLBUF !  flags FFI-DLBUF 8 + !
   FFI-DLBUF DLOPEN-SLOT @ ffi-call ;
: DLSYM ( n ptr u8 -- n ) {: handle name:ptr :}
   handle FFI-DLBUF !  name P>N FFI-DLBUF 8 + !
   FFI-DLBUF DLSYM-SLOT @ ffi-call ;
