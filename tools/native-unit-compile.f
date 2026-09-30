\ Bounded compilation of one package source through the retained evaluator.
\ The engine calls GUARD at each real token dispatch and before an immediate
\ executes. SOURCE-UNIT owns the nested source frame and require registry.

require lib/prelude.f
require lib/string.f
require src/core/include.f

package UNIT-COMPILE

private

64 constant NAME-CAP
70 constant UNIT-RC

create NAME NAME-CAP allot
variable NAME-U
TYPED-VARIABLE BODY-XT [ -- bool ]
variable ACTIVE
variable STAGE
variable REQUIRE-XT

\ Stages: prologue, parsed package pending, body, literal pending constant,
\ current WID pending protection, closed, imported.
0 constant PROLOGUE
1 constant PACKAGE-NAME
2 constant BODY
3 constant CONSTANT-NAME
4 constant PROTECT-WID
5 constant CLOSED
6 constant IMPORTED

TRUSTED: CLEAR-BORROWED ( -- ) 0 BODY-XT ! ;
TRUSTED: INPUT@ ( -- ptr u8 ) data-base INP-CELL + @ ;
TRUSTED: INPUT! ( ptr u8 -- ) data-base INP-CELL + ! ;

: TOKEN= ( ptr u8 n ptr u8 n -- bool ) STR= ;

\ Compare the resolved code entry, not the token spelling: an open package
\ may shadow any ordinary dictionary word used by the allowed top-level forms.
TRUSTED: BOUND-REQUIRE? ( n -- bool ) REQUIRE-XT @ = ;
TRUSTED: ORIGINAL-CURRENT? ( n -- bool ) ['] get-current = ;
TRUSTED: ORIGINAL-PROTECT? ( n -- bool ) ['] prot-wid-add = ;

: RESOLVED-TOKEN ( ptr u8 n n -- n )
   {: a:ptr u:n actual:n :}
   a u s" require" TOKEN= if
      actual BOUND-REQUIRE? 0= if UNIT-RC throw then 0 exit
   then
   a u s" get-current" TOKEN= if
      actual ORIGINAL-CURRENT? 0= if UNIT-RC throw then 0 exit
   then
   a u s" prot-wid-add" TOKEN= if
      actual ORIGINAL-PROTECT? 0= if UNIT-RC throw then 0 exit
   then
   UNIT-RC throw ;

: REQUIRE-AVAILABLE ( -- )
   INPUT@ {: saved:ptr :}
   parse-name {: path:ptr size:n :}
   saved INPUT!
   size 0= if UNIT-RC throw then
   path size SOURCE-ROOT:RESOLVE {: known:bool :}
   2drop
   known 0= if UNIT-RC throw then ;

: PACKAGE-ENTRY ( ptr u8 n -- bool )
   STAGE @ PACKAGE-NAME <> if UNIT-RC throw then
   NAME NAME-U @ TOKEN= 0= if UNIT-RC throw then
   BODY-XT @ execute {: skip:bool :}
   skip if IMPORTED else BODY then STAGE !
   skip ;

: PROLOGUE-TOKEN ( ptr u8 n -- n )
   2dup s" require" TOKEN= if
      2drop REQUIRE-AVAILABLE 0 exit
   then
   s" package" TOKEN= if PACKAGE-NAME STAGE ! 0 exit then
   UNIT-RC throw ;

: BODY-TOKEN ( ptr u8 n n -- n )
   {: a:ptr u:n cls:n :}
   cls 1 = if CONSTANT-NAME STAGE ! 0 exit then
   a u s" private" TOKEN= if 0 exit then
   a u s" public" TOKEN= if 0 exit then
   a u s" :" TOKEN= if 0 exit then
   a u s" get-current" TOKEN= if PROTECT-WID STAGE ! 0 exit then
   a u s" ;package" TOKEN= if CLOSED STAGE ! 0 exit then
   UNIT-RC throw ;

: INTERPRET-TOKEN ( ptr u8 n n -- n )
   {: a:ptr u:n cls:n :}
   STAGE @ PROLOGUE = if a u PROLOGUE-TOKEN exit then
   STAGE @ BODY = if a u cls BODY-TOKEN exit then
   STAGE @ CONSTANT-NAME = if
      a u s" constant" TOKEN= 0= if UNIT-RC throw then
      BODY STAGE ! 0 exit
   then
   STAGE @ PROTECT-WID = if
      a u s" prot-wid-add" TOKEN= 0= if UNIT-RC throw then
      BODY STAGE ! 0 exit
   then
   UNIT-RC throw ;

: GUARD ( ptr u8 n n n -- n )
   {: a:ptr u:n cls:n event:n :}
   event 3 = if UNIT-RC throw then
   event 4 = if a u cls RESOLVED-TOKEN exit then
   event 5 = if
      STAGE @ PACKAGE-NAME = if 0 exit then
      STAGE @ CLOSED = STAGE @ IMPORTED = or 0= if UNIT-RC throw then
      0 exit
   then
   event 2 = if a u PACKAGE-ENTRY if -1 else 0 then exit then
   a u cls INTERPRET-TOKEN ;

TRUSTED: RUN ( [ -- ] -- n ) ['] GUARD swap unit-compile-run ;

public

\ The caller pins the loader from the active source window before selecting a
\ unit. The resolved word must match this entry at the real pre-BLR dispatch.
: BIND-REQUIRE ( n -- )
   ACTIVE @ if UNIT-RC throw then
   dup 0= if UNIT-RC throw then
   REQUIRE-XT ! ;

: WITH ( ptr u8 n [ -- bool ] [ -- ] -- )
   {: a:ptr u:n body q :}
   ACTIVE @ 0<> REQUIRE-XT @ 0= or
   u 0 <= or u NAME-CAP > or tier@ 1 <> or if UNIT-RC throw then
   a NAME u BYTE-COPY u NAME-U !
   body BODY-XT !
   PROLOGUE STAGE !
   1 ACTIVE !
   q RUN {: rc:n :}
   0 ACTIVE !
   CLEAR-BORROWED
   rc 0<> if rc throw then
   STAGE @ CLOSED = STAGE @ IMPORTED = or 0= if UNIT-RC throw then ;

;package
