\ Explicit checked package reuse for native builds. Ordinary builds do not
\ load this module or its source identity and NBR publication dependencies.

require tools/native-build-core.f
require tools/native-source-view.f
require tools/native-unit-compile.f
require tools/native-unit-object.f

package NATIVE-BUILD

private

1 constant UNIT-EXPORT
2 constant UNIT-IMPORT
variable UNIT-MODE
variable UNIT-SEEN
variable UNIT-SELECTED-ID
create UNIT-KEY-BUF 64 allot
create UNIT-ARTIFACT FS-PATH-CAP allot
variable UNIT-ARTIFACT-U
create UNIT-PATH FS-PATH-CAP allot
variable UNIT-PATH-U
TYPED-VARIABLE UNIT-SOURCE [ -- ]

\ The retained host can predate these append-only target owner callbacks.
\ Load-time constants are unavailable to this driver until the fresh target
\ checker loads, so use their frozen ABI byte offsets at this call boundary.
$2B0 constant UNIT-MARK-OFF
$2B8 constant UNIT-EXPORT-OFF
$2C0 constant UNIT-IMPORT-OFF

: UNIT-ARTIFACT$ ( -- ptr u8 n ) UNIT-ARTIFACT UNIT-ARTIFACT-U @ ;

: UNIT-ARTIFACT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= u FS-PATH-CAP > or if E-FS-PATH throw then
   a c@ 47 = if
      a UNIT-ARTIFACT u BYTE-COPY u UNIT-ARTIFACT-U ! exit
   then
   SOURCE-ROOT:CWD$ a u UNIT-ARTIFACT JOIN-PATH UNIT-ARTIFACT-U ! ;

: UNIT-CHECKER-MARK ( -- )
   CHECKER-OWNER UNIT-MARK-OFF + CELL-VIEW @
   RESET-XT execute ;

\ The owner record holds its unit callbacks as code address integers.
CAST: UNIT-EXPORT-XT ( n -- [ -- ptr u8 n ] )
CAST: UNIT-IMPORT-XT ( n -- [ ptr u8 n -- ] )

: UNIT-CHECKER$ ( -- ptr u8 n )
   CHECKER-OWNER UNIT-EXPORT-OFF + CELL-VIEW @
   UNIT-EXPORT-XT execute ;

: UNIT-CHECKER-IMPORT ( ptr u8 n -- )
   CHECKER-OWNER UNIT-IMPORT-OFF + CELL-VIEW @
   UNIT-IMPORT-XT execute ;

: UNIT-CUT ( -- bool )
   UNIT-SELECTED-ID @ SOURCE-VIEW:UNIT-KEY {: key:ptr size:n :}
   size 64 <> if E-NUNIT-PROFILE throw then
   key UNIT-KEY-BUF size BYTE-COPY
   UNIT-CHECKER-MARK
   UNIT-MODE @ UNIT-EXPORT = if
      NUNIT-CAPTURE:START-UNIT false exit
   then
   UNIT-MODE @ UNIT-IMPORT = if
      UNIT-ARTIFACT$ UNIT-KEY-BUF 64 NUNIT-OBJECT:IMPORT
      UNIT-CHECKER-IMPORT
      NUNIT-OBJECT:CLOSE
      s" native-build: NBR unit hit" type cr
      true exit
   then
   E-NUNIT-PROFILE throw ;

: UNIT-COMPILE-SOURCE ( -- )
   s" NBR" ['] UNIT-CUT UNIT-SOURCE @ UNIT-COMPILE:WITH ;

: UNIT-EXPORT-SOURCE ( -- )
   ['] UNIT-COMPILE-SOURCE NUNIT-CAPTURE:WITH
   UNIT-CHECKER$ {: checker:ptr size:n :}
   UNIT-ARTIFACT$ UNIT-KEY-BUF 64 checker size NUNIT-OBJECT:EXPORT-UNIT
   NUNIT-OBJECT:CLOSE ;

: UNIT-SOURCE-RUN ( -- )
   UNIT-MODE @ UNIT-EXPORT = if UNIT-EXPORT-SOURCE exit then
   UNIT-COMPILE-SOURCE ;

: UNIT-FILE? ( ptr u8 n -- bool )
   UNIT-PATH UNIT-PATH-U @ STR= ;

: UNIT-LOAD-OWNED ( ptr u8 n ptr u8 n ptr u8 [ -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr q :}
   path pathu root rootu source SOURCE-VIEW:START-LOAD {: id:n :}
   path pathu UNIT-FILE? if
      UNIT-SEEN @ 0<> if E-NUNIT-PROFILE throw then
      id UNIT-SELECTED-ID !
      UNIT-SOURCE @ {: prior :}
      q UNIT-SOURCE !
      1 UNIT-SEEN !
      ['] UNIT-SOURCE-RUN catch
      prior UNIT-SOURCE !
   else
      q catch
   then {: rc:n :}
   id SOURCE-VIEW:FINISH-LOAD
   rc 0<> if rc throw then ;

: UNIT-PREFLIGHT-BODY ( -- )
   SOURCE-ROOT:CWD$ s" src/compiler/native/branch.f" UNIT-PATH JOIN-PATH
   UNIT-PATH swap SOURCE-ROOT:CANON-OS {: a:ptr size:n exists:bool :}
   exists 0= if E-BUILD-SOURCE throw then
   a UNIT-PATH size BYTE-COPY size UNIT-PATH-U !
   s" tools/native-unit-build.f" SOURCE-VIEW:COLLECT
   s" lib/c2-owner.f" SOURCE-VIEW:COLLECT
   SOURCE-VIEW:USE ;

: UNIT-PREFLIGHT ( -- )
   SOURCE-VIEW:OPEN
   ['] UNIT-PREFLIGHT-BODY catch
   dup 0<> if SOURCE-VIEW:CLOSE throw then drop ;

\ OPEN-TARGET-XT answers a target operation's entry as a code address integer.
CAST: SOURCE-USE-XT ( n -- [ [ ptr u8 n -- ptr u8 n bool ] [ ptr u8 n ptr u8 n -- ptr u8 n ] -- ] )
CAST: SOURCE-UNIT-USE-XT ( n -- [ [ ptr u8 n ptr u8 n ptr u8 [ -- ] -- ] -- ] )

: BIND-TARGET-SOURCE ( -- )
   1 TARGET-SOURCE-BOUND !
   SOURCE-VIEW:CALLBACKS
   s" SOURCE-INPUT:USE" OPEN-TARGET-XT SOURCE-USE-XT execute
   ['] UNIT-LOAD-OWNED
   s" SOURCE-UNIT:USE" OPEN-TARGET-XT SOURCE-UNIT-USE-XT execute
   s" require" OPEN-TARGET-XT UNIT-COMPILE:BIND-REQUIRE
   s" REQUIRE-BOOT-OPEN" OPEN-TARGET-XT SOURCE-RESET-XT execute ;

: UNIT-CLASS-ARG! ( n -- ) {: at:n :}
   CLASS-SEALED CLASS-WANTED !
   SCRIPT-ARGC at 1+ < if exit then
   at SCRIPT-ARGV$ WHITEBOX-ARG$ STR= 0= if E-NUNIT-PROFILE throw then
   CLASS-WHITEBOX CLASS-WANTED ! ;

: UNIT-ARGS! ( -- n )
   0 SCRIPT-ARGV$ s" --export-unit" STR= if
      SCRIPT-ARGC 4 < SCRIPT-ARGC 5 > or if E-NUNIT-PROFILE throw then
      1 SCRIPT-ARGV$ s" NBR" STR= 0= if E-NUNIT-PROFILE throw then
      2 SCRIPT-ARGV$ UNIT-ARTIFACT!
      4 UNIT-CLASS-ARG!
      UNIT-EXPORT UNIT-MODE ! 3 exit
   then
   0 SCRIPT-ARGV$ s" --import-unit" STR= if
      SCRIPT-ARGC 3 < SCRIPT-ARGC 4 > or if E-NUNIT-PROFILE throw then
      1 SCRIPT-ARGV$ UNIT-ARTIFACT!
      3 UNIT-CLASS-ARG!
      UNIT-IMPORT UNIT-MODE ! 2 exit
   then
   E-NUNIT-PROFILE throw ;

: UNIT-CHECK-SEEN ( -- )
   UNIT-SEEN @ 0= if E-NUNIT-PROFILE throw then ;

: UNIT-CLOSE-SOURCE ( -- )
   SOURCE-VIEW:READY? if SOURCE-VIEW:CLOSE then ;

public

: RUN-UNIT ( [ n n -- n ] bool -- ) {: query bootstrap:bool :}
   BUILD-TARGET:IDLE-CK
   UNIT-ARGS! {: out-arg:n :}
   0 UNIT-SEEN !
   UNIT-PREFLIGHT
   ['] BIND-TARGET-SOURCE ['] UNIT-CHECK-SEEN ['] UNIT-CLOSE-SOURCE SOURCE-POLICY!
   out-arg SCRIPT-ARGV$ OUTPUT!
   query bootstrap ['] SOURCE-WRITER-DISPATCH RUN-READY-RC {: rc:n :}
   rc 0= if REPORT-CLASS then
   s" " rc EXIT-RC die ;

;package
