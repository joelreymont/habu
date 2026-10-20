\ Explicit source-file object export/import on the compiler already running.
\ The selected file is loaded by the ordinary source loader. Its object carries
\ native code, dictionary rows, checker facts and nested source inputs.

1 set-tier
require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/engine-id.f
require src/core/sha256.f
require src/habu/app-image.f
require src/habu/xref.f
require src/os/script-argv.f
require src/os/env-base.f
require tools/native-file-cache.f

package NATIVE-FILE-EXPLICIT
private

variable EXPORTING
variable UNIT-SEEN
variable COLLECTING
create UNIT-PATH FS-PATH-CAP allot variable UNIT-PATH-U
create UNIT-ROOT FS-PATH-CAP allot variable UNIT-ROOT-U
create ARTIFACT FS-PATH-CAP allot variable ARTIFACT-U
create KEY 64 allot
create HASH-CTX SHA256-CTX-BYTES allot
create HASH-RAW 32 allot
create HASH-CELL 1 cells allot
create CAPTURED-PRODUCER 32 allot
variable CAPTURED-PRODUCER?
create SAVED-ROOT FS-PATH-CAP allot
variable SAVED-ROOT-U
PTR-VARIABLE PREFIX-PATH variable PREFIX-U
PTR-VARIABLE SOURCE-PATH variable SOURCE-U
PTR-VARIABLE DOWNSTREAM-PATH variable DOWNSTREAM-U
TYPED-VARIABLE SOURCE-BODY [ n n -- ]
variable SOURCE-BODY-U
variable ENV-RESET

CAST: USER-NEXT-XT ( n -- [ [ ptr u8 -- ] -- ] )
CAST: ENV-USE-XT ( n -- [ [ ptr u8 n ptr u8 n -- ] -- ] )
CAST: ENV-RESET-XT ( n -- [ -- ] )

: ENV-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if
      drop s" native-build: engine lacks ENV-INPUT hooks" 74 die
   then XREF-START ;

: ARTIFACT$ ( -- ptr u8 n ) ARTIFACT ARTIFACT-U @ ;
: BAD ( -- ) E-NUNIT-PROFILE throw ;
: HASH-N ( n -- )
   HASH-CELL !
   HASH-CTX HASH-CELL CELL SHA256-FEED ;
: HASH-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   u HASH-N
   HASH-CTX a u SHA256-FEED ;
: KEY-BEGIN ( -- )
   HASH-CTX SHA256-BEGIN
   s" native-file-explicit-hfo-v2" HASH-BYTES
   ENGINE-ID:KEY$ HASH-BYTES
   CAPTURED-PRODUCER? @ if
      CAPTURED-PRODUCER 32
   else
      SOURCE-PRODUCER:HASH$
   then HASH-BYTES ;
: HASH-USER-CELL ( ptr u8 -- ) CELL-VIEW @ HASH-N ;
: HASH-USER-NEXT ( -- )
   \ Resolve after the prefix: it may have introduced TASK.
   s" TASK:OWNED-USER-NEXT" XREF-FIND dup XREF-FOUND? if
      1 HASH-N
      XREF-START USER-NEXT-XT ['] HASH-USER-CELL swap execute
   else
      drop 0 HASH-N
   then ;
: KEY-END ( -- )
   HASH-USER-NEXT
   HASH-CTX HASH-RAW SHA256-END
   HASH-RAW KEY SHA256>HEX ;

: ARTIFACT! ( ptr u8 n -- ) {: path:ptr size:n :}
   size 0 <= size FS-PATH-CAP > or if E-FS-PATH throw then
   path c@ 47 = if
      path ARTIFACT size BYTE-COPY size ARTIFACT-U ! exit
   then
   SOURCE-ROOT:CWD$ path size ARTIFACT JOIN-PATH ARTIFACT-U ! ;

: SELECT-SOURCE ( ptr u8 n -- )
   SOURCE-ROOT:CANON-OS {: path:ptr size:n exists:bool :}
   exists 0= size FS-PATH-CAP > or if E-BUILD-SOURCE throw then
   path UNIT-PATH size BYTE-COPY size UNIT-PATH-U ! ;

: CLASS-CHECK ( -- )
   SCRIPT-ARGC 7 = if
      6 SCRIPT-ARGV$ s" whitebox" STR= 0= if BAD then
      ENGINE-INTERNAL:IMAGE-CLASS ENGINE-INTERNAL:IMAGE-WHITEBOX <>
         if BAD then
   else
      ENGINE-INTERNAL:IMAGE-CLASS ENGINE-INTERNAL:IMAGE-SEALED <>
         if BAD then
   then ;

: ARGS ( -- )
   SCRIPT-ARGC 6 < SCRIPT-ARGC 7 > or if BAD then
   0 SCRIPT-ARGV$ s" --export-file" STR= if 1 EXPORTING !
   else 0 SCRIPT-ARGV$ s" --import-file" STR= if 0 EXPORTING !
   else BAD then then
   1 SCRIPT-ARGV$ PREFIX-U ! PREFIX-PATH !
   2 SCRIPT-ARGV$ 2dup SOURCE-U ! SOURCE-PATH ! SELECT-SOURCE
   3 SCRIPT-ARGV$ DOWNSTREAM-U ! DOWNSTREAM-PATH !
   4 SCRIPT-ARGV$ ARTIFACT!
   CLASS-CHECK ;

: SOURCE-WHOLE ( -- )
   0 SOURCE-BODY-U @ SOURCE-BODY @ execute ;
: EXPORT-SOURCE ( -- )
   NUNIT-CAPTURE:START-UNIT
   1 COLLECTING !
   ['] SOURCE-WHOLE catch {: rc:n :}
   0 COLLECTING !
   rc 0<> if rc throw then ;
: EXPORT-LOAD ( -- )
   KEY-END
   UNIT-PATH UNIT-PATH-U @ UNIT-ROOT UNIT-ROOT-U @
      NFILE-CACHE:EXPLICIT-START
   CWORK:FILE-MARK
   NUNIT-DEP:BEGIN-UNIT
   ['] EXPORT-SOURCE NUNIT-CAPTURE:WITH
   NFILE-INPUTS:BYTES$ {: inputs:ptr inputu:n :}
   UNIT-PATH UNIT-PATH-U @ UNIT-ROOT UNIT-ROOT-U @
      KEY 64 HASH-RAW ARTIFACT$ SOURCE-BODY-U @ inputs inputu
      NFILE-CACHE:EXPLICIT-EXPORT
   NUNIT-DEP:CLOSE ;
: IMPORT-LOAD ( -- )
   KEY-END
   UNIT-PATH UNIT-PATH-U @ UNIT-ROOT UNIT-ROOT-U @
      NFILE-CACHE:EXPLICIT-START
   UNIT-PATH UNIT-PATH-U @ UNIT-ROOT UNIT-ROOT-U @
      KEY 64 HASH-RAW ARTIFACT$ SOURCE-BODY-U @
      NFILE-CACHE:EXPLICIT-IMPORT
   s" native-build: file unit hit" type cr ;

: ACTIVE-BYTES ( -- ptr u8 n )
   SOURCE-ROOT:ACTIVE-SOURCE
      {: path:ptr pathu:n root:ptr rootu:n bytes:ptr size:n :}
   path drop pathu drop root drop rootu drop
   bytes size ;
: HASH-LOAD ( ptr u8 n -- )
   HASH-BYTES ACTIVE-BYTES HASH-BYTES ;
: CANON ( ptr u8 n -- ptr u8 n bool )
   {: path:ptr size:n :}
   path size SOURCE-ROOT:CANON-OS {: answer:ptr answeru:n exists:bool :}
   COLLECTING @ if path size answer answeru exists NFILE-INPUTS:ADD-CANON then
   answer answeru exists ;
: READ ( ptr u8 n ptr u8 n -- ptr u8 n )
   SOURCE-ROOT:READ-OS ;
: ENV ( ptr u8 n ptr u8 n -- )
   COLLECTING @ if NFILE-INPUTS:ADD-ENV else 2drop 2drop then ;
: SELECTED? ( ptr u8 n -- bool )
   UNIT-PATH UNIT-PATH-U @ STR= ;
: LOAD ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   UNIT-SEEN @ 0= if path pathu HASH-LOAD then
   COLLECTING @ if path pathu ACTIVE-BYTES NFILE-INPUTS:ADD then
   path pathu SELECTED? UNIT-SEEN @ 0= and if
      path pathu ACTIVE-BYTES NFILE-INPUTS:ADD
      SOURCE-BODY @ {: prior :} SOURCE-BODY-U @ {: prior-u:n :}
      q SOURCE-BODY ! sourceu SOURCE-BODY-U !
      rootu FS-PATH-CAP > if E-FS-PATH throw then
      root UNIT-ROOT rootu BYTE-COPY rootu UNIT-ROOT-U !
      1 UNIT-SEEN !
      EXPORTING @ if ['] EXPORT-LOAD else ['] IMPORT-LOAD then
         catch {: rc:n :}
      prior SOURCE-BODY ! prior-u SOURCE-BODY-U !
      rc 0<> if rc throw then
   else 0 sourceu q execute then ;

: SOURCE-LOAD ( -- )
   s" ENV-INPUT:RESET" ENV-XT ENV-RESET !
   NFILE-INPUTS:RESET
   KEY-BEGIN
   NUNIT-CAPTURE:BIND-PRODUCER
   NUNIT-CAPTURE:START-FILE-PREFIX
   ['] CANON ['] READ SOURCE-INPUT:USE
   ['] LOAD SOURCE-UNIT:USE
   ['] ENV s" ENV-INPUT:USE" ENV-XT ENV-USE-XT execute
   PREFIX-PATH @ PREFIX-U @ required
   SOURCE-PATH @ SOURCE-U @ required
   DOWNSTREAM-PATH @ DOWNSTREAM-U @ included
   UNIT-SEEN @ 0= if BAD then ;
: RESTORE-ROOT ( -- )
   SAVED-ROOT-U @ 0 > if
      SAVED-ROOT SAVED-ROOT-U @ SOURCE-ROOT:ENGINE!
   then
   FS-PATH-CAP 0 ?do 0 SAVED-ROOT i + c! loop
   0 SAVED-ROOT-U ! ;
: CLOSE ( -- )
   SOURCE-UNIT:RESET SOURCE-INPUT:RESET
   ENV-RESET @ dup 0<> if ENV-RESET-XT execute else drop then
   0 ENV-RESET !
   NFILE-CACHE:EXPLICIT-CLOSE ;

public
: CAPTURE-PRODUCER ( -- )
   SOURCE-PRODUCER:HASH$ {: a:ptr u:n :}
   u 32 <> if BAD then
   a CAPTURED-PRODUCER u BYTE-COPY
   1 CAPTURED-PRODUCER? ! ;

: SAVE-ROOT ( -- )
   SOURCE-ROOT:ENGINE$ {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a SAVED-ROOT u BYTE-COPY
   u SAVED-ROOT-U ! ;

: RUN-INNER ( -- )
   RESTORE-ROOT
   ARGS
   0 UNIT-SEEN ! 0 COLLECTING !
   ['] SOURCE-LOAD ['] CLOSE finally
   5 SCRIPT-ARGV$ APP-IMAGE:SAVE ;
: RUN ( -- )
   ['] RUN-INNER catch {: rc:n :}
   rc 0<> if
      s" native-build: uncaught throw code " type rc . cr
      s" " 74 die
   then ;
;package
