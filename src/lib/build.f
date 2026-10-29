\ build.f - checked helpers for Habu build scripts (package BUILD).
\ Build children get /dev/null as stdin; raw process exits belong only at the
\ final CLI or script boundary.
\ STORAGE CLASS. PROCESS-WIDE, single-task: ARTIFACT's path lives until the
\ next call; CHECK's file mapping is released when it returns or throws.
require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/process.f

package BUILD

10 constant BUILD-LF
13 constant BUILD-CR
32 constant BUILD-SP
58 constant BUILD-COLON
59 constant BUILD-SEMI

create BUILD-PATH-BUF PATH-CAP allot

PTR-VARIABLE BUILD-SOURCE               \ the mapping CHECK reads the file into
variable BUILD-SOURCE-LEN
variable BUILD-FD                       \ CHECK's open descriptor until READ-SIZED owns it
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

: BUILD-FIND-CHAR ( n n -- n ) {: start ch :}
   start begin dup BUILD-SOURCE-LEN @ < while
      dup BUILD-SOURCE @ + c@ ch = if exit then
      1+
   repeat drop -1 ;

: BUILD-SKIP-WHITE ( n -- n )
   begin dup BUILD-SOURCE-LEN @ < while
      dup BUILD-SOURCE @ + c@ BUILD-WHITE? if
         1+
      else
         exit
      then
   repeat ;

: BUILD-CHECK-ONE ( n n -- ) {: start finish :}
   finish start <= if E-BUILD-SOURCE throw then
   BUILD-SOURCE @ start + finish start - CHECK! -1 <> if
      E-BUILD-SOURCE throw
   then ;

: BUILD-CHECK-NEXT ( n -- n ) {: start :}
   start BUILD-COLON BUILD-FIND-CHAR dup 0 < if exit then
   1+ BUILD-SKIP-WHITE BUILD-START !
   BUILD-START @ BUILD-SEMI BUILD-FIND-CHAR dup 0 < if E-BUILD-SOURCE throw then
   BUILD-END !
   BUILD-START @ BUILD-END @ BUILD-CHECK-ONE
   BUILD-DEFS @ 1+ BUILD-DEFS !
   BUILD-END @ 1+ ;

: BUILD-SCAN ( -- )
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

: BUILD-READ-BODY ( fd n ptr u8 NUM:alloc-byte-len -- )
   drop {: file:fd size:n buf:ptr :}
   -1 BUILD-FD !
   file buf size FS:READ-SIZED
   buf BUILD-SOURCE !
   size BUILD-SOURCE-LEN !
   BUILD-SCAN ;

: BUILD-READ ( fd n -- ) {: file:fd size:n :}
   file size  size MEM:BYTES-ALLOC-LEN [: BUILD-READ-BODY ;] MEM:WITH-BYTES ;

: BUILD-CHECK-DONE ( -- )
   BUILD-FD @ 0 >= if BUILD-FD @ close then
   -1 BUILD-FD !
   NULL-PTR BUILD-SOURCE !
   0 BUILD-SOURCE-LEN ! ;

public

: CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-BUILD-SOURCE throw then
   a u FS:FILE? 0= if E-BUILD-SOURCE throw then
   a u FS:OPEN-SIZED {: file:fd size:n found:bool :}
   found 0= if E-BUILD-SOURCE throw then
   size 0= if file FD>N close E-BUILD-SOURCE throw then
   file FD>N BUILD-FD !
   file size [: BUILD-READ ;] [: BUILD-CHECK-DONE ;] finally ;

private

: BUILD-EXPECT ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-BUILD-PATH throw then
   a u FS:FILE? 0= if E-BUILD-PATH throw then ;

public

: ARTIFACT ( ptr u8 n ptr u8 n -- ptr u8 n ) {: root:ptr rootu:n name:ptr nameu:n :}
   rootu 0 <= if E-BUILD-PATH throw then
   nameu 0 <= if E-BUILD-PATH throw then
   \ The name is compared with the room the root and its separator leave: in a
   \ sum, a length near the maximum cell wraps back into range.
   nameu PATH-CAP 1 - rootu - > if E-BUILD-PATH throw then
   root rootu name nameu BUILD-PATH-BUF FS:JOIN-PATH
   BUILD-PATH-BUF swap ;

: STEP ( ptr u8 n [ -- n ] -- ) {: name:ptr nameu:n q :}
   nameu 0 <= if E-BUILD-COMMAND throw then
   q execute {: rc:n :}
   rc 0 <> if E-BUILD-STATUS throw then ;

private

\ A build child never inherits the caller's stdin: a `bin/hb` command script
\ that falls into the stdin REPL would block the build on a never-EOF pipe (a
\ gate pool worker's).
: BUILD-DEV-NULL-RD ( -- fd )
   s" /dev/null" FS:>PATHZ open-rd dup 0 < if drop E-FS-OPEN throw then >FD ;

public

: RUN ( ptr u8 n ptr u8 n -- n ) {: cmd:ptr cmdu:n artifact:ptr artifactu:n :}
   cmdu 0 <= if E-BUILD-COMMAND throw then
   cmd cmdu FS:FILE? 0= if E-BUILD-COMMAND throw then
   BUILD-DEV-NULL-RD {: nullfd:fd :}
   cmd cmdu >LEN nullfd -1 >FD -1 >FD PROC-RUN-IO-RC
   nullfd FD>N close
   MATCH result
     ok  OF ENDOF
     err OF drop E-BUILD-STATUS throw ENDOF              \ nonzero exit / signal -> build failure
   ;MATCH {: rc:n :}
   artifact artifactu BUILD-EXPECT
   rc ;

;package
