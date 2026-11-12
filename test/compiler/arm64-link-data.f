\ arm64-link-data.f - a stripped program's DATA, written by LINKER:WRITE and
\ run, its kernel the tree's kernel object. One build writes two programs. main
\ prints `12 6 0 11 11 0`: the word an xt cell holds, which reads a variable
\ through a data address; the quotation an xt cell holds, stored as the build
\ ran; a scratch cell the build set to 7; a plain cell; a DATA pointer's
\ target; its `here` less the build's as it linked. The program is PIE: these
\ hold only if the start-up slides each code cell once and no DATA cell. over
\ allots past its ceiling and exits 76 naming its DP and ceiling as the build's
\ floor and ceiling place them. A second link of main writes the same bytes.
\ Each refusal (arm64-link-data-bad.f) dies, so each runs in its own child.
\ Fixture words are LD- and defined once per process: the host checker keeps a
\ symbol for each name a build defines.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/habu/layout.f
require src/habu/boot-cells.f
require src/habu/xref.f
require src/compiler/target/model.f
require src/habu/linker.f
require src/habu/link-data.f
require tools/hb-build-core.f

package ARM64-LINK-DATA

CAST: ENTRY-XT ( n -- [ -- ] )

create ROOT-BUF PATH-CAP allot variable ROOT-U
create PROG-BUF PATH-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;

: PROG$ ( ptr u8 n -- ptr u8 n )
   {: name:ptr name-n:n :}
   ROOT$ name name-n PROG-BUF FS:JOIN-PATH {: u:n :}
   PROG-BUF u ;


\ LD-LEAF2 is reached only from the quotation LD-MAKE stores. LD-HERE holds
\ the build's LD-DIST as it links main.
: FIXTURE ( -- )
   s" variable LD-V  11 LD-V !" evaluate-closed
   s" : LD-PTR ( -- ) create here ptr-cell-mark 0 , does> ( -- ptr ptr n ) ;" evaluate-closed
   s" : LD-CODE ( -- ) create 0 , does> ( -- ptr [ n -- n ] ) ;" evaluate-closed
   s" LD-PTR LD-P  LD-V LD-P !" evaluate-closed
   s" : LD-ADDR ( -- ptr n ) LD-V ;" evaluate-closed
   s" : LD-LEAF ( n -- n ) LD-ADDR @ + ;" evaluate-closed
   s" : LD-LEAF2 ( n -- n ) 5 + ;" evaluate-closed
   s" LD-CODE LD-XT  ' LD-LEAF LD-XT xt!" evaluate-closed
   s" variable LD-SCR  7 LD-SCR !  LD-SCR scratch-cell-mark" evaluate-closed
   s" LD-CODE LD-Q" evaluate-closed
   s" : LD-MAKE ( -- ) [: LD-LEAF2 ;] LD-Q xt! ;" evaluate-closed
   s" LD-MAKE" evaluate-closed
   s" : LD-PV ( -- n ) LD-P @ @ ;" evaluate-closed
   s" : LD-DIST ( -- n ) here NULL-PTR - LD-V NULL-PTR - - ;" evaluate-closed
   s" : LD-U ( n -- ) dup 9 > if dup 10 / recurse then 10 mod 48 + emit ;" evaluate-closed
   s" : LD-U_ ( n -- ) LD-U 32 emit ;" evaluate-closed
   s" variable LD-HERE" evaluate-closed
   s" : LD-MAIN ( -- ) 1 LD-XT @ execute LD-U_  1 LD-Q @ execute LD-U_  LD-SCR @ LD-U_  LD-V @ LD-U_  LD-PV LD-U_  LD-DIST LD-HERE @ - LD-U cr ;" evaluate-closed
   s" : LD-OVER ( -- ) 1 41 lshift allot ;" evaluate-closed ;


\ What over's refusal names: its DP and ceiling less the floor's base, read
\ from the build's cells as the kernel reads the program's.
variable OVER-DP
variable OVER-CEILING

: OVER-EXPECT ( -- )
   data-base BOOT:DP-FLOOR-CELL + @ DATA-START - {: base:n :}
   here NULL-PTR - 1 41 lshift + base - OVER-DP !
   data-base BOOT:DP-CEILING-CELL + @ base - OVER-CEILING ! ;


: STORE-ENTRY ( ptr u8 n -- )
   XREF-FIND-INDEX XREF-REC XREF-START ENTRY-XT
   data-base APP-ENTRY:XT-CELL + xt! ;


DYNAMIC-BUFFER FIRST-LINK u8

: FILE-BYTES ( ptr u8 n -- ptr u8 n )
   {: path:ptr path-n:n :}
   path path-n FS:FILE-SIZE {: size:n :}
   size FIRST-LINK-RESERVE
   path path-n 0  0 FIRST-LINK size FS:READ-AT
   0 FIRST-LINK size ;

DYNAMIC-BUFFER SECOND-LINK u8

\ main linked again from the same build gives the same file.
: SECOND-CASE ( -- )
   s" main" PROG$ FILE-BYTES {: a:ptr a-n:n :}
   a-n SECOND-LINK-RESERVE
   a 0 SECOND-LINK a-n BYTE-COPY
   s" main" PROG$ LINKER:WRITE
   s" main" PROG$ FILE-BYTES {: b:ptr b-n:n :}
   s" a second link writes the same bytes" T-LABEL
   b b-n  0 SECOND-LINK a-n  STR= TTRUE
   FIRST-LINK-RELEASE SECOND-LINK-RELEASE ;


: BODY ( -- )
   FIXTURE
   OVER-EXPECT
   s" LD-DIST LD-HERE !" evaluate-closed
   s" LD-OVER" STORE-ENTRY  s" over" PROG$ LINKER:WRITE
   s" LD-MAIN" STORE-ENTRY  s" main" PROG$ LINKER:WRITE
   SECOND-CASE ;


: RUN-PROG ( CMD:command ptr u8 n -- CMD:command n )
   {: file:ptr file-n:n :}
   file file-n PROG$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N ;

: RUN-MAIN ( CMD:command -- CMD:command )
   s" main" RUN-PROG {: rc:n :}
   s" main: xt cell, quotation, scratch, plain, DATA pointer, DP" T-LABEL
   CMD:OUT$ s\" 12 6 0 11 11 0\n" T$=
   s" main exits 0" T-LABEL  rc 0 T= ;

: RUN-OVER ( CMD:command -- CMD:command )
   s" over" RUN-PROG {: rc:n :}
   s" over names its DP and ceiling as the build's floor and ceiling place them"
      T-LABEL
   SB-RESET  OVER-DP @ FMT:SB-INT  s"  of " SB-APPEND  OVER-CEILING @ FMT:SB-INT
   CMD:ERR$ SB$ CONTAINS? TTRUE
   s" over exits 76" T-LABEL  rc 76 T= ;


: CHILD-LABEL ( ptr u8 n ptr u8 n -- )
   {: name:ptr name-n:n what:ptr what-n:n :}
   SB-RESET  s" refusal, " SB-APPEND  name name-n SB-APPEND
   s" : " SB-APPEND  what what-n SB-APPEND  SB$ T-LABEL ;

: CHILD-RC ( CMD:command ptr u8 n -- CMD:command n )
   {: name:ptr name-n:n :}
   s" --load" CMD:ARG+  s" test/compiler/arm64-link-data-bad.f" CMD:ARG+
   s" --" CMD:ARG+  name name-n CMD:ARG+  HB-BUILD-CORE:KERNEL$ CMD:ARG+
   s" bad" PROG$ CMD:ARG+
   ENGINE-CANDIDATE:PATH$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N ;

\ name's child arms, its link is refused for the reason err, and it exits 74.
: BAD-CHILD ( CMD:command ptr u8 n ptr u8 n -- CMD:command )
   {: name:ptr name-n:n err:ptr err-n:n :}
   name name-n CHILD-RC {: rc:n :}
   name name-n s" the child arms before the link" CHILD-LABEL
   CMD:OUT$ s\" ARM64-LINK-DATA-BAD-ARMED\n" T$=
   name name-n s" the link's reason" CHILD-LABEL
   CMD:ERR$ err err-n CONTAINS? TTRUE
   name name-n s" the link exits 74" CHILD-LABEL  rc 74 T= ;

: NAMES-LABEL ( ptr u8 n -- ) s" the link names what it refuses" CHILD-LABEL ;

: BAD-DATA ( CMD:command -- CMD:command )
   s" data"  s" link: a DATA pointer outside the build's space" BAD-CHILD
   s" data" NAMES-LABEL
   CMD:ERR$ s\" link: a DATA pointer outside the build's space: 1\n" CONTAINS? TTRUE ;

: BAD-HEADER ( CMD:command -- CMD:command )
   s" header"  s" link: a build stored into a header cell the link writes" BAD-CHILD
   s" header" NAMES-LABEL
   SB-RESET  s" link: the build stored into the header cell at offset " SB-APPEND
   UNCGH-CELL FMT:SB-INT  10 SB-APPEND-C
   CMD:ERR$ SB$ CONTAINS? TTRUE ;

: BAD-EVAL ( CMD:command -- CMD:command )
   s" eval"  s" link: a stripped program reaches a word it cannot run" BAD-CHILD
   s" eval" NAMES-LABEL
   CMD:ERR$ s" link: LD-EV calls evaluate-closed, which a stripped program cannot run"
   CONTAINS? TTRUE ;


public

: RUN ( -- )
   T-RESET
   s" habu-link-data" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   false RTARGET:HOST-TARGET HB-BUILD-CORE:KERNEL-BUILD
   s" app" RTARGET:HOST-TARGET [: BODY ;] LINKER:WITH
   [: RUN-MAIN ;] CMD:WITH-COMMAND
   [: RUN-OVER ;] CMD:WITH-COMMAND
   [: BAD-DATA ;] CMD:WITH-COMMAND
   [: BAD-HEADER ;] CMD:WITH-COMMAND
   [: BAD-EVAL ;] CMD:WITH-COMMAND
   ROOT$ FS:REMOVE-TREE
   T-REPORT ;

;package

ARM64-LINK-DATA:RUN
