\ install-e2e.f - installs into a scratch HOME while an hb started through the
\ installed engine is loading. Trees A and B copy this checkout and differ only
\ in src/install-race-x.f, which defines RACE-A or RACE-B, and
\ src/install-race-y.f, which calls it. With A installed, a reader loads x.f,
\ blocks on a pipe the test holds while B is installed, then loads y.f: it ends
\ cleanly only if it reads one release's pair whole. A's tree outlives two
\ installs of B while the reader runs and goes at the next. FRESH reads a release
\ installed before it starts, DEAD installs over every leftover a killed install
\ can leave, and BOTH runs two installs at once.
require src/lib/string.f
require src/lib/test.f
require src/lib/fd.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fs-list.f
require src/lib/process.f
require src/lib/spawn.f
require src/lib/engine-candidate.f
require src/core/include.f

package INSTALL-E2E
private

$1000 constant IO-CAP
30000 constant TIMEOUT-MS
create ROOT PATH-CAP allot variable ROOT-U
create HOME PATH-CAP allot variable HOME-U
create TREE-A PATH-CAP allot variable TREE-A-U
create TREE-B PATH-CAP allot variable TREE-B-U
create READER PATH-CAP allot variable READER-U
create ENGINE PATH-CAP allot variable ENGINE-U
create TREES PATH-CAP allot variable TREES-U
create LINK PATH-CAP allot variable LINK-U
create NEXT PATH-CAP allot variable NEXT-U
create KEPT PATH-CAP allot variable KEPT-U
create INTO PATH-CAP allot variable INTO-U
create TO PATH-CAP allot variable TO-U
create PATH PATH-CAP allot
IO-CAP BUFFER: OUT variable OUT-U
variable TREES-N

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: HOME$ ( -- ptr u8 n ) HOME HOME-U @ ;
: TREE-A$ ( -- ptr u8 n ) TREE-A TREE-A-U @ ;
: TREE-B$ ( -- ptr u8 n ) TREE-B TREE-B-U @ ;
: READER$ ( -- ptr u8 n ) READER READER-U @ ;
: ENGINE$ ( -- ptr u8 n ) ENGINE ENGINE-U @ ;
: TREES$ ( -- ptr u8 n ) TREES TREES-U @ ;
: LINK$ ( -- ptr u8 n ) LINK LINK-U @ ;
: NEXT$ ( -- ptr u8 n ) NEXT NEXT-U @ ;
: KEPT$ ( -- ptr u8 n ) KEPT KEPT-U @ ;
: INTO$ ( -- ptr u8 n ) INTO INTO-U @ ;
: TO$ ( -- ptr u8 n ) TO TO-U @ ;

: INTO! ( ptr u8 n -- ) {: a:ptr u:n :}
   a INTO u BYTE-COPY u INTO-U ! ;

\ Checkout path p as the same path in the tree being made.
: TO! ( ptr u8 n -- ) {: p:ptr u:n :}
   SOURCE-ROOT:CWD$ nip 1 + {: ru:n :}
   INTO$ p ru + u ru - TO FS:JOIN-PATH TO-U ! ;

\ Symlinks are not carried, as the installer carries none.
: CARRY? ( ptr u8 n -- bool ) {: p:ptr u:n :}
   p u FS:SYMLINK? if false exit then
   p u FS:DIR? if p u TO! TO$ FS:MAKE-DIRS then
   true ;

: CARRY ( ptr u8 n -- ) {: p:ptr u:n :}
   p u TO!  p u TO$ FS:COPY-FILE-STREAM ;

: CARRY-DIR ( ptr u8 n -- )
   SOURCE-ROOT:CWD$ 2swap PATH FS:JOIN-PATH {: u:n :}
   PATH u [: CARRY? ;] [: CARRY ;] FS:WALK-FILES-IF ;

: CARRY-BIN ( -- )
   INTO$ s" bin" TO FS:JOIN-PATH TO-U ! TO$ FS:MAKE-DIRS
   INTO$ s" bin/hb" TO FS:JOIN-PATH TO-U !
   ENGINE-CANDIDATE:PATH$ TO$ FS:COPY-FILE-STREAM
   TO$ FS:CHMOD-X
   SOURCE-ROOT:CWD$ s" bin/hb.map" PATH FS:JOIN-PATH {: u:n :}
   INTO$ s" bin/hb.map" TO FS:JOIN-PATH TO-U !
   PATH u TO$ FS:COPY-FILE-STREAM ;

: RACE! ( ptr u8 n ptr u8 n -- ) {: rel:ptr relu:n data:ptr size:n :}
   INTO$ rel relu TO FS:JOIN-PATH TO-U !
   TO$ data size FS:WRITE-ALL ;

: COPY-TREE ( ptr u8 n -- )
   INTO!
   s" src" CARRY-DIR
   s" tools" CARRY-DIR
   s" test" CARRY-DIR
   CARRY-BIN ;

: WRITE-A ( -- )
   TREE-A$ COPY-TREE
   s" src/install-race-x.f" S\" : RACE-A ( -- ) ;\n" RACE!
   s" src/install-race-y.f" S\" : RACE-USE ( -- ) RACE-A ;\n" RACE! ;

: WRITE-B ( -- )
   TREE-B$ COPY-TREE
   s" src/install-race-x.f" S\" : RACE-B ( -- ) ;\n" RACE!
   s" src/install-race-y.f" S\" : RACE-USE ( -- ) RACE-B ;\n" RACE! ;

: READER+ ( ptr u8 n -- ) {: a:ptr u:n :}
   READER$ a u FS:APPEND-FILE ;

\ The reader writes with `write`, so each line reaches the pipe at once.
: WRITE-READER ( -- )
   ROOT$ s" reader.f" READER FS:JOIN-PATH READER-U !
   S\" require src/install-race-x.f\npackage INSTALL-E2E-READER\n" READER+
   S\" private\n1 BUFFER: CH\npublic\n" READER+
   S\" : SAY ( ptr u8 n -- )\n   {: a:ptr u:n :}\n" READER+
   S\"    1 a u write drop  10 CH c!  1 CH 1 write drop ;\n" READER+
   S\" : AWAIT ( -- )\n" READER+
   S\"    begin 0 CH 1 read 1 = while CH c@ 10 = if exit then repeat ;\n" READER+
   S\" ;package\ns\q loaded\q INSTALL-E2E-READER:SAY\n" READER+
   S\" INSTALL-E2E-READER:AWAIT\nrequire src/install-race-y.f\n" READER+
   S\" s\q done\q INSTALL-E2E-READER:SAY\n" READER+ ;

: HOME-PATHS! ( -- )
   ROOT$ s" home" HOME FS:JOIN-PATH HOME-U !
   HOME$ s" .local/lib/habu/bin/hb" ENGINE FS:JOIN-PATH ENGINE-U !
   HOME$ s" .local/lib/habu-trees" TREES FS:JOIN-PATH TREES-U !
   HOME$ s" .local/lib/habu" LINK FS:JOIN-PATH LINK-U !
   HOME$ s" .local/lib/habu.next" NEXT FS:JOIN-PATH NEXT-U ! ;

: SETUP ( -- )
   s" habu-install-e2e" FS:MKTEMP-DIR {: a:ptr u:n :}
   a u FS:CLEANUP-TREE+
   a ROOT u BYTE-COPY u ROOT-U !
   HOME-PATHS!
   ROOT$ s" a" TREE-A FS:JOIN-PATH TREE-A-U !
   ROOT$ s" b" TREE-B FS:JOIN-PATH TREE-B-U !
   WRITE-A WRITE-B WRITE-READER ;

: SPAWN-INSTALL ( ptr u8 n -- pid ) {: tree:ptr treeu:n :}
   tree treeu s" bin/hb" PATH FS:JOIN-PATH {: hbu:n :}
   PATH hbu PROCESS:NEW
   s" --load" PROCESS:ARG
   s" tools/install.f" PROCESS:ARG
   s" HOME" HOME$ PROCESS:ENV
   tree treeu PROCESS:CWD
   PROCESS:SPAWN ;

: INSTALLED ( pid -- )
   PROCESS:WAIT PROCESS:STATUS>RC RC>N 0 T= ;

: INSTALL ( ptr u8 n -- ) SPAWN-INSTALL INSTALLED ;

: COUNT-TREES ( -- n )
   0 TREES-N !
   TREES$ [: 2drop 1 TREES-N +! ;] FS-LIST:EACH
   TREES-N @ ;

: KEEP-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   TREES$ a u KEPT FS:JOIN-PATH KEPT-U ! ;

\ Run with one tree in habu-trees, so KEPT names it.
: RECORD-TREE ( -- ) TREES$ [: KEEP-NAME ;] FS-LIST:EACH ;

: LAID-OUT ( -- )
   COUNT-TREES 1 T=
   LINK$ FS:SYMLINK? TTRUE
   NEXT$ FS:TRY-LSTAT TFALSE ;

: PUBLISHED ( -- )
   NEXT$ FS:TRY-LSTAT TFALSE
   ENGINE$ FS:FILE? TTRUE ;

\ An install sweeps before it publishes, so A's tree is the link's target
\ through B's first install and only the reader keeps it through the second.
: HELD ( n -- )
   COUNT-TREES swap T=
   KEPT$ FS:DIR? TTRUE ;

: SWEPT ( -- )
   TREE-B$ INSTALL
   KEPT$ FS:TRY-LSTAT TFALSE ;

\ Run from the scratch root, which is no tree, so its requires resolve in the
\ installed engine's tree.
: SPAWN-READER ( fd fd -- pid ) {: in:fd out:fd :}
   ENGINE$ PROCESS:NEW
   s" --load" PROCESS:ARG
   READER$ PROCESS:ARG
   s" HOME" HOME$ PROCESS:ENV
   ROOT$ PROCESS:CWD
   0 >FD in PROCESS:FD
   1 >FD out PROCESS:FD
   PROCESS:SPAWN ;

: REAP ( pid -- ) {: child:pid :}
   child TIMEOUT-MS >MS PROCESS:WAIT-BOUNDED MATCH PROCESS:ending
      exited OF 0 T= ENDOF
      signaled OF 128 + 0 T= ENDOF
      timeout OF
         child PROCESS:SIGKILL PROCESS:KILL-RAW drop
         child PROCESS:WAIT drop
         false TTRUE
      ENDOF
   ;MATCH ;

: READ-SOME ( fd -- n ) {: r:fd :}
   r FD>N OUT OUT-U @ + IO-CAP OUT-U @ - read ;

: MORE ( fd -- bool )
   READ-SOME dup 0 > if OUT-U +! true exit then
   drop false ;

: LINE? ( -- bool ) OUT OUT-U @ S\" \n" CONTAINS? ;

: AWAIT-LINE ( fd -- )
   begin LINE? 0= if dup MORE else false then while repeat drop ;

: DRAIN ( fd -- ) begin dup MORE while repeat FD>N close ;

: GO ( fd -- ) {: w:fd :}
   w [: w FD>N S\" go\n" write drop ;] FD:WITH-NOSIGPIPE ;

\ Each case starts its own reader: the load under test is that process's.
: IN-FLIGHT ( -- )
   s" a load in flight reads its own release whole" T-LABEL
   RECORD-TREE
   FD:PIPE-PAIR {: in-r:fd in-w:fd :}
   FD:PIPE-PAIR {: out-r:fd out-w:fd :}
   in-r out-w SPAWN-READER {: child:pid :}
   in-r FD>N close  out-w FD>N close
   0 OUT-U !
   out-r AWAIT-LINE
   OUT OUT-U @ S\" loaded\n" STR= if
      TREE-B$ INSTALL 2 HELD  TREE-B$ INSTALL 3 HELD  in-w GO
   then
   in-w FD>N close
   child REAP
   out-r DRAIN
   OUT OUT-U @ S\" loaded\ndone\n" T$=
   SWEPT ;

: READ-WHOLE ( -- )
   FD:PIPE-PAIR {: in-r:fd in-w:fd :}
   FD:PIPE-PAIR {: out-r:fd out-w:fd :}
   in-w FD>N close
   in-r out-w SPAWN-READER {: child:pid :}
   in-r FD>N close  out-w FD>N close
   0 OUT-U !
   child REAP
   out-r DRAIN
   OUT OUT-U @ S\" loaded\ndone\n" T$= ;

: FRESH ( -- )
   s" a load started after an install reads that release whole" T-LABEL
   READ-WHOLE ;

: TREE-PATH ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   TREES$ a u PATH FS:JOIN-PATH PATH swap ;

\ A partial tree, a complete one nobody runs and the staged link naming it.
: DEAD! ( -- )
   s" tree-dead-0/src" TREE-PATH FS:MAKE-DIRS
   s" tree-dead-1/bin" TREE-PATH FS:MAKE-DIRS
   ENGINE-CANDIDATE:PATH$ s" tree-dead-1/bin/hb" TREE-PATH
   FS:COPY-FILE-STREAM
   s" habu-trees/tree-dead-1" NEXT$ FS:MAKE-SYMLINK ;

: GONE ( ptr u8 n -- ) TREE-PATH FS:TRY-LSTAT TFALSE ;

: DEAD ( -- )
   s" an install removes what a dead install left" T-LABEL
   DEAD!
   TREE-A$ INSTALL
   s" tree-dead-0" GONE  s" tree-dead-1" GONE
   PUBLISHED
   READ-WHOLE ;

: BOTH ( -- )
   s" two installs at once publish one tree whole" T-LABEL
   TREE-A$ SPAWN-INSTALL  TREE-B$ SPAWN-INSTALL {: a:pid b:pid :}
   a INSTALLED  b INSTALLED
   PUBLISHED
   READ-WHOLE ;

public

: RUN ( -- )
   T-RESET SETUP
   s" a release installs into an empty HOME" T-LABEL
   TREE-A$ INSTALL
   LAID-OUT
   IN-FLIGHT FRESH DEAD BOTH
   T-REPORT ;

;package

INSTALL-E2E:RUN
