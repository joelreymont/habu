\ host-checker-row-e2e.f - a host whose checker states a PRIM: row for another
\ word than this tree does builds this tree into the same engine, with the same
\ .names sidecar, as an engine of this tree does.
\
\ WHY A ROW. The host certifies the window's prefix, src/ up to
\ src/core/layout-valid.f, and src/core/checker.f TRANSFER-CHECKED copies each
\ certified effect into the window's own checker, which appends a record and a
\ no-return row per copy, a defer row for a deferred word, and interns every
\ name it does not hold yet. A host numbers the names it holds a PRIM: row for
\ ahead of all others, so copies taken in the host's symbol order put the host's
\ primitive table into the image (docs/bootstrap.md, "The checker's records do
\ not follow the host's symbol numbering"): a word without its row moved its own
\ record, and a word with an extra row renumbered every symbol interned after
\ it. The prefix defines CHECKER-STORAGE-INFO, which has a row, and
\ CHECKER-CAPTURE-PREPARE, which has none; the host here is this tree's engine
\ built from a copy whose checker.f gives the first word's row to the second.
\
\ Three builds, each tools/native-build.f in a fresh process: the reference (the
\ engine under test builds the copied tree), the host (the engine under test
\ builds the copy with the moved row) and the rebuild (that host builds the
\ copied tree). The engine under test has to carry this tree's compiler: a
\ pending codegen change makes the reference and the rebuild differ by design.
\
\ Run explicitly: bin/hb --load test/host-checker-row-e2e.f
\ The printed private directory keeps both source trees, the host and both
\ products with their name maps; `tools/two-generation-build.f -- --compare`
\ names the first differing offset of a failing pair.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-cwd.f
require lib/engine-candidate.f
require tools/chain-run.f
require lib/tree-copy.f

package HOST-ROW-TEST

$4000 constant CAP
1800000 constant BUILD-TIMEOUT-MS

create ROOT FS-PATH-CAP allot          variable ROOT-U
create TREE FS-PATH-CAP allot          variable TREE-U
create VARIANT-ROOT FS-PATH-CAP allot  variable VARIANT-ROOT-U
create TMP FS-PATH-CAP allot           variable TMP-U
create REF FS-PATH-CAP allot           variable REF-U
create HOST FS-PATH-CAP allot          variable HOST-U
create REBUILT FS-PATH-CAP allot       variable REBUILT-U
create DEST FS-PATH-CAP allot          variable DEST-U
create NAME-A FS-PATH-CAP allot        variable NAME-A-U
create NAME-B FS-PATH-CAP allot        variable NAME-B-U
create OUT CAP allot                   variable OUT-U
create ERR CAP allot                   variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: TREE$ ( -- ptr u8 n ) TREE TREE-U @ ;
: VARIANT$ ( -- ptr u8 n ) VARIANT-ROOT VARIANT-ROOT-U @ ;
: TMP$ ( -- ptr u8 n ) TMP TMP-U @ ;
: REF$ ( -- ptr u8 n ) REF REF-U @ ;
: HOST$ ( -- ptr u8 n ) HOST HOST-U @ ;
: REBUILT$ ( -- ptr u8 n ) REBUILT REBUILT-U @ ;
: DEST$ ( -- ptr u8 n ) DEST DEST-U @ ;

: ROOT-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   ROOT$ a u dst JOIN-PATH up ! ;

: SETUP ( -- )
   s" host-checker-row-e2e" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" tree" TREE TREE-U ROOT-PATH!
   s" variant" VARIANT-ROOT VARIANT-ROOT-U ROOT-PATH!
   s" tmp" TMP TMP-U ROOT-PATH! TMP$ MAKE-DIRS
   s" ref-hb" REF REF-U ROOT-PATH!
   s" host-hb" HOST HOST-U ROOT-PATH!
   s" rebuilt-hb" REBUILT REBUILT-U ROOT-PATH! ;

\ The row the variant replaces, found at a line start up to its name so a
\ changed effect still matches, and replaced through its line end. The row put
\ in its place states the definition it is checked against below.
: ROW$ ( -- ptr u8 n ) S\" \nPRIM: CHECKER-STORAGE-INFO " ;
: NEW-ROW$ ( -- ptr u8 n ) S\" PRIM: CHECKER-CAPTURE-PREPARE PRIM;\n" ;
: NEW-DEF$ ( -- ptr u8 n ) S\" \n: CHECKER-CAPTURE-PREPARE ( -- )\n" ;
: NEW-NAME$ ( -- ptr u8 n ) S\" \nPRIM: CHECKER-CAPTURE-PREPARE " ;

: LINE-END ( ptr u8 n n -- n ) {: a:ptr u:n at:n :}
   at begin
      dup u < if a over + c@ 10 <> else false then
   while 1 + repeat ;

: EXPECT-ROWS ( ptr u8 n -- ) {: buf:ptr size:n :}
   buf size NEW-DEF$ FIND-SUB MATCH option
      none OF
         s" host-checker-row-e2e: checker.f no longer defines CHECKER-CAPTURE-PREPARE ( -- ); pick another word" 74 die
      ENDOF
      some OF drop ENDOF
   ;MATCH
   buf size NEW-NAME$ FIND-SUB MATCH option
      none OF ENDOF
      some OF drop
         s" host-checker-row-e2e: checker.f already has a PRIM: CHECKER-CAPTURE-PREPARE row; pick another word" 74 die
      ENDOF
   ;MATCH ;

: MOVE-ROW ( -- )
   VARIANT$ s" src/core/checker.f" DEST JOIN-PATH DEST-U !
   DEST$ FILE-SIZE {: size:n :}
   size MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: buf:ptr len :}
   DEST$ buf size READ-ALL size <> if
      s" host-checker-row-e2e: short read of the variant's checker.f" 74 die
   then
   buf size EXPECT-ROWS
   buf size ROW$ FIND-SUB MATCH option
      none OF
         s" host-checker-row-e2e: checker.f has no PRIM: CHECKER-STORAGE-INFO row; pick another row of a prefix definition" 74 die
      ENDOF
      some OF IDX>N 1 + {: at:n :}
         buf size at LINE-END 1 + size min {: after:n :}
         DEST$ buf at WRITE-ALL
         DEST$ NEW-ROW$ APPEND-FILE
         DEST$ buf after + size after - APPEND-FILE
      ENDOF
   ;MATCH
   buf len MEM:RELEASE-BYTES ;

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc RC>N RC ! ENDOF
   ;MATCH ;

: SUCCESS ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T= ;

: ARGV+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ENV! ( -- )
   s" HB_TMP" >LEN TMP$ >LEN PROC-ENV+
   s" HABU_WHITEBOX_IMAGE" >LEN NULL$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

\ engine builds the tree it runs in into path.
: BUILD ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: engine:ptr engineu:n tree:ptr treeu:n path:ptr pathu:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" ARGV+
   s" tools/native-build.f" ARGV+
   s" --" ARGV+
   path pathu ARGV+
   ENV!
   engine engineu >LEN tree treeu >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT
   SUCCESS ;

: NAMES! ( ptr u8 n ptr u8 ptr n -- )
   {: path:ptr pathu:n dst:ptr up:ptr :}
   pathu s" .names" nip + FS-PATH-CAP > if E-FS-PATH throw then
   path dst pathu BYTE-COPY
   s" .names" dst pathu + swap BYTE-COPY
   pathu s" .names" nip + up ! ;

: SAME-PRODUCT ( ptr u8 n ptr u8 n -- )
   {: a:ptr au:n b:ptr bu:n :}
   a au b bu CHAIN-RUN:SAME-FILES? TTRUE
   a au NAME-A NAME-A-U NAMES!
   b bu NAME-B NAME-B-U NAMES!
   NAME-A NAME-A-U @ NAME-B NAME-B-U @ CHAIN-RUN:SAME-FILES? TTRUE ;

\ Private copies keep all three builds on exactly the same source bytes, and the
\ variant's edit away from the checkout.
: RUN ( -- )
   T-RESET
   SETUP
   s" host-checker-row-e2e artifacts: " type ROOT$ type cr
   TREE$ TREE-COPY:BUILD-SOURCES
   VARIANT$ TREE-COPY:BUILD-SOURCES
   MOVE-ROW
   s" the engine under test builds the copied tree" T-LABEL
   ENGINE-CANDIDATE:PATH$ TREE$ REF$ BUILD
   s" the engine under test builds the copy with the moved PRIM: row" T-LABEL
   ENGINE-CANDIDATE:PATH$ VARIANT$ HOST$ BUILD
   s" that host builds the copied tree" T-LABEL
   HOST$ TREE$ REBUILT$ BUILD
   s" a host whose PRIM: row names another word builds the same engine and name map" T-LABEL
   REF$ REBUILT$ SAME-PRODUCT
   T-REPORT ;

RUN
;package
