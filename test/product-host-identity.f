\ product-host-identity.f - the same target tree built by its ordinary
\ engine and by a host with one extra primitive must produce identical
\ executable and name map. The variant is built from a private source copy.
\
\ Run explicitly: bin/hb --load test/product-host-identity.f
\ The printed directory retains the copied trees and all four products.

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

package PRODUCT-HOST-TEST

$4000 constant CAP
1800000 constant BUILD-TIMEOUT-MS

create ROOT FS-PATH-CAP allot          variable ROOT-U
create TREE FS-PATH-CAP allot          variable TREE-U
create VARIANT-ROOT FS-PATH-CAP allot  variable VARIANT-ROOT-U
create TMP FS-PATH-CAP allot           variable TMP-U
create BASE FS-PATH-CAP allot          variable BASE-U
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
: BASE$ ( -- ptr u8 n ) BASE BASE-U @ ;
: REF$ ( -- ptr u8 n ) REF REF-U @ ;
: HOST$ ( -- ptr u8 n ) HOST HOST-U @ ;
: REBUILT$ ( -- ptr u8 n ) REBUILT REBUILT-U @ ;
: DEST$ ( -- ptr u8 n ) DEST DEST-U @ ;

: ROOT-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   ROOT$ a u dst JOIN-PATH up ! ;

: SETUP ( -- )
   s" product-host-identity" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" tree" TREE TREE-U ROOT-PATH!
   s" variant" VARIANT-ROOT VARIANT-ROOT-U ROOT-PATH!
   s" tmp" TMP TMP-U ROOT-PATH! TMP$ MAKE-DIRS
   s" base-hb" BASE BASE-U ROOT-PATH!
   s" ref-hb" REF REF-U ROOT-PATH!
   s" host-hb" HOST HOST-U ROOT-PATH!
   s" rebuilt-hb" REBUILT REBUILT-U ROOT-PATH! ;

\ A variant with one additional ARM primitive, built from copied sources.
\ The specification row and ARM body are both required by the kernel's
\ completeness check. The body reuses BCR because the new word is never run.
: SPEC-ANCHOR$ ( -- ptr u8 n ) s" \ ---- primitives whose effect" ;
: SPEC-ROW$ ( -- ptr u8 n ) S\" EPRIM: host-extra EPRIM;\n\n" ;
: ARM-ANCHOR$ ( -- ptr u8 n ) S\" \n   s\" type\" ['] BTYPE  FPRIM-L ;" ;
: ARM-ROW$ ( -- ptr u8 n ) S\" \n   s\" host-extra\" ['] BCR FPRIM-L" ;

: INSERT-ROW ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n anchor:ptr anchoru:n row:ptr rowu:n :}
   VARIANT$ path pathu DEST JOIN-PATH DEST-U !
   DEST$ FILE-SIZE {: size:n :}
   size MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: buf:ptr len :}
   DEST$ buf size READ-ALL size <> if
      s" product-host-identity: short variant source read" 74 die
   then
   buf size anchor anchoru FIND-SUB MATCH option
      none OF
         s" product-host-identity: primitive insertion anchor missing" 74 die
      ENDOF
      some OF IDX>N {: at:n :}
         DEST$ buf at WRITE-ALL
         DEST$ row rowu APPEND-FILE
         DEST$ buf at + size at - APPEND-FILE
      ENDOF
   ;MATCH
   buf len MEM:RELEASE-BYTES ;

: ADD-PRIMITIVE ( -- )
   s" src/habu/prims.f" SPEC-ANCHOR$ SPEC-ROW$ INSERT-ROW
   s" src/habu/habu1.f" ARM-ANCHOR$ ARM-ROW$ INSERT-ROW ;

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

\ The seed may predate this tree's compiler. Build both hosts from these copied
\ sources, then ask each host to build the identical unmodified tree.
: RUN ( -- )
   T-RESET
   SETUP
   s" product-host-identity artifacts: " type ROOT$ type cr
   TREE$ TREE-COPY:BUILD-SOURCES
   VARIANT$ TREE-COPY:BUILD-SOURCES
   ADD-PRIMITIVE
   s" the seed builds the ordinary host" T-LABEL
   ENGINE-CANDIDATE:PATH$ TREE$ BASE$ BUILD
   s" the engine under test builds a host with one extra primitive" T-LABEL
   ENGINE-CANDIDATE:PATH$ VARIANT$ HOST$ BUILD
   s" the ordinary host builds the copied tree" T-LABEL
   BASE$ TREE$ REF$ BUILD
   s" that host builds the copied tree" T-LABEL
   HOST$ TREE$ REBUILT$ BUILD
   s" both hosts build the same target engine and name map" T-LABEL
   REF$ REBUILT$ SAME-PRODUCT
   T-REPORT ;

RUN
;package
