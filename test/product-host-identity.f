split-str
\ product-host-identity.f - the same target tree built by its ordinary
\ engine and by a host with one extra primitive must produce identical
\ executable and name map. The variant is built from a private source copy.
\
\ Not registered in the suite; run it by hand.
\ The printed directory retains the copied trees and all four products.

require src/core/exit-status.f
require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require tools/chain-run.f
require src/lib/tree-copy.f

package PRODUCT-HOST-TEST

$4000 constant CAP

create ROOT PATH-CAP allot          variable ROOT-U
create TREE PATH-CAP allot          variable TREE-U
create VARIANT-ROOT PATH-CAP allot  variable VARIANT-ROOT-U
create TMP PATH-CAP allot           variable TMP-U
create BASE PATH-CAP allot          variable BASE-U
create REF PATH-CAP allot           variable REF-U
create HOST PATH-CAP allot          variable HOST-U
create REBUILT PATH-CAP allot       variable REBUILT-U
create DEST PATH-CAP allot          variable DEST-U
create NAME-A PATH-CAP allot        variable NAME-A-U
create NAME-B PATH-CAP allot        variable NAME-B-U
create OUT CAP allot                   variable OUT-U
create ERR CAP allot                   variable ERR-U
variable RC

: ROOT$ ( -- ^u8 n ) ROOT ROOT-U @ ;
: TREE$ ( -- str ) TREE TREE-U @ ;
: VARIANT$ ( -- str ) VARIANT-ROOT VARIANT-ROOT-U @ ;
: TMP$ ( -- str ) TMP TMP-U @ ;
: BASE$ ( -- str ) BASE BASE-U @ ;
: REF$ ( -- str ) REF REF-U @ ;
: HOST$ ( -- str ) HOST HOST-U @ ;
: REBUILT$ ( -- str ) REBUILT REBUILT-U @ ;
: DEST$ ( -- str ) DEST DEST-U @ ;

: ROOT-PATH! ( str ^u8 ^n -- ) {: a:^ u:n dst:^ up:^ :}
   ROOT$ a u dst FS:JOIN-PATH up ! ;

: SETUP ( -- )
   s" product-host-identity" FS:MKTEMP-DIR {: a:^ u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" tree" TREE TREE-U ROOT-PATH!
   s" variant" VARIANT-ROOT VARIANT-ROOT-U ROOT-PATH!
   s" tmp" TMP TMP-U ROOT-PATH! TMP$ FS:MAKE-DIRS
   s" base-hb" BASE BASE-U ROOT-PATH!
   s" ref-hb" REF REF-U ROOT-PATH!
   s" host-hb" HOST HOST-U ROOT-PATH!
   s" rebuilt-hb" REBUILT REBUILT-U ROOT-PATH! ;

\ A variant with one additional ARM primitive, built from copied sources.
\ The specification row and ARM body are both required by the kernel's
\ completeness check. The body reuses BCR because the new word is never run.
: SPEC-ANCHOR$ ( -- str ) s" \ Primitives whose effect" ;
: SPEC-ROW$ ( -- str ) S\" EPRIM: host-extra EPRIM;\n\n" ;
: ARM-ANCHOR$ ( -- str ) S\" \n   s\" type\" ['] BTYPE  FPRIM-L ;" ;
: ARM-ROW$ ( -- str ) S\" \n   s\" host-extra\" ['] BCR FPRIM-L" ;

: INSERT-ROW ( str str str -- )
   {: path:^ pathu:n anchor:^ anchoru:n row:^ rowu:n :}
   VARIANT$ path pathu DEST FS:JOIN-PATH DEST-U !
   DEST$ FS:FILE-SIZE {: size:n :}
   size MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: buf:^ len :}
   DEST$ buf size FS:READ-ALL size <> if
      s" product-host-identity: short variant source read" EXIT-STATUS:IOERR die
   then
   buf size anchor anchoru STR:FIND-SUB MATCH option
      none OF
         s" product-host-identity: primitive insertion anchor missing" EXIT-STATUS:FAILURE die
      ENDOF
      some OF IDX>N {: at:n :}
         DEST$ buf at FS:WRITE-ALL
         DEST$ row rowu FS:APPEND-FILE
         DEST$ buf at + size at - FS:APPEND-FILE
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

: ARGV+ ( str -- ) PROC-ARGV+ ;

: ENV! ( -- )
   s" TMPDIR" TMP$ PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

\ engine builds the tree it runs in into path.
: BUILD ( ^u8 n ^u8 n ^u8 n -- )
   {: engine:^ engineu:n tree:^ treeu:n path:^ pathu:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" ARGV+
   s" tools/hb-build.f" ARGV+
   s" --" ARGV+
   s" --engine" ARGV+
   s" -o" ARGV+
   path pathu ARGV+
   ENV!
   engine engineu tree treeu
   OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT
   SUCCESS ;

: NAMES! ( str ^u8 ^n -- )
   {: path:^ pathu:n dst:^ up:^ :}
   pathu s" .map" nip + PATH-CAP > if E-FS-PATH throw then
   path dst pathu BYTE-COPY
   s" .map" dst pathu + swap BYTE-COPY
   pathu s" .map" nip + up ! ;

: SAME-PRODUCT ( str str -- )
   {: a:^ au:n b:^ bu:n :}
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
