\ Moving bytes across file boundaries must invalidate an artifact's source key.
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require lib/fs-mutate.f
require lib/engine-id.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/aot-decl.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-file.f

package AOT-SOURCE-TEST
using AOT-BUF
using AOT-WINDOW
using AOT-IDENT
using AOT-FILE

create ROOT-BUF FS-PATH-CAP allot variable ROOT-U
create LEFT-BUF FS-PATH-CAP allot variable LEFT-U
create RIGHT-BUF FS-PATH-CAP allot variable RIGHT-U
create ART-BUF FS-PATH-CAP allot variable ART-U
create JOINED 256 allot
create KEY 32 allot
create BEFORE 32 allot create AFTER 32 allot
create ART-BEFORE 32 allot create ART-AFTER 32 allot
$1000 constant IO-CAP
create OUT IO-CAP allot create ERR IO-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: LEFT$ ( -- ptr u8 n ) LEFT-BUF LEFT-U @ ;
: RIGHT$ ( -- ptr u8 n ) RIGHT-BUF RIGHT-U @ ;
: ART$ ( -- ptr u8 n ) ART-BUF ART-U @ ;

\ LEFT ends inside an EOF comment, deliberately without a final newline.
: LEFT-SOURCE$ ( -- ptr u8 n )
   s" package SRCBOUND public : LEFT ( -- n ) 1 ; ;package \ " ;
: RIGHT-SOURCE$ ( -- ptr u8 n )
   S\" package SRCBOUND public : RIGHT ( -- n ) 2 ; ;package\n" ;

: SETUP ( -- )
   s" hb-source-identity" TMPDIR-MKDIR {: root:ptr size:n :}
   root ROOT-BUF size BYTE-COPY size ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" left.f" LEFT-BUF JOIN-PATH LEFT-U !
   ROOT$ s" right.f" RIGHT-BUF JOIN-PATH RIGHT-U !
   ROOT$ s" source.aot" ART-BUF JOIN-PATH ART-U !
   LEFT$ LEFT-SOURCE$ WRITE-ALL
   RIGHT$ RIGHT-SOURCE$ WRITE-ALL
   ENGINE-ID:PATH$ KEY SHA256-FILE 0 T= ;

: MOVE-BOUNDARY ( -- )
   LEFT-SOURCE$ JOINED swap BYTE-COPY
   RIGHT-SOURCE$ 1- JOINED LEFT-SOURCE$ nip + swap BYTE-COPY
   LEFT$ JOINED LEFT-SOURCE$ nip RIGHT-SOURCE$ nip + 1- WRITE-ALL
   RIGHT$ S\" \n" WRITE-ALL ;

: SOURCE-KEY ( ptr u8 -- )
   RESET LEFT$ PATH+ RIGHT$ PATH+ CHAIN-DIGEST ;

\ A valid minimal artifact isolates source identity from registry/relocation
\ validation. Its only instruction is ARM64 RET; it is never executed.
: ARTIFACT ( -- )
   4 AOT-BLOB-LEN ! $D65F03C0 AOT-BLOB-BUF@ CELL-VIEW !
   0 AOT-REC-N ! 0 AOT-SITE-N ! 0 AOT-NAMES-LEN !
   0 AOT-DSITE-N ! 0 AOT-CSITE-N !
   0 AOT-DATA-D0 ! 0 AOT-CODE-B0 !
   0 AOT-WID-W0 ! 0 AOT-WID-SPAN ! 0 AOT-DATA-SIZE !
   RUNS-RESET 0 XTOFF-N !
   0 AOT-XTSITE:N ! 0 AOT-BOOTRUN-LEN !
   0 AOT-PWIN-N ! 0 AOT-SIG-N ! 0 AOT-SIG-STR-LEN ! 0 AOT-REG-LEN !
   KEY ART$ AOT-FILE:WRITE ;

public

: READ-ARTIFACT ( -- ) KEY ART$ AOT-FILE:READ ;

TRUSTED: LOAD-SOURCE ( -- )
   LEFT$ included RIGHT$ included
   s" SRCBOUND:LEFT SRCBOUND:RIGHT + ." evaluate ;

private

: CHILD ( ptr u8 n n -- n n ) {: wanted:n :}
   OUT IO-CAP >LEN ERR IO-CAP >LEN 5000 >MS SUBJECT:RUN
   wanted T-OUTCOME-EXITED= {: outu:len erru:len :}
   outu LEN>N erru LEN>N ;

public

: RUN ( -- )
   T-RESET CLEANUP-RESET SETUP
   s" AOT-SOURCE-TEST:LOAD-SOURCE" 0 CHILD
   0 T= OUT swap S\" 3\n" T$=
   BEFORE SOURCE-KEY ARTIFACT
   ART$ ART-BEFORE SHA256-FILE 0 T=
   s" AOT-SOURCE-TEST:READ-ARTIFACT" 0 CHILD 0 T= 0 T=
   MOVE-BOUNDARY AFTER SOURCE-KEY
   BEFORE 32 AFTER 32 STR= 0= TTRUE
   s" AOT-SOURCE-TEST:LOAD-SOURCE" 70 CHILD
   ERR swap s" SRCBOUND:RIGHT" CONTAINS? TTRUE drop
   s" AOT-SOURCE-TEST:READ-ARTIFACT" 75 CHILD
   ERR swap s" aot-file: the chain sources have changed since this capture" T$= 0 T=
   ART$ ART-AFTER SHA256-FILE 0 T=
   ART-BEFORE 32 ART-AFTER 32 STR= TTRUE
   CLEANUP-RUN T-REPORT ;

;using
;using
;using
;using
;package

AOT-SOURCE-TEST:RUN
