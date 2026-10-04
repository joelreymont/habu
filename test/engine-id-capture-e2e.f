\ A copied engine's physical pathname and content key must not travel into an
\ application image. A child loads app-image and lib/engine-id.f through the
\ normal source loader, prints PATH$ and KEY$, and saves; on macOS KEY$ also
\ leaves the pathname in the digest context's path field and in the
\ mapped-region answer. The saved process writes its restored DATA to a file,
\ asking nothing first. The parent counts, in the DATA file, the path and key
\ the child printed: every count must be 0. The saved process then resolves
\ its own pathname. Keep the saved image, DATA file and logs as evidence.
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-cwd.f
require lib/engine-candidate.f

package ENGINE-ID-CAPTURE-E2E
private

$4000 constant IO-CAP
180000 constant TIMEOUT-MS
64 constant KEY-LEN
create ROOT FS-PATH-CAP allot variable ROOT-U
create TREE FS-PATH-CAP allot variable TREE-U
create ENGINE FS-PATH-CAP allot variable ENGINE-U
create SAVED FS-PATH-CAP allot variable SAVED-U
create DUMP FS-PATH-CAP allot variable DUMP-U
create SEEN FS-PATH-CAP allot variable SEEN-U
create KEY KEY-LEN allot variable KEY-U
create LOG FS-PATH-CAP allot
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC
variable HITS
variable AT

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: TREE$ ( -- ptr u8 n ) TREE TREE-U @ ;
: ENGINE$ ( -- ptr u8 n ) ENGINE ENGINE-U @ ;
: SAVED$ ( -- ptr u8 n ) SAVED SAVED-U @ ;
: DUMP$ ( -- ptr u8 n ) DUMP DUMP-U @ ;
: SEEN$ ( -- ptr u8 n ) SEEN SEEN-U @ ;
: KEY-HEX$ ( -- ptr u8 n ) KEY KEY-U @ ;

: SETUP ( -- )
   s" hb-engine-id-capture" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ s" tree" TREE JOIN-PATH TREE-U !
   TREE$ s" bin" LOG JOIN-PATH {: binu:n :}
   LOG binu MAKE-DIRS
   TREE$ s" bin/hb" ENGINE JOIN-PATH ENGINE-U !
   ENGINE-CANDIDATE:PATH$ ENGINE$ COPY-FILE-STREAM
   ENGINE$ CHMOD-X
   ROOT$ s" saved" SAVED JOIN-PATH SAVED-U !
   ROOT$ s" data" DUMP JOIN-PATH DUMP-U ! ;

: RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

: LOG-RESULT ( ptr u8 n ptr u8 n -- ) {: outname:ptr outu:n errname:ptr erru:n :}
   ROOT$ outname outu LOG JOIN-PATH {: size:n :}
   LOG size OUT OUT-U @ WRITE-ALL
   ROOT$ errname erru LOG JOIN-PATH {: esize:n :}
   LOG esize ERR ERR-U @ WRITE-ALL ;

: CHECK-RESULT ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T= ERR-U @ 0 T= ;

\ app-image comes first: code compiled before its tier switch lacks native
\ provenance, and the save refuses it (exit 100).
: SAVE-IMAGE ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" --" >LEN PROC-ARGV+ SAVED$ >LEN PROC-ARGV+
   ENGINE$ >LEN SOURCE-ROOT:CWD$ >LEN
   src srcu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT
   s" save.out" s" save.err" LOG-RESULT
   CHECK-RESULT
   SAVED$ EXECUTABLE? TTRUE ;

\ The line of OUT that starts at start, and the offset after its newline.
: OUT-LINE ( n -- ptr u8 n n )
   {: start:n :}
   OUT OUT-U @ 10 start SPLIT-NEXT {: a:ptr u:n next:n found:bool :}
   found 0= if E-FS-IO throw then
   a u next ;

: KEEP ( ptr u8 n ptr u8 n -- n )
   {: src:ptr srcu:n dst:ptr cap:n :}
   srcu cap > if E-FS-CAPACITY throw then
   src dst srcu BYTE-COPY srcu ;

\ The child printed its PATH$, then its KEY$, before it saved.
: KEEP-PRINTED ( -- )
   0 OUT-LINE {: p:ptr pu:n next:n :}
   p pu SEEN FS-PATH-CAP KEEP SEEN-U !
   next OUT-LINE drop KEY KEY-LEN KEEP KEY-U !
   SEEN-U @ 0 > TTRUE
   KEY-U @ KEY-LEN T= ;

\ Occurrences of the needle in the haystack, overlaps counted: an empty needle
\ counts every offset rather than none.
: COUNT-IN ( ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n b:ptr v:n :}
   0 HITS ! 0 AT !
   begin AT @ u <= while
      a AT @ + u AT @ - b v FIND-SUB MATCH option
         none OF u 1+ AT ! ENDOF
         some OF IDX>N AT @ + 1+ AT ! 1 HITS +! ENDOF
      ;MATCH
   repeat
   HITS @ ;

: SCAN ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: bytes:n :}
   bytes MEM-ALLOC-BYTES drop {: buf:ptr :}
   path pathu buf bytes READ-ALL bytes T=
   buf bytes SEEN$ COUNT-IN {: path-hits:n :}
   buf bytes KEY-HEX$ COUNT-IN {: key-hits:n :}
   buf bytes munmap 0 T=
   path pathu type cr
   s" path=" type path-hits . s" key=" type key-hits .
   path-hits 0 T= key-hits 0 T= ;

\ The image file stores most of DATA as the snapshot's cell grid, whose
\ varints hide byte strings from a scan of the file, so the saved process
\ writes out DATA as it restored it.
: DUMP-DATA ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" --" >LEN PROC-ARGV+ DUMP$ >LEN PROC-ARGV+
   SAVED$ >LEN SOURCE-ROOT:CWD$ >LEN
   S\" require lib/fs.f\n0 SCRIPT-ARGV$ data-base BYTE-VIEW here data-base - WRITE-ALL\n" >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT
   s" dump.out" s" dump.err" LOG-RESULT
   CHECK-RESULT ;

: RUN-SAVED ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   SAVED$ >LEN SOURCE-ROOT:CWD$ >LEN
   S\" ENGINE-ID:PATH$ type cr\n" >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT
   s" reload.out" s" reload.err" LOG-RESULT
   CHECK-RESULT
   OUT OUT-U @ SAVED$ CONTAINS? TTRUE ;

: CHECK-CASE ( -- )
   DUMP-DATA DUMP$ SCAN RUN-SAVED ;

: KEY-SOURCE$ ( -- ptr u8 n )
   S\" require src/habu/app-image.f\nrequire lib/engine-id.f\nENGINE-ID:PATH$ type cr\nENGINE-ID:KEY$ type cr\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

public

: RUN ( -- )
   T-RESET SETUP
   KEY-SOURCE$ SAVE-IMAGE KEEP-PRINTED CHECK-CASE
   s" engine-id capture tree: " type ROOT$ type cr
   T-REPORT ;

;package

ENGINE-ID-CAPTURE-E2E:RUN
