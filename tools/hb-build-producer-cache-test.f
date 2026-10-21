\ Producer edits change the application on a warm cache: a builder edit from an
\ external CWD, then a runtime edit racing the maker
\ (tools/hb-build-producer-race-test.f, run in this copy after those builds).
\ Run: bin/hb --load tools/hb-build-producer-cache-test.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-cwd.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/tree-copy.f
require tools/build-support.f

package HB-BUILD-PRODUCER-TEST

65536 constant CAP
262144 constant IMAGE-CAP
600000 constant TIMEOUT-MS
create ROOT FS-PATH-CAP allot
create PROJECT FS-PATH-CAP allot
create HB FS-PATH-CAP allot
create APP FS-PATH-CAP allot
create OUT-PATH FS-PATH-CAP allot
create BUILDER FS-PATH-CAP allot
create TOOL FS-PATH-CAP allot
create SAVED FS-PATH-CAP allot
create TMP FS-PATH-CAP allot
create CACHE FS-PATH-CAP allot
create IMAGE IMAGE-CAP allot
create OUT CAP allot
create ERR CAP allot
variable ROOT-U
variable PROJECT-U
variable HB-U
variable APP-U
variable OUT-U
variable BUILDER-U
variable TOOL-U
variable SAVED-U
variable TMP-U
variable CACHE-U
variable CAP-OUT-U
variable CAP-ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: PROJECT$ ( -- ptr u8 n ) PROJECT PROJECT-U @ ;
: HB$ ( -- ptr u8 n ) HB HB-U @ ;
: APP$ ( -- ptr u8 n ) APP APP-U @ ;
: OUT$ ( -- ptr u8 n ) OUT-PATH OUT-U @ ;
: BUILDER$ ( -- ptr u8 n ) BUILDER BUILDER-U @ ;
: TOOL$ ( -- ptr u8 n ) TOOL TOOL-U @ ;
: SAVED$ ( -- ptr u8 n ) SAVED SAVED-U @ ;
: TMP$ ( -- ptr u8 n ) TMP TMP-U @ ;
: CACHE$ ( -- ptr u8 n ) CACHE CACHE-U @ ;

: PATHS ( -- )
   ROOT$ s" project" PROJECT JOIN-PATH PROJECT-U !
   ROOT$ s" bin/hb" HB JOIN-PATH HB-U !
   ROOT$ s" app.f" APP JOIN-PATH APP-U !
   ROOT$ s" app" OUT-PATH JOIN-PATH OUT-U !
   ROOT$ s" tools/hb-build-lib.f" BUILDER JOIN-PATH BUILDER-U !
   ROOT$ s" tools/hb-build.f" TOOL JOIN-PATH TOOL-U !
   ROOT$ s" maker-saved.f" SAVED JOIN-PATH SAVED-U !
   ROOT$ s" hbtmp" TMP JOIN-PATH TMP-U !
   ROOT$ s" cache" CACHE JOIN-PATH CACHE-U ! ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-producer-cache" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ TREE-COPY:BUILD-SOURCES
   s" bin/hb" ROOT$ TREE-COPY:FILE
   PATHS
   HB$ CHMOD-X
   PROJECT$ MAKE-DIR
   TMP$ MAKE-DIR
   CACHE$ MAKE-DIR
   APP$ s" : MAIN ( -- ) ;" WRITE-ALL ;

: CAPTURE ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N CAP-OUT-U ! erru LEN>N CAP-ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N CAP-OUT-U ! erru LEN>N CAP-ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

: BUILD ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   APP$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   OUT$ >LEN PROC-ARGV+
   s" HB_TMP" >LEN TMP$ >LEN PROC-ENV+
   \ Mask a parent override: the invoking absolute engine is the CLI default.
   s" HABU_FIXPOINT_ENGINE" >LEN s" " >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN CACHE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN PROJECT$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE
   RC @ 0<> if OUT CAP-OUT-U @ type ERR CAP-ERR-U @ type then
   RC @ 0 T= ;

: BUILD-RACE ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/hb-build-producer-race-test.f" >LEN PROC-ARGV+
   s" HB_TMP" >LEN TMP$ >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN CACHE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN ROOT$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE
   RC @ 0<> if OUT CAP-OUT-U @ type ERR CAP-ERR-U @ type then
   RC @ 0 T= ;

: RUN-TOOL ( ptr u8 n -- )
   >LEN -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-IO-RC
   MATCH result
      ok OF 0 T= ENDOF
      err OF drop E-FS-IO throw ENDOF
   ;MATCH ;

: PRESERVE-BUILDER-TIME ( -- )
   PROC-ARGV-RESET
   s" -p" >LEN PROC-ARGV+
   BUILDER$ >LEN PROC-ARGV+
   SAVED$ >LEN PROC-ARGV+
   s" /bin/cp" RUN-TOOL ;

: RESTORE-BUILDER-TIME ( -- )
   PROC-ARGV-RESET
   s" -r" >LEN PROC-ARGV+
   SAVED$ >LEN PROC-ARGV+
   BUILDER$ >LEN PROC-ARGV+
   s" /usr/bin/touch" RUN-TOOL ;

: BUILDER-MTIME-SAME ( -- )
   SAVED$ FILE-META {: size:n sec:n ns:n csec:n cns:n :}
   csec drop cns drop
   BUILDER$ FILE-META {: bsize:n bsec:n bns:n bcsec:n bcns:n :}
   bcsec drop bcns drop
   bsize size T= bsec sec T= bns ns T= ;

: READ-TEXT ( ptr u8 n -- ptr u8 n )
   BUILD-FIXPOINT:BF-READ-SOURCE
   BUILD-FIXPOINT:BF-SOURCE-BUF BUILD-FIXPOINT:BF-SOURCE-LEN @ ;

: CHANGE-BUILDER-SAME-MTIME ( -- )
   PRESERVE-BUILDER-TIME
   BUILDER$ READ-TEXT {: text:ptr size:n :}
   text 2 + c@ $68 T=          \ 'h' in the opening comment
   $6a text 2 + c!           \ 'j'; source length and behavior stay the same
   BUILDER$ text size WRITE-ALL
   RESTORE-BUILDER-TIME
   BUILDER-MTIME-SAME ;

: BUILD-AFTER-BUILDER-EDIT ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" --report-json" >LEN PROC-ARGV+
   APP$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   OUT$ >LEN PROC-ARGV+
   s" HB_TMP" >LEN TMP$ >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN HB$ >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN CACHE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN PROJECT$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE
   RC @ 0<> if OUT CAP-OUT-U @ type ERR CAP-ERR-U @ type then
   RC @ 0 T=
   OUT CAP-OUT-U @ S\" \"maker_ran\":true" CONTAINS? TTRUE
   OUT CAP-OUT-U @ S\" \"artifact_hit\":false" CONTAINS? TTRUE ;

: IMAGE-HAS ( ptr u8 n ptr u8 n -- ) {: yes:ptr yesu:n no:ptr nou:n :}
   OUT$ IMAGE IMAGE-CAP READ-ALL {: u:n :}
   IMAGE u yes yesu CONTAINS? TTRUE
   IMAGE u no nou CONTAINS? TFALSE ;

public

: RUN ( -- )
   T-RESET
   PREPARE
   BUILD
   s" hb: cannot map guarded VM stack" s" hb: REVIEW map guarded VM stack" IMAGE-HAS
   CHANGE-BUILDER-SAME-MTIME
   BUILD-AFTER-BUILDER-EDIT
   s" hb: cannot map guarded VM stack" s" hb: REVIEW map guarded VM stack" IMAGE-HAS
   BUILD-RACE
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE
   T-REPORT
   s" hb-build-producer-cache-test: ok" type cr ;

;package

HB-BUILD-PRODUCER-TEST:RUN
