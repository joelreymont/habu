\ A producer source edit changes the emitted application on a warm cache.
\ Run: bin/hb --load tools/hb-build-producer-cache-test.f

require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-cwd.f
require lib/process-argv.f
require lib/process-env.f
require lib/tree-copy.f

package HB-BUILD-PRODUCER-TEST

65536 constant CAP
262144 constant IMAGE-CAP
600000 constant TIMEOUT-MS
create ROOT FS-PATH-CAP allot
create HB FS-PATH-CAP allot
create APP FS-PATH-CAP allot
create OUT-PATH FS-PATH-CAP allot
create RT FS-PATH-CAP allot
create TMP FS-PATH-CAP allot
create CACHE FS-PATH-CAP allot
create TEXT CAP allot
create IMAGE IMAGE-CAP allot
create OUT CAP allot
create ERR CAP allot
variable ROOT-U
variable HB-U
variable APP-U
variable OUT-U
variable RT-U
variable TMP-U
variable CACHE-U
variable TEXT-U
variable CAP-OUT-U
variable CAP-ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: HB$ ( -- ptr u8 n ) HB HB-U @ ;
: APP$ ( -- ptr u8 n ) APP APP-U @ ;
: OUT$ ( -- ptr u8 n ) OUT-PATH OUT-U @ ;
: RT$ ( -- ptr u8 n ) RT RT-U @ ;
: TMP$ ( -- ptr u8 n ) TMP TMP-U @ ;
: CACHE$ ( -- ptr u8 n ) CACHE CACHE-U @ ;

: PATHS ( -- )
   ROOT$ s" bin/hb" HB JOIN-PATH HB-U !
   ROOT$ s" app.f" APP JOIN-PATH APP-U !
   ROOT$ s" app" OUT-PATH JOIN-PATH OUT-U !
   ROOT$ s" src/habu/rt.f" RT JOIN-PATH RT-U !
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
   s" tools/hb-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" app.f" >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   s" app" >LEN PROC-ARGV+
   s" HB_TMP" >LEN TMP$ >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN CACHE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN ROOT$ >LEN
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

: CHANGE-RT ( -- )
   RT$ TEXT CAP READ-ALL TEXT-U !
   TEXT TEXT-U @ s" hb: cannot map guarded VM stack" FIND-SUB MATCH option
      none OF s" rt.f has no guarded-stack diagnostic" T-EX-FAIL die ENDOF
      some OF
         IDX>N TEXT +
         s" hb: REVIEW map guarded VM stack" rot swap BYTE-COPY
      ENDOF
   ;MATCH
   RT$ TEXT TEXT-U @ WRITE-ALL ;

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
   CHANGE-RT
   BUILD
   s" hb: REVIEW map guarded VM stack" s" hb: cannot map guarded VM stack" IMAGE-HAS
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE
   PREPARE
   BUILD-RACE
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE
   T-REPORT
   s" hb-build-producer-cache-test: ok" type cr ;

;package

HB-BUILD-PRODUCER-TEST:RUN
