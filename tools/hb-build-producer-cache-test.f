\ Producer edits change the application on a warm cache: a builder edit from an
\ external CWD, a runtime edit the link reads after the application loaded,
\ then a runtime edit racing the maker (tools/hb-build-producer-race-test.f,
\ run in this copy after those builds). Between them, the report's per-file
\ counts across a cold build, an unchanged rebuild and a one-file edit.
\ Run: bin/hb --load tools/hb-build-producer-cache-test.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-cwd.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/tree-copy.f
require src/lib/json-read.f
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
create RT FS-PATH-CAP allot
create RT-B FS-PATH-CAP allot
create LIB FS-PATH-CAP allot
create IMAGE IMAGE-CAP allot
create RT-TEXT CAP allot
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
variable RT-U
variable RT-B-U
variable LIB-U
variable RT-TEXT-U
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
: RT$ ( -- ptr u8 n ) RT RT-U @ ;
: RT-B$ ( -- ptr u8 n ) RT-B RT-B-U @ ;
: LIB$ ( -- ptr u8 n ) LIB LIB-U @ ;

: PATHS ( -- )
   ROOT$ s" project" PROJECT JOIN-PATH PROJECT-U !
   ROOT$ s" bin/hb" HB JOIN-PATH HB-U !
   ROOT$ s" app.f" APP JOIN-PATH APP-U !
   ROOT$ s" app" OUT-PATH JOIN-PATH OUT-U !
   ROOT$ s" tools/hb-build-lib.f" BUILDER JOIN-PATH BUILDER-U !
   ROOT$ s" tools/hb-build.f" TOOL JOIN-PATH TOOL-U !
   ROOT$ s" maker-saved.f" SAVED JOIN-PATH SAVED-U !
   ROOT$ s" hbtmp" TMP JOIN-PATH TMP-U !
   ROOT$ s" cache" CACHE JOIN-PATH CACHE-U !
   ROOT$ s" src/habu/rt.f" RT JOIN-PATH RT-U !
   ROOT$ s" rt-b.f" RT-B JOIN-PATH RT-B-U !
   ROOT$ s" counts-lib.f" LIB JOIN-PATH LIB-U ! ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-producer-cache" TMPDIR-MKDIR {: a:ptr u:n :}
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

: BUILD-WITH ( bool -- )
   {: report:bool :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   report if s" --report-json" >LEN PROC-ARGV+ then
   APP$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   OUT$ >LEN PROC-ARGV+
   s" TMPDIR" >LEN TMP$ >LEN PROC-ENV+
   \ Mask a parent override: the invoking absolute engine is the CLI default.
   s" HABU_FIXPOINT_ENGINE" >LEN s" " >LEN PROC-ENV+
   s" HABU_OUT" >LEN CACHE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN PROJECT$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE
   RC @ 0<> if OUT CAP-OUT-U @ type ERR CAP-ERR-U @ type then
   RC @ 0 T= ;

: BUILD ( -- ) false BUILD-WITH ;

: BUILD-RACE ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/hb-build-producer-race-test.f" >LEN PROC-ARGV+
   s" TMPDIR" >LEN TMP$ >LEN PROC-ENV+
   s" HABU_OUT" >LEN CACHE$ >LEN PROC-ENV+
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
   s" TMPDIR" >LEN TMP$ >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN HB$ >LEN PROC-ENV+
   s" HABU_OUT" >LEN CACHE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN PROJECT$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE
   RC @ 0<> if OUT CAP-OUT-U @ type ERR CAP-ERR-U @ type then
   RC @ 0 T=
   OUT CAP-OUT-U @ S\" \"maker_ran\":true" CONTAINS? TTRUE ;

: IMAGE-HAS ( ptr u8 n ptr u8 n -- ) {: yes:ptr yesu:n no:ptr nou:n :}
   OUT$ IMAGE IMAGE-CAP READ-ALL {: u:n :}
   IMAGE u yes yesu CONTAINS? TTRUE
   IMAGE u no nou CONTAINS? TFALSE ;


TYPED-VARIABLE REPORT-STATE JR:cursor

\ The report-json build's files_reused and files_compiled.
: BUILD-FILES ( -- n n )
   true BUILD-WITH
   REPORT-STATE OUT CAP-OUT-U @ JR:INIT
   JR:NEXT JR:T-OBJ T=
   s" files_reused" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   JR:INT {: reused:n :}
   s" files_compiled" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   JR:INT {: compiled:n :}
   JR:CLOSE
   reused compiled ;


\ The application is exactly two files, so every build counts two: a session
\ that is not the application load, overwriting the counts or adding its own,
\ changes the sum. The cold build compiles both. Which of them the rebuild and
\ the edit reuse is the recompile gate's to assert.
2 constant APP-FILES

: FILE-COUNTS ( -- )
   LIB$ s" : COUNTS-ONE ( -- n ) 1 ;" WRITE-ALL
   APP$ S\" require counts-lib.f\n: MAIN ( -- ) ;\n" WRITE-ALL
   BUILD-FILES {: cold-reused:n cold-compiled:n :}
   cold-reused 0 T= cold-compiled APP-FILES T=
   BUILD-FILES + APP-FILES T=
   APP$ S\" require counts-lib.f\n: MAIN ( -- ) COUNTS-ONE drop ;\n" WRITE-ALL
   BUILD-FILES + APP-FILES T=
   APP$ s" : MAIN ( -- ) ;" WRITE-ALL ;


\ The application's load moves rt-b.f, the runtime with its guarded-stack
\ diagnostic edited, over rt.f once, the way an edit lands after the maker
\ loaded the application and before the link reads the runtime.
: WRITE-SWAP-APP ( -- )
   SB-RESET
   s" require src/lib/fs.f" SB-APPEND 10 SB-APPEND-C
   s" require src/lib/fs-mutate.f" SB-APPEND 10 SB-APPEND-C
   s" package LINK-RACE-APP" SB-APPEND 10 SB-APPEND-C
   S\" : B$ ( -- ptr u8 n ) s\" " SB-APPEND RT-B$ SB-APPEND S\" \" ;" SB-APPEND
   10 SB-APPEND-C
   s" public" SB-APPEND 10 SB-APPEND-C
   s" : SWAP-RT ( -- ) B$ FILE? if B$ " SB-APPEND
   S\" s\" " SB-APPEND RT$ SB-APPEND S\" \" RENAME-FILE then ;" SB-APPEND
   10 SB-APPEND-C
   s" ;package" SB-APPEND 10 SB-APPEND-C
   s" LINK-RACE-APP:SWAP-RT" SB-APPEND 10 SB-APPEND-C
   s" : MAIN ( -- ) ;" SB-APPEND 10 SB-APPEND-C
   APP$ SB$ WRITE-ALL ;


\ Offset of the guarded-stack diagnostic in the original runtime text.
: RT-AT ( -- n )
   RT$ RT-TEXT CAP READ-ALL RT-TEXT-U !
   RT-TEXT RT-TEXT-U @ s" hb: cannot map guarded VM stack" FIND-SUB MATCH option
      none OF s" rt.f has no guarded-stack diagnostic" T-EX-FAIL die ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;


\ RT-TEXT keeps the original; rt-b.f gets the edited copy.
: WRITE-RT-B ( -- )
   RT-AT RT-TEXT + {: at:ptr :}
   s" hb: REVIEW map guarded VM stack" at swap BYTE-COPY
   RT-B$ RT-TEXT RT-TEXT-U @ WRITE-ALL
   s" hb: cannot map guarded VM stack" at swap BYTE-COPY ;


\ The first build's link reads the edited runtime. Restoring the original
\ must miss that artifact: the link's reads are build inputs too.
: LINK-INPUT-RACE ( -- )
   WRITE-SWAP-APP
   WRITE-RT-B
   BUILD
   RT-B$ EXISTS? TFALSE
   s" hb: REVIEW map guarded VM stack" s" hb: cannot map guarded VM stack" IMAGE-HAS
   RT$ RT-TEXT RT-TEXT-U @ WRITE-ALL
   BUILD
   s" hb: cannot map guarded VM stack" s" hb: REVIEW map guarded VM stack" IMAGE-HAS
   APP$ s" : MAIN ( -- ) ;" WRITE-ALL ;


public

: RUN ( -- )
   T-RESET
   PREPARE
   BUILD
   s" hb: cannot map guarded VM stack" s" hb: REVIEW map guarded VM stack" IMAGE-HAS
   CHANGE-BUILDER-SAME-MTIME
   BUILD-AFTER-BUILDER-EDIT
   s" hb: cannot map guarded VM stack" s" hb: REVIEW map guarded VM stack" IMAGE-HAS
   LINK-INPUT-RACE
   FILE-COUNTS
   BUILD-RACE
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE
   T-REPORT
   s" hb-build-producer-cache-test: ok" type cr ;

;package

HB-BUILD-PRODUCER-TEST:RUN
