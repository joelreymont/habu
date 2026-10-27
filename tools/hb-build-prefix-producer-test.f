\ Reused caller definitions bind the producer loaded by the current process.
\ Nested includes and edits after loading preserve the consumed source bytes.
\ Run: HABU_UNDER_TEST=/path/to/hb /path/to/hb --load tools/hb-build-prefix-producer-test.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-cwd.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/tree-copy.f

package HB-BUILD-PREFIX-PRODUCER-TEST

65536 constant CAP
600000 constant TIMEOUT-MS
create ROOT FS-PATH-CAP allot
create HB FS-PATH-CAP allot
create APP FS-PATH-CAP allot
create OUT-PATH FS-PATH-CAP allot
create PRODUCER FS-PATH-CAP allot
create HELPER FS-PATH-CAP allot
create WRAPPER FS-PATH-CAP allot
create TMP FS-PATH-CAP allot
create CACHE FS-PATH-CAP allot
create OUT CAP allot
create ERR CAP allot
variable ROOT-U
variable HB-U
variable APP-U
variable OUT-U
variable PRODUCER-U
variable HELPER-U
variable WRAPPER-U
variable TMP-U
variable CACHE-U
variable CAP-OUT-U
variable CAP-ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: HB$ ( -- ptr u8 n ) HB HB-U @ ;
: APP$ ( -- ptr u8 n ) APP APP-U @ ;
: OUT$ ( -- ptr u8 n ) OUT-PATH OUT-U @ ;
: PRODUCER$ ( -- ptr u8 n ) PRODUCER PRODUCER-U @ ;
: HELPER$ ( -- ptr u8 n ) HELPER HELPER-U @ ;
: WRAPPER$ ( -- ptr u8 n ) WRAPPER WRAPPER-U @ ;
: TMP$ ( -- ptr u8 n ) TMP TMP-U @ ;
: CACHE$ ( -- ptr u8 n ) CACHE CACHE-U @ ;

: PATHS ( -- )
   ROOT$ s" bin/hb" HB JOIN-PATH HB-U !
   ROOT$ s" app.f" APP JOIN-PATH APP-U !
   ROOT$ s" app" OUT-PATH JOIN-PATH OUT-U !
   ROOT$ s" producer.f" PRODUCER JOIN-PATH PRODUCER-U !
   ROOT$ s" helper.f" HELPER JOIN-PATH HELPER-U !
   ROOT$ s" hbtmp" TMP JOIN-PATH TMP-U ! ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-prefix-producer" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ TREE-COPY:BUILD-SOURCES
   PATHS
   HB$ SOURCE-ROOT:DIRNAME MAKE-DIRS
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" then HB$ COPY-FILE-STREAM
   HB$ CHMOD-X
   TMP$ MAKE-DIR
   APP$ s" : MAIN ( -- ) FILE-PRODUCER:VALUE . ;" WRITE-ALL ;

\ Each case keys its objects and builder images in its own cache and out root
\ under the one prepared tree.
: CACHE! ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   ROOT$ name nameu CACHE JOIN-PATH CACHE-U !
   CACHE$ MAKE-DIR ;

: CAPTURE ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N CAP-OUT-U ! erru LEN>N CAP-ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N CAP-OUT-U ! erru LEN>N CAP-ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

: WRAPPER! ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   ROOT$ name nameu WRAPPER JOIN-PATH WRAPPER-U !
   WRAPPER$ body bodyu WRITE-ALL ;

: BUILD ( ptr u8 n ptr u8 n n -- )
   {: input:ptr inputu:n expect:ptr expectu:n edit:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" app.f" >LEN PROC-ARGV+
   s" app" >LEN PROC-ARGV+
   s" HB_TMP" >LEN TMP$ >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN CACHE$ >LEN PROC-ENV+
   s" HABU_OUT" >LEN CACHE$ >LEN PROC-ENV+
   edit 0<> if s" HABU_PREFIX_TEST_EDIT" >LEN s" 1" >LEN PROC-ENV+ then
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN ROOT$ >LEN input inputu >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE
   RC @ 0<> if OUT CAP-OUT-U @ type ERR CAP-ERR-U @ type then
   RC @ 0 T=
   OUT CAP-OUT-U @ expect expectu CONTAINS? TTRUE ;

: RUN-APP ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   PROC-ENV-INHERIT-MISSING
   OUT$ >LEN ROOT$ >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE
   RC @ 0 T=
   OUT CAP-OUT-U @ want wantu CONTAINS? TTRUE ;

: INCLUDE-CASE ( -- )
   s" cache-include" CACHE!
   PRODUCER$ S\" include helper.f\n" WRITE-ALL
   HELPER$ S\" package FILE-PRODUCER public : VALUE ( -- n ) 41 ; ;package\n" WRITE-ALL
   s" launch.f" S\" require tools/app-build.f\nrequire producer.f\n" WRAPPER!
   S\" require launch.f\nAPP-BUILD:RUN\n" s" compiled 1" 0 BUILD
   s" 41" RUN-APP
   HELPER$ S\" package FILE-PRODUCER public : PAD ( -- n ) 999 ; : VALUE ( -- n ) 41 ; ;package\n" WRITE-ALL
   \ An unrelated preloaded declaration leaves MAIN reusable.
   S\" require launch.f\nAPP-BUILD:RUN\n" s" compiled 0" 0 BUILD
   s" 41" RUN-APP ;

: EDIT-AFTER-LOAD-CASE ( -- )
   s" cache-edit" CACHE!
   PRODUCER$ S\" package FILE-PRODUCER public : VALUE ( -- n ) 41 ; ;package\n" WRITE-ALL
   s" launch.f" S\" require tools/app-build.f\nrequire producer.f\n: MAYBE-EDIT ( -- ) s\" HABU_PREFIX_TEST_EDIT\" GETENV nip 0 > if s\" producer.f\" s\" package FILE-PRODUCER public : PAD ( -- n ) 999 ; : VALUE ( -- n ) 999 ; ;package\" WRITE-ALL then ;\nMAYBE-EDIT\n" WRAPPER!
   S\" require launch.f\nAPP-BUILD:RUN\n" s" compiled 1" 1 BUILD
   s" 41" RUN-APP
   \ The new process relinks reused MAIN to the VALUE it loaded.
   S\" require launch.f\nAPP-BUILD:RUN\n" s" compiled 0" 0 BUILD
   s" 999" RUN-APP ;

public

: RUN ( -- )
   T-RESET
   PREPARE
   INCLUDE-CASE
   EDIT-AFTER-LOAD-CASE
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE
   T-REPORT
   s" hb-build-prefix-producer-test: ok" type cr ;

;package

HB-BUILD-PREFIX-PRODUCER-TEST:RUN
