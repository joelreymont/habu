\ A FILE that extends CWORK must run its source on every build: a native
\ range cannot replay the workspace layout or its mapped default values.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-app-lib.f

package NATIVE-FILE-CWORK-TEST
$8000 constant CAP
create ROOT FS-PATH-CAP allot variable ROOT-U
create TEMP FS-PATH-CAP allot
create APP FS-PATH-CAP allot variable APP-U
create BIN-PATH FS-PATH-CAP allot variable BIN-PATH-U
create CACHE FS-PATH-CAP allot variable CACHE-U
create OUT CAP allot variable OUT-U
create ERR CAP allot variable ERR-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n ) ROOT$ 2swap TEMP JOIN-PATH TEMP swap ;
: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   name nameu AT body bodyu WRITE-ALL ;
: SUCCESS ( n -- )
   dup 0<> if NATIVE-FILE-APP:OUT$ type NATIVE-FILE-APP:ERR$ type then 0 T= ;
: BUILD ( ptr u8 n -- )
   AT {: a:ptr u:n :} a BIN-PATH u BYTE-COPY u BIN-PATH-U !
   APP APP-U @ BIN-PATH BIN-PATH-U @ CACHE CACHE-U @ NATIVE-FILE-APP:BUILD SUCCESS
   NATIVE-FILE-APP:OUT$ type NATIVE-FILE-APP:ERR$ type ;
: RUN-BIN-PATH ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   BIN-PATH BIN-PATH-U @ >LEN s" " >LEN
   OUT CAP >LEN ERR CAP >LEN 60000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   OUT OUT-U @ type ERR ERR-U @ type rc 0 T= ;
: APP-COLD ( -- )
   s" app.f" S\" require unit.f\nvariable OBSERVED\nCWORK-FILE:VALUE @ OBSERVED !\n: MAIN ( -- ) OBSERVED @ . CWORK-FILE:VALUE @ . 42 . ;\n" PUT ;
: APP-WARM ( -- )
   s" app.f" S\" require unit.f\nvariable OBSERVED\nCWORK-FILE:VALUE @ OBSERVED !\n: MAIN ( -- ) OBSERVED @ . CWORK-FILE:VALUE @ . 43 . ;\n" PUT ;

: CHECK-TYPED-STORAGE ( -- )
   s" unit.f" S\" package CWORK-FILE public\n2 CWORK-BUFFER ROWS n\n2 CWORK-SPAN WORDS n\n;package\n" PUT
   s" app.f" S\" require unit.f\n: MAIN ( -- )\n17 0 CWORK-FILE:ROWS ! 0 CWORK-FILE:ROWS @ .\n23 CWORK-FILE:WORDS ! CWORK-FILE:WORDS @ .\n[: -1 CWORK-FILE:ROWS drop ;] catch .\n[: 2 CWORK-FILE:ROWS drop ;] catch .\n;\n" PUT
   s" CWORK typed storage survives capture and bounds its rows" T-LABEL
   s" typed-hb" BUILD
   RUN-BIN-PATH
   OUT OUT-U @ S\" 17\n23\n7122\n7122\n" CONTAINS? TTRUE
   s" app.f" S\" require unit.f\npackage CWORK-CLIENT public\n: WRONG ( n -- ptr u8 ) CWORK-FILE:ROWS ;\n;package\n: MAIN ( -- ) ;\n" PUT
   s" CWORK accessors reject the wrong pointee" T-LABEL
   APP APP-U @ BIN-PATH BIN-PATH-U @ CACHE CACHE-U @ NATIVE-FILE-APP:BUILD 70 T=
   NATIVE-FILE-APP:ERR$ s" expected: ptr u8 actual: ptr n" CONTAINS? TTRUE ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-cwork" HB-TMP-MKDIR {: root:ptr rootu:n :}
   root ROOT rootu BYTE-COPY rootu ROOT-U !
   s" cache" AT {: cache:ptr cacheu:n :}
   cache CACHE cacheu BYTE-COPY cacheu CACHE-U ! CACHE CACHE-U @ MAKE-DIR
   s" app.f" AT {: app:ptr appu:n :}
   app APP appu BYTE-COPY appu APP-U !
   s" unit.f" S\" package CWORK-FILE public\n-1 CWORK-VARIABLE VALUE n\n;package\ns\" cwork unit source ran\" type cr\n" PUT
   APP-COLD
   s" cold FILE compilation observes the late default" T-LABEL
   s" cold-hb" BUILD
   NATIVE-FILE-APP:OUT$ s" cwork unit source ran" CONTAINS? TTRUE
   RUN-BIN-PATH
   OUT OUT-U @ S\" -1\n-1\n42\n" CONTAINS? TTRUE
   APP-WARM
   s" changed parent reruns CWORK child source" T-LABEL
   s" warm-hb" BUILD
   NATIVE-FILE-APP:OUT$ s" cwork unit source ran" CONTAINS? TTRUE
   NATIVE-FILE-APP:OUT$ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   RUN-BIN-PATH
   OUT OUT-U @ S\" -1\n-1\n43\n" CONTAINS? TTRUE
   CHECK-TYPED-STORAGE
   T-REPORT
   s" CWORK FILE artifacts: " type ROOT$ type cr ;
;package
NATIVE-FILE-CWORK-TEST:MAIN
