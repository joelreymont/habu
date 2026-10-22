\ A file frame records each dependency the file consumes from earlier files
\ once, and none of the records the file makes itself. Each file that compiles
\ whole also writes one object under a key that names the file's place in the
\ load, not its text, so an edit keeps the key.
\ bin/hb --load test/native-file-frame-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-FRAME-TEST

$8000 constant CAP
create INPUT CAP allot variable INPUT-U
64 constant KEY-LEN
create APP-KEY KEY-LEN allot
create LIB-KEY KEY-LEN allot

: IN+ ( ptr u8 n -- ) INPUT CAP INPUT-U BUF-APPEND ;

\ The diagnostic builder session: app.f loads through NDEF-CACHE:LOAD with the
\ cache diagnostics on, so every file frame and object prints its line on stderr.
: PROBE ( -- n )
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   0 INPUT-U !
   S\" 1 set-tier\nNATIVE-RUNTIME:CAPTURE-PREPARE\ns\" " IN+
   s" app.f" AT IN+
   S\" \" s\" repl\" NDEF-CACHE:LOAD\ns\" use=\" type FRAME-USE . cr\n" IN+
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" HABU_NATIVE_CACHE_DIAG" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   builder builderu INPUT INPUT-U @ CAPTURE ;

\ The stderr line whose path ends in name followed by tag, from that tail to
\ its newline: tag " rows=" finds the file frame, " key=" the file object.
: LINE$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n tag:ptr tagu:n :}
   0 INPUT-U !
   name nameu IN+ tag tagu IN+
   ERR$ INPUT INPUT-U @ FIND-SUB MATCH option
     none OF T-FAIL s" " ENDOF
     some OF IDX>N {: at:n :}
        ERR$ drop at +  ERR$ nip at -  {: a:ptr u:n :}
        a u 10 INDEX-OF MATCH option
          none OF a u ENDOF
          some OF IDX>N a swap ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

: FRAME$ ( ptr u8 n -- ptr u8 n ) s"  rows=" LINE$ ;
: OBJECT$ ( ptr u8 n -- ptr u8 n ) s"  key=" LINE$ ;

\ The value of field (" name=") in line, up to the next space or the end.
: FIELD$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: line:ptr lineu:n field:ptr fieldu:n :}
   line lineu field fieldu FIND-SUB MATCH option
     none OF T-FAIL s" " ENDOF
     some OF IDX>N fieldu + {: at:n :}
        line at +  lineu at -  {: a:ptr u:n :}
        a u 32 INDEX-OF MATCH option
          none OF a u ENDOF
          some OF IDX>N a swap ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

: KEY$ ( ptr u8 n -- ptr u8 n ) OBJECT$ s"  key=" FIELD$ ;
: DEFS$ ( ptr u8 n -- ptr u8 n ) OBJECT$ s"  defs=" FIELD$ ;

\ name's object holds count input rows.
: INPUTS ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n count:ptr countu:n :}
   name nameu OBJECT$ s"  inputs=" FIELD$ count countu T$= ;

\ The object file the key names exists in the build cache.
: STORED ( ptr u8 n -- )
   {: key:ptr keyu:n :}
   0 INPUT-U !
   s" cache/hb-file-" IN+ key keyu IN+ s" .hfo" IN+
   INPUT INPUT-U @ AT EXISTS? TTRUE ;

: KEEP-KEY ( ptr u8 n ptr u8 -- )
   {: key:ptr keyu:n dest:ptr :}
   keyu KEY-LEN T=
   key dest keyu KEY-LEN min BYTE-COPY ;

: LIB-SOURCE ( ptr u8 n ptr u8 n -- )
   {: head:ptr headu:n twice:ptr twiceu:n :}
   0 INPUT-U !
   head headu IN+
   S\" require base.f\nrequire deep.f\npackage FRAME-LIB\npublic\n: TWICE ( n -- n ) " IN+
   twice twiceu IN+
   S\"  ;\n: QUAD ( n -- n ) TWICE TWICE FRAME-BASE:ONE + ;\nTRUSTED: BUMP ( n -- n ) 1 + ;\n7 constant SEVEN\n;package\n" IN+
   s" lib.f" INPUT INPUT-U @ PUT ;

\ A rebuild after an edit to lib.f: the app still runs, both files still
\ write objects with every colon definition, under the keys of the first build.
: REBUILT ( ptr u8 n -- )
   T-LABEL
   PROBE SUCCESS
   OUT$ S\" use=67\n" CONTAINS? TTRUE
   s" app.f" DEFS$ s" 2" T$=
   s" lib.f" DEFS$ s" 3" T$=
   s" app.f" KEY$ APP-KEY KEY-LEN T$=
   s" lib.f" KEY$ LIB-KEY KEY-LEN T$= ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-frame" TREE
   s" cache" AT MAKE-DIR
   s" base.f" S\" package FRAME-BASE\npublic\n: ONE ( -- n ) 1 ;\n;package\n" PUT
   s" deep.f" S\" package FRAME-DEEP\npublic\n: TEN ( -- n ) 10 ;\n;package\n" PUT
   s" " s" dup +" LIB-SOURCE
   s" app.f" S\" require base.f\n: PRE ( -- n ) FRAME-BASE:ONE ;\nrequire lib.f\n: FRAME-USE ( -- n )\n   FRAME-LIB:SEVEN FRAME-LIB:TWICE FRAME-LIB:TWICE FRAME-LIB:TWICE FRAME-BASE:ONE + FRAME-DEEP:TEN + ;\n" PUT
   \ Each build below is its own builder process: a key that stays stable
   \ across separate builds is the boundary under test.
   PROBE {: rc:n :}
   s" probe-out.log" OUT$ PUT
   s" probe-err.log" ERR$ PUT
   rc SUCCESS
   OUT$ S\" use=67\n" CONTAINS? TTRUE
   s" app.f consumes ONE, TWICE, TEN and SEVEN once each, and SEVEN's value" T-LABEL
   s" app.f" FRAME$ s"  ordered=0 binding=4 value=1" ENDS-WITH? TTRUE
   s" lib.f consumes ONE only, none of its own or deep.f's records" T-LABEL
   s" lib.f" FRAME$ s"  ordered=0 binding=1 value=0" ENDS-WITH? TTRUE
   s" each file object holds its colon definitions, TRUSTED: BUMP included" T-LABEL
   s" base.f" DEFS$ s" 1" T$=
   s" deep.f" DEFS$ s" 1" T$=
   s" app.f" DEFS$ s" 2" T$=
   s" lib.f" DEFS$ s" 3" T$=
   s" each file object holds exactly its own input rows" T-LABEL
   s" base.f" s" 1" INPUTS
   s" deep.f" s" 1" INPUTS
   s" lib.f" s" 4" INPUTS
   s" app.f" s" 5" INPUTS
   s" each object is stored under its key" T-LABEL
   s" app.f" KEY$ APP-KEY KEEP-KEY
   s" lib.f" KEY$ LIB-KEY KEEP-KEY
   APP-KEY KEY-LEN STORED
   LIB-KEY KEY-LEN STORED
   S\" \\ edited\n" s" dup +" LIB-SOURCE
   s" a comment line added to lib.f keeps both keys" REBUILT
   s" " s" 2 *" LIB-SOURCE
   s" a changed TWICE body keeps both keys" REBUILT
   T-REPORT
   s" file frame artifacts: " type ROOT$ type cr ;

;package
;using

NATIVE-FILE-FRAME-TEST:MAIN
