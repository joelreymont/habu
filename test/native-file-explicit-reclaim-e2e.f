\ Explicit export keeps only sites still in the selected source's final code.
\ bin/hb --load test/native-file-explicit-reclaim-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require tools/native-unit-file.f

package NATIVE-FILE-EXPLICIT-RECLAIM-TEST
private

$8000 constant CAP
create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
create OUT CAP allot variable OUT-U
create ERR CAP allot variable ERR-U
DYNAMIC-BUFFER ART-BYTES u8
variable ORIGIN-OFF
variable ORIGIN-COUNT

: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT ROOT-U @ name u PATH JOIN-PATH PATH swap ;

: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr u:n body:ptr bodyu:n :}
   name u AT body bodyu WRITE-ALL ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: CAPTURE ( ptr u8 n ptr u8 n -- n )
   {: exe:ptr exeu:n input:ptr inputu:n :}
   exe exeu >LEN input inputu >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;

: SUCCESS ( n -- )
   dup 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   0 T= ;

: BUILD ( ptr u8 n ptr u8 n -- )
   {: mode:ptr modeu:n product:ptr productu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/native-unit-build.f" ARG s" --" ARG
   mode modeu ARG
   s" prefix.f" AT ARG
   s" selected.f" AT ARG
   s" downstream.f" AT ARG
   s" artifact.hfo" AT ARG
   product productu AT ARG
   ENGINE-CANDIDATE:PATH$ S\" \n" CAPTURE SUCCESS ;

: RUN-PRODUCT ( ptr u8 n ptr u8 n -- )
   {: product:ptr productu:n want:ptr wantu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   product productu AT S\" MAIN\n" CAPTURE SUCCESS
   OUT OUT-U @ want wantu CONTAINS? TTRUE ;

: ROUNDTRIP ( ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n result:ptr resultu:n :}
   s" --export-file" s" export-hb" BUILD
   OUT OUT-U @ label labelu CONTAINS? TTRUE
   s" artifact.hfo" AT EXISTS? TTRUE
   s" export-hb" result resultu RUN-PRODUCT
   s" --import-file" s" import-hb" BUILD
   OUT OUT-U @ s" native-build: file unit hit" CONTAINS? TTRUE
   OUT OUT-U @ label labelu CONTAINS? 0= TTRUE
   s" import-hb" result resultu RUN-PRODUCT ;

: ORIGIN-ROWS ( ptr u8 n ptr u8 n -- n )
   {: want-pkg:ptr want-pkgu:n want-tail:ptr want-tailu:n :}
   s" artifact.hfo" AT FILE-SIZE {: size:n :}
   size ART-BYTES-RESERVE
   s" artifact.hfo" AT 0 ART-BYTES size READ-ALL size <> if E-FS-IO throw then
   0 ART-BYTES {: file:ptr :}
   file 16 + LE:U64@ file 24 + LE:U64@ + 128 + {: event:n :}
   file event + 24 + LE:U64@ {: rangeu:n :}
   file event + 32 + {: range:ptr :}
   range LE:U64@ {: unitu:n :}
   unitu rangeu 8 - > if E-NUNIT-FILE throw then
   range 8 + unitu NUNIT-FILE:ARCH-AARCH64 s" FILE"
      file 32 + 64 NUNIT-FILE:READ-BYTES
   16 NUNIT-FILE:SECTION$ {: origin:ptr originu:n :}
   originu 48 < if E-NUNIT-FILE throw then
   origin 8 + LE:U64@ {: count:n :}
   48 ORIGIN-OFF !
   0 ORIGIN-COUNT !
   count 0 ?do
      originu ORIGIN-OFF @ - 72 < if E-NUNIT-FILE throw then
      origin ORIGIN-OFF @ + {: row:ptr :}
      row 16 + LE:U64@ {: pkgu:n :}
      row 32 + LE:U64@ {: tailu:n :}
      row LE:U64@ 1 =
      row 72 + pkgu want-pkg want-pkgu STR= and
      row 72 + pkgu + tailu want-tail want-tailu STR= and if
         1 ORIGIN-COUNT +!
      then
      72 pkgu + tailu + row 40 + LE:U64@ +
         row 48 + LE:U64@ + ORIGIN-OFF +!
   loop
   ORIGIN-OFF @ originu <> if E-NUNIT-FILE throw then
   NUNIT-FILE:CLOSE
   ORIGIN-COUNT @ ;

: REUSE-SOURCE ( -- )
   SB-RESET
   S\" package EXPLICIT-RECLAIM\npublic\nvariable OLD-XT\n: VANISH ( -- n ) 9 ;\n" SB-APPEND
   S\" s\" EXPLICIT-RECLAIM:VANISH\" XREF-FIND XREF-START OLD-XT !\n" SB-APPEND
   S\" s\" EXPLICIT-RECLAIM:VANISH\" FORGET-DEFS-FROM\n" SB-APPEND
   S\" : REUSED ( -- n ) 4 ;\n" SB-APPEND
   S\" : CHECK-REUSE ( -- ) s\" EXPLICIT-RECLAIM:REUSED\" XREF-FIND XREF-START OLD-XT @ <> if 70 throw then 0 OLD-XT ! ;\nCHECK-REUSE\n" SB-APPEND
   S\" ;package\ns\" reused-ran\" type cr\n" SB-APPEND
   s" selected.f" SB$ PUT ;

public
: MAIN ( -- )
   T-RESET
   s" explicit capture prunes retired call sites" T-LABEL
   s" native-file-explicit-reclaim" HB-TMP-MKDIR {: root:ptr u:n :}
   root ROOT u BYTE-COPY u ROOT-U !
   s" prefix.f" S\" : PREFIX-NOOP ( -- ) ;\n" PUT
   s" selected.f"
   S\" package EXPLICIT-RECLAIM\npublic\nvariable RESULT\n: OLD ( -- n ) 1 ;\n: SAVE ( -- ) OLD RESULT ! ;\nSAVE\ns\" EXPLICIT-RECLAIM:OLD\" FORGET-DEFS-FROM\n;package\ns\" selected-ran\" type cr\n" PUT
   s" downstream.f" S\" : MAIN ( -- ) EXPLICIT-RECLAIM:RESULT @ . ;\n" PUT
   s" selected-ran" S\" 1\n" ROUNDTRIP

   s" retained caller still binds the original definition" T-LABEL
   s" selected.f"
   S\" package EXPLICIT-RECLAIM\npublic\n: OLD ( -- n ) 1 ;\n: OLD-CALLER ( -- n ) OLD ;\nundefine OLD\n: OLD ( -- n ) 2 ;\n;package\ns\" retained-ran\" type cr\n" PUT
   s" downstream.f"
   S\" : MAIN ( -- ) EXPLICIT-RECLAIM:OLD-CALLER . EXPLICIT-RECLAIM:OLD . ;\n" PUT
   s" retained-ran" S\" 1\n2\n" ROUNDTRIP
   s" explicit-reclaim" s" old" ORIGIN-ROWS 2 T=

   s" a reclaimed slot cannot revive its old origin" T-LABEL
   REUSE-SOURCE
   s" downstream.f" S\" : MAIN ( -- ) EXPLICIT-RECLAIM:REUSED . ;\n" PUT
   s" reused-ran" S\" 4\n" ROUNDTRIP
   s" explicit-reclaim" s" vanish" ORIGIN-ROWS 0 T=
   s" explicit-reclaim" s" reused" ORIGIN-ROWS 1 T=

   s" an explicit structure keeps its nominal identity" T-LABEL
   s" selected.f"
   S\" package ENOM\npublic\nSTRUCTURE pair FIELD left n ;STRUCTURE\n;package\ns\" nominal-ran\" type cr\n" PUT
   s" downstream.f"
   S\" : MAIN ( -- ) 42 ENOM-PAIR:MAKE ENOM-PAIR:UNMAKE . ;\n" PUT
   s" nominal-ran" S\" 42\n" ROUNDTRIP
   s" definitions published before acceptance keep their own origin" T-LABEL
   s" selected.f"
   S\" package PUB-ORDER\npublic\ncreate CREATED 8 allot\n42 constant NUMBER\n;package\ns\" published-ran\" type cr\n" PUT
   s" downstream.f" S\" : MAIN ( -- ) PUB-ORDER:NUMBER . ;\n" PUT
   s" published-ran" S\" 42\n" ROUNDTRIP
   s" pub-order" s" created" ORIGIN-ROWS 1 T=
   s" pub-order" s" number" ORIGIN-ROWS 1 T=
   s" a failed checked definition cannot lend its origin to a reused slot" T-LABEL
   s" selected.f"
   S\" package FAILED-ORIGIN\npublic\n: MAKE ( n -- ) create , does> ( -- ptr n ) ;\n: CHECK-FAILURE ( -- ) [: s\" : FAILED ( n -- ) MAKE does> ( -- ptr n ) [: 1 ;] drop ;\" evaluate-closed ;] catch E-NELAB-QUOT <> if 70 throw then ;\nCHECK-FAILURE\ncreate CREATED 8 allot\nTRUSTED: NEXT ( -- n ) 7 ;\n;package\ns\" failed-origin-ran\" type cr\n" PUT
   s" downstream.f" S\" : MAIN ( -- ) FAILED-ORIGIN:NEXT . ;\n" PUT
   s" failed-origin-ran" S\" 7\n" ROUNDTRIP
   s" failed-origin" s" failed" ORIGIN-ROWS 0 T=
   s" failed-origin" s" created" ORIGIN-ROWS 1 T=
   s" failed-origin" s" next" ORIGIN-ROWS 0 T=
   ART-BYTES-RELEASE
   T-REPORT
   s" native file explicit reclaim artifacts: " type ROOT ROOT-U @ type cr ;
;package

NATIVE-FILE-EXPLICIT-RECLAIM-TEST:MAIN
