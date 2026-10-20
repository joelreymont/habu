\ Compile an explicitly selected source between ordinary prefix and client
\ files. Keep the object and executable products for repeatable inspection.
\ bin/hb --load test/native-file-unit-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/lib/process-argv.f
require src/lib/process-env.f
require test/whitebox-engine.f
require src/lib/process-cwd.f
require src/lib/test/subject.f
require src/lib/tree-copy.f
require tools/image-size-lib.f
require tools/native-unit-file.f
require tools/native-file-object.f

package NATIVE-FILE-UNIT-TEST

$8000 constant CAP
create ROOT FS-PATH-CAP allot  variable ROOT-U
create ALT FS-PATH-CAP allot   variable ALT-U
create APP FS-PATH-CAP allot   variable APP-U
create FOREIGN FS-PATH-CAP allot variable FOREIGN-U
create ORIGINAL FS-PATH-CAP allot variable ORIGINAL-U
create AT-BUF FS-PATH-CAP allot
create ALT-BUF FS-PATH-CAP allot
create ORIGINAL-AT-BUF FS-PATH-CAP allot
create PUT-BUF FS-PATH-CAP allot
create OUT CAP allot  variable OUT-U
create ERR CAP allot  variable ERR-U
create PROGRAM CAP allot  variable PROGRAM-U
create CUT CAP allot  variable CUT-U
DYNAMIC-BUFFER IMAGE-A u8
DYNAMIC-BUFFER IMAGE-B u8
variable ORIGIN-OFF
variable CODE-A-O variable CODE-A-U
variable CODE-B-O variable CODE-B-U
variable WHITEBOX-ARG?
variable EXTERNAL?
variable ROOTED?
variable ALT?
variable PREFIX-TWO?
variable ENV-VALUE

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: ALT$ ( -- ptr u8 n ) ALT ALT-U @ ;
: APP$ ( -- ptr u8 n ) APP APP-U @ ;
: FOREIGN$ ( -- ptr u8 n ) FOREIGN FOREIGN-U @ ;
: ORIGINAL$ ( -- ptr u8 n ) ORIGINAL ORIGINAL-U @ ;
: ORIGINAL-AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ORIGINAL$ name u ORIGINAL-AT-BUF JOIN-PATH ORIGINAL-AT-BUF swap ;
: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   APP$ name u AT-BUF JOIN-PATH AT-BUF swap ;
: ROOT-AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u AT-BUF JOIN-PATH AT-BUF swap ;
: ALT-AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ALT$ name u ALT-BUF JOIN-PATH ALT-BUF swap ;

: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   APP$ name nameu PUT-BUF JOIN-PATH PUT-BUF swap body bodyu WRITE-ALL ;

: PUT-ROOT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   ROOT$ name nameu PUT-BUF JOIN-PATH PUT-BUF swap body bodyu WRITE-ALL ;

: PREFIX ( -- )
   s" prefix.f"
   S\" package FILE-PREFIX\nprivate\n: BASE ( -- n ) 10 ;\npublic\n: PLUS-BASE ( n -- n ) BASE + ;\n;package\n" PUT ;

: TASK-PREFIX ( -- )
   s" prefix.f"
   S\" require src/lib/task.f\nrequire src/lib/process-env.f\npackage FILE-TASK-PREFIX\npublic\nTASK:#USER s\" HABU_TASK_PREFIX_TWO\" GETENV nip 1+ cells TASK:+USER PREFIX-SLOT drop\n;package\n" PUT ;

: TASK-UNIT ( -- )
   s" nested/unit.f"
   S\" require ../prefix.f\npackage FILE-TASK-UNIT\npublic\nTASK:#USER CELL TASK:+USER LEAF drop\n: LEAF@ ( -- n ) LEAF @ ;\n;package\ns\" task-unit-source-ran\" type cr\n" PUT ;

: TASK-DOWNSTREAM ( -- )
   s" downstream.f"
   S\" require nested/unit.f\npackage FILE-TASK-CLIENT\npublic\nTASK:#USER CELL TASK:+USER OWN drop\n: LEAF@ ( -- n ) FILE-TASK-UNIT:LEAF@ ;\n: OWN@ ( -- n ) OWN @ ;\n111 FILE-TASK-UNIT:LEAF !\n222 OWN !\n;package\n" PUT ;

: CHANGED-PREFIX ( -- )
   s" prefix.f"
   S\" package FILE-PREFIX\nprivate\n: BASE ( -- n ) 11 ;\npublic\n: PLUS-BASE ( n -- n ) BASE + ;\n;package\n" PUT ;

: CHILD ( -- )
   s" child.f"
   S\" package FILE-UNIT\npublic\n: DELTA ( -- n ) 32 ;\n;package\n" PUT-ROOT ;

: CHANGED-CHILD ( -- )
   s" child.f"
   S\" package FILE-UNIT\npublic\n: DELTA ( -- n ) 33 ;\n;package\n" PUT-ROOT ;

: UNIT ( -- )
   s" nested/unit.f"
   S\" require ../prefix.f\nrequire child.f\npackage FILE-UNIT\npublic\n: VALUE ( -- n ) DELTA FILE-PREFIX:PLUS-BASE ;\n;package\ns\" explicit-unit-source-ran\" type cr\n" PUT ;

: DOWNSTREAM ( -- )
   s" downstream.f"
   S\" require nested/unit.f\npackage FILE-DOWNSTREAM\npublic\n: VALUE ( -- n ) FILE-UNIT:VALUE 1+ ;\n;package\n" PUT ;

: CHANGED-DOWNSTREAM ( -- )
   s" downstream.f"
   S\" require nested/unit.f\npackage FILE-DOWNSTREAM\npublic\n: VALUE ( -- n ) FILE-UNIT:VALUE 2 + ;\n;package\n" PUT ;

: BAD-DOWNSTREAM ( -- )
   s" downstream.f"
   S\" require nested/unit.f\npackage FILE-DOWNSTREAM\npublic\n: BAD ( -- ptr u8 ) FILE-UNIT:VALUE ;\n;package\n" PUT ;

: SHADOW-CHILD ( -- )
   s" nested/child.f"
   S\" package FILE-UNIT\npublic\n: DELTA ( -- n ) 34 ;\n;package\n" PUT ;

: CHANGED-DRIVER ( -- )
   s" tools/native-file-explicit.f" ROOT-AT
   S\" \n\\ The explicit file driver changed.\n" APPEND-FILE ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: CAPTURE ( ptr u8 n ptr u8 n -- n )
   {: exe:ptr exeu:n stdin:ptr stdinu:n :}
   ROOTED? @ if
      ALT? @ if ALT$ else ROOT$ then
   else
      EXTERNAL? @ if FOREIGN$ else ROOT$ then
   then {: cwd:ptr cwdu:n :}
   0 PROGRAM-U ! s" cwd: " PROGRAM CAP PROGRAM-U BUF-APPEND
   cwd cwdu PROGRAM CAP PROGRAM-U BUF-APPEND STR-LF PROGRAM CAP PROGRAM-U BUF-APPEND-C
   exe exeu stdin stdinu PROGRAM CAP PROGRAM-U PROC-ARGV-PROGRAM+
   exe exeu >LEN cwd cwdu >LEN stdin stdinu >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   PROGRAM PROGRAM-U @ OUT OUT-U @ ERR ERR-U @ rc SUBJECT:RC-TIMED-OUT
   rc ;

: SUCCESS ( n -- )
   dup 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   0 T= ;

: BUILD-OBJECT ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: mode:ptr modeu:n product:ptr productu:n object:ptr objectu:n :}
   PROC-ARGV-ENV-RESET
   EXTERNAL? @ if
      s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   then
   ENV-VALUE @ 0<> if
      SB-RESET ENV-VALUE @ SB-APPEND-C
      s" FILE_UNIT_ENV" >LEN SB$ >LEN PROC-ENV+
   then
   s" HABU_TASK_PREFIX_TWO" >LEN
   PREFIX-TWO? @ if s" 1" else s" " then >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   EXTERNAL? @ if s" tools/native-unit-build.f" ROOT-AT
   else s" tools/native-unit-build.f" then ARG
   s" --" ARG
   mode modeu ARG
   s" prefix.f" AT ARG
   s" nested/unit.f" AT ARG
   s" downstream.f" AT ARG
   object objectu AT ARG
   product productu AT ARG
   WHITEBOX-ARG? @ if s" whitebox" ARG then
   EXTERNAL? @ if s" bin/hb" ROOT-AT else WHITEBOX-ENGINE:PATH$ then
   s" " CAPTURE ;

: BUILD ( ptr u8 n ptr u8 n -- n )
   s" file.unit" BUILD-OBJECT ;

: RUN-IMPORTED ( ptr u8 n ptr u8 n -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   {: product:ptr productu:n expected:ptr expectu:n :}
   product productu AT s" FILE-DOWNSTREAM:VALUE ." CAPTURE SUCCESS
   OUT OUT-U @ expected expectu CONTAINS? TTRUE ;

: RUN-TASK ( ptr u8 n -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   AT s" FILE-TASK-CLIENT:LEAF@ . FILE-TASK-CLIENT:OWN@ ." CAPTURE SUCCESS
   OUT OUT-U @ S\" 111\n222\n" CONTAINS? TTRUE ;

: TASK-CASE ( -- )
   s" a FILE carries TASK reservations and keys its incoming watermark" T-LABEL
   0 PREFIX-TWO? !
   TASK-PREFIX TASK-UNIT TASK-DOWNSTREAM
   s" --export-file" s" task-export-hb" s" task.unit" BUILD-OBJECT SUCCESS
   OUT OUT-U @ s" task-unit-source-ran" CONTAINS? TTRUE
   s" task-export-hb" RUN-TASK
   s" --import-file" s" task-import-hb" s" task.unit" BUILD-OBJECT SUCCESS
   OUT OUT-U @ s" native-build: file unit hit" CONTAINS? TTRUE
   OUT OUT-U @ s" task-unit-source-ran" CONTAINS? TFALSE
   s" task-import-hb" RUN-TASK
   1 PREFIX-TWO? !
   s" --import-file" s" task-wrong-hb" s" task.unit" BUILD-OBJECT 74 T=
   OUT OUT-U @ s" native-build: uncaught throw code -8599" CONTAINS? TTRUE
   OUT OUT-U @ s" native-build: file unit hit" CONTAINS? TFALSE
   OUT OUT-U @ s" task-unit-source-ran" CONTAINS? TFALSE
   s" task-wrong-hb" AT EXISTS? TFALSE
   0 PREFIX-TWO? ! ;

: SAME-CODE? ( -- bool )
   s" export-hb" AT IMAGE-SIZE:MEASURE
   IMAGE-SIZE:CODE-BLOB-RANGE CODE-A-U ! CODE-A-O !
   s" import-hb" AT IMAGE-SIZE:MEASURE
   IMAGE-SIZE:CODE-BLOB-RANGE CODE-B-U ! CODE-B-O !
   CODE-A-U @ CODE-B-U @ <> if false exit then
   s" export-hb" AT FILE-SIZE {: asize:n :}
   asize IMAGE-A-RESERVE
   s" export-hb" AT 0 IMAGE-A BYTE-VIEW asize READ-ALL asize <> if E-FS-IO throw then
   s" import-hb" AT FILE-SIZE {: bsize:n :}
   bsize IMAGE-B-RESERVE
   s" import-hb" AT 0 IMAGE-B BYTE-VIEW bsize READ-ALL bsize <> if E-FS-IO throw then
   0 IMAGE-A BYTE-VIEW CODE-A-O @ BYTE+ CODE-A-U @
   0 IMAGE-B BYTE-VIEW CODE-B-O @ BYTE+ CODE-B-U @ STR= ;

: SAME-CUT? ( -- bool )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" export-hb" AT s" cp@ dbase@ - . ndict@ . wordlist . here data-base - ."
      CAPTURE SUCCESS
   OUT CUT OUT-U @ BYTE-COPY OUT-U @ CUT-U !
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" import-hb" AT s" cp@ dbase@ - . ndict@ . wordlist . here data-base - ."
      CAPTURE SUCCESS
   CUT CUT-U @ OUT OUT-U @ STR= ;

: IMPORT-OCCURRENCE ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" import-hb" AT
   S\" package FILE-OCC\nCAST: OCC-XT ( n -- [ -- n ] )\n$94000000 s\q FILE-UNIT:VALUE\q XREF-FIND DEF-OCC:SELECT DEF-OCC:CALLABLE OCC-XT execute .\n;package\n"
   CAPTURE SUCCESS
   OUT OUT-U @ S\" 42\n" CONTAINS? TTRUE ;

: REFUSE-TYPED-CLIENT ( -- )
   BAD-DOWNSTREAM
   s" --import-file" s" typed-hb" BUILD 74 T=
   OUT OUT-U @ s" native-build: file unit hit" CONTAINS? TTRUE
   OUT OUT-U @ s" explicit-unit-source-ran" CONTAINS? 0= TTRUE
   s" typed-hb" AT EXISTS? TFALSE
   DOWNSTREAM ;

: REFUSE-OTHER-ROOT ( -- )
   s" native-file-other-app" HB-TMP-MKDIR {: other:ptr otheru:n :}
   APP$ {: old:ptr oldu:n :}
   old ORIGINAL oldu BYTE-COPY oldu ORIGINAL-U !
   other APP otheru BYTE-COPY otheru APP-U !
   s" nested" AT MAKE-DIR
   PREFIX UNIT DOWNSTREAM
   s" file.unit" ORIGINAL-AT s" file.unit" AT COPY-FILE-STREAM
   s" --import-file" s" other-root-hb" BUILD 74 T=
   OUT OUT-U @ s" native-build: uncaught throw code -8599" CONTAINS? TTRUE
   s" other-root-hb" AT EXISTS? TFALSE
   ORIGINAL$ {: prior:ptr prioru:n :}
   prior APP prioru BYTE-COPY prioru APP-U ! ;

: REFUSE-WRONG-CLASS ( -- )
   0 WHITEBOX-ARG? !
   s" --import-file" s" sealed-hb" BUILD 74 T=
   OUT OUT-U @ s" explicit-unit-source-ran" CONTAINS? 0= TTRUE
   s" sealed-hb" AT EXISTS? TFALSE
   1 WHITEBOX-ARG? ! ;

: REFUSE-MALFORMED-OBJECT ( -- )
   s" a malformed explicit object never reaches the selected source" T-LABEL
   s" malformed.unit" AT s" bad object" WRITE-ALL
   s" --import-file" s" malformed-hb" s" malformed.unit" BUILD-OBJECT 74 T=
   OUT OUT-U @ s" native-build: file unit hit" CONTAINS? 0= TTRUE
   OUT OUT-U @ s" explicit-unit-source-ran" CONTAINS? 0= TTRUE
   s" malformed-hb" AT EXISTS? TFALSE ;

: MISSING-ORIGIN-KEY ( ptr u8 n -- )
   {: unit:ptr size:n :}
   unit size NUNIT-FILE:ARCH-AARCH64 s" FILE" unit 96 + 64
      NUNIT-FILE:READ-BYTES
   16 NUNIT-FILE:SECTION$ nip {: originu:n :}
   originu 48 < if s" FILE fixture has no origin header" 70 die then
   unit size originu - + {: origin:ptr :}
   origin LE:U64@ $4F524947494E3034 <> if
      s" FILE fixture has no origin rows" 70 die then
   origin 8 + LE:U64@ {: count:n :}
   48 ORIGIN-OFF !
   count 0 ?do
      originu ORIGIN-OFF @ - 72 < if
         s" FILE fixture has a truncated origin row" 70 die then
      origin ORIGIN-OFF @ + {: row:ptr :}
      row 16 + LE:U64@ {: pkgu:n :}
      row 32 + LE:U64@ {: tailu:n :}
      72 pkgu + tailu + row 40 + LE:U64@ +
         row 48 + LE:U64@ +
         {: rowu:n :}
      rowu originu ORIGIN-OFF @ - > if
         s" FILE fixture has an oversized origin row" 70 die then
      row 24 + LE:U64@ 2 =
      row 72 + pkgu s" FILE-UNIT" STR=CI and
      row 72 + pkgu + tailu s" VALUE" STR=CI and if
         [char] Q row 72 + pkgu + c!
         NUNIT-FILE:CLOSE unloop exit
      then
      rowu ORIGIN-OFF +!
   loop
   s" FILE fixture has no FILE-UNIT:VALUE origin key" 70 die ;

: REFUSE-MISSING-ORIGIN-KEY ( -- )
   s" a missing nominal origin key refuses the explicit FILE before publication" T-LABEL
   s" file.unit" AT FILE-SIZE {: size:n :}
   size IMAGE-A-RESERVE
   s" file.unit" AT 0 IMAGE-A BYTE-VIEW size READ-ALL size <> if
      E-FS-IO throw then
   size 128 < if E-NUNIT-FILE throw then
   0 IMAGE-A BYTE-VIEW {: outer:ptr :}
   outer 16 + LE:U64@ {: pathu:n :}
   outer 24 + LE:U64@ {: rootu:n :}
   pathu 0 <= rootu 0 < or pathu size 128 - > or if
      E-NUNIT-FILE throw then
   rootu size 128 pathu + - > if E-NUNIT-FILE throw then
   s" file.unit" AT
      outer 128 + pathu outer 128 pathu + + rootu
      outer 32 + 64 outer 96 + LE:U64@
      NFILE-OBJECT:TRY-OPEN {: h:n hit:bool :}
   hit 0= if E-NUNIT-FILE throw then
   h NFILE-OBJECT:NEXT
      {: kind:n end:n mode:n bytes:ptr payload:n more:bool :}
   end drop mode drop
   more 0= kind NFILE-OBJECT:RANGE-KIND <> or if E-NUNIT-FILE throw then
   bytes payload NFILE-OBJECT:SPLIT-RANGE
      {: unit:ptr unitu:n signature:ptr scope:ptr
         session:n module:n gen:n epoch:n before:n lease:n openlease:n
         openepoch:n beforemodule:n openmodulebefore:n openmoduleafter:n
         beforegen:n beforeepoch:n beforelease:n
         build:n beforebuild:n openbuildbefore:n openbuildafter:n :}
   unit unitu MISSING-ORIGIN-KEY
   outer 128 + pathu outer 128 pathu + + rootu
      outer 32 + 64 outer 96 + LE:U64@ NFILE-OBJECT:START
      {: changed:n :}
   changed end mode 1 and mode 2 and 2 / unit unitu signature scope
      session module gen epoch before lease openlease openepoch
      beforemodule openmodulebefore openmoduleafter beforegen beforeepoch
      beforelease build beforebuild openbuildbefore openbuildafter
      NFILE-OBJECT:ADD-RANGE
   h NFILE-OBJECT:INPUTS$ {: inputs:ptr inputu:n :}
   changed s" missing-key.unit" AT inputs inputu NFILE-OBJECT:WRITE
   NFILE-OBJECT:CLOSE
   s" --import-file" s" missing-key-hb" s" missing-key.unit"
      BUILD-OBJECT 74 T=
   OUT OUT-U @ s" native-build: uncaught throw code -8599" CONTAINS? TTRUE
   OUT OUT-U @ s" explicit-unit-source-ran" CONTAINS? 0= TTRUE
   s" missing-key-hb" AT EXISTS? TFALSE ;

: LEASE-FILES ( -- )
   s" prefix.f"
   S\" package REVIEW-LEASE\npublic\n1 TYPED-BUFFER SAVED NLEASE:lease\n: SAVE ( -- ) [: 0 SAVED ! ;] NLEASE:WITH ;\n: PROBE ( -- ) 32 0 ?do [: drop 0 SAVED @ NLEASE:LIVE? if 73 throw then ;] NLEASE:WITH loop ;\n;package\n" PUT
   s" nested/unit.f" S\" REVIEW-LEASE:SAVE\ns\" lease-source-ran\" type cr\n" PUT
   s" downstream.f"
   S\" s\" lease-probe=\" type ' REVIEW-LEASE:PROBE catch . cr\n" PUT ;

: LEASE-CASE ( -- )
   LEASE-FILES
   s" --export-file" s" lease-export-hb" BUILD SUCCESS
   OUT OUT-U @ s" lease-probe=0" CONTAINS? TTRUE
   OUT OUT-U @ s" lease-source-ran" CONTAINS? TTRUE
   s" --import-file" s" lease-import-hb" BUILD SUCCESS
   OUT OUT-U @ s" native-build: file unit hit" CONTAINS? TTRUE
   OUT OUT-U @ s" lease-source-ran" CONTAINS? 0= TTRUE
   OUT OUT-U @ s" lease-probe=0" CONTAINS? TTRUE ;

: SCOPE-FILES ( -- )
   s" prefix.f" s" " PUT
   s" nested/unit.f"
   S\" package REVIEW-SCOPE\npublic\n: VALUE ( -- n ) 41 ;\n" PUT
   s" downstream.f"
   S\" : RESULT ( -- n ) VALUE 1+ ;\n;package\ns\" scope=\" type REVIEW-SCOPE:RESULT . cr\n" PUT ;

: SCOPE-CASE ( -- )
   SCOPE-FILES
   s" --export-file" s" scope-export-hb" BUILD SUCCESS
   OUT OUT-U @ s" scope=42" CONTAINS? TTRUE
   s" --import-file" s" scope-import-hb" BUILD SUCCESS
   OUT OUT-U @ s" native-build: file unit hit" CONTAINS? TTRUE
   OUT OUT-U @ s" scope=42" CONTAINS? TTRUE ;

: BUILDER-FILES ( -- )
   s" prefix.f"
   S\" package REVIEW-BUILDER\npublic\n1 TYPED-BUFFER SAVED IR-BUILD:builder\n: NEW ( IR-CTX:ctx -- IR-BUILD:builder ) IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT HIR:NEW-BUILDER ;\n: SAVE ( -- ) NABI:BINDING [: NEW dup 0 SAVED ! IR-BUILD:ABORT ;] IR-CTX:WITH-CONTEXT ;\n: PROBE ( -- ) 128 0 ?do NABI:BINDING [: NEW 0 SAVED @ IR-BUILD:LIVE? if 73 throw then IR-BUILD:ABORT ;] IR-CTX:WITH-CONTEXT loop ;\n;package\n" PUT
   s" nested/unit.f" S\" REVIEW-BUILDER:SAVE\ns\" builder-source-ran\" type cr\n" PUT
   s" downstream.f"
   S\" s\" builder-probe=\" type ' REVIEW-BUILDER:PROBE catch . cr\n" PUT ;

: BUILDER-CASE ( -- )
   BUILDER-FILES
   s" --export-file" s" builder-export-hb" BUILD SUCCESS
   OUT OUT-U @ s" builder-probe=0" CONTAINS? TTRUE
   OUT OUT-U @ s" builder-source-ran" CONTAINS? TTRUE
   s" --import-file" s" builder-import-hb" BUILD SUCCESS
   OUT OUT-U @ s" native-build: file unit hit" CONTAINS? TTRUE
   OUT OUT-U @ s" builder-source-ran" CONTAINS? 0= TTRUE
   OUT OUT-U @ s" builder-probe=0" CONTAINS? TTRUE ;

: EXTERNAL-DOWNSTREAM ( -- )
   s" downstream.f"
   S\" require nested/unit.f\nrequire src/lib/json-read.f\npackage FILE-DOWNSTREAM\npublic\n: VALUE ( -- n ) FILE-UNIT:VALUE JR:STORAGE-BYTES drop 1+ ;\n;package\n" PUT ;

: EXTERNAL-CASE ( -- )
   s" bin" ROOT-AT MAKE-DIR
   WHITEBOX-ENGINE:PATH$ s" bin/hb" ROOT-AT COPY-FILE-STREAM
   s" bin/hb" ROOT-AT CHMOD-X
   s" cache" AT MAKE-DIR
   EXTERNAL-DOWNSTREAM
   1 EXTERNAL? !
   s" --export-file" s" external-export-hb" s" external.unit" BUILD-OBJECT SUCCESS
   OUT OUT-U @ s" explicit-unit-source-ran" CONTAINS? TTRUE
   s" --import-file" s" external-import-hb" s" external.unit" BUILD-OBJECT SUCCESS
   OUT OUT-U @ s" native-build: file unit hit" CONTAINS? TTRUE
   OUT OUT-U @ s" explicit-unit-source-ran" CONTAINS? 0= TTRUE
   s" external-import-hb" s" 43" RUN-IMPORTED
   0 EXTERNAL? !
   DOWNSTREAM ;

: ROOTED-DOWNSTREAM ( -- )
   s" downstream.f"
   S\" require nested/unit.f\npackage FILE-DOWNSTREAM\npublic\n: VALUE ( -- n ) FILE-UNIT:VALUE 1+ ;\n;package\ns\" driver-root=\" type SOURCE-ROOT:ENGINE$ type cr\n" PUT ;

: ROOT-MARKED? ( ptr u8 n -- bool )
   SB-RESET s" driver-root=" SB-APPEND SB-APPEND
   OUT OUT-U @ SB$ CONTAINS? ;

: ROOTED-CASE ( -- )
   s" native-file-alt" HB-TMP-MKDIR {: alt:ptr altu:n :}
   alt ALT altu BYTE-COPY altu ALT-U !
   s" src" ROOT-AT s" src" ALT-AT MAKE-SYMLINK
   s" tools" ROOT-AT s" tools" ALT-AT MAKE-SYMLINK
   s" child.f" ROOT-AT s" child.f" ALT-AT MAKE-SYMLINK
   ROOTED-DOWNSTREAM
   1 EXTERNAL? ! 1 ROOTED? !
   s" --export-file" s" rooted-a-hb" s" rooted-a.unit" BUILD-OBJECT SUCCESS
   ROOT$ ROOT-MARKED? TTRUE
   1 ALT? !
   s" --export-file" s" rooted-b-hb" s" rooted-b.unit" BUILD-OBJECT SUCCESS
   ALT$ ROOT-MARKED? TTRUE
   0 ALT? ! 0 ROOTED? ! 0 EXTERNAL? !
   DOWNSTREAM ;

: FOREIGN-SESSION-CASE ( -- )
   s" prefix.f"
   S\" package REVIEW-FOREIGN\npublic\n: PING ( -- n ) 1 ;\n: OPEN-FOREIGN ( -- ) NABI:BINDING IR-CTX:SESSION-OPEN drop ;\n;package\nIR-CTX:SESSION-CLOSE\nREVIEW-FOREIGN:OPEN-FOREIGN\ns\" foreign-prefix-ready\" type cr\n" PUT
   s" nested/unit.f" S\" s\" foreign-unit-ran\" type cr\n" PUT
   s" downstream.f" s" " PUT
   s" foreign.unit" AT EXISTS? TFALSE
   s" --export-file" s" foreign-hb" s" foreign.unit" BUILD-OBJECT 74 T=
   OUT OUT-U @ s" foreign-prefix-ready" CONTAINS? TTRUE
   OUT OUT-U @ s" uncaught throw code -8597" CONTAINS? TTRUE
   OUT OUT-U @ s" foreign-unit-ran" CONTAINS? TFALSE
   s" foreign.unit" AT EXISTS? TFALSE ;

: CWORK-CASE ( -- )
   s" prefix.f" s" " PUT
   s" nested/unit.f"
   S\" package CWORK-EXPLICIT public\n-1 CWORK-VARIABLE VALUE n\n;package\ns\" cwork-source-ran\" type cr\n" PUT
   s" downstream.f" s" " PUT
   s" cwork.unit" AT EXISTS? TFALSE
   s" --export-file" s" cwork-hb" s" cwork.unit" BUILD-OBJECT 74 T=
   OUT OUT-U @ s" cwork-source-ran" CONTAINS? TTRUE
   OUT OUT-U @ s" uncaught throw code -8597" CONTAINS? TTRUE
   s" cwork.unit" AT EXISTS? TFALSE
   s" cwork-hb" AT EXISTS? TFALSE ;

: CWORK-DEFAULT-CASE ( -- )
   s" prefix.f"
   S\" package CWORK-EXPLICIT public\n0 CWORK-VARIABLE VALUE n\n;package\n" PUT
   s" nested/unit.f"
   S\" CWORK:DECLARED-BYTES CELL - -1 CWORK:DEFAULT\ns\" cwork-default-source-ran\" type cr\n" PUT
   s" downstream.f" s" " PUT
   s" default.unit" AT EXISTS? TFALSE
   s" --export-file" s" default-hb" s" default.unit" BUILD-OBJECT 74 T=
   OUT OUT-U @ s" cwork-default-source-ran" CONTAINS? TTRUE
   OUT OUT-U @ s" uncaught throw code -8597" CONTAINS? TTRUE
   s" default.unit" AT EXISTS? TFALSE
   s" default-hb" AT EXISTS? TFALSE ;

: ENV-CASE ( -- )
   s" explicit file imports check observed environment values" T-LABEL
   PREFIX
   s" nested/unit.f"
   S\" package FILE-UNIT\npublic\ns\" FILE_UNIT_ENV\" GETENV drop c@ constant VALUE\n;package\n" PUT
   s" downstream.f"
   S\" require nested/unit.f\npackage FILE-DOWNSTREAM\npublic\n: VALUE ( -- n ) FILE-UNIT:VALUE ;\n;package\n" PUT
   65 ENV-VALUE !
   s" --export-file" s" env-export-hb" s" env.unit" BUILD-OBJECT SUCCESS
   s" --import-file" s" env-import-hb" s" env.unit" BUILD-OBJECT SUCCESS
   s" env-export-hb" s" 65" RUN-IMPORTED
   s" env-import-hb" s" 65" RUN-IMPORTED
   66 ENV-VALUE !
   s" --import-file" s" env-changed-hb" s" env.unit" BUILD-OBJECT 74 T=
   OUT OUT-U @ s" native-build: uncaught throw code -8597" CONTAINS? TTRUE
   s" env-changed-hb" AT EXISTS? TFALSE
   0 ENV-VALUE ! ;

public
: MAIN ( -- )
   T-RESET
   s" explicit file unit through the native build loader" T-LABEL
   s" native-file-unit" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ TREE-COPY:BUILD-SOURCES
   s" native-file-app" HB-TMP-MKDIR {: app:ptr appu:n :}
   app APP appu BYTE-COPY appu APP-U !
   \ The absolute unit is outside cwd, so ../prefix.f uses its nested root.
   s" native-file-cwd" HB-TMP-MKDIR {: foreign:ptr foreignu:n :}
   foreign FOREIGN foreignu BYTE-COPY foreignu FOREIGN-U !
   1 WHITEBOX-ARG? !
   \ The fixture builder stages process arguments of its own on a cold run.
   WHITEBOX-ENGINE:PATH$ 2drop
   s" nested" AT MAKE-DIR
   PREFIX CHILD UNIT DOWNSTREAM
   s" --export-file" s" export-hb" BUILD SUCCESS
   OUT OUT-U @ s" explicit-unit-source-ran" CONTAINS? TTRUE
   s" --import-file" s" import-hb" BUILD SUCCESS
   OUT OUT-U @ s" native-build: file unit hit" CONTAINS? TTRUE
   OUT OUT-U @ s" explicit-unit-source-ran" CONTAINS? 0= TTRUE
   \ Compare the executed source and imported products. Their full REPL images
   \ may retain different inactive compiler scratch after a skipped load.
   s" export-hb" s" 43" RUN-IMPORTED
   s" import-hb" s" 43" RUN-IMPORTED
   SAME-CODE? TTRUE
   SAME-CUT? TTRUE
   IMPORT-OCCURRENCE
   EXTERNAL-CASE
   ROOTED-CASE
   REFUSE-TYPED-CLIENT
   REFUSE-OTHER-ROOT
   REFUSE-WRONG-CLASS
   REFUSE-MALFORMED-OBJECT
   REFUSE-MISSING-ORIGIN-KEY
   CHANGED-DOWNSTREAM
   s" --import-file" s" downstream-hb" BUILD SUCCESS
   s" downstream-hb" s" 44" RUN-IMPORTED
   CHANGED-CHILD
   s" --import-file" s" changed-child-hb" BUILD 74 T=
   s" changed-child-hb" AT EXISTS? TFALSE
   CHILD SHADOW-CHILD
   s" --import-file" s" shadow-child-hb" BUILD 74 T=
   s" shadow-child-hb" AT EXISTS? TFALSE
   CHANGED-DRIVER
   s" --import-file" s" compiler-hb" BUILD 74 T=
   OUT OUT-U @ s" native-build: uncaught throw code -8599" CONTAINS? TTRUE
   OUT OUT-U @ s" explicit-unit-source-ran" CONTAINS? 0= TTRUE
   s" compiler-hb" AT EXISTS? TFALSE
   CHANGED-PREFIX
   s" --import-file" s" changed-hb" BUILD 74 T=
   s" changed-hb" AT EXISTS? TFALSE
   LEASE-CASE
   SCOPE-CASE
   BUILDER-CASE
   FOREIGN-SESSION-CASE
   CWORK-CASE
   CWORK-DEFAULT-CASE
   TASK-CASE
   ENV-CASE
   T-REPORT
   s" native file unit artifacts: " type APP$ type cr ;
;package

NATIVE-FILE-UNIT-TEST:MAIN
