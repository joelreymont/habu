\ A stripped build keeps bytes no span declares as bytes, and checks a declared
\ cell whatever window it lies in.
\ The first subject lays two ERROR:CODE rows whose text is the 8 address bytes
\ of a block it allocates at load and keeps, so the builder has that address
\ mapped wherever it lands. Each row starts aligned and its header is whole
\ cells, so the 9-byte name E-SHIFTED puts the address in an undeclared window
\ that is not cell-aligned and the 8-byte name E-ALIGN8 puts it in an aligned
\ undeclared window. Bytes are not an address by type, so the build
\ succeeds and the application runs and exits 0. MAIN does not read the rows;
\ the build keeps them all the same.
\ The second subject stores a dictionary-record address in a declared DATA
\ cell laid with `allot`. The record lies outside hb's data and the build's
\ space, so the linker refuses the file's object, which cannot name it.
\ The third subject is a file and a file it requires, each reading the loader at
\ top level: SOURCE-ROOT:ACTIVE-SOURCE, CURRENT$, and CURRENT$ inside a WITH;
\ the required file then sets its root with SOURCE-ROOT:ROOT!, which its load's
\ end undoes. Built through tools/hb-build.f, the file's `SOURCE-ROOT:` words
\ are the build's copy of src/core/include.f and the loads are hb's; both reach
\ the one record the process names, so the build prints the lines `hb --load`
\ prints. Run as `hb probe.f`, the program file is a loaded file to the loader
\ and prints them too. A file whose word only resolves its own path loads
\ nothing: its build prints the directory `hb --load` prints.
\ The fourth subject's top level reads an environment variable: a build reuses
\ its object only while the variable reads as its compile read it.
\ The fifth subject's top level sets its root with ROOT! and requires a file only
\ that root reaches; a second build into the same out root reuses its object, so
\ it runs none of the file's top-level code. That file then requires a second
\ file only its root reaches, which the first required file loaded already, so
\ the requiring file's object holds a registry act made under the set root, not
\ a request; the rebuild reuses it all the same. A third build after the first
\ required file changes prints its new line.
\ Two more files set their root and require a helper: under a root through a
\ symlink the second build finds repointed, and by its absolute path after
\ setting the longest root ROOT! takes. Each builds twice into one out root.
\ A program reading data that a library lays between two dead variables has
\ the bytes of the one built from the library without them.
require test/gate-common.f
require src/lib/engine-candidate.f
require src/lib/test/runner.f
require src/lib/process.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f

package AOT-SHIFTED-BYTES-TEST
private


create SUBJECT PATH-CAP allot
create IMAGE PATH-CAP allot
create RECORD PATH-CAP allot
create REFUSED PATH-CAP allot
create PROBE PATH-CAP allot
create PROBE-ROOT PATH-CAP allot
create PROBE-INNER PATH-CAP allot
create PROBE-OUT PATH-CAP allot
create PROBE-IMAGE PATH-CAP allot
create RESOLVES PATH-CAP allot
create RESOLVES-OUT PATH-CAP allot
create RESOLVES-IMAGE PATH-CAP allot
create ENVTOP PATH-CAP allot
create ENVTOP-OUT PATH-CAP allot
create ENVTOP-IMAGE PATH-CAP allot
create ROOTED PATH-CAP allot
create ROOTED-DIR PATH-CAP allot
create ROOTED-HELPER PATH-CAP allot
create ROOTED-SHARED PATH-CAP allot
create ROOTED-OUT PATH-CAP allot
create ROOTED-IMAGE PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U
variable RECORD-U
variable REFUSED-U
variable PROBE-U
variable PROBE-ROOT-U
variable PROBE-INNER-U
variable PROBE-OUT-U
variable PROBE-IMAGE-U
variable RESOLVES-U
variable RESOLVES-OUT-U
variable RESOLVES-IMAGE-U
variable ENVTOP-U
variable ENVTOP-OUT-U
variable ENVTOP-IMAGE-U
variable ROOTED-U
variable ROOTED-DIR-U
variable ROOTED-HELPER-U
variable ROOTED-SHARED-U
variable ROOTED-OUT-U
variable ROOTED-IMAGE-U
\ what `hb --load` printed, which the build must print too
4096 constant LOADED-CAP
create LOADED LOADED-CAP allot
variable LOADED-U

: SUBJECT$ ( -- str ) SUBJECT SUBJECT-U @ >str ;
: IMAGE$ ( -- str ) IMAGE IMAGE-U @ >str ;
: RECORD$ ( -- str ) RECORD RECORD-U @ >str ;
: REFUSED$ ( -- str ) REFUSED REFUSED-U @ >str ;
: PROBE$ ( -- str ) PROBE PROBE-U @ >str ;
: PROBE-ROOT$ ( -- str ) PROBE-ROOT PROBE-ROOT-U @ >str ;
: PROBE-INNER$ ( -- str ) PROBE-INNER PROBE-INNER-U @ >str ;
: PROBE-OUT$ ( -- str ) PROBE-OUT PROBE-OUT-U @ >str ;
: PROBE-IMAGE$ ( -- str ) PROBE-IMAGE PROBE-IMAGE-U @ >str ;
: RESOLVES$ ( -- str ) RESOLVES RESOLVES-U @ >str ;
: RESOLVES-OUT$ ( -- str ) RESOLVES-OUT RESOLVES-OUT-U @ >str ;
: RESOLVES-IMAGE$ ( -- str ) RESOLVES-IMAGE RESOLVES-IMAGE-U @ >str ;
: LOADED$ ( -- str ) LOADED LOADED-U @ >str ;
: ENVTOP$ ( -- str ) ENVTOP ENVTOP-U @ >str ;
: ENVTOP-OUT$ ( -- str ) ENVTOP-OUT ENVTOP-OUT-U @ >str ;
: ENVTOP-IMAGE$ ( -- str ) ENVTOP-IMAGE ENVTOP-IMAGE-U @ >str ;
: ROOTED$ ( -- str ) ROOTED ROOTED-U @ >str ;
: ROOTED-DIR$ ( -- str ) ROOTED-DIR ROOTED-DIR-U @ >str ;
: ROOTED-HELPER$ ( -- str ) ROOTED-HELPER ROOTED-HELPER-U @ >str ;
: ROOTED-SHARED$ ( -- str ) ROOTED-SHARED ROOTED-SHARED-U @ >str ;
: ROOTED-OUT$ ( -- str ) ROOTED-OUT ROOTED-OUT-U @ >str ;
: ROOTED-IMAGE$ ( -- str ) ROOTED-IMAGE ROOTED-IMAGE-U @ >str ;
\ a line the probe's --load output must hold, naming its canonical directory
create WANT PATH-CAP 2 * allot
variable WANT-U
: WANT$ ( -- str ) WANT WANT-U @ >str ;

\ The text is the kept block's own first 8 bytes, so no image byte but the
\ rows' holds the address. KEPT is a PTR-VARIABLE, which a save writes as null.
: SOURCE$ ( -- str )
   S\" require src/lib/error.f\nrequire src/lib/memory.f\nPTR-VARIABLE KEPT\n64 MEM-ALLOC-BYTES drop KEPT !\n: LAY ( -- ) KEPT @ NULL-PTR - {: a:n :} 8 0 do a i 8 * rshift $FF and KEPT @ i + c! loop ;\nLAY\n-110001 KEPT @ 8 >str ERROR:CODE E-SHIFTED\n-110002 KEPT @ 8 >str ERROR:CODE E-ALIGN8\n: MAIN ( -- ) s\q ok\q type cr ;\n" ;

: RECORD-SOURCE$ ( -- str )
   S\" require src/habu/xref.f\n1 PERSISTED-PTR-U8-TABLE T\n0 XREF-REC BYTE-VIEW T 0 ptr-field !\n: MAIN ( -- ) s\q ok\q type cr ;\n" ;

\ WITH takes /tmp absolute: a relative root resolves against the working
\ directory, not the current root.
: PROBE-SOURCE$ ( -- str )
   S\" require sub/inner.f\nSOURCE-ROOT:ACTIVE-SOURCE s\q probe bytes \q type str> . drop s\q probe root \q type type cr s\q probe path \q type type cr\ns\q probe current \q type SOURCE-ROOT:CURRENT$ type cr\n: PROBE-WITH ( -- ) s\q probe with \q type SOURCE-ROOT:CURRENT$ type cr ;\ns\q /tmp\q ' PROBE-WITH SOURCE-ROOT:WITH\n: MAIN ( -- ) ;\n" ;

: INNER-SOURCE$ ( -- str )
   S\" SOURCE-ROOT:ACTIVE-SOURCE s\q inner bytes \q type str> . drop s\q inner root \q type type cr s\q inner path \q type type cr\ns\q inner current \q type SOURCE-ROOT:CURRENT$ type cr\n: INNER-WITH ( -- ) s\q inner with \q type SOURCE-ROOT:CURRENT$ type cr ;\ns\q /tmp\q ' INNER-WITH SOURCE-ROOT:WITH\ns\q sub/root\q SOURCE-ROOT:ROOT!\ns\q inner root! \q type SOURCE-ROOT:CURRENT$ type cr\n" ;

: RESOLVES-SOURCE$ ( -- str )
   S\" : RESOLVES-DIR ( -- ) s\q resolves.f\q SOURCE-ROOT:RESOLVE drop SOURCE-ROOT:DIRNAME s\q resolves dir \q type type cr ;\nRESOLVES-DIR\n: MAIN ( -- ) ;\n" ;

: ENVTOP-SOURCE$ ( -- str )
   S\" s\q envtop top level\q type cr\ns\q HABU_TEST_SIDE\q GETENV str> nip constant SIDE\n: MAIN ( -- ) SIDE . ;\n" ;

: ROOTED-SOURCE$ ( -- str )
   S\" s\q rooted top level\q type cr\ns\q beside\q SOURCE-ROOT:ROOT!\nrequire helper.f\nrequire shared.f\n: MAIN ( -- ) s\q rooted main \q type BESIDE-WORD type s\q  and \q type SHARED-WORD type cr ;\n" ;

: HELPER-SOURCE$ ( -- str )
   S\" require shared.f\n: BESIDE-WORD ( -- str ) s\q from beside\q ;\n" ;

: HELPER-CHANGED-SOURCE$ ( -- str )
   S\" require shared.f\n: BESIDE-WORD ( -- str ) s\q from beside, changed\q ;\n" ;

: SHARED-SOURCE$ ( -- str )
   S\" : SHARED-WORD ( -- str ) s\q shared\q ;\n" ;

: HELPER-1$ ( -- str ) S\" : HELPER-K ( -- n ) 1 ;\n" ;
: HELPER-2$ ( -- str ) S\" : HELPER-K ( -- n ) 2 ;\n" ;

\ Each subject is its own hb-build: a refusal ends the build.
: BUILD-ARGV ( str str -- ) {: src:str out:str :}
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+ src GE-ARG+
   s" -o" GE-ARG+ out GE-ARG+ ;

: BUILD-RUN ( -- )
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER GE-RUN-ENV ;

: HB-BUILD ( str str -- )
   GE-HB-RESET BUILD-ARGV BUILD-RUN ;

: BUILD ( -- )
   SUBJECT$ SOURCE$ FS:WRITE-ALL
   SUBJECT$ IMAGE$ HB-BUILD
   s" stripped build of shifted and aligned address bytes" GE-EXPECT-OK ;

: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ PROCESS:WAIT-FOREVER GE-RUN-ENV
   s" the stripped application" GE-EXPECT-OK
   S\" ok\n" s" the stripped application's output" GE-EXPECT-OUT ;

: BUILD-RECORD ( -- )
   RECORD$ RECORD-SOURCE$ FS:WRITE-ALL
   RECORD$ REFUSED$ HB-BUILD
   s" stripped build of a record pointer in a declared DATA cell" GE-EXPECT-NONZERO
   s" outside hb's data and the build's space" s" the record-pointer rejection's found"
   GE-EXPECT-ERR-HAS
   s" or clear the cell before the file ends" s" the record-pointer rejection's fix"
   GE-EXPECT-ERR-HAS ;

: PROBE-WRITE ( -- )
   PROBE-ROOT$ FS:MAKE-DIRS
   PROBE-INNER$ INNER-SOURCE$ FS:WRITE-ALL
   PROBE$ PROBE-SOURCE$ FS:WRITE-ALL ;

: WANT+ ( str -- ) {: s:str :}
   s STR:DATA WANT WANT-U @ + s STR:LEN BYTE-COPY  s STR:LEN WANT-U +! ;

: WANT! ( str str -- ) {: label:str tail:str :}
   0 WANT-U !  label WANT+
   PROBE$ SOURCE-ROOT:DIRNAME SOURCE-ROOT:CANONICAL drop WANT+
   tail WANT+  S\" \n" WANT+ ;

: PROBE-LOAD ( -- )
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ PROBE$ GE-ARG+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER GE-RUN-ENV
   s" hb --load of a file whose loads read the loader" GE-EXPECT-OK
   GT-OUT$ {: out:str :}
   out STR:LEN LOADED-CAP > if s" the probe's --load output fits its buffer" GE-FAIL then
   out STR:DATA LOADED out STR:LEN BYTE-COPY  out STR:LEN LOADED-U !
   s" inner root! " s" /sub/root" WANT!
   WANT$ s" a required file's ROOT! makes its directory the current root" GE-EXPECT-OUT-HAS
   s" probe current " s" " WANT!
   WANT$ s" the requiring file's root is back after the required file's ROOT!" GE-EXPECT-OUT-HAS ;

: PROBE-PLAIN ( -- )
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   PROBE$ GE-ARG+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER GE-RUN-ENV
   s" hb probe.f, a program file whose loads read the loader" GE-EXPECT-OK
   LOADED$ s" the loader's lines under hb probe.f, as under hb --load" GE-EXPECT-OUT-HAS ;

\ The build caches objects under its out root, and a cached probe runs none of
\ its top-level code, so each run builds under its own.
: PROBE-BUILD ( -- )
   GE-HB-RESET
   s" HABU_OUT" PROBE-OUT$ PROC-ENV-SET
   PROBE$ PROBE-IMAGE$ BUILD-ARGV BUILD-RUN
   s" stripped build of a file whose loads read the loader" GE-EXPECT-OK
   LOADED$ s" the loader's lines in the build, as under hb --load" GE-EXPECT-OUT-HAS ;

: RESOLVES-LOAD ( -- )
   RESOLVES$ RESOLVES-SOURCE$ FS:WRITE-ALL
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ RESOLVES$ GE-ARG+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER GE-RUN-ENV
   s" hb --load of a file whose word resolves its own path" GE-EXPECT-OK
   s" resolves dir " s" " WANT!
   WANT$ s" the directory the word resolves under hb --load" GE-EXPECT-OUT-HAS ;

\ Its own out root: a cached object runs no top-level code. RESOLVES-LOAD left
\ in WANT the line hb --load printed.
: RESOLVES-BUILD ( -- )
   GE-HB-RESET
   s" HABU_OUT" RESOLVES-OUT$ PROC-ENV-SET
   RESOLVES$ RESOLVES-IMAGE$ BUILD-ARGV BUILD-RUN
   s" stripped build of a file whose word resolves its own path" GE-EXPECT-OK
   WANT$ s" the directory the word resolves in the build, as under hb --load" GE-EXPECT-OUT-HAS ;

\ A build into ENVTOP-OUT with HABU_TEST_SIDE the value, unset for NULL-PTR,
\ that compiles the top level when fresh is true, then a run printing want.
: ENVTOP-BUILD ( str bool str -- )
   {: val:str fresh:bool want:str :}
   GE-HB-RESET
   s" HABU_OUT" ENVTOP-OUT$ PROC-ENV-SET
   val STR:DATA NULL-PTR <> if s" HABU_TEST_SIDE" val PROC-ENV-SET then
   ENVTOP$ ENVTOP-IMAGE$ BUILD-ARGV BUILD-RUN
   s" build of a file whose top level reads HABU_TEST_SIDE" GE-EXPECT-OK
   GT-OUT$ s" envtop top level" STR:CONTAINS? {: seen:bool :}
   fresh if seen 0= else seen then if
      s" the build compiles the top level exactly when HABU_TEST_SIDE changed" GE-FAIL
   then
   GE-HB-RESET
   ENVTOP-IMAGE$ GE-ARGV+
   ENVTOP-IMAGE$ PROCESS:WAIT-FOREVER GE-RUN-ENV
   s" the program built from envtop.f" GE-EXPECT-OK
   want s" the length of HABU_TEST_SIDE the program's compile read" GE-EXPECT-OUT ;

: ENVTOP-BUILDS ( -- )
   ENVTOP$ ENVTOP-SOURCE$ FS:WRITE-ALL
   s" ab" true S\" 2\n" ENVTOP-BUILD
   s" abcd" true S\" 4\n" ENVTOP-BUILD
   s" abcd" false S\" 4\n" ENVTOP-BUILD
   NULL-PTR 0 >str true S\" 0\n" ENVTOP-BUILD ;

: EXPECT-OUT-LACKS ( str str -- ) {: want:str label:str :}
   GT-OUT$ want STR:CONTAINS? if label GE-FAIL then ;

: ROOTED-WRITE ( -- )
   ROOTED-DIR$ FS:MAKE-DIRS
   ROOTED-HELPER$ HELPER-SOURCE$ FS:WRITE-ALL
   ROOTED-SHARED$ SHARED-SOURCE$ FS:WRITE-ALL
   ROOTED$ ROOTED-SOURCE$ FS:WRITE-ALL ;

: ROOTED-RUN ( str -- ) {: want:str :}
   GE-HB-RESET
   ROOTED-IMAGE$ GE-ARGV+
   ROOTED-IMAGE$ PROCESS:WAIT-FOREVER GE-RUN-ENV
   s" the program built from a file whose ROOT! reaches its require" GE-EXPECT-OK
   want s" MAIN's line, from the words the ROOT!-reached files define" GE-EXPECT-OUT ;

: ROOTED-LINE$ ( -- str ) S\" rooted main from beside and shared\n" ;

: ROOTED-BUILD ( -- )
   GE-HB-RESET
   s" HABU_OUT" ROOTED-OUT$ PROC-ENV-SET
   ROOTED$ ROOTED-IMAGE$ BUILD-ARGV BUILD-RUN
   s" stripped build of a file that requires a file only its ROOT! reaches" GE-EXPECT-OK
   s" rooted top level" s" the first build runs the file's top level" GE-EXPECT-OUT-HAS
   ROOTED-LINE$ ROOTED-RUN ;

: ROOTED-REBUILD ( -- )
   GE-HB-RESET
   s" HABU_OUT" ROOTED-OUT$ PROC-ENV-SET
   ROOTED$ ROOTED-IMAGE$ BUILD-ARGV BUILD-RUN
   s" second build of it into the same out root" GE-EXPECT-OK
   s" rooted top level" s" the second build reuses the cached object, running no top-level code" EXPECT-OUT-LACKS
   ROOTED-LINE$ ROOTED-RUN ;

: ROOTED-CHANGED ( -- )
   ROOTED-HELPER$ HELPER-CHANGED-SOURCE$ FS:WRITE-ALL
   GE-HB-RESET
   s" HABU_OUT" ROOTED-OUT$ PROC-ENV-SET
   ROOTED$ ROOTED-IMAGE$ BUILD-ARGV BUILD-RUN
   s" third build of it into the same out root, after its required file changed" GE-EXPECT-OK
   S\" rooted main from beside, changed and shared\n" ROOTED-RUN ;

\ Each build and run is its own process: the out root and the file tree between builds are the boundary under test.
create C-DIR PATH-CAP allot
create C-MAIN PATH-CAP allot
create C-OUT PATH-CAP allot
create C-IMAGE PATH-CAP allot
create C-A PATH-CAP allot
create C-B PATH-CAP allot
create C-H PATH-CAP allot
create C-LINK PATH-CAP allot
create C-REL PATH-CAP allot
variable C-DIR-U
variable C-MAIN-U
variable C-OUT-U
variable C-IMAGE-U
variable C-A-U
variable C-B-U
variable C-H-U
variable C-LINK-U
variable C-REL-U
: C-DIR$ ( -- str ) C-DIR C-DIR-U @ >str ;
: C-MAIN$ ( -- str ) C-MAIN C-MAIN-U @ >str ;
: C-OUT$ ( -- str ) C-OUT C-OUT-U @ >str ;
: C-IMAGE$ ( -- str ) C-IMAGE C-IMAGE-U @ >str ;
: C-A$ ( -- str ) C-A C-A-U @ >str ;
: C-H$ ( -- str ) C-H C-H-U @ >str ;
: C-LINK$ ( -- str ) C-LINK C-LINK-U @ >str ;
: C-REL$ ( -- str ) C-REL C-REL-U @ >str ;

\ main.f's text: a root and a helper's path, each up to PATH-CAP, and the code around them
PATH-CAP 2 * 256 + constant TXT-CAP
create TXT TXT-CAP allot
variable TXT-U
: TXT$ ( -- str ) TXT TXT-U @ >str ;
64 constant NAME-CAP
create C-NAME NAME-CAP allot
variable C-NAME-U
256 constant LBL-CAP
create LBL LBL-CAP allot
variable LBL-U

: APPEND ( str ^u8 ^n n -- ) {: s:str dst:^ up:^ cap:n :}
   up @ s STR:LEN + cap > if s" a case's text fits its buffer" GE-FAIL then
   s STR:DATA dst up @ + s STR:LEN BYTE-COPY  s STR:LEN up +! ;

: TXT+ ( str -- ) TXT TXT-U TXT-CAP APPEND ;

: LABEL ( str -- str ) {: s:str :}
   0 LBL-U !  C-NAME C-NAME-U @ >str LBL LBL-U LBL-CAP APPEND
   s" : " LBL LBL-U LBL-CAP APPEND  s LBL LBL-U LBL-CAP APPEND
   LBL LBL-U @ >str ;

: IN-CASE ( str ^u8 -- n ) {: name:str dst:^ :} C-DIR$ name dst FS:JOIN-PATH ;

: CASE-OPEN ( str -- ) {: name:str :}
   0 C-NAME-U !  name C-NAME C-NAME-U NAME-CAP APPEND
   name C-DIR GT-PATH C-DIR-U !  C-DIR$ FS:MAKE-DIRS
   s" main.f" C-MAIN IN-CASE C-MAIN-U !
   s" out" C-OUT IN-CASE C-OUT-U !
   s" program" C-IMAGE IN-CASE C-IMAGE-U ! ;

: HELPER-DIR ( str ^u8 ^n str -- ) {: name:str dst:^ up:^ text:str :}
   name dst IN-CASE up !  dst up @ >str FS:MAKE-DIRS
   dst up @ >str s" helper.f" C-H FS:JOIN-PATH C-H-U !
   C-H$ text FS:WRITE-ALL ;

\ main.f: a top-level line, then the root expression TXT holds after MAIN-START, ROOT!, a
\ require of the helper and a MAIN that prints its value.
: MAIN-START ( -- ) 0 TXT-U !  S\" s\q case top level\q type cr\n" TXT+ ;
: MAIN-END ( str -- ) {: h:str :}
   S\"  SOURCE-ROOT:ROOT!\nrequire " TXT+  h TXT+  S\" \n: MAIN ( -- ) HELPER-K . ;\n" TXT+
   C-MAIN$ TXT$ FS:WRITE-ALL ;

: CASE-BUILD ( str str -- ) {: out:str image:str :}
   GE-HB-RESET
   s" HABU_OUT" out PROC-ENV-SET
   C-MAIN$ image BUILD-ARGV BUILD-RUN
   s" the build of main.f" LABEL GE-EXPECT-OK ;

: CASE-RUN ( str str -- ) {: image:str want:str :}
   GE-HB-RESET
   image GE-ARGV+
   image PROCESS:WAIT-FOREVER GE-RUN-ENV
   s" the program built from main.f" LABEL GE-EXPECT-OK
   want s" the program prints the value of the helper its root reaches" LABEL GE-EXPECT-OUT ;

: WARM ( -- ) C-OUT$ C-IMAGE$ CASE-BUILD ;
: WARM-RUN ( str -- ) {: want:str :} C-IMAGE$ want CASE-RUN ;

: SYMLINK-CASE ( -- )
   s" symlink" CASE-OPEN
   s" v1" C-A C-A-U HELPER-1$ HELPER-DIR
   s" v2" C-B C-B-U HELPER-2$ HELPER-DIR
   s" beside" C-LINK IN-CASE C-LINK-U !
   s" v1" C-LINK$ FS:MAKE-SYMLINK
   MAIN-START  S\" s\q beside\q" TXT+  s" helper.f" MAIN-END
   WARM  S\" 1\n" WARM-RUN
   C-LINK$ FS:REMOVE-FILE  s" v2" C-LINK$ FS:MAKE-SYMLINK
   WARM  S\" 2\n" WARM-RUN ;

\ bytes: the longest root ROOT! takes; its searchable test appends /. to it.
PATH-CAP 2 - constant LONG-ROOT

\ C-REL: u bytes of 200-byte directory names under the case's directory.
: REL! ( n -- ) {: u:n :}
   u 0 ?do i 1 + 201 mod 0= if 47 else 100 then C-REL i + c! loop
   C-REL u 1 - + c@ 47 = if 100 C-REL u 1 - + c! then
   u C-REL-U ! ;

\ main.f's root is LONG-ROOT bytes, canonical; the second build reuses main.f's object, so the link
\ sets the root. The helper is required by absolute path: under that root its path would pass PATH-CAP.
: LONG-CASE ( -- )
   s" long" CASE-OPEN
   C-DIR$ SOURCE-ROOT:CANONICAL drop {: c:str :}
   LONG-ROOT c STR:LEN - 1 - REL!
   c C-REL$ C-A FS:JOIN-PATH C-A-U !
   C-A-U @ LONG-ROOT <> if s" the long root is LONG-ROOT bytes" LABEL GE-FAIL then
   C-A$ FS:MAKE-DIRS
   s" h.f" C-H IN-CASE C-H-U !
   C-H$ HELPER-1$ FS:WRITE-ALL
   MAIN-START  S\" s\q " TXT+  C-A$ TXT+  S\" \q" TXT+  C-H$ MAIN-END
   WARM  S\" 1\n" WARM-RUN
   WARM
   s" case top level" s" the second build reuses main.f's object, running no top-level code" LABEL EXPECT-OUT-LACKS
   S\" 1\n" WARM-RUN ;

\ lib.f lays the data MAIN reads between two variables nothing reads; STK-END
\ holds STK's end, MAIN's only way to STK. GAP-LEAN$ is lib.f without the two.
: GAP-LIB$ ( -- str )
   S\" variable DEAD-A\nvariable LIVE-V\ncreate STK 64 allot\nPERSISTED-PTR-VARIABLE STK-END\nvariable DEAD-B\nSTK 64 + STK-END !\n" ;
: GAP-LEAN$ ( -- str )
   S\" variable LIVE-V\ncreate STK 64 allot\nPERSISTED-PTR-VARIABLE STK-END\nSTK 64 + STK-END !\n" ;
: GAP-MAIN$ ( -- str )
   S\" require lib.f\n: MAIN ( -- ) 7 LIVE-V !  9 STK-END @ 1 cells - !  LIVE-V @ .  STK-END @ 1 cells - @ . ;\n" ;

create G-LIB PATH-CAP allot
create G-OUT PATH-CAP allot
create G-IMAGE PATH-CAP allot
variable G-LIB-U
variable G-OUT-U
variable G-IMAGE-U
: G-OUT$ ( -- str ) G-OUT G-OUT-U @ >str ;
: G-IMAGE$ ( -- str ) G-IMAGE G-IMAGE-U @ >str ;
\ The bytes of the file at the path, in storage its size makes.
: PROGRAM-BYTES ( str -- str ) {: p:str :}
   p FS:FILE-SIZE MEM-ALLOC-BYTES {: b:^ bu:n :}
   p b bu FS:READ-ALL {: got:n :}
   b got >str ;

\ lib.f as the text, built into the out root and run.
: GAP-BUILD ( str str str -- ) {: lib:str out:str image:str :}
   G-LIB G-LIB-U @ >str lib FS:WRITE-ALL
   out image CASE-BUILD
   GE-HB-RESET
   image GE-ARGV+
   image PROCESS:WAIT-FOREVER GE-RUN-ENV
   s" the program built from main.f" LABEL GE-EXPECT-OK
   S\" 7\n9\n" s" the program prints LIVE-V and the cell below STK-END's pointer" LABEL GE-EXPECT-OUT ;

: GAP-CASE ( -- )
   s" gap" CASE-OPEN
   s" lib.f" G-LIB IN-CASE G-LIB-U !
   s" lean-out" G-OUT IN-CASE G-OUT-U !
   s" lean-program" G-IMAGE IN-CASE G-IMAGE-U !
   C-MAIN$ GAP-MAIN$ FS:WRITE-ALL
   GAP-LIB$ C-OUT$ C-IMAGE$ GAP-BUILD
   GAP-LEAN$ G-OUT$ G-IMAGE$ GAP-BUILD
   C-IMAGE$ PROGRAM-BYTES G-IMAGE$ PROGRAM-BYTES STR:= 0= if
      s" dead variables around the data MAIN reads leave the program's bytes as they are" LABEL GE-FAIL
   then ;

\ lib.f as lib's text and main.f as main's, built into the out root and refused.
: ADDRESS-REFUSED ( str str -- ) {: lib:str main:str :}
   s" lib.f" G-LIB IN-CASE G-LIB-U !
   G-LIB G-LIB-U @ >str lib FS:WRITE-ALL
   C-MAIN$ main FS:WRITE-ALL
   GE-HB-RESET
   s" HABU_OUT" C-OUT$ PROC-ENV-SET
   C-MAIN$ C-IMAGE$ BUILD-ARGV BUILD-RUN
   70 s" the build of main.f" LABEL GE-EXPECT-RC
   s" LINKER:E-ADDRESS" s" the build's error" LABEL GE-EXPECT-ERR-HAS ;

\ A cell holding the address where a file's data ends, past its last datum, is refused.
: FILE-END-CASE ( -- )
   s" file-end" CASE-OPEN
   S\" PERSISTED-PTR-VARIABLE P  create U 8 allot  42 U !  U 16 + P !\n"
   S\" require lib.f\n: MAIN ( -- ) P @ NULL-PTR - P NULL-PTR - - .  P @ 16 - @ . ;\n"
   ADDRESS-REFUSED ;

\ A later file's cell holding the address where a required file's data ends, which lies in the gap the later file's
\ data starts after, is refused.
: LATER-END-CASE ( -- )
   s" later-end" CASE-OPEN
   S\" create U 8 allot  42 U !\n"
   S\" require lib.f\nPERSISTED-PTR-VARIABLE P\nU 16 + P !\n: MAIN ( -- ) P @ 16 - @ . ;\n"
   ADDRESS-REFUSED ;

\ The STRUCTURE's texts are laid after lib.f's data, so the address where U's extent ends lies in the cell LAY sets
\ apart before them, in no extent: refused.
: EXTENT-END-CASE ( -- )
   s" extent-end" CASE-OPEN
   S\" STRUCTURE pair FIELD a n FIELD b n ;STRUCTURE\ncreate U 8 allot  42 U !\n"
   S\" require lib.f\nPERSISTED-PTR-VARIABLE P\nU 16 + P !\n: MAIN ( -- ) P @ NULL-PTR - U NULL-PTR - - .  P @ 16 - @ . ;\n"
   ADDRESS-REFUSED ;

\ A sum's cell is an address cell when any variant puts an address in it, so `some` of a variable's address in an
\ `option<^n>`, whose first variant is `none`, is relocated, and `none` stored over a heap address leaves a null.
: OPTION-LIB$ ( -- str )
   S\" require src/lib/adt/option.f\nrequire src/lib/memory.f\nvariable PT-CELL\n42 PT-CELL !\n1 TYPED-BUFFER PT-ROW option<^n>\nPT-CELL OPTION:SOME 0 PT-ROW !\n1 TYPED-BUFFER HEAP-ROW option<^n>\n1 >COUNT MEM-ALLOC-CELLS OPTION:SOME 0 HEAP-ROW !\nOPTION:NONE 0 HEAP-ROW !\n" ;
: OPTION-MAIN$ ( -- str )
   S\" require lib.f\n: SHOW ( option<^n> -- ) MATCH option none OF 1 . ENDOF some OF @ . ENDOF ;MATCH ;\n: MAIN ( -- ) 0 PT-ROW @ SHOW  0 HEAP-ROW @ SHOW ;\n" ;

: OPTION-CASE ( -- )
   s" option" CASE-OPEN
   s" lib.f" G-LIB IN-CASE G-LIB-U !
   G-LIB G-LIB-U @ >str OPTION-LIB$ FS:WRITE-ALL
   C-MAIN$ OPTION-MAIN$ FS:WRITE-ALL
   WARM
   GE-HB-RESET
   C-IMAGE$ GE-ARGV+
   C-IMAGE$ PROCESS:WAIT-FOREVER GE-RUN-ENV
   s" the program built from main.f" LABEL GE-EXPECT-OK
   S\" 42\n1\n" s" the program prints the variable `some` holds, then `none`" LABEL GE-EXPECT-OUT ;

\ A heap address in an `option<^n>` is refused.
: HEAP-CASE ( -- )
   s" heap" CASE-OPEN
   S\" require src/lib/adt/option.f\nrequire src/lib/memory.f\n1 TYPED-BUFFER HEAP-ROW option<^n>\n1 >COUNT MEM-ALLOC-CELLS OPTION:SOME 0 HEAP-ROW !\n"
   S\" require lib.f\n: MAIN ( -- ) 0 HEAP-ROW @ MATCH option none OF 1 . ENDOF some OF @ . ENDOF ;MATCH ;\n"
   ADDRESS-REFUSED ;

\ A build lays a deferred buffer's region apart from its block, so the block reaches it through a marked address
\ cell: bound and filled at top level, the region's address cell is relocated and its number kept.
: DEFER-LIB$ ( -- str )
   S\" STRUCTURE dfcell FIELD p ^n FIELD x n ;STRUCTURE\nvariable DF-CELL\n42 DF-CELL !\nDEFER-LAYOUT-BUFFER DF-ROW dfcell\n3 DF-ROW-BIND\nDF-CELL 7 DFCELL:MAKE 0 DF-ROW !\n" ;
: DEFER-MAIN$ ( -- str )
   S\" require lib.f\n: MAIN ( -- ) 0 DF-ROW @ DFCELL:UNMAKE {: p:^ x:n :} p @ . x . ;\n" ;

: DEFER-CASE ( -- )
   s" defer" CASE-OPEN
   s" lib.f" G-LIB IN-CASE G-LIB-U !
   G-LIB G-LIB-U @ >str DEFER-LIB$ FS:WRITE-ALL
   C-MAIN$ DEFER-MAIN$ FS:WRITE-ALL
   WARM
   GE-HB-RESET
   C-IMAGE$ GE-ARGV+
   C-IMAGE$ PROCESS:WAIT-FOREVER GE-RUN-ENV
   s" the program built from main.f" LABEL GE-EXPECT-OK
   S\" 42\n7\n" s" the program prints the address cell's target, then the number" LABEL GE-EXPECT-OUT ;

\ A queue its top level made and destroyed keeps no semaphore address: built and run. One left live is rejected.
: QUEUE-LIB$ ( -- str )
   S\" require src/lib/queue.f\n4 QUEUE:QUEUE JOBS\nJOBS QUEUE:INIT\nJOBS QUEUE:DESTROY\n" ;
: QUEUE-MAIN$ ( -- str )
   S\" require lib.f\nrequire src/lib/queue.f\n: MAIN ( -- ) JOBS QUEUE:INIT 7 JOBS QUEUE:PUSH JOBS QUEUE:POP . JOBS QUEUE:DESTROY ;\n" ;

: QUEUE-CASE ( -- )
   s" queue" CASE-OPEN
   s" lib.f" G-LIB IN-CASE G-LIB-U !
   G-LIB G-LIB-U @ >str QUEUE-LIB$ FS:WRITE-ALL
   C-MAIN$ QUEUE-MAIN$ FS:WRITE-ALL
   WARM
   GE-HB-RESET
   C-IMAGE$ GE-ARGV+
   C-IMAGE$ PROCESS:WAIT-FOREVER GE-RUN-ENV
   s" the program built from main.f" LABEL GE-EXPECT-OK
   S\" 7\n" s" the program prints what it pushed through the queue" LABEL GE-EXPECT-OUT ;

: LIVE-QUEUE-CASE ( -- )
   s" live-queue" CASE-OPEN
   S\" require src/lib/queue.f\n4 QUEUE:QUEUE JOBS\nJOBS QUEUE:INIT\n"
   S\" require lib.f\nrequire src/lib/queue.f\n: MAIN ( -- ) 7 JOBS QUEUE:PUSH JOBS QUEUE:POP . ;\n"
   ADDRESS-REFUSED ;

: BODY ( -- )
   s" aot-shifted-bytes" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U !
   s" record.f" RECORD GT-PATH RECORD-U !
   s" refused" REFUSED GT-PATH REFUSED-U !
   s" probe.f" PROBE GT-PATH PROBE-U !
   s" sub/root" PROBE-ROOT GT-PATH PROBE-ROOT-U !
   s" sub/inner.f" PROBE-INNER GT-PATH PROBE-INNER-U !
   s" out" PROBE-OUT GT-PATH PROBE-OUT-U !
   s" probe" PROBE-IMAGE GT-PATH PROBE-IMAGE-U !
   s" resolves.f" RESOLVES GT-PATH RESOLVES-U !
   s" resolves-out" RESOLVES-OUT GT-PATH RESOLVES-OUT-U !
   s" resolves" RESOLVES-IMAGE GT-PATH RESOLVES-IMAGE-U !
   s" envtop.f" ENVTOP GT-PATH ENVTOP-U !
   s" envtop-out" ENVTOP-OUT GT-PATH ENVTOP-OUT-U !
   s" envtop" ENVTOP-IMAGE GT-PATH ENVTOP-IMAGE-U !
   s" rooted.f" ROOTED GT-PATH ROOTED-U !
   s" beside" ROOTED-DIR GT-PATH ROOTED-DIR-U !
   s" beside/helper.f" ROOTED-HELPER GT-PATH ROOTED-HELPER-U !
   s" beside/shared.f" ROOTED-SHARED GT-PATH ROOTED-SHARED-U !
   s" rooted-out" ROOTED-OUT GT-PATH ROOTED-OUT-U !
   s" rooted" ROOTED-IMAGE GT-PATH ROOTED-IMAGE-U !
   BUILD RUN-IMAGE BUILD-RECORD
   PROBE-WRITE PROBE-LOAD PROBE-PLAIN PROBE-BUILD
   RESOLVES-LOAD RESOLVES-BUILD
   ENVTOP-BUILDS
   ROOTED-WRITE ROOTED-BUILD ROOTED-REBUILD ROOTED-CHANGED
   SYMLINK-CASE LONG-CASE GAP-CASE FILE-END-CASE LATER-END-CASE EXTENT-END-CASE
   OPTION-CASE HEAP-CASE DEFER-CASE QUEUE-CASE LIVE-QUEUE-CASE
   s" PASS: a stripped build of shifted and aligned address bytes, a refused record pointer in a declared DATA cell, a program file and a build whose loads read and set the loader's root as hb --load does, a build whose word resolves its own path as hb --load does, builds that reuse a part only while the environment variable its top level read holds, a file whose ROOT! reaches its require, built, rebuilt from its cached object and rebuilt after its required file changed, warm builds of a root set through a symlink and at the longest root, a program whose bytes dead variables around its data leave as they are, and refused builds storing the address where a file's data or an extent ends, in that file or a later one, a variable's address in a sum's second variant, run, a heap address there, refused, a deferred buffer bound at top level holding an address and a number, run, a queue the top level destroyed, run, and one it left live, refused" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

;package

AOT-SHIFTED-BYTES-TEST:RUN
