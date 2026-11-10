\ A stripped build keeps bytes no span declares as bytes, and checks a declared
\ cell whatever window it lies in.
\ The first subject lays two ERROR:CODE rows whose text is the 8 address bytes
\ of a block it allocates at load and keeps, so the builder has that address
\ mapped wherever it lands. Each row (src/lib/error.f ROW,) starts aligned with
\ a 32-byte header. The 9-byte name E-SHIFTED puts the address at row offset
\ 41, an undeclared window that is not cell-aligned; the 8-byte name E-ALIGN8
\ puts it at row offset 40, an aligned undeclared window. Bytes are not an
\ address by type (DECISIONS.md "typed data"), so the build succeeds and the
\ application runs and exits 0. MAIN does not read the rows; the build keeps
\ them all the same.
\ The second subject stores a dictionary-record address in a declared DATA
\ cell laid with `allot` (src/core/pointer-storage.f PERSISTED-PTR-U8-TABLE).
\ The record lies outside hb's data and the build's space, so the linker
\ refuses the file's object, which cannot name it (src/habu/linker.f CELL-REF+).
\ The third subject is a file and a file it requires, each reading the loader at
\ top level: SOURCE-ROOT:ACTIVE-SOURCE, CURRENT$, and CURRENT$ inside a WITH;
\ the required file then sets its root with SOURCE-ROOT:ROOT!, which its load's
\ end undoes. Built through tools/hb-build.f, the file's `SOURCE-ROOT:` words
\ are the build's copy of src/core/include.f and the loads are hb's; both reach
\ the one record the process names in SRCLOC:SOURCE-CELL, so the build prints
\ the lines `hb --load` prints. Run as `hb probe.f`, the program file is a
\ loaded file to the loader (SOURCE-ROOT:RUN-PROGRAM) and prints them too. A
\ file whose word only resolves its own path, as Maki's
\ src/vocabulary.f does, loads nothing: its build prints the directory
\ `hb --load` prints. A file whose top level sets its root with ROOT! and requires a
\ file only that root reaches loads and builds; a second build into the same out
\ root reuses its object, so it runs none of the file's top-level code. That
\ file then requires a second file only its root reaches, which the first
\ required file loaded already, so the requiring file's object holds a registry
\ act made under the set root, not a request; the rebuild reuses it all the same.
\ A third build after the first required file changes prints its new line.
require test/gate-common.f
require src/lib/engine-candidate.f

package AOT-SHIFTED-BYTES-TEST
private

\ ms for each stripped application's hb-build, held to an engine build's limit
300000 constant BUILD-MS

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

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: RECORD$ ( -- ptr u8 n ) RECORD RECORD-U @ ;
: REFUSED$ ( -- ptr u8 n ) REFUSED REFUSED-U @ ;
: PROBE$ ( -- ptr u8 n ) PROBE PROBE-U @ ;
: PROBE-ROOT$ ( -- ptr u8 n ) PROBE-ROOT PROBE-ROOT-U @ ;
: PROBE-INNER$ ( -- ptr u8 n ) PROBE-INNER PROBE-INNER-U @ ;
: PROBE-OUT$ ( -- ptr u8 n ) PROBE-OUT PROBE-OUT-U @ ;
: PROBE-IMAGE$ ( -- ptr u8 n ) PROBE-IMAGE PROBE-IMAGE-U @ ;
: RESOLVES$ ( -- ptr u8 n ) RESOLVES RESOLVES-U @ ;
: RESOLVES-OUT$ ( -- ptr u8 n ) RESOLVES-OUT RESOLVES-OUT-U @ ;
: RESOLVES-IMAGE$ ( -- ptr u8 n ) RESOLVES-IMAGE RESOLVES-IMAGE-U @ ;
: ROOTED$ ( -- ptr u8 n ) ROOTED ROOTED-U @ ;
: ROOTED-DIR$ ( -- ptr u8 n ) ROOTED-DIR ROOTED-DIR-U @ ;
: ROOTED-HELPER$ ( -- ptr u8 n ) ROOTED-HELPER ROOTED-HELPER-U @ ;
: ROOTED-SHARED$ ( -- ptr u8 n ) ROOTED-SHARED ROOTED-SHARED-U @ ;
: ROOTED-OUT$ ( -- ptr u8 n ) ROOTED-OUT ROOTED-OUT-U @ ;
: ROOTED-IMAGE$ ( -- ptr u8 n ) ROOTED-IMAGE ROOTED-IMAGE-U @ ;
: LOADED$ ( -- ptr u8 n ) LOADED LOADED-U @ ;
\ a line the probe's --load output must hold, naming its canonical directory
create WANT PATH-CAP 2 * allot
variable WANT-U
: WANT$ ( -- ptr u8 n ) WANT WANT-U @ ;

\ The text is the kept block's own first 8 bytes, so no image byte but the
\ rows' holds the address. KEPT is a PTR-VARIABLE, which a save writes as null.
: SOURCE$ ( -- ptr u8 n )
   S\" require src/lib/error.f\nrequire src/lib/memory.f\nPTR-VARIABLE KEPT\n64 MEM-ALLOC-BYTES drop KEPT !\n: LAY ( -- ) KEPT @ NULL-PTR - {: a:n :} 8 0 do a i 8 * rshift $FF and KEPT @ i + c! loop ;\nLAY\n-110001 KEPT @ 8 ERROR:CODE E-SHIFTED\n-110002 KEPT @ 8 ERROR:CODE E-ALIGN8\n: MAIN ( -- ) s\q ok\q type cr ;\n" ;

\ Record 0's address in the table's one declared DATA cell.
: RECORD-SOURCE$ ( -- ptr u8 n )
   S\" require src/habu/xref.f\n1 PERSISTED-PTR-U8-TABLE T\n0 XREF-REC BYTE-VIEW T 0 ptr-field !\n: MAIN ( -- ) s\q ok\q type cr ;\n" ;

\ The probe: its path, root and byte count, its current root, and the current
\ root inside a WITH of /tmp, all at top level. WITH takes /tmp absolute: a
\ relative root resolves against the working directory, not the current root.
: PROBE-SOURCE$ ( -- ptr u8 n )
   S\" require sub/inner.f\nSOURCE-ROOT:ACTIVE-SOURCE s\q probe bytes \q type . drop s\q probe root \q type type cr s\q probe path \q type type cr\ns\q probe current \q type SOURCE-ROOT:CURRENT$ type cr\n: PROBE-WITH ( -- ) s\q probe with \q type SOURCE-ROOT:CURRENT$ type cr ;\ns\q /tmp\q ' PROBE-WITH SOURCE-ROOT:WITH\n: MAIN ( -- ) ;\n" ;

: INNER-SOURCE$ ( -- ptr u8 n )
   S\" SOURCE-ROOT:ACTIVE-SOURCE s\q inner bytes \q type . drop s\q inner root \q type type cr s\q inner path \q type type cr\ns\q inner current \q type SOURCE-ROOT:CURRENT$ type cr\n: INNER-WITH ( -- ) s\q inner with \q type SOURCE-ROOT:CURRENT$ type cr ;\ns\q /tmp\q ' INNER-WITH SOURCE-ROOT:WITH\ns\q sub/root\q SOURCE-ROOT:ROOT!\ns\q inner root! \q type SOURCE-ROOT:CURRENT$ type cr\n" ;

\ A word that resolves its own file's path and prints the directory, called at
\ the file's top level: Maki's src/vocabulary.f shape.
: RESOLVES-SOURCE$ ( -- ptr u8 n )
   S\" : RESOLVES-DIR ( -- ) s\q resolves.f\q SOURCE-ROOT:RESOLVE drop SOURCE-ROOT:DIRNAME s\q resolves dir \q type type cr ;\nRESOLVES-DIR\n: MAIN ( -- ) ;\n" ;

\ A file that prints a line at top level, sets its root to beside/ and requires
\ helper.f and then shared.f, which only that root resolves and helper.f has
\ loaded; MAIN prints a word each of them defines.
: ROOTED-SOURCE$ ( -- ptr u8 n )
   S\" s\q rooted top level\q type cr\ns\q beside\q SOURCE-ROOT:ROOT!\nrequire helper.f\nrequire shared.f\n: MAIN ( -- ) s\q rooted main \q type BESIDE-WORD type s\q  and \q type SHARED-WORD type cr ;\n" ;

: HELPER-SOURCE$ ( -- ptr u8 n )
   S\" require shared.f\n: BESIDE-WORD ( -- ptr u8 n ) s\q from beside\q ;\n" ;

\ helper.f rewritten: its word answers another line.
: HELPER-CHANGED-SOURCE$ ( -- ptr u8 n )
   S\" require shared.f\n: BESIDE-WORD ( -- ptr u8 n ) s\q from beside, changed\q ;\n" ;

: SHARED-SOURCE$ ( -- ptr u8 n )
   S\" : SHARED-WORD ( -- ptr u8 n ) s\q shared\q ;\n" ;

\ Each subject is its own hb-build: a refusal ends the build.
: BUILD-ARGV ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n out:ptr outu:n :}
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+ src srcu GE-ARG+
   s" -o" GE-ARG+ out outu GE-ARG+ ;

: BUILD-RUN ( -- ) ENGINE-CANDIDATE:PATH$ BUILD-MS GE-RUN-ENV ;

: HB-BUILD ( ptr u8 n ptr u8 n -- )
   GE-HB-RESET BUILD-ARGV BUILD-RUN ;

: BUILD ( -- )
   SUBJECT$ SOURCE$ FS:WRITE-ALL
   SUBJECT$ IMAGE$ HB-BUILD
   s" stripped build of shifted and aligned address bytes" GE-EXPECT-OK ;

: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ GE-TIMEOUT-MS GE-RUN-ENV
   s" the stripped application" GE-EXPECT-OK
   S\" ok\n" s" the stripped application's output" GE-EXPECT-OUT ;

: BUILD-RECORD ( -- )
   RECORD$ RECORD-SOURCE$ FS:WRITE-ALL
   RECORD$ REFUSED$ HB-BUILD
   s" stripped build of a record pointer in a declared DATA cell" GE-EXPECT-NONZERO
   s" which an object cannot name" s" the record-pointer refusal" GE-EXPECT-ERR-HAS ;

: PROBE-WRITE ( -- )
   PROBE-ROOT$ FS:MAKE-DIRS
   PROBE-INNER$ INNER-SOURCE$ FS:WRITE-ALL
   PROBE$ PROBE-SOURCE$ FS:WRITE-ALL ;

: WANT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a WANT WANT-U @ + u BYTE-COPY  u WANT-U +! ;

\ The label, the probe's canonical directory, the tail and a newline.
: WANT! ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n tail:ptr tailu:n :}
   0 WANT-U !  label labelu WANT+
   PROBE$ SOURCE-ROOT:DIRNAME SOURCE-ROOT:CANONICAL drop WANT+
   tail tailu WANT+  S\" \n" WANT+ ;

: PROBE-LOAD ( -- )
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ PROBE$ GE-ARG+
   ENGINE-CANDIDATE:PATH$ GE-TIMEOUT-MS GE-RUN-ENV
   s" hb --load of a file whose loads read the loader" GE-EXPECT-OK
   GT-OUT$ {: a:ptr u:n :}
   u LOADED-CAP > if s" the probe's --load output fits its buffer" GE-FAIL then
   a LOADED u BYTE-COPY  u LOADED-U !
   s" inner root! " s" /sub/root" WANT!
   WANT$ s" a required file's ROOT! makes its directory the current root" GE-EXPECT-OUT-HAS
   s" probe current " s" " WANT!
   WANT$ s" the requiring file's root is back after the required file's ROOT!" GE-EXPECT-OUT-HAS ;

\ The program file of `hb probe.f`: its frame names it as --load's does.
: PROBE-PLAIN ( -- )
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   PROBE$ GE-ARG+
   ENGINE-CANDIDATE:PATH$ GE-TIMEOUT-MS GE-RUN-ENV
   s" hb probe.f, a program file whose loads read the loader" GE-EXPECT-OK
   LOADED$ s" the loader's lines under hb probe.f, as under hb --load" GE-EXPECT-OUT-HAS ;

\ The build caches objects under its out root, and a cached probe runs none of
\ its top-level code, so each run builds under its own.
: PROBE-BUILD ( -- )
   GE-HB-RESET
   s" HABU_OUT" >LEN PROBE-OUT$ >LEN PROC-ENV-SET
   PROBE$ PROBE-IMAGE$ BUILD-ARGV BUILD-RUN
   s" stripped build of a file whose loads read the loader" GE-EXPECT-OK
   LOADED$ s" the loader's lines in the build, as under hb --load" GE-EXPECT-OUT-HAS ;

: RESOLVES-LOAD ( -- )
   RESOLVES$ RESOLVES-SOURCE$ FS:WRITE-ALL
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ RESOLVES$ GE-ARG+
   ENGINE-CANDIDATE:PATH$ GE-TIMEOUT-MS GE-RUN-ENV
   s" hb --load of a file whose word resolves its own path" GE-EXPECT-OK
   s" resolves dir " s" " WANT!
   WANT$ s" the directory the word resolves under hb --load" GE-EXPECT-OUT-HAS ;

\ Its own out root, as PROBE-BUILD's: a cached object runs no top-level code.
\ RESOLVES-LOAD left in WANT the line hb --load printed.
: RESOLVES-BUILD ( -- )
   GE-HB-RESET
   s" HABU_OUT" >LEN RESOLVES-OUT$ >LEN PROC-ENV-SET
   RESOLVES$ RESOLVES-IMAGE$ BUILD-ARGV BUILD-RUN
   s" stripped build of a file whose word resolves its own path" GE-EXPECT-OK
   WANT$ s" the directory the word resolves in the build, as under hb --load" GE-EXPECT-OUT-HAS ;

: EXPECT-OUT-LACKS ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu:n label:ptr labelu:n :}
   GT-OUT$ want wantu CONTAINS? if label labelu GE-FAIL then ;

: ROOTED-LOAD ( -- )
   ROOTED-DIR$ FS:MAKE-DIRS
   ROOTED-HELPER$ HELPER-SOURCE$ FS:WRITE-ALL
   ROOTED-SHARED$ SHARED-SOURCE$ FS:WRITE-ALL
   ROOTED$ ROOTED-SOURCE$ FS:WRITE-ALL
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ ROOTED$ GE-ARG+
   ENGINE-CANDIDATE:PATH$ GE-TIMEOUT-MS GE-RUN-ENV
   s" hb --load of a file that requires a file only its ROOT! reaches" GE-EXPECT-OK ;

\ The program run, printing MAIN's line.
: ROOTED-RUN ( ptr u8 n -- ) {: want:ptr wantu:n :}
   GE-HB-RESET
   ROOTED-IMAGE$ GE-ARGV+
   ROOTED-IMAGE$ GE-TIMEOUT-MS GE-RUN-ENV
   s" the program built from a file whose ROOT! reaches its require" GE-EXPECT-OK
   want wantu s" MAIN's line, from the words the ROOT!-reached files define" GE-EXPECT-OUT ;

: ROOTED-LINE$ ( -- ptr u8 n ) S\" rooted main from beside and shared\n" ;

\ Both builds share ROOTED-OUT: the first compiles the file, running its top
\ level; the second reuses its cached object, which runs no top-level code.
: ROOTED-BUILD ( -- )
   GE-HB-RESET
   s" HABU_OUT" >LEN ROOTED-OUT$ >LEN PROC-ENV-SET
   ROOTED$ ROOTED-IMAGE$ BUILD-ARGV BUILD-RUN
   s" stripped build of a file that requires a file only its ROOT! reaches" GE-EXPECT-OK
   s" rooted top level" s" the first build runs the file's top level" GE-EXPECT-OUT-HAS
   ROOTED-LINE$ ROOTED-RUN ;

: ROOTED-REBUILD ( -- )
   GE-HB-RESET
   s" HABU_OUT" >LEN ROOTED-OUT$ >LEN PROC-ENV-SET
   ROOTED$ ROOTED-IMAGE$ BUILD-ARGV BUILD-RUN
   s" second build of it into the same out root" GE-EXPECT-OK
   s" rooted top level" s" the second build reuses the cached object, running no top-level code" EXPECT-OUT-LACKS
   ROOTED-LINE$ ROOTED-RUN ;

\ A third build into the same out root, after helper.f changed, links its new word.
: ROOTED-CHANGED ( -- )
   ROOTED-HELPER$ HELPER-CHANGED-SOURCE$ FS:WRITE-ALL
   GE-HB-RESET
   s" HABU_OUT" >LEN ROOTED-OUT$ >LEN PROC-ENV-SET
   ROOTED$ ROOTED-IMAGE$ BUILD-ARGV BUILD-RUN
   s" third build of it into the same out root, after its required file changed" GE-EXPECT-OK
   S\" rooted main from beside, changed and shared\n" ROOTED-RUN ;

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
   s" rooted.f" ROOTED GT-PATH ROOTED-U !
   s" beside" ROOTED-DIR GT-PATH ROOTED-DIR-U !
   s" beside/helper.f" ROOTED-HELPER GT-PATH ROOTED-HELPER-U !
   s" beside/shared.f" ROOTED-SHARED GT-PATH ROOTED-SHARED-U !
   s" rooted-out" ROOTED-OUT GT-PATH ROOTED-OUT-U !
   s" rooted" ROOTED-IMAGE GT-PATH ROOTED-IMAGE-U !
   BUILD RUN-IMAGE BUILD-RECORD
   PROBE-WRITE PROBE-LOAD PROBE-PLAIN PROBE-BUILD
   RESOLVES-LOAD RESOLVES-BUILD
   ROOTED-LOAD ROOTED-BUILD ROOTED-REBUILD ROOTED-CHANGED
   s" PASS: a stripped build of shifted and aligned address bytes, a refused record pointer in a declared DATA cell, a program file and a build whose loads read and set the loader's root as hb --load does, a build whose word resolves its own path as hb --load does, and a file whose ROOT! reaches its require, loaded, built, rebuilt from its cached object and rebuilt after its required file changed" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

;package

AOT-SHIFTED-BYTES-TEST:RUN
