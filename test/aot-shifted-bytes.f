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
\ top level: SOURCE-ROOT:ACTIVE-SOURCE, CURRENT$, and CURRENT$ inside a WITH.
\ Built through tools/hb-build.f, the file's `SOURCE-ROOT:` words are the
\ build's copy of src/core/include.f and the loads are hb's; both reach the one
\ record the process names in SRCLOC:SOURCE-CELL, so the build prints the lines
\ `hb --load` prints.
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
create PROBE-SUB PATH-CAP allot
create PROBE-INNER PATH-CAP allot
create PROBE-OUT PATH-CAP allot
create PROBE-IMAGE PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U
variable RECORD-U
variable REFUSED-U
variable PROBE-U
variable PROBE-SUB-U
variable PROBE-INNER-U
variable PROBE-OUT-U
variable PROBE-IMAGE-U
\ what `hb --load` printed, which the build must print too
4096 constant LOADED-CAP
create LOADED LOADED-CAP allot
variable LOADED-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: RECORD$ ( -- ptr u8 n ) RECORD RECORD-U @ ;
: REFUSED$ ( -- ptr u8 n ) REFUSED REFUSED-U @ ;
: PROBE$ ( -- ptr u8 n ) PROBE PROBE-U @ ;
: PROBE-SUB$ ( -- ptr u8 n ) PROBE-SUB PROBE-SUB-U @ ;
: PROBE-INNER$ ( -- ptr u8 n ) PROBE-INNER PROBE-INNER-U @ ;
: PROBE-OUT$ ( -- ptr u8 n ) PROBE-OUT PROBE-OUT-U @ ;
: PROBE-IMAGE$ ( -- ptr u8 n ) PROBE-IMAGE PROBE-IMAGE-U @ ;
: LOADED$ ( -- ptr u8 n ) LOADED LOADED-U @ ;

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
   S\" SOURCE-ROOT:ACTIVE-SOURCE s\q inner bytes \q type . drop s\q inner root \q type type cr s\q inner path \q type type cr\ns\q inner current \q type SOURCE-ROOT:CURRENT$ type cr\n: INNER-WITH ( -- ) s\q inner with \q type SOURCE-ROOT:CURRENT$ type cr ;\ns\q /tmp\q ' INNER-WITH SOURCE-ROOT:WITH\n" ;

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
   PROBE-SUB$ FS:MAKE-DIRS
   PROBE-INNER$ INNER-SOURCE$ FS:WRITE-ALL
   PROBE$ PROBE-SOURCE$ FS:WRITE-ALL ;

: PROBE-LOAD ( -- )
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ PROBE$ GE-ARG+
   ENGINE-CANDIDATE:PATH$ GE-TIMEOUT-MS GE-RUN-ENV
   s" hb --load of a file whose loads read the loader" GE-EXPECT-OK
   GT-OUT$ {: a:ptr u:n :}
   u LOADED-CAP > if s" the probe's --load output fits its buffer" GE-FAIL then
   a LOADED u BYTE-COPY  u LOADED-U ! ;

\ The build caches objects under its out root, and a cached probe runs none of
\ its top-level code, so each run builds under its own.
: PROBE-BUILD ( -- )
   GE-HB-RESET
   s" HABU_OUT" >LEN PROBE-OUT$ >LEN PROC-ENV-SET
   PROBE$ PROBE-IMAGE$ BUILD-ARGV BUILD-RUN
   s" stripped build of a file whose loads read the loader" GE-EXPECT-OK
   LOADED$ s" the loader's lines in the build, as under hb --load" GE-EXPECT-OUT-HAS ;

: BODY ( -- )
   s" aot-shifted-bytes" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U !
   s" record.f" RECORD GT-PATH RECORD-U !
   s" refused" REFUSED GT-PATH REFUSED-U !
   s" probe.f" PROBE GT-PATH PROBE-U !
   s" sub" PROBE-SUB GT-PATH PROBE-SUB-U !
   s" sub/inner.f" PROBE-INNER GT-PATH PROBE-INNER-U !
   s" out" PROBE-OUT GT-PATH PROBE-OUT-U !
   s" probe" PROBE-IMAGE GT-PATH PROBE-IMAGE-U !
   BUILD RUN-IMAGE BUILD-RECORD
   PROBE-WRITE PROBE-LOAD PROBE-BUILD
   s" PASS: a stripped build of shifted and aligned address bytes, a refused record pointer in a declared DATA cell, and a build whose loads read the loader as hb --load does" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

;package

AOT-SHIFTED-BYTES-TEST:RUN
