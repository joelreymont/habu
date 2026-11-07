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
\ cell laid with `allot` (src/core/pointer-storage.f PERSISTED-PTR-U8-TABLE),
\ which no span covers. Its row (src/habu/aot-closure.f XTD-ROW) is its only
\ check, so the build is refused with the record-pointer diagnostic.
require test/gate-common.f
require src/lib/engine-candidate.f
require test/aot-builder-engine.f

package AOT-SHIFTED-BYTES-TEST
private

\ ms for each stripped application's hb-build, held to an engine build's limit
300000 constant BUILD-MS

create SUBJECT PATH-CAP allot
create IMAGE PATH-CAP allot
create RECORD PATH-CAP allot
create REFUSED PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U
variable RECORD-U
variable REFUSED-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: RECORD$ ( -- ptr u8 n ) RECORD RECORD-U @ ;
: REFUSED$ ( -- ptr u8 n ) REFUSED REFUSED-U @ ;

\ The text is the kept block's own first 8 bytes, so no image byte but the
\ rows' holds the address. KEPT is a PTR-VARIABLE, which a save writes as null.
: SOURCE$ ( -- ptr u8 n )
   S\" require src/lib/error.f\nrequire src/lib/memory.f\nPTR-VARIABLE KEPT\n64 MEM-ALLOC-BYTES drop KEPT !\n: LAY ( -- ) KEPT @ NULL-PTR - {: a:n :} 8 0 do a i 8 * rshift $FF and KEPT @ i + c! loop ;\nLAY\n-110001 KEPT @ 8 ERROR:CODE E-SHIFTED\n-110002 KEPT @ 8 ERROR:CODE E-ALIGN8\n: MAIN ( -- ) s\q ok\q type cr ;\n" ;

\ Record 0's address in the table's one declared DATA cell.
: RECORD-SOURCE$ ( -- ptr u8 n )
   S\" require src/habu/xref.f\n1 PERSISTED-PTR-U8-TABLE T\n0 XREF-REC BYTE-VIEW T 0 ptr-field !\n: MAIN ( -- ) s\q ok\q type cr ;\n" ;

\ A fresh subject path has no observed-input index, so hb-build links it with
\ the shared maker. Each subject is its own hb-build: a refusal ends the build.
: HB-BUILD ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n out:ptr outu:n :}
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+ src srcu GE-ARG+
   s" -o" GE-ARG+ out outu GE-ARG+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ BUILD-MS GE-RUN-ENV ;

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
   s" dictionary-record pointer" s" the record-pointer refusal" GE-EXPECT-ERR-HAS ;

: BODY ( -- )
   s" aot-shifted-bytes" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U !
   s" record.f" RECORD GT-PATH RECORD-U !
   s" refused" REFUSED GT-PATH REFUSED-U !
   AOT-BUILDER-ENGINE:ENSURE
   BUILD RUN-IMAGE BUILD-RECORD
   s" PASS: a stripped build of shifted and aligned address bytes, and a refused record pointer in a declared DATA cell" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

;package

AOT-SHIFTED-BYTES-TEST:RUN
