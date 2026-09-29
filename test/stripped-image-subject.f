\ The stripped family's run-time subjects in one application, linked and run
\ once by test/stripped-image.f. Each required subject is a package whose RUN
\ prints its line or dies naming itself; its header says what the line guards.
\ The row writes MAIN, which calls RUN here and ends in UNMAP.
require lib/memory.f
require lib/aio.f
require test/stripped-quotation-subject.f
require test/stripped-sparse-data-subject.f
require test/aot-image-class-subject.f
require test/compiler/aot-xt-cells-subject.f
require test/stripped-lifecycle-prepare-subject.f
require test/stripped-lifecycle-tasks-subject.f

\ Baked defining words and a fresh does> clause. The definer is retained, so a
\ word it creates ends in a branch to its ;does clause, whose record the link
\ has to keep (src/habu/aot-capture.f ACAP-GRAPH-NAME-DOES).
BEGIN-STRUCTURE STRIP-REC-BYTES
CELL +FIELD STRIP-FIELD
END-STRUCTURE
0 ENUM+ STRIP-E0
1 ENUM4+ STRIP-E4
: STRIP-MAKE ( n -- ) create , does> ( -- n ) @ ;
23 STRIP-MAKE STRIP-VALUE

package STRIPPED-IMAGE-SUBJECT
private

: DOES-RUN ( -- )
   STRIP-REC-BYTES . STRIP-E0 . STRIP-E4 . STRIP-VALUE . ;

\ This deliberately invalid munmap range reaches MEM's baked error string.
\ The syscall refuses the unaligned address before touching any memory.
TRUSTED: BAD-SPAN ( -- ptr u8 NUM:byte-len ) 4097 4096 ;

public

: RUN ( -- )
   STRIPPED-QUOTATION-SUBJECT:RUN
   DOES-RUN
   STRIPPED-SPARSE-DATA-SUBJECT:RUN
   AOT-IMAGE-CLASS-SUBJECT:RUN
   AOT-XT-CELL-SUBJECT:RUN
   STRIPPED-LIFECYCLE-PREPARE-SUBJECT:RUN
   STRIPPED-LIFECYCLE-TASKS-SUBJECT:RUN ;

\ Exits 71 with `memory: unmap failed`, a literal the stripped link kept.
: UNMAP ( -- )
   BAD-SPAN MEM:UNMAP ;

;package
