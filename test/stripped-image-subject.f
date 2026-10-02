\ The stripped family's run-time subjects in one application, linked and run
\ once by test/stripped-image.f. Each required subject is a package whose RUN
\ prints its line or dies naming itself; its header says what the line guards.
\ The row writes MAIN, which calls RUN here and ends in UNMAP.
require lib/num-types.f
require lib/memory.f
require lib/aio.f
require test/stripped-quotation-subject.f
require test/stripped-sparse-data-subject.f
require test/aot-image-class-subject.f
require test/compiler/aot-xt-cells-subject.f
require test/stripped-lifecycle-prepare-subject.f
require test/stripped-lifecycle-tasks-subject.f
require test/stripped-lifecycle-semaphore-subject.f

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

create KEY-CTX SHA256-FILE-CTX-BYTES allot
create KEY-CHECK 64 allot

: DOES-RUN ( -- )
   STRIP-REC-BYTES . STRIP-E0 . STRIP-E4 . STRIP-VALUE . ;

\ Both public engine identity queries must build their process-local caches
\ after the stripped entry restores its DATA.
: ENGINE-ID-RUN ( -- )
   ENGINE-ID:PATH$ {: path:ptr pathu:n :}
   pathu 0= if s" stripped-engine-id: empty path" 74 die then
   ENGINE-ID:KEY$ {: key:ptr keyu:n :}
   keyu 64 <> if s" stripped-engine-id: bad key length" 74 die then
   KEY-CTX path pathu KEY-CHECK SHA256-FILE-HEX-IN 0<> if
      s" stripped-engine-id: cannot hash image" 74 die
   then
   key keyu KEY-CHECK 64 STR= 0= if
      s" stripped-engine-id: wrong image key" 74 die
   then
   s" stripped-engine-id: ok" type cr ;

\ This deliberately invalid munmap range reaches MEM's baked error string.
\ The syscall refuses the unaligned address before touching any memory.
\ The address has no typed source; `NULL-PTR BYTE-VIEW +` would be a closed
\ forgery, so the integer is cast.
CAST: >BYTES ( n -- ptr u8 )

: PAGE-LEN ( -- NUM:byte-len )
   4096 NUM:BYTE-LEN MATCH NUM:numeric-result
      ok OF ENDOF negative OF 79 throw ENDOF zero OF 79 throw ENDOF
      overflow OF 79 throw ENDOF underflow OF 79 throw ENDOF
      bad-alignment OF 79 throw ENDOF misaligned OF 79 throw ENDOF
   ;MATCH ;

: BAD-SPAN ( -- ptr u8 NUM:byte-len ) 4097 >BYTES PAGE-LEN ;

public

: RUN ( -- )
   STRIPPED-QUOTATION-SUBJECT:RUN
   DOES-RUN
   STRIPPED-SPARSE-DATA-SUBJECT:RUN
   AOT-IMAGE-CLASS-SUBJECT:RUN
   ENGINE-ID-RUN
   AOT-XT-CELL-SUBJECT:RUN
   STRIPPED-LIFECYCLE-PREPARE-SUBJECT:RUN
   STRIPPED-LIFECYCLE-TASKS-SUBJECT:RUN
   STRIPPED-LIFECYCLE-SEMAPHORE-SUBJECT:RUN ;

\ Exits 71 with `memory: unmap failed`, a literal the stripped link kept.
: UNMAP ( -- )
   BAD-SPAN MEM:UNMAP ;

;package
