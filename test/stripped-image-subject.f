\ The stripped family's run-time subjects in one application, linked and run
\ once by test/stripped-image.f. Each required subject is a package whose RUN
\ prints its line or dies naming itself; its header says what the line guards.
\ The row writes MAIN, which calls RUN here and ends in UNMAP.
require src/lib/string.f
require src/lib/fmt.f
require src/lib/num-types.f
require src/lib/memory.f
require src/lib/aio.f
require src/lib/engine-id.f
require test/stripped-quotation-subject.f
require test/stripped-address-pool-subject.f
require test/stripped-sparse-data-subject.f
require test/aot-image-class-subject.f
require test/compiler/aot-xt-cells-subject.f
require test/stripped-lifecycle-prepare-subject.f
require test/stripped-lifecycle-tasks-subject.f
require test/stripped-lifecycle-semaphore-subject.f
require test/ffi-library-lifetime-subject.f

\ Baked defining words and a fresh does> clause. The definer is retained, so a
\ word it creates ends in a branch to its ;does clause, whose record the link
\ has to keep (src/habu/aot-capture.f ACAP-GRAPH-NAME-DOES).
BEGIN-STRUCTURE STRIP-REC-BYTES
CELL +FIELD STRIP-FIELD
END-STRUCTURE
0 ENUM+ STRIP-E0 ENUM4+ STRIP-E4 drop
: STRIP-MAKE ( n -- ) create , does> ( -- n ) @ ;
23 STRIP-MAKE STRIP-VALUE

\ The stripped root MAIN is resolved by the global token: the row's global MAIN
\ runs, never these.
package STRIPPED-ENTRY-HOSTILE
private
: MAIN ( -- ) s" private-main" type cr ;
public
: MAIN ( -- ) s" public-main" type cr ;
;package

package STRIPPED-IMAGE-SUBJECT
private

create KEY-CTX SHA256-FILE-CTX-BYTES allot
create KEY-CHECK 64 allot
create PPH-DIG $20 allot
create PPH-HEX $40 allot
create PPH-CTX SHA256-CTX-BYTES allot

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

\ Printing, parsing and hashing read baked constants the link carries by name
\ (src/habu/aot-owned-cells.f): a zeroed KK or HH0 changes the SHA-256 of "abc".
: PPH-RUN ( -- )
   42 FMT:.INT cr
   s" 123" STR-PARSE-POS MATCH option
     none OF s" none" type cr ENDOF
     some OF FMT:.INT cr ENDOF
   ;MATCH
   PPH-CTX s" abc" PPH-DIG SHA256-IN
   PPH-DIG PPH-HEX SHA256>HEX
   PPH-HEX $40 type cr ;

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
   STRIPPED-ADDRESS-POOL-SUBJECT:RUN
   DOES-RUN
   STRIPPED-SPARSE-DATA-SUBJECT:RUN
   AOT-IMAGE-CLASS-SUBJECT:RUN
   ENGINE-ID-RUN
   AOT-XT-CELL-SUBJECT:RUN
   STRIPPED-LIFECYCLE-PREPARE-SUBJECT:RUN
   STRIPPED-LIFECYCLE-TASKS-SUBJECT:RUN
   STRIPPED-LIFECYCLE-SEMAPHORE-SUBJECT:RUN
   FFI-LIBRARY-LIFETIME:RUN
   PPH-RUN ;

\ Exits 71 with `memory: unmap failed`, a literal the stripped link kept.
: UNMAP ( -- )
   BAD-SPAN MEM:UNMAP ;

;package
