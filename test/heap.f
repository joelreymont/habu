\ HEAP records come from the real definer and runtime: each is zero-filled,
\ typed through its STRUCTURE's accessors, distinct from the next, and keeps
\ its contents across another record's NEW and RELEASE; a record holding a
\ JSON:cursor runs a reader with no cast; a record holds an arena by pointer;
\ a refused declaration (a sub-cell element, an unknown type) defines
\ nothing; checked code can neither call nor tick RECORD-NEW or
\ RECORD-RELEASE outside HEAP's generated words; a record pointer still enters
\ no other generic-pointer word; and tools/check.f --verify-only certifies a
\ word using a HEAP's words instead of deferring the rest of the source to the
\ run.
require src/lib/test.f
require src/lib/json.f
require src/lib/arena.f
require src/lib/string.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require src/lib/fs-mutate.f
require test/e2e-tree.f

package HEAP-TEST

STRUCTURE note DERIVE addr FIELD id n FIELD text ptr u8 ;STRUCTURE
HEAP NOTES note

STRUCTURE part DERIVE addr FIELD reader JSON:cursor FIELD total n ;STRUCTURE
HEAP PARTS part

STRUCTURE holder DERIVE addr FIELD arena ptr ARENA:arena ;STRUCTURE
HEAP HOLDERS holder


package RECORD-PX
public
EXPORT DYNAMIC-STORAGE:RECORD-RELEASE
;package

\ An ordinary word with RECORD-RELEASE's generic pointee.
: TAKE ( ptr a n -- ) 2drop ;

TYPED-VARIABLE EVAL-A ptr u8
variable EVAL-U
: EVAL-RUN ( -- ) EVAL-A @ EVAL-U @ INCLUDE-EVALUATE ;
: EVAL ( ptr u8 n -- n ) EVAL-U ! EVAL-A ! [: EVAL-RUN ;] catch ;

: UNDEFINED? ( ptr u8 n -- bool ) get-current search-wl 0= ;


: RECORDS ( -- )
   s" a record is zero-filled and typed" T-LABEL
   NOTES-NEW {: a:ptr :}
   a NOTE-ID @ 0 T=
   a NOTE-TEXT @ NULL-PTR = TTRUE
   7 a NOTE-ID !  s" hi" drop a NOTE-TEXT !
   a NOTE-ID @ 7 T=
   s" a second record is distinct" T-LABEL
   NOTES-NEW {: b:ptr :}
   b NOTE-ID @ 0 T=
   9 b NOTE-ID !
   a NOTE-ID @ 7 T=
   s" a record keeps its contents across another's NEW and RELEASE" T-LABEL
   NOTES-NEW NOTES-RELEASE
   b NOTES-RELEASE
   a NOTE-ID @ 7 T=
   a NOTE-TEXT @ c@ 104 T=
   a NOTES-RELEASE ;


\ A record of its own holding another package's OPAQUE reader state, reached
\ through its field with no cast.
: READ-PART ( ptr part -- )
   {: p:ptr :}
   p PART-READER s" [40,2]" JSON:READ-INIT
   JSON:READ-NEXT JSON:T-ARR T=
   JSON:READ-NEXT JSON:T-INT T=  JSON:READ-INT p PART-TOTAL !
   JSON:READ-NEXT JSON:T-INT T=  JSON:READ-INT p PART-TOTAL @ + p PART-TOTAL !
   JSON:READ-NEXT JSON:T-ARR-END T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;


: READER ( -- )
   s" a record holding a JSON:cursor runs a reader" T-LABEL
   PARTS-NEW {: p:ptr :}
   p READ-PART
   p PART-TOTAL @ 42 T=
   p PARTS-RELEASE ;


: HOLDER ( -- )
   s" a record holds an arena by pointer" T-LABEL
   HOLDERS-NEW {: h:ptr :}
   h HOLDER-ARENA @ NULL-PTR = TTRUE
   ARENA:NEW {: a:ptr :}
   a h HOLDER-ARENA !
   h HOLDER-ARENA @ NULL-PTR - a NULL-PTR - T=
   h HOLDER-ARENA @ ARENA:RELEASE
   h HOLDERS-RELEASE ;


: REFUSALS ( -- )
   s" a refused declaration defines nothing" T-LABEL
   s" HEAP BAD u16" EVAL CHECKER-REJECT-RC T=
   s" BAD-NEW" UNDEFINED? TTRUE
   s" HEAP WORSE no-such-type" EVAL CHECKER-REJECT-RC T=
   s" WORSE-NEW" UNDEFINED? TTRUE ;


\ Were Z1 admitted, a 1-byte record would stand for a cell. The pointee is a
\ plain cell, which the layout fence would let through, so the call gate alone
\ refuses these, in a body, a quotation, at top level and through a re-export.
: CALLS ( -- )
   s" checked code neither calls nor ticks the record runtime words" T-LABEL
   s" : Z1 ( -- ptr n ) 1 DYNAMIC-STORAGE:RECORD-NEW ;" EVAL CHECKER-REJECT-RC T=
   s" Z1" UNDEFINED? TTRUE
   s" : Z2 ( ptr n -- ) 16 DYNAMIC-STORAGE:RECORD-RELEASE ;" EVAL CHECKER-REJECT-RC T=
   s" Z2" UNDEFINED? TTRUE
   s" : Z3 ( -- ptr n ) 1 ['] DYNAMIC-STORAGE:RECORD-NEW execute ;" EVAL CHECKER-REJECT-RC T=
   s" Z3" UNDEFINED? TTRUE
   s" ' DYNAMIC-STORAGE:RECORD-RELEASE drop" EVAL CHECKER-REJECT-RC T=
   s" : Z5 ( -- ptr n ) 1 [: DYNAMIC-STORAGE:RECORD-NEW ;] execute ;" EVAL CHECKER-REJECT-RC T=
   s" Z5" UNDEFINED? TTRUE
   s" NULL-PTR 16 DYNAMIC-STORAGE:RECORD-RELEASE" EVAL CHECKER-REJECT-RC T=
   s" : Z6 ( -- ) NULL-PTR 16 RECORD-PX:RECORD-RELEASE ;" EVAL CHECKER-REJECT-RC T=
   s" Z6" UNDEFINED? TTRUE
   s" a record pointer enters no other generic-pointer word" T-LABEL
   s" : Z4 ( ptr note -- ) 16 TAKE ;" EVAL CHECKER-REJECT-RC T=
   s" Z4" UNDEFINED? TTRUE ;


\ tools/check.f --verify-only checks a file without running it, so it gets its
\ own child over a scratch tree's copy.
: VERIFY-SRC$ ( -- ptr u8 n )
   S\" package HEAP-VERIFY\nSTRUCTURE item DERIVE addr FIELD count n ;STRUCTURE\nHEAP ITEMS item\n: USE ( -- n ) ITEMS-NEW {: p:ptr :} 5 p ITEM-COUNT ! p ITEM-COUNT @ p ITEMS-RELEASE ;\n;package\n" ;

: VERIFY ( -- )
   s" tools/check.f --verify-only certifies a word using a HEAP's words" T-LABEL
   s" heap-verify" E2E-TREE:TREE
   E2E-TREE:ROOT$ FS:CLEANUP-TREE+
   s" test/heap-use.f" E2E-TREE:AT-A VERIFY-SRC$ FS:WRITE-ALL
   s" tools/check.f" E2E-TREE:ARGS
   s" --" >LEN PROC-ARGV+
   s" --verify-only" >LEN PROC-ARGV+
   s" test/heap-use.f" >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON
   E2E-TREE:OUT$ s" deferred to the run" CONTAINS? 0= TTRUE
   E2E-TREE:ERR$ nip 0 T=
   E2E-TREE:RC@ 0 T= ;


: RUN ( -- )
   T-RESET
   RECORDS READER HOLDER REFUSALS CALLS VERIFY
   T-REPORT ;

RUN
;package
