split-str
\ HEAP records come from the real definer and runtime: each is zero-filled,
\ typed through its STRUCTURE's accessors, distinct from the next, and keeps
\ its contents across another record's NEW and RELEASE; a record holding a
\ JSON:cursor runs a reader with no cast; a record holds an arena by pointer;
\ a refused declaration (a sub-cell element, an unknown type) defines
\ nothing; checked code can neither call nor tick MAP-RECORD or UNMAP-RECORD
\ outside DYNAMIC-STORAGE; a record pointer enters no generic-pointer word that
\ reads a cell; and tools/check.f --verify-only certifies a word using a HEAP's
\ words instead of deferring the rest of the source to the run.
require src/lib/test.f
require src/lib/json.f
require src/lib/arena.f
require src/lib/string.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require src/lib/fs-mutate.f
require test/e2e-tree.f

package HEAP-CAPTURE

\ A part spelled as the runtime's package, whose words would answer a null
\ record and skip the unmap.
package DYNAMIC-STORAGE
CAST: N>RECORD ( n -- ^a )
public
variable RELEASED
: MAP-RECORD ( ^n -- ^a ) drop 0 N>RECORD ;
: UNMAP-RECORD ( ^a ^n -- ) 2drop 1 RELEASED ! ;
;package

STRUCTURE cap DERIVE addr FIELD id n ;STRUCTURE
HEAP CAPS cap

public

: CAPTURE ( -- )
   s" a part spelled as the runtime's package captures no HEAP word" T-LABEL
   CAPS-NEW {: a:^ :}
   a NULL-PTR = TFALSE
   a CAPS-RELEASE
   DYNAMIC-STORAGE:RELEASED @ 0 T= ;

;package

package HEAP-TEST

STRUCTURE note DERIVE addr FIELD id n FIELD text ^u8 ;STRUCTURE
HEAP NOTES note

STRUCTURE part DERIVE addr FIELD reader JSON:cursor FIELD total n ;STRUCTURE
HEAP PARTS part

STRUCTURE holder DERIVE addr FIELD arena ^ARENA:arena ;STRUCTURE
HEAP HOLDERS holder


\ An ordinary word that reads its generic pointee as a cell.
: TAKE ( ^a n -- ) drop @ drop ;

TYPED-VARIABLE EVAL-A ^u8
variable EVAL-U
: EVAL-RUN ( -- ) EVAL-A @ EVAL-U @ evaluate-closed ;
: EVAL ( ^u8 n -- n ) EVAL-U ! EVAL-A ! [: EVAL-RUN ;] catch ;

: UNDEFINED? ( str -- bool ) get-current search-wl 0= ;


: RECORDS ( -- )
   s" a record is zero-filled and typed" T-LABEL
   NOTES-NEW {: a:^ :}
   a NOTE-ID @ 0 T=
   a NOTE-TEXT @ NULL-PTR = TTRUE
   7 a NOTE-ID !  s" hi" drop a NOTE-TEXT !
   a NOTE-ID @ 7 T=
   s" a second record is distinct" T-LABEL
   NOTES-NEW {: b:^ :}
   b NOTE-ID @ 0 T=
   9 b NOTE-ID !
   a NOTE-ID @ 7 T=
   s" a record keeps its contents across another's NEW and RELEASE" T-LABEL
   NOTES-NEW NOTES-RELEASE
   b NOTES-RELEASE
   a NOTE-ID @ 7 T=
   a NOTE-TEXT @ c@ 104 T=
   a NOTES-RELEASE ;


: READ-PART ( ^part -- )
   {: p:^ :}
   p PART-READER s" [40,2]" JSON:READ-INIT
   JSON:READ-NEXT JSON:T-ARR T=
   JSON:READ-NEXT JSON:T-INT T=  JSON:READ-INT p PART-TOTAL !
   JSON:READ-NEXT JSON:T-INT T=  JSON:READ-INT p PART-TOTAL @ + p PART-TOTAL !
   JSON:READ-NEXT JSON:T-ARR-END T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;


: READER ( -- )
   s" a record holding a JSON:cursor runs a reader" T-LABEL
   PARTS-NEW {: p:^ :}
   p READ-PART
   p PART-TOTAL @ 42 T=
   p PARTS-RELEASE ;


: HOLDER ( -- )
   s" a record holds an arena by pointer" T-LABEL
   HOLDERS-NEW {: h:^ :}
   h HOLDER-ARENA @ NULL-PTR = TTRUE
   ARENA:NEW {: a:^ :}
   a h HOLDER-ARENA !
   h HOLDER-ARENA @ a = TTRUE
   h HOLDER-ARENA @ ARENA:RELEASE
   h HOLDERS-RELEASE ;


: REFUSALS ( -- )
   s" a refused declaration defines nothing" T-LABEL
   s" HEAP BAD u16" EVAL 70 T=
   s" BAD-NEW" UNDEFINED? TTRUE
   s" HEAP WORSE no-such-type" EVAL 70 T=
   s" WORSE-NEW" UNDEFINED? TTRUE ;


\ MAP-RECORD trusts the width its cell holds, so the record runtime words are
\ DYNAMIC-STORAGE's private words: outside it a tick or a call is undefined.
: CALLS ( -- )
   s" checked code neither calls nor ticks the record runtime words" T-LABEL
   s" ' DYNAMIC-STORAGE:MAP-RECORD drop" EVAL 70 T=
   s" : Z7 ( -- ) NULL-PTR 16 DYNAMIC-STORAGE:UNMAP-RECORD ;" EVAL 70 T=
   s" Z7" UNDEFINED? TTRUE
   s" a record pointer enters no generic-pointer word that reads a cell" T-LABEL
   s" : Z4 ( ^note -- ) 16 TAKE ;" EVAL 70 T=
   s" Z4" UNDEFINED? TTRUE ;


\ tools/check.f --verify-only checks a file without running it, so it gets its
\ own child over a scratch tree's copy.
: VERIFY-SRC$ ( -- str )
   S\" package HEAP-VERIFY\nSTRUCTURE item DERIVE addr FIELD count n ;STRUCTURE\nHEAP ITEMS item\n: USE ( -- n ) ITEMS-NEW {: p:^ :} 5 p ITEM-COUNT ! p ITEM-COUNT @ p ITEMS-RELEASE ;\n;package\n" ;

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
   HEAP-CAPTURE:CAPTURE
   T-REPORT ;

RUN
;package
