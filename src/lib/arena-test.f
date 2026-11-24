\ arena-test.f - package ARENA through its public words, in one loaded program.
\ The cases catch an arena that: hands out bytes not zeroed, not cell-aligned
\ or overlapping; moves or clobbers earlier bytes when it maps a chunk; serves
\ a request larger than its first chunk wrongly; skips an action, runs one
\ twice, out of order or on another allocation; leaves bytes behind a RESET;
\ leaks a child, releases one twice, loses a middle sibling or runs its own
\ actions before its children's; leaks under WITH when the body throws; lets a
\ throwing action stop the rest or loses its code; changes when a mapping is
\ refused; fails on NULL-PTR;
\ cannot be held in a run-time record's field; leaves a later chunk mapped
\ after RESET or keeps every first chunk RELEASE gives back; runs an action
\ twice when a release halted in it resumes; moves a newest block it could
\ grow in place, gives back an older block's bytes when it shrinks, or loses
\ a block's bytes or leaves stale ones in it when its allocator resizes it.

require src/lib/test.f
require src/lib/span.f
require src/lib/allocator.f
require src/lib/arena.f
require src/lib/process.f
require src/lib/task.f
require src/lib/adt/result.f                 \ a halted task's join answer is MATCHed
require src/lib/test/wait.f
require src/lib/test/ending.f
require src/lib/test/subject.f

package ARENA-TEST

variable BAD                       \ failed checks inside a loop
variable ORDER                     \ the ids actions saw in run order, a digit each

: CHECK ( bool -- ) 0= if 1 BAD +! then ;
: ALIGNED? ( ^u8 -- bool ) NULL-PTR - CELL mod 0= ;
: FILL-FF ( str -- ) {: s:str :}
   s STR:DATA s STR:LEN SPAN:MAKE $FF swap SPAN:FILL ;


: ZEROED? ( str -- bool )
   {: p:str :}
   true p STR:LEN 0 ?do p STR:DATA i + c@ 0<> if drop false then loop ;


: NOTE ( ^u8 -- ) c@ ORDER @ 10 * + ORDER ! ;

: TAGGED ( ^ARENA:arena n -- )
   {: a:^ id:n :}
   a 8 [: NOTE ;] ARENA:ALLOC-DISPOSE drop id swap c! ;


: NEXT-ALLOC ( ^ARENA:arena n ^u8 -- n ^u8 )
   {: a:^ jumps:n prev:^ :}
   a 1000 ARENA:ALLOC {: q:^ qn:n :}
   q qn >str ZEROED? CHECK
   q ALIGNED? CHECK
   q qn >str FILL-FF
   q prev - 1000 = if jumps else jumps 1+ then q ;


\ 1. Allocations are zeroed, aligned and distinct across three chunks.
: MANY ( -- )
   s" many" T-LABEL
   0 BAD !
   ARENA:NEW {: a:^ :}
   a 5 ARENA:ALLOC {: first:^ fn:n :}
   fn 5 T=
   first fn >str ZEROED? TTRUE
   first ALIGNED? TTRUE
   5 0 do i 1+ first i + c! loop
   a 1000 ARENA:ALLOC {: prev:^ pn:n :}
   prev first - 8 T=
   prev pn >str FILL-FF
   0 prev 120 0 do a -rot NEXT-ALLOC loop drop
   2 >= TTRUE
   BAD @ 0 T=
   first c@ 1 T=  first 4 + c@ 5 T=
   a ARENA:RELEASE ;


\ 2. A request larger than the first chunk.
: LARGE ( -- )
   s" large" T-LABEL
   ARENA:NEW {: a:^ :}
   a 40000 ARENA:ALLOC {: p:^ pn:n :}
   pn 40000 T=
   p pn >str ZEROED? TTRUE
   p pn >str FILL-FF
   a 16 ARENA:ALLOC >str ZEROED? TTRUE
   p pn + 1- c@ $FF T=
   a ARENA:RELEASE ;


\ 3. Actions run once each, newest first, on their own allocations.
: ACTIONS-ONCE ( -- )
   s" actions-once" T-LABEL
   0 ORDER !
   ARENA:NEW {: a:^ :}
   a 1 TAGGED a 2 TAGGED a 3 TAGGED
   ORDER @ 0 T=
   a ARENA:RESET
   ORDER @ 321 T=
   a ARENA:RELEASE
   ORDER @ 321 T= ;


\ 4. RESET keeps the handle and zeroes what was written.
: RESET-ZEROES ( -- )
   s" reset-zeroes" T-LABEL
   ARENA:NEW {: a:^ :}
   a 64 ARENA:ALLOC {: p:^ pn:n :}
   p pn >str FILL-FF
   a 40000 ARENA:ALLOC >str FILL-FF
   a ARENA:RESET
   a 64 ARENA:ALLOC {: q:^ qn:n :}
   q p = TTRUE
   q qn >str ZEROED? TTRUE
   a ARENA:RELEASE ;


\ 5. Children: released by a parent's RELEASE and RESET before its own
\ actions run, never twice, and a middle child released first unlinks itself.
\ A child RESET stays linked and runs no action twice; the oldest child
\ released first unlinks itself too.
: CHILDREN ( -- )
   s" children" T-LABEL
   0 ORDER !
   ARENA:NEW {: p:^ :}
   p 6 TAGGED
   p ARENA:NEW-IN 1 TAGGED
   p ARENA:NEW-IN {: mid:^ :}
   mid 2 TAGGED
   p ARENA:NEW-IN 3 TAGGED
   mid ARENA:RELEASE
   ORDER @ 2 T=
   p ARENA:RELEASE
   ORDER @ 2316 T=
   0 ORDER !
   ARENA:NEW {: r:^ :}
   r 7 TAGGED
   r ARENA:NEW-IN {: kid:^ :}
   kid ARENA:NEW-IN 5 TAGGED
   kid 4 TAGGED
   r ARENA:RESET
   ORDER @ 547 T=
   r 8 ARENA:ALLOC >str ZEROED? TTRUE
   r ARENA:RELEASE
   ORDER @ 547 T=
   0 ORDER !
   ARENA:NEW {: q:^ :}
   q ARENA:NEW-IN {: son:^ :}
   son 4 TAGGED
   son ARENA:RESET
   ORDER @ 4 T=
   son 5 TAGGED
   q 6 TAGGED
   q ARENA:RELEASE
   ORDER @ 456 T=
   0 ORDER !
   ARENA:NEW {: s:^ :}
   s 9 TAGGED
   s ARENA:NEW-IN {: oldest:^ :}
   oldest 1 TAGGED
   s ARENA:NEW-IN 2 TAGGED
   s ARENA:NEW-IN 3 TAGGED
   oldest ARENA:RELEASE
   ORDER @ 1 T=
   s ARENA:RELEASE
   ORDER @ 1329 T= ;


\ 6. WITH releases on return and when the body throws.
: WITH-RELEASES ( -- )
   s" with" T-LABEL
   0 ORDER !
   [: 6 TAGGED 42 ;] ARENA:WITH 42 T=
   ORDER @ 6 T=
   0 ORDER !
   [: [: 7 TAGGED E-DIV-ZERO throw ;] ARENA:WITH ;] E-DIV-ZERO TTHROWSQ
   ORDER @ 7 T= ;


: THROW-SIZE ( ^u8 -- ) drop E-MEM-SIZE throw ;
: THROW-DIV ( ^u8 -- ) drop E-DIV-ZERO throw ;

\ 7. A throwing action stops none of the others; RESET rethrows the first code.
: ACTION-THROWS ( -- )
   s" action-throws" T-LABEL
   0 ORDER !
   ARENA:NEW {: a:^ :}
   a 1 TAGGED
   a 8 [: THROW-SIZE ;] ARENA:ALLOC-DISPOSE 2drop
   a 3 TAGGED
   a 8 [: THROW-DIV ;] ARENA:ALLOC-DISPOSE 2drop
   [: a ARENA:RESET ;] E-DIV-ZERO TTHROWSQ
   ORDER @ 31 T=
   a ARENA:RELEASE
   ORDER @ 31 T= ;


\ 8. A refused mapping and RELEASE of NULL-PTR.
: EDGES ( -- )
   s" edges" T-LABEL
   NULL-PTR ARENA:RELEASE
   ARENA:NEW {: a:^ :}
   a 8 ARENA:ALLOC drop {: p:^ :}
   [: a MEM-MAX-N 2 / ARENA:ALLOC 2drop ;] E-MEM-MAP TTHROWSQ
   a 8 ARENA:ALLOC drop p - 8 T=
   a ARENA:RELEASE ;


\ 9. A run-time record holds an arena in a field, and an action typed for a
\ record runs through a private cast to `[ ^u8 -- ]`.
STRUCTURE root DERIVE addr FIELD store ^ARENA:arena FIELD id n ;STRUCTURE
STRUCTURE item DERIVE addr FIELD id n ;STRUCTURE
HEAP ROOTS root

private

CAST: >ITEM ( ^u8 -- ^item )
CAST: ERASE-ITEM ( [ ^item -- ] -- [ ^u8 -- ] )

: ITEM-NOTE ( ^item -- ) ITEM-ID @ ORDER @ 10 * + ORDER ! ;

public

: HELD ( -- )
   s" held" T-LABEL
   0 ORDER !
   ROOTS-NEW {: r:^ :}
   r ROOT-STORE @ ARENA:RELEASE
   ARENA:NEW r ROOT-STORE !
   r ROOT-STORE @ ITEM-BYTES [: ITEM-NOTE ;] ERASE-ITEM ARENA:ALLOC-DISPOSE drop
   >ITEM 9 swap ITEM-ID !
   r ROOT-STORE @ 100 ARENA:ALLOC >str FILL-FF
   r ROOT-STORE @ 40000 ARENA:ALLOC >str FILL-FF
   r ROOT-STORE @ ARENA:RELEASE
   ORDER @ 9 T=
   ARENA:NEW r ROOT-STORE !
   r ROOT-STORE @ 200 ARENA:ALLOC >str ZEROED? TTRUE
   r ROOT-STORE @ ARENA:RELEASE
   ORDER @ 9 T=
   r ROOTS-RELEASE ;


\ 10. RESET unmaps a later chunk, and RELEASE a parent's first chunk once its
\ 64 children fill the spares: a subject child touching either exits 110.
: RESET-TOUCH ( -- )
   ARENA:NEW {: a:^ :}
   a 40000 ARENA:ALLOC drop {: p:^ :}
   a ARENA:RESET
   p c@ drop ;


: RELEASE-TOUCH ( -- )
   ARENA:NEW {: a:^ :}
   a 8 ARENA:ALLOC drop {: p:^ :}
   64 0 do a ARENA:NEW-IN drop loop
   a ARENA:RELEASE
   p c@ drop ;

private

$1000 constant CAP
CAP BUFFER: CAP-OUT
CAP BUFFER: CAP-ERR


: EXPECT-FAULT ( str -- )
   {: src:str :}
   src CAP-OUT CAP >LEN CAP-ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   {: outu:len erru:len oc :}
   src CAP-OUT outu LEN>N >str CAP-ERR erru LEN>N >str oc
   110 T-ENDING-EXITED= ;


: UNMAPPED ( -- )
   s" unmapped" T-LABEL
   s" ARENA-TEST:RESET-TOUCH" EXPECT-FAULT
   s" ARENA-TEST:RELEASE-TOUCH" EXPECT-FAULT ;


\ 11. A RESET halted inside an action leaves that action out of the RELEASE
\ after it: the halt unwinds past every catch.
variable HALT-READY
variable HALT-SEEN                 \ the halting action has run once
TYPED-VARIABLE HALT-ARENA ^ARENA:arena
TASK:MIN-STACK TASK:TASK HALT-TASK

: HALT-NOTE ( ^u8 -- )
   NOTE
   HALT-SEEN @ 0<> if exit then
   1 HALT-SEEN !
   1 HALT-READY atomic!
   begin TASK:PAUSE again ;

: HALT-WORK ( -- ) HALT-ARENA @ ARENA:RESET ;


: HALTED ( -- )
   s" halted" T-LABEL
   0 ORDER !  0 HALT-SEEN !  0 HALT-READY atomic!
   ARENA:NEW {: a:^ :}
   a 1 TAGGED a 2 TAGGED
   a 8 [: HALT-NOTE ;] ARENA:ALLOC-DISPOSE drop 3 swap c!
   a HALT-ARENA !
   ['] HALT-WORK HALT-TASK TASK:ACTIVATE
   HALT-READY 1 HALT-TASK TEST-WAIT:FOR-CELL
   HALT-TASK TASK:HALT
   HALT-TASK TASK:JOIN
   MATCH result
      ok OF drop false TTRUE ENDOF
      err OF E-TASK-NO-RESULT T= ENDOF
   ;MATCH
   a ARENA:RELEASE
   ORDER @ 321 T= ;


\ The newest block grows in place; an older one moves, its old bytes as written.
: RESIZE-MOVES ( ^ALLOC:allocator -- ^u8 )
   {: al:^ :}
   al 16 ALLOC:BYTES drop {: block:^ :}
   block 16 >str FILL-FF
   al block 16 40 ALLOC:RESIZE 40 T= {: grown:^ :}
   grown block = TTRUE
   al 16 ALLOC:BYTES drop {: mid:^ :}
   al grown 40 64 ALLOC:RESIZE 64 T= {: moved:^ :}
   moved grown <> TTRUE  moved 15 + c@ $FF T=
   moved 16 + 48 >str ZEROED? TTRUE
   grown c@ $FF T=  grown 15 + c@ $FF T=
   al mid 16 8 ALLOC:RESIZE 8 T= mid = TTRUE
   moved ;


\ A shrunk newest block regrows in place past its old extent and reads zeros
\ there; a growth past the first chunk moves the block and keeps its bytes.
: RESIZE-REGROWS ( ^ALLOC:allocator ^u8 -- )
   {: al:^ block:^ :}
   block 64 >str FILL-FF
   al block 64 20 ALLOC:RESIZE 20 T= block = TTRUE
   al block 20 128 ALLOC:RESIZE 128 T= block = TTRUE
   block 19 + c@ $FF T=  block 20 + 108 >str ZEROED? TTRUE
   al block 128 $5000 ALLOC:RESIZE $5000 T= {: far:^ :}
   far block <> TTRUE
   far c@ $FF T=  far 19 + c@ $FF T=
   far 20 + $5000 20 - >str ZEROED? TTRUE ;


\ 12. ALLOC:RESIZE through an arena's allocator: in place, by a move, after a
\ shrink and past the first chunk.
: RESIZES ( -- )
   s" resize" T-LABEL
   ARENA:NEW {: a:^ :}
   a ARENA:ALLOCATOR {: al:^ :}
   al al RESIZE-MOVES RESIZE-REGROWS
   a ARENA:RELEASE ;

public

: RUN ( -- )
   T-RESET
   MANY LARGE ACTIONS-ONCE RESET-ZEROES CHILDREN WITH-RELEASES ACTION-THROWS
   EDGES HELD UNMAPPED HALTED RESIZES
   T-REPORT ;

RUN

false [if] \ off until dot habu-add-the-checks-a49d054d
\ An arena that admits a zero, negative or overflowing size.
: EDGE-SIZES ( -- )
   s" edge sizes" T-LABEL
   ARENA:NEW {: a:^ :}
   [: a 0 ARENA:ALLOC 2drop ;] E-MEM-SIZE TTHROWSQ
   [: a -1 ARENA:ALLOC 2drop ;] E-MEM-SIZE TTHROWSQ
   [: a MEM-MAX-N ARENA:ALLOC 2drop ;] E-MEM-SIZE TTHROWSQ
   a ARENA:RELEASE ;
T-RESET EDGE-SIZES T-REPORT
[then]
;package
