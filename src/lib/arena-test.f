\ arena-test.f - package ARENA through its public words, in one loaded program.
\ The cases catch an arena that: hands out bytes not zeroed, not cell-aligned
\ or overlapping; moves or clobbers earlier bytes when it maps a chunk; serves
\ a request larger than its first chunk wrongly; skips an action, runs one
\ twice, out of order or on another allocation; leaves bytes behind a RESET;
\ leaks a child, releases one twice, loses a middle sibling or runs its own
\ actions before its children's; leaks under WITH when the body throws; lets a
\ throwing action stop the rest or loses its code; admits a zero, negative or
\ overflowing size; changes when a mapping is refused; fails on NULL-PTR;
\ cannot be held in a run-time record's field; leaves a chunk mapped after
\ RESET or RELEASE; runs an action twice when a release halted in it resumes.

require src/lib/test.f
require src/lib/span.f
require src/lib/arena.f
require src/lib/task.f
require src/lib/adt/result.f                 \ a halted task's join answer is MATCHed
require src/lib/test/wait.f
require src/lib/test/ending.f
require src/lib/test/subject.f

package ARENA-TEST

variable BAD                       \ failed checks inside a loop
variable ORDER                     \ the ids actions saw in run order, a digit each

: CHECK ( bool -- ) 0= if 1 BAD +! then ;
: ALIGNED? ( ptr u8 -- bool ) NULL-PTR - CELL mod 0= ;
: FILL-FF ( ptr u8 n -- ) SPAN:MAKE $FF swap SPAN:FILL ;


: ZEROED? ( ptr u8 n -- bool )
   {: p:ptr len:n :}
   true len 0 ?do p i + c@ 0<> if drop false then loop ;


\ An action: appends the id byte its allocation holds to ORDER.
: NOTE ( ptr u8 -- ) c@ ORDER @ 10 * + ORDER ! ;

\ An 8-byte allocation holding `id`, with NOTE as its action.
: TAGGED ( ptr ARENA:arena n -- )
   {: a:ptr id:n :}
   a 8 [: NOTE ;] ARENA:ALLOC-DISPOSE drop id swap c! ;


\ Answers `jumps` plus one when the next 1000 bytes do not follow `prev`'s.
: NEXT-ALLOC ( ptr ARENA:arena n ptr u8 -- n ptr u8 )
   {: a:ptr jumps:n prev:ptr :}
   a 1000 ARENA:ALLOC {: q:ptr qn:n :}
   q qn ZEROED? CHECK
   q ALIGNED? CHECK
   q qn FILL-FF
   q prev - 1000 = if jumps else jumps 1+ then q ;


\ 1. Allocations are zeroed, aligned and distinct across three chunks.
: MANY ( -- )
   s" many" T-LABEL
   0 BAD !
   ARENA:NEW {: a:ptr :}
   a 5 ARENA:ALLOC {: first:ptr fn:n :}
   fn 5 T=
   first fn ZEROED? TTRUE
   first ALIGNED? TTRUE
   5 0 do i 1+ first i + c! loop
   a 1000 ARENA:ALLOC {: prev:ptr pn:n :}
   prev first - 8 T=
   prev pn FILL-FF
   0 prev 120 0 do a -rot NEXT-ALLOC loop drop
   2 >= TTRUE
   BAD @ 0 T=
   first c@ 1 T=  first 4 + c@ 5 T=
   a ARENA:RELEASE ;


\ 2. A request larger than the first chunk.
: LARGE ( -- )
   s" large" T-LABEL
   ARENA:NEW {: a:ptr :}
   a 40000 ARENA:ALLOC {: p:ptr pn:n :}
   pn 40000 T=
   p pn ZEROED? TTRUE
   p pn FILL-FF
   a 16 ARENA:ALLOC ZEROED? TTRUE
   p pn + 1- c@ $FF T=
   a ARENA:RELEASE ;


\ 3. Actions run once each, newest first, on their own allocations.
: ACTIONS-ONCE ( -- )
   s" actions-once" T-LABEL
   0 ORDER !
   ARENA:NEW {: a:ptr :}
   a 1 TAGGED a 2 TAGGED a 3 TAGGED
   ORDER @ 0 T=
   a ARENA:RESET
   ORDER @ 321 T=
   a ARENA:RELEASE
   ORDER @ 321 T= ;


\ 4. RESET keeps the handle and zeroes what was written.
: RESET-ZEROES ( -- )
   s" reset-zeroes" T-LABEL
   ARENA:NEW {: a:ptr :}
   a 64 ARENA:ALLOC {: p:ptr pn:n :}
   p pn FILL-FF
   a 40000 ARENA:ALLOC FILL-FF
   a ARENA:RESET
   a 64 ARENA:ALLOC {: q:ptr qn:n :}
   q p = TTRUE
   q qn ZEROED? TTRUE
   a ARENA:RELEASE ;


\ 5. Children: released by a parent's RELEASE and RESET before its own
\ actions run, never twice, and a middle child released first unlinks itself.
\ A child RESET stays linked and runs no action twice; the oldest child
\ released first unlinks itself too.
: CHILDREN ( -- )
   s" children" T-LABEL
   0 ORDER !
   ARENA:NEW {: p:ptr :}
   p 6 TAGGED
   p ARENA:NEW-IN 1 TAGGED
   p ARENA:NEW-IN {: mid:ptr :}
   mid 2 TAGGED
   p ARENA:NEW-IN 3 TAGGED
   mid ARENA:RELEASE
   ORDER @ 2 T=
   p ARENA:RELEASE
   ORDER @ 2316 T=
   0 ORDER !
   ARENA:NEW {: r:ptr :}
   r 7 TAGGED
   r ARENA:NEW-IN {: kid:ptr :}
   kid ARENA:NEW-IN 5 TAGGED
   kid 4 TAGGED
   r ARENA:RESET
   ORDER @ 547 T=
   r 8 ARENA:ALLOC ZEROED? TTRUE
   r ARENA:RELEASE
   ORDER @ 547 T=
   0 ORDER !
   ARENA:NEW {: q:ptr :}
   q ARENA:NEW-IN {: son:ptr :}
   son 4 TAGGED
   son ARENA:RESET
   ORDER @ 4 T=
   son 5 TAGGED
   q 6 TAGGED
   q ARENA:RELEASE
   ORDER @ 456 T=
   0 ORDER !
   ARENA:NEW {: s:ptr :}
   s 9 TAGGED
   s ARENA:NEW-IN {: oldest:ptr :}
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
   [: [: 7 TAGGED 1 0 / drop ;] ARENA:WITH ;] E-DIV-ZERO TTHROWSQ
   ORDER @ 7 T= ;


: THROW-SIZE ( ptr u8 -- ) drop E-MEM-SIZE throw ;
: THROW-DIV ( ptr u8 -- ) drop E-DIV-ZERO throw ;

\ 7. A throwing action stops none of the others; RESET rethrows the first code.
: ACTION-THROWS ( -- )
   s" action-throws" T-LABEL
   0 ORDER !
   ARENA:NEW {: a:ptr :}
   a 1 TAGGED
   a 8 [: THROW-SIZE ;] ARENA:ALLOC-DISPOSE 2drop
   a 3 TAGGED
   a 8 [: THROW-DIV ;] ARENA:ALLOC-DISPOSE 2drop
   [: a ARENA:RESET ;] E-DIV-ZERO TTHROWSQ
   ORDER @ 31 T=
   a ARENA:RELEASE
   ORDER @ 31 T= ;


\ 8. Refused sizes and mappings, and RELEASE of NULL-PTR.
: EDGES ( -- )
   s" edges" T-LABEL
   NULL-PTR ARENA:RELEASE
   ARENA:NEW {: a:ptr :}
   a 8 ARENA:ALLOC drop {: p:ptr :}
   [: a 0 ARENA:ALLOC 2drop ;] E-MEM-SIZE TTHROWSQ
   [: a -1 ARENA:ALLOC 2drop ;] E-MEM-SIZE TTHROWSQ
   [: a MEM-MAX-N ARENA:ALLOC 2drop ;] E-MEM-SIZE TTHROWSQ
   [: a MEM-MAX-N 2 / ARENA:ALLOC 2drop ;] E-MEM-MAP TTHROWSQ
   a 8 ARENA:ALLOC drop p - 8 T=
   a ARENA:RELEASE ;


\ 9. A run-time record holds an arena in a field, and an action typed for a
\ record runs through a private cast to `[ ptr u8 -- ]`.
STRUCTURE root DERIVE addr FIELD store ptr ARENA:arena FIELD id n ;STRUCTURE
STRUCTURE item DERIVE addr FIELD id n ;STRUCTURE
HEAP ROOTS root

private

CAST: >ITEM ( ptr u8 -- ptr item )
CAST: ERASE-ITEM ( [ ptr item -- ] -- [ ptr u8 -- ] )

: ITEM-NOTE ( ptr item -- ) ITEM-ID @ ORDER @ 10 * + ORDER ! ;

public

: HELD ( -- )
   s" held" T-LABEL
   0 ORDER !
   ROOTS-NEW {: r:ptr :}
   r ROOT-STORE @ ARENA:RELEASE
   ARENA:NEW r ROOT-STORE !
   r ROOT-STORE @ ITEM-BYTES [: ITEM-NOTE ;] ERASE-ITEM ARENA:ALLOC-DISPOSE drop
   >ITEM 9 swap ITEM-ID !
   r ROOT-STORE @ 100 ARENA:ALLOC FILL-FF
   r ROOT-STORE @ ARENA:RELEASE
   ORDER @ 9 T=
   ARENA:NEW r ROOT-STORE !
   r ROOT-STORE @ 8 ARENA:ALLOC ZEROED? TTRUE
   r ROOT-STORE @ ARENA:RELEASE
   ORDER @ 9 T=
   r ROOTS-RELEASE ;


\ 10. RESET unmaps a later chunk and RELEASE the first: a subject child that
\ touches the allocation afterwards dies of SIGSEGV (src/habu/crash.f
\ C-CRASH-RERAISE). SUBJECT:RUN's child evaluates each by its public name.
: RESET-TOUCH ( -- )
   ARENA:NEW {: a:ptr :}
   a 40000 ARENA:ALLOC drop {: p:ptr :}
   a ARENA:RESET
   p c@ drop ;


: RELEASE-TOUCH ( -- )
   ARENA:NEW {: a:ptr :}
   a 8 ARENA:ALLOC drop {: p:ptr :}
   a ARENA:RELEASE
   p c@ drop ;

private

11 constant FAULT-SIG
$1000 constant CAP
1000 constant TIMEOUT-MS
CAP BUFFER: CAP-OUT
CAP BUFFER: CAP-ERR


: EXPECT-FAULT ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   src srcu CAP-OUT CAP >LEN CAP-ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   src srcu CAP-OUT outu LEN>N CAP-ERR erru LEN>N oc FAULT-SIG T-ENDING-SIGNALED= ;


: UNMAPPED ( -- )
   s" unmapped" T-LABEL
   s" ARENA-TEST:RESET-TOUCH" EXPECT-FAULT
   s" ARENA-TEST:RELEASE-TOUCH" EXPECT-FAULT ;


\ 11. A RESET halted inside an action leaves that action out of the RELEASE
\ after it: the halt unwinds past every catch (src/lib/task.f HALT-UNWIND).
variable HALT-READY
variable HALT-SEEN                 \ the halting action has run once
TYPED-VARIABLE HALT-ARENA ptr ARENA:arena
TASK:MIN-STACK TASK:TASK HALT-TASK

\ An action: notes its id; on its first run raises HALT-READY and pauses until
\ halted.
: HALT-NOTE ( ptr u8 -- )
   NOTE
   HALT-SEEN @ 0<> if exit then
   1 HALT-SEEN !
   1 HALT-READY atomic!
   begin TASK:PAUSE again ;

: HALT-WORK ( -- ) HALT-ARENA @ ARENA:RESET ;


: HALTED ( -- )
   s" halted" T-LABEL
   0 ORDER !  0 HALT-SEEN !  0 HALT-READY atomic!
   ARENA:NEW {: a:ptr :}
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

public

: RUN ( -- )
   T-RESET
   MANY LARGE ACTIONS-ONCE RESET-ZEROES CHILDREN WITH-RELEASES ACTION-THROWS
   EDGES HELD UNMAPPED HALTED
   T-REPORT ;

RUN
;package
