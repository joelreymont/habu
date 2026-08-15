\ clobber.f - what a routine the native chain published really destroys, kept
\ against the address its code starts at. One concern: the record a call site
\ narrows its caller-save discipline against.
\
\ The key is the ADDRESS and not the name: a name can be redefined, and a row is
\ right exactly as long as the code there is the routine it was written for.
\
\ A row may only ever NARROW. A caller compiled against one saved everything
\ outside its set, so a second record destroying more is E-NCLOB-WIDEN.
\
\ Row lifetime follows code lifetime: a forget or a declaration rollback moves
\ the free code slot back, so DROP-FROM runs from xref.f's CODE-RECLAIM.
\
\ An address with no row answers the caller's worst case, and the engine's own
\ `catch` entry must stay that case - a different body runs behind it per site.

require lib/prelude.f
require lib/errors.f
require lib/vector.f
require src/compiler/a64-effect.f

package NCLOB

private

\ ---- how many routines this file can remember at once -------------------------
\ A row is never dropped to make space, so the table grows rather than caps.
128 constant ROWS-SEED

\ ---- and the one ceiling that is left -----------------------------------------
\ A row exists because a publication claimed a code slot for it, slots are
\ claimed in strictly increasing order (src/compiler/native/publish.f SLOT-CK
\ refuses one below the last routine's end) and every one of them is an
\ instruction-aligned address inside the engine's code region. So the rows this
\ record can be holding at once are at most the instruction slots that region
\ has, and E-NCLOB-CAP is that bound.
\
\ IT IS A BACKSTOP AND NOT A PATH, which is the shape the widen refusal in RECORD
\ already has here. The publication seam runs out of code arena long before it
\ runs out of slots - src/compiler/native/publish.f ROOM-CK refuses with
\ E-NPUB-ROOM at the end reserve - so a program cannot reach this number through
\ the seam at all. It is asked because a caller that records addresses of its own
\ is not the seam, and a record that grew without a bound would be a table with
\ no answer for how large it may become.
\ Slots are claimed in strictly increasing order and each is an aligned address
\ in the code region, so its instruction slots bound the rows. A backstop.
4 constant INSN-BYTES
REGION INSN-BYTES / constant ROWS-CEIL

create R-ENTRY VEC-HEADER-CELLS cells allot
create R-GPR VEC-HEADER-CELLS cells allot
create R-FPR VEC-HEADER-CELLS cells allot

\ The three mappings are NOT taken while this file loads: a mapping base is
\ process-local, and an AOT capture copies this file's DATA into another engine.
\ The guard asks the LAST column, so a part-way allocation cannot look finished.
: TABLE-INIT ( -- )
   R-FPR VEC-CAP@ COUNT>N 0= 0= if exit then
   R-ENTRY ROWS-SEED VEC-COUNT VEC-INIT
   R-GPR ROWS-SEED VEC-COUNT VEC-INIT
   R-FPR ROWS-SEED VEC-COUNT VEC-INIT ;

\ The three columns are written and truncated together.
: ROWS# ( -- n )
   R-ENTRY VEC-LEN@ LEN>N ;

\ Read raw: this is the one reader on a path whose length is the population, and
\ the checked accessor cost twelve times as much. The row columns keep it.
: ENTRY-AT ( n -- n ) {: k:n :}
   R-ENTRY VEC-DATA@ k cells + @ ;

: GPR-AT ( n -- n ) {: k:n :}
   R-GPR k VEC-IDX VEC-N@ ;

: FPR-AT ( n -- n ) {: k:n :}
   R-FPR k VEC-IDX VEC-N@ ;

: GPR-AT! ( n n -- ) {: v:n k:n :}
   v R-GPR k VEC-IDX VEC-N! ;

: FPR-AT! ( n n -- ) {: v:n k:n :}
   v R-FPR k VEC-IDX VEC-N! ;

\ The three columns move together, or a row answers another routine's registers.
: TRUNC-TO ( n -- ) {: k:n :}
   k VEC-LEN R-ENTRY VEC-LEN!
   k VEC-LEN R-GPR VEC-LEN!
   k VEC-LEN R-FPR VEC-LEN! ;

\ Linear: a hash collision would hand one routine's set to another's callers.
: ROW-OF ( n -- n )
   {: entry:n :}
   -1
   ROWS# 0 ?do
      i ENTRY-AT entry = if drop i leave then
   loop ;

: NARROWS? ( n n -- bool )
   {: old:n new:n :}
   new old and new = ;

\ Room is taken in front because an allocation can fail and the append runs in a
\ publication's commit phase, which may not throw. Taking room changes no row.
: ROOM-CK ( -- )
   TABLE-INIT
   ROWS# 1+ {: need:n :}
   need ROWS-CEIL > if E-NCLOB-CAP throw then
   R-ENTRY need VEC-COUNT VEC-ENSURE
   R-GPR need VEC-COUNT VEC-ENSURE
   R-FPR need VEC-COUNT VEC-ENSURE ;

: ROW+ ( n n n -- )
   {: entry:n g:n f:n :}
   ROOM-CK
   entry R-ENTRY VEC-PUSH-N drop
   g R-GPR VEC-PUSH-N drop
   f R-FPR VEC-PUSH-N drop ;

\ The live table is in publication order and publish.f refuses a slot below the
\ last routine's end (E-NPUB-SLOT), so the rows a reclamation drops are a SUFFIX.
: FLOOR-ROW ( n -- n )
   {: floor:n :}
   ROWS#
   ROWS# 0 ?do
      i ENTRY-AT floor >= if drop i leave then
   loop ;

\ A row below the floor means this table is not that sequence: a defect here with
\ no correct answer to give, and a watcher may not throw.
: ORDER-CK ( n n -- )
   {: floor:n k:n :}
   ROWS# k ?do
      i ENTRY-AT floor < if
         s" nclob: recorded routines out of publication order" 76 die
      then
   loop ;

\ Registered once with CODE-RECLAIM: dropping a row is sound exactly when the code
\ it describes is gone, which only the file reclaiming the space knows.
: DROP-FROM ( n -- )
   {: floor:n :}
   floor FLOOR-ROW {: k:n :}
   floor k ORDER-CK
   k TRUNC-TO ;

public

: KNOWN? ( n -- bool )
   ROW-OF 0 >= ;

\ Two readers: a shortage in one register file is not answered by the other.
: GPR-CLOB ( n A64EFF:gprs -- A64EFF:gprs )
   {: entry:n worst:A64EFF:gprs :}
   entry ROW-OF {: k:n :}
   k 0 < if worst exit then
   k GPR-AT A64EFF:GPR-SET ;

: FPR-CLOB ( n A64EFF:fprs -- A64EFF:fprs )
   {: entry:n worst:A64EFF:fprs :}
   entry ROW-OF {: k:n :}
   k 0 < if worst exit then
   k FPR-AT A64EFF:FPR-SET ;

\ The same refusals asked while a refusal still costs nothing. RECORD runs once
\ the routine is live, so a refusal there leaves a row describing another routine.
: RECORD-CK ( n A64EFF:gprs A64EFF:fprs -- )
   {: entry:n g:A64EFF:gprs f:A64EFF:fprs :}
   entry ROW-OF {: k:n :}
   k 0 < if ROOM-CK exit then
   k GPR-AT g A64EFF:GPRS-N NARROWS? 0= if E-NCLOB-WIDEN throw then
   k FPR-AT f A64EFF:FPRS-N NARROWS? 0= if E-NCLOB-WIDEN throw then ;

: RECORD ( n A64EFF:gprs A64EFF:fprs -- )
   {: entry:n g:A64EFF:gprs f:A64EFF:fprs :}
   g A64EFF:GPRS-N {: gb:n :}
   f A64EFF:FPRS-N {: fb:n :}
   entry ROW-OF {: k:n :}
   k 0 < if entry gb fb ROW+ exit then
   k GPR-AT gb NARROWS? 0= if E-NCLOB-WIDEN throw then
   k FPR-AT fb NARROWS? 0= if E-NCLOB-WIDEN throw then
   gb k GPR-AT!
   fb k FPR-AT! ;

: ROWS ( -- n )
   ROWS# ;

private

\ One registration, and there is no way to undo it.
: WATCH-INSTALL ( -- )
   [: DROP-FROM ;] CODE-RECLAIM:WATCH ;

WATCH-INSTALL

get-current prot-wid-add

public
get-current prot-wid-add

;package
