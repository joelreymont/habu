\ suite.f - package TEST: suite, group and row registration and the run loop.
\ A project adapter installs the hooks (SETUP! through
\ STDIN-RUNNER!); fixture helper words belong in a private package, not
\ global stemmed names.
\ STORAGE CLASS. PROCESS-WIDE: the row and group tables, argument buffer and
\ hooks are static; one task registers and runs a suite.

require src/lib/error.f
require src/lib/string.f

package TEST

128 constant NAME-CAP
1024 constant STDIN-CAP
\ Shared suite-table budget: a full table is the loud E-TBL-BOUNDS wall
\ (ITEM-CHECK, ITEM-ALLOC); grow the constant. test/gate-pool.f sizes its red
\ table from it.
1024 constant ITEM-MAX
32 constant GROUP-MAX
$10000 constant ARG-CAP
0 constant GROUP-PARALLEL
1 constant GROUP-SEQUENTIAL
0 constant ITEM-FILE
1 constant ITEM-STDIN

create ITEM-NAMES ITEM-MAX NAME-CAP * allot
create ITEM-NAME-US ITEM-MAX cells allot
create ITEM-KINDS ITEM-MAX cells allot
create ITEM-GROUPS ITEM-MAX cells allot
create ITEM-ARG-OFFS ITEM-MAX cells allot
create ITEM-ARG-COUNTS ITEM-MAX cells allot
create ITEM-PRELOAD-COUNTS ITEM-MAX cells allot
create ITEM-STDINS ITEM-MAX STDIN-CAP * allot
create ITEM-STDIN-US ITEM-MAX cells allot
create ITEM-CHOSEN ITEM-MAX cells allot
create GROUP-NAMES GROUP-MAX NAME-CAP * allot
create GROUP-NAME-US GROUP-MAX cells allot
create GROUP-MODES GROUP-MAX cells allot
create ARGS ARG-CAP allot

variable ITEM-N
variable ITEM-RAN
variable CHOSEN?
variable GROUP-N
variable GROUP-CUR
variable ARG-U
variable ARG-SCAN
variable DEF-ID
variable DEF-SCRIPT
variable DEF-ENTRIES
variable LAST-GROUP

defer SETUP ( -- )
defer TEARDOWN ( n -- )
defer DRAIN ( -- )
defer ARGS-BEGIN ( -- )
defer ARG+ ( str -- )
defer RUNNER ( str -- )
defer STDIN-RUNNER ( str str -- )

: TRUE ( -- bool )
   0 0= ;

: NOOP ( -- )
;

: ARG-DROP ( str -- )
   drop ;

: RUN-MISSING ( str -- )
   drop E-FS-OPEN throw ;

: STDIN-RUN-MISSING ( str str -- )
   drop drop E-FS-OPEN throw ;

: CAP-CHECK ( n n -- ) {: u:n cap:n :}
   u 0 < if E-STR-BOUNDS throw then
   u cap > if E-STR-CAPACITY throw then ;

: ITEM-CHECK ( n -- ) {: id:n :}
   id 0 < if E-TBL-BOUNDS throw then
   id ITEM-MAX >= if E-TBL-BOUNDS throw then ;

: GROUP-CHECK ( n -- ) {: id:n :}
   id 0 < if E-TBL-BOUNDS throw then
   id GROUP-MAX >= if E-TBL-BOUNDS throw then ;

: ITEM-NAME-BUF ( n -- ^u8 ) {: id:n :}
   id ITEM-CHECK
   ITEM-NAMES id NAME-CAP * + ;

: ITEM-STDIN-BUF ( n -- ^u8 ) {: id:n :}
   id ITEM-CHECK
   ITEM-STDINS id STDIN-CAP * + ;

: GROUP-NAME-BUF ( n -- ^u8 ) {: id:n :}
   id GROUP-CHECK
   GROUP-NAMES id NAME-CAP * + ;

: ITEM-CELL ( ^a n -- ^a ) {: base:^ id:n :}
   id ITEM-CHECK
   base id cells + ;

: GROUP-CELL ( ^a n -- ^a ) {: base:^ id:n :}
   id GROUP-CHECK
   base id cells + ;

: ITEM-NAME! ( str n -- ) {: s:str id:n :}
   s STR:LEN NAME-CAP CAP-CHECK
   s STR:DATA id ITEM-NAME-BUF s STR:LEN BYTE-COPY
   s STR:LEN ITEM-NAME-US id ITEM-CELL ! ;

: GROUP-NAME! ( str n -- ) {: s:str id:n :}
   s STR:LEN NAME-CAP CAP-CHECK
   s STR:DATA id GROUP-NAME-BUF s STR:LEN BYTE-COPY
   s STR:LEN GROUP-NAME-US id GROUP-CELL ! ;

: ITEM-STDIN! ( str n -- ) {: s:str id:n :}
   s STR:LEN STDIN-CAP CAP-CHECK
   s STR:DATA id ITEM-STDIN-BUF s STR:LEN BYTE-COPY
   s STR:LEN ITEM-STDIN-US id ITEM-CELL ! ;

: ITEM-NAME$ ( n -- str ) {: id:n :}
   id ITEM-NAME-BUF
   ITEM-NAME-US id ITEM-CELL @ >str ;

: GROUP-NAME$ ( n -- str ) {: id:n :}
   id GROUP-NAME-BUF
   GROUP-NAME-US id GROUP-CELL @ >str ;

: ITEM-STDIN$ ( n -- str ) {: id:n :}
   id ITEM-STDIN-BUF
   ITEM-STDIN-US id ITEM-CELL @ >str ;

: ITEM-GROUP@ ( n -- n )
   ITEM-GROUPS swap ITEM-CELL @ ;

: ITEM-KIND@ ( n -- n )
   ITEM-KINDS swap ITEM-CELL @ ;

: ITEM-ARG-OFF@ ( n -- n )
   ITEM-ARG-OFFS swap ITEM-CELL @ ;

: ITEM-ARG-COUNT@ ( n -- n )
   ITEM-ARG-COUNTS swap ITEM-CELL @ ;

: ITEM-PRELOAD-COUNT@ ( n -- n )
   ITEM-PRELOAD-COUNTS swap ITEM-CELL @ ;

: GROUP-MODE@ ( n -- n )
   GROUP-MODES swap GROUP-CELL @ ;

\ A token that must be there. Never name it `parse-name`: the wordlist is
\ case-insensitive, so a private PARSE-NAME shadows the engine's word for every
\ later definition in this package, and PARSE-ARGS' end-of-input branch then
\ answers E-STR-BOUNDS instead of the row refusal.
: REQUIRED-NAME ( -- str )
   parse-name dup STR:LEN 0= if drop E-STR-BOUNDS throw then ;

: RESERVED-NAME? ( str -- bool ) {: s:str :}
   s s" GROUP"       STR:=CI
   s s" SUITE"       STR:=CI or
   s s" SUITE-STDIN" STR:=CI or
   s s" ;GROUP"      STR:=CI or
   s s" ;SUITE"      STR:=CI or
   s s" SEQ"         STR:=CI or
   s s" PARA"        STR:=CI or ;

: MODE-OF ( str -- n ) {: s:str :}
   s s" SEQ"  STR:=CI if GROUP-SEQUENTIAL else
   s s" PARA" STR:=CI if GROUP-PARALLEL else
      E-SUITE-MODE throw
   then then ;

: CHECK-NAME ( str -- str )
   dup STR:LEN 0= if drop E-SUITE-NAME throw then
   dup RESERVED-NAME? if drop E-SUITE-NAME throw then ;

: MODE-TOKEN ( -- n )
   REQUIRED-NAME MODE-OF ;

: NAME-TOKEN ( -- str )
   REQUIRED-NAME CHECK-NAME ;

: ;SUITE? ( str -- bool )              \ suite terminator: qualified TEST:;SUITE or bare ;SUITE under `using TEST`
   dup s" TEST:;SUITE" STR:= if drop TRUE exit then
   s" ;SUITE" STR:= ;

: UNQUALIFIED ( str -- str ) {: s:str :}
   s STR:LEN 5 > if s STR:DATA 5 >str s" TEST:" STR:=CI if s STR:DATA 5 + s STR:LEN 5 - >str exit then then
   s ;

\ A row opener, bare or TEST:-qualified. Such a token can never be an argument:
\ the row before it was never closed, so its argument list ran on into this row.
\ SEQ/PARA are absent on purpose — they are mode tokens, reserved only as names.
: ROW-KEYWORD? ( str -- bool )
   UNQUALIFIED {: s:str :}
   s s" SUITE"          STR:=CI
   s s" SUITE-STDIN"    STR:=CI or
   s s" GROUP"          STR:=CI or
   s s" ;GROUP"         STR:=CI or ;

: ITEM-ALLOC ( -- n )
   ITEM-N @ ITEM-MAX >= if E-TBL-BOUNDS throw then
   ITEM-N @ {: id:n :}
   id 1+ ITEM-N !
   id ;

: GROUP-ALLOC ( -- n )
   GROUP-N @ GROUP-MAX >= if E-TBL-BOUNDS throw then
   GROUP-N @ {: id:n :}
   id 1+ GROUP-N !
   id ;

: GROUP-ADD ( str n -- n ) {: s:str mode:n :}
   GROUP-ALLOC {: id:n :}
   s id GROUP-NAME!
   mode GROUP-MODES id GROUP-CELL !
   id ;

: DEFAULT-GROUP ( -- )
   s" default" GROUP-PARALLEL GROUP-ADD GROUP-CUR ! ;

: ARG-ROOM ( n -- )
   ARG-U @ + ARG-CAP > if E-STR-CAPACITY throw then ;

: ARG-WRITE ( str -- ) {: s:str :}
   s STR:LEN 0 < if E-STR-BOUNDS throw then
   s STR:LEN cell + ARG-ROOM
   ARG-U @ {: off:n :}
   s STR:LEN ARGS off + !
   s STR:DATA ARGS off + cell + s STR:LEN BYTE-COPY
   off cell + s STR:LEN + ARG-U ! ;

: ITEM-ADD-ARG ( str n -- ) {: s:str id:n :}
   s ARG-WRITE
   ITEM-ARG-COUNTS id ITEM-CELL @ 1+ ITEM-ARG-COUNTS id ITEM-CELL ! ;

: ITEM-ARGS-BEGIN ( n -- ) {: id:n :}
   ARG-U @ ITEM-ARG-OFFS id ITEM-CELL !
   0 ITEM-ARG-COUNTS id ITEM-CELL !
   0 ITEM-PRELOAD-COUNTS id ITEM-CELL ! ;

: ROW-UNTERMINATED ( n -- ) {: id:n :}
   s" test: row " type id ITEM-NAME$ type s"  has no ;SUITE" type cr
   E-SUITE-ROW throw ;

: ROW-BAD-ENTRIES ( n -- ) {: id:n :}
   s" test: row " type id ITEM-NAME$ type s"  has invalid ENTRIES boundary" type cr
   E-SUITE-ROW throw ;

: ENTRY-PRESENT? ( n -- bool ) {: id:n :}
   id ITEM-ARG-COUNT@ id ITEM-PRELOAD-COUNT@ > ;

\ One token of a row's argument list. A row ends at ;SUITE and nowhere else, so
\ the end of input and a row keyword are both the missing terminator, named.
: ARG-TOKEN ( str n -- ) {: s:str id:n :}
   s ;SUITE? if
      DEF-ENTRIES @ if id ENTRY-PRESENT? 0= if id ROW-BAD-ENTRIES then then
      -1 DEF-ID ! exit
   then
   s STR:LEN 0= if id ROW-UNTERMINATED then
   s ROW-KEYWORD? if id ROW-UNTERMINATED then
   s s" --" STR:= if
      DEF-ENTRIES @ if id ENTRY-PRESENT? 0= if id ROW-BAD-ENTRIES then then
      0 0= DEF-SCRIPT !
   then
   DEF-SCRIPT @ 0= if
      s s" ENTRIES" STR:=CI if
         DEF-ENTRIES @ if id ROW-BAD-ENTRIES then
         id ITEM-ARG-COUNT@ ITEM-PRELOAD-COUNTS id ITEM-CELL !
         0 0= DEF-ENTRIES ! exit
      then
   then
   s id ITEM-ADD-ARG ;

: PARSE-ARGS ( n -- ) {: id:n :}
   0 DEF-ID !
   0 DEF-SCRIPT !
   0 DEF-ENTRIES !
   begin DEF-ID @ 0= while
      parse-name id ARG-TOKEN
   repeat ;

: ITEM-ADD ( n str str -- ) {: kind:n name:str inptr:str :}
   ITEM-ALLOC {: id:n :}
   name id ITEM-NAME!
   kind ITEM-KINDS id ITEM-CELL !
   GROUP-CUR @ ITEM-GROUPS id ITEM-CELL !
   inptr id ITEM-STDIN!
   0 ITEM-CHOSEN id ITEM-CELL !
   id ITEM-ARGS-BEGIN
   id PARSE-ARGS ;

: SEQUENTIAL? ( n -- bool )
   ITEM-GROUP@ GROUP-MODE@ GROUP-SEQUENTIAL = ;

: ITEM-ARGS-FEED ( n -- ) {: id:n :}
   id ITEM-ARG-OFF@ ARG-SCAN !
   id ITEM-ARG-COUNT@ 0 ?do
      ARGS ARG-SCAN @ + @ {: u:n :}
      ARGS ARG-SCAN @ + cell + u >str ARG+
      ARG-SCAN @ cell + u + ARG-SCAN !
   loop ;

: ITEM-RUN-FILE ( n -- ) {: id:n :}
   ARGS-BEGIN
   id ITEM-ARGS-FEED
   id ITEM-NAME$ RUNNER ;

: ITEM-RUN-STDIN ( n -- ) {: id:n :}
   ARGS-BEGIN
   id ITEM-ARGS-FEED
   id ITEM-STDIN$ id ITEM-NAME$ STDIN-RUNNER ;

: ITEM-DISPATCH ( n -- ) {: id:n :}
   id ITEM-KIND@
   case
      ITEM-FILE of id ITEM-RUN-FILE endof
      ITEM-STDIN of id ITEM-RUN-STDIN endof
      E-TBL-FIELD throw
   endcase ;

: GROUP-HEADER? ( n -- bool ) {: gid:n :}
   gid LAST-GROUP @ <> ;

: GROUP-HEADER ( n -- ) {: gid:n :}
   gid GROUP-HEADER? if
      gid GROUP-MODE@ GROUP-SEQUENTIAL = if DRAIN then
      s" GROUP: " type gid GROUP-NAME$ type
      gid GROUP-MODE@ GROUP-SEQUENTIAL = if s"  (sequential)" else s"  (parallel)" then
      type cr
      gid LAST-GROUP !
   then ;

: ITEM-RUN ( n -- ) {: id:n :}
   id ITEM-GROUP@ GROUP-HEADER
   id SEQUENTIAL? if DRAIN then
   id ITEM-DISPATCH
   ITEM-RAN @ 1+ ITEM-RAN !
   id SEQUENTIAL? if DRAIN then ;

: RUNS? ( n -- bool )
   {: id:n :}
   CHOSEN? @ 0= if TRUE exit then
   ITEM-CHOSEN id ITEM-CELL @ 0<> ;

: RUN-BODY ( -- )
   -1 LAST-GROUP !
   ITEM-N @ 0 ?do
      i RUNS? if i ITEM-RUN then
   loop
   DRAIN ;

: RUN-ACT ( -- )
   SETUP
   [: RUN-BODY ;] catch {: rc:n :}
   rc TEARDOWN
   rc 0 <> if rc throw then ;

public

EXPORT ITEM-MAX
EXPORT ITEM-NAME$

: SETUP! ( [ -- ] -- )
   is SETUP ;

: TEARDOWN! ( [ n -- ] -- )
   is TEARDOWN ;

: DRAIN! ( [ -- ] -- )
   is DRAIN ;

: ARGS-BEGIN! ( [ -- ] -- )
   is ARGS-BEGIN ;

: ARG+! ( [ str -- ] -- )
   is ARG+ ;

: RUNNER! ( [ str -- ] -- )
   is RUNNER ;

: STDIN-RUNNER! ( [ str str -- ] -- )
   is STDIN-RUNNER ;

: ITEMS-REGISTERED ( -- n )
   ITEM-N @ ;

: ITEMS-RUN ( -- n )
   ITEM-RAN @ ;

\ Mark every item with this name; answer whether one has it. Once any is marked,
\ RUN and VISIT-LOAD-FILES skip the unmarked ones.
: ONLY ( str -- bool )
   {: s:str :}
   TRUE 0=
   ITEM-N @ 0 ?do
      i ITEM-NAME$ s STR:= if
         TRUE ITEM-CHOSEN i ITEM-CELL !
         TRUE CHOSEN? !
         drop TRUE
      then
   loop ;

\ Visit the source files before a row's script separator. The registry is the
\ authority for what the adapter passes after --load; the same file can belong
\ to several rows with different argv.
: VISIT-ROW-FILES ( n [ n bool str -- ] -- ) {: id:n visit :}
   id ITEM-ARG-OFF@
   id ITEM-ARG-COUNT@ 0 ?do
      dup ARGS + @ {: u:n :}
      dup ARGS + cell + u >str s" --" STR:= if drop unloop exit then
      dup ARGS + cell + {: path:^ :}
      id i id ITEM-PRELOAD-COUNT@ >= path u >str visit execute
      cell + u +
   loop drop ;

: VISIT-LOAD-FILES ( [ n bool str -- ] -- ) {: visit :}
   ITEM-N @ 0 ?do i RUNS? if i visit VISIT-ROW-FILES then loop ;

: DEFAULTS ( -- )
   [: NOOP ;] SETUP!
   [: drop ;] TEARDOWN!
   [: NOOP ;] DRAIN!
   [: NOOP ;] ARGS-BEGIN!
   [: ARG-DROP ;] ARG+!
   [: RUN-MISSING ;] RUNNER!
   [: STDIN-RUN-MISSING ;] STDIN-RUNNER! ;

: RESET ( -- )
   0 ITEM-N !
   0 ITEM-RAN !
   0 CHOSEN? !
   0 GROUP-N !
   0 ARG-U !
   DEFAULT-GROUP ;

\ `TEST:GROUP SEQ|PARA name` ... `TEST:;GROUP`; PARA rows start without waiting
\ for each other, so a row that needs an idle pool belongs in a SEQ group.
: GROUP ( -- )
   MODE-TOKEN {: mode:n :}
   NAME-TOKEN {: s:str :}
   s mode GROUP-ADD GROUP-CUR ! ;

: ;GROUP ( -- )
   0 GROUP-CUR ! ;

: SUITE ( -- )
   REQUIRED-NAME {: name:str :}
   ITEM-FILE name s" " ITEM-ADD ;

: SUITE-STDIN ( -- )
   REQUIRED-NAME {: name:str :}
   REQUIRED-NAME {: inptr:str :}
   ITEM-STDIN name inptr ITEM-ADD ;

: ;SUITE ( -- )
;

\ What each opener reads, so the source pre-verifier checks what follows it
\ (docs/forth-card.md § 3).
parses: GROUP 2
parses-through: SUITE 1 ( ;SUITE TEST:;SUITE )
parses-through: SUITE-STDIN 2 ( ;SUITE TEST:;SUITE )

\ Called once. Offers rows to their runner in registration order between SETUP
\ and TEARDOWN, which receives the run's throw code; RUN rethrows it. DRAIN runs
\ on entry to a SEQ group, before and after each of its rows, and once at the
\ end. A row whose arguments reach another row keyword or the end of input
\ throws E-SUITE-ROW (`test: row <name> has no ;SUITE`).
: RUN ( -- )
   RUN-ACT ;

DEFAULTS
RESET

;package
