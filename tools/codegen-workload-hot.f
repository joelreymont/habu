\ codegen-workload-hot.f - the words the workloads run hot, the data they run
\ over, and the two ways each of them is put into the dictionary. One concern:
\ the subjects.
\
\ EVERY SUBJECT IS ONE STRING, COMPILED TWICE. A body is written down once here
\ and handed to two publishers: the engine's own interpret path, and
\ src/compiler/native/migrate.f, which drives the native chain over the tape the
\ checker filled while it certified that same text. So the two columns a
\ measurement compares are not two programs that resemble each other - they are
\ ONE program, and the only difference between the arms is which code generator
\ produced the machine code behind it. Nothing here writes a body out twice, and
\ there is therefore no way for the two arms to drift apart in a way a reader
\ would have to notice by eye.
\
\ THREE PUBLICATIONS AND WHY THERE ARE THREE.
\
\   HOT-ENGINE  the subjects as the engine compiles them. The before-arm's
\               drivers are compiled against these.
\   HOT-CHAIN   the same strings, compiled by the native chain and republished
\               through the publication seam. The after-arm's drivers are
\               compiled against these.
\   HOT-FIXED   the same strings again, compiled by the engine and never
\               migrated. This is the CONTROL. Its two drivers are compiled at
\               exactly the two moments the real arms' drivers are - one before
\               the migration and one after - so everything that changed between
\               those two moments and is NOT the code of the subject words lands
\               on the control row: the code region grew, every later definition
\               moved, the caches hold different things, and the host has had
\               longer to do whatever it was going to do. A control row whose
\               delta is as big as a workload row's would mean the harness can
\               manufacture a delta out of nothing, and the report would be
\               worthless.
\
\ AND ONE PUBLICATION THAT IS NOT A SUBJECT AT ALL: HOT-REACH, the checker's own
\ fold, migrated so that the call instructions already in this binary can be
\ moved onto it. It is timed by nothing; what it changes is the code the
\ compile-shaped workload's after-arm runs INSIDE the checker. Its own section
\ is at the end of this file.
\
\ THE DRIVERS ARE ONE STRING EACH, TOO, AND THE ARMS DIFFER ONLY IN THE SEARCH
\ ORDER THEY ARE COMPILED UNDER. A driver body names its subjects BARE, and the
\ file that runs the choreography opens `using HOT-ENGINE` around the before-arm
\ and `using HOT-CHAIN` around the after-arm. So the two drivers are compiled
\ from identical text, by the same engine, in the same process, over the same
\ data; what differs is which record the bare name resolves to at the moment the
\ call site is compiled. That is the fairest form of the comparison available:
\ there is no second copy of the body to keep in step, and no argument to make
\ about whether two texts are the same.
\
\ THE DEFINITION'S NAME IS NOT PART OF ITS BODY. Each driver is published under a
\ different name, because two records with one name in one wordlist is a
\ duplicate definition and the engine refuses it. A name lives in the dictionary
\ record; the compiled code does not contain it, and the machine code of two
\ definitions with the same body and different names is the same code. The
\ acceptance suite checks that directly, by comparing the byte counts of the two
\ arms' drivers.
\
\ WHY THESE FOUR SUBJECTS. They are the surveyed hot words of
\ tools/codegen-compare-corpus2.f, chosen here for the three DIFFERENT ways the
\ engine's compile-time inliner treats them - because that, and not the code
\ generator, is what decides whether a caller ever reaches the new code at all:
\
\   FOLD-C    src/core/checker.f:3542's SYM-FOLD-C, run once per byte of every
\             symbol the checker compares. Its body has branches, so the engine
\             will never copy it into a caller: it is a real call in both arms,
\             and a workload that calls it measures the two code generators with
\             no inlining in the way.
\   COUNT-CH  lib/string.f:103's COUNT-CHAR, a loop carrying two values over a
\             byte span. Also a real call in both arms, and the whole scan is
\             INSIDE it - so a workload that calls it once per buffer spends
\             almost all of its time in migrated code, which is the opposite
\             extreme from FOLD-C. Nothing under src/ calls COUNT-CHAR: its
\             callers are lib/string-test.f, lib/build-cache-test.f and the
\             codegen corpora. It is here because of its SHAPE - the whole loop
\             inside the callee - and not because the system spends time in it.
\   TERM-TAG  src/core/checker.f:152's TAG, and
\   TERM-PAY  its sibling PAY. One mask and one shift, no branches, small enough
\             that the engine copies the body into every caller it compiles and
\             emits no call at all. A workload over these measures whether a
\             migration reaches a word the engine inlines - and it does, for
\             callers compiled AFTER the migration, because what gets copied is
\             whatever the record holds at that moment.
\
\ THE DATA IS GENERATED, NOT COMMITTED. A buffer of a few kilobytes is
\ regenerable, and a regenerable artifact does not belong in the repository. It
\ is filled by a linear congruential sequence from a fixed seed, so it is the
\ same bytes on every host and in every run, and the acceptance suite pins its
\ checksum - which is what makes "the two arms computed the same answer" a
\ statement about the code rather than about the data.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require src/compiler/native/migrate.f
require src/compiler/native/reach.f

package CODEGEN-HOT

public

-7225 constant E-WLHOT-CAP      \ a rendered definition, or a generated batch, longer than its buffer

\ ---- the one boundary this work needs ---------------------------------------
\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is the engine's own interpret path: a definition handed to it is compiled and
\ published exactly as a definition typed at top level is. There is no checked
\ construction that puts a definition through that path, which is why
\ src/compiler/native/migrate.f carries the same boundary under the same name for
\ the same reason. Dot habu-parse-a-migrated-b38a83d9 is the capability that
\ retires both: a definer that reads a definition out of the input stream.
\
\ It is PUBLIC because the acceptance suite has definitions of its own to publish
\ through the engine's compile path - fixtures for the inline rule - and a second
\ `evaluate` wrapper over there would be a second unchecked surface for one need.
\ One boundary, both callers named here.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

private

\ ---- the subjects, one string each ------------------------------------------
\ Each is its surveyed body with its integer literals written in decimal, which
\ is the one liberty tools/codegen-compare-migrated2.f documents and measures:
\ the tape records a literal by reading its spelling back with the stdlib's
\ decimal reader, and that reader declines a hexadecimal spelling (dot
\ habu-record-the-engine-79c570ed). The engine compiles a named constant and the
\ number behind it to byte-identical code, so this changes the spelling and not
\ the program.

: FOLD-C$ ( -- ptr u8 n )
   s" : FOLD-C ( n -- n ) {: c:n :} c 65 < if c exit then c 90 > if c exit then c 32 or ;" ;

: COUNT-CH$ ( -- ptr u8 n )
   s" : COUNT-CH ( ptr u8 n n -- n ) {: a:ptr u:n c:n :} 0 0 begin dup u < while dup a + c@ c = if swap 1+ swap then 1+ repeat drop ;" ;

: TERM-TAG$ ( -- ptr u8 n )
   s" : TERM-TAG ( n -- n ) 7 and ;" ;

: TERM-PAY$ ( -- ptr u8 n )
   s" : TERM-PAY ( n -- n ) 3 rshift ;" ;

\ The register budgets the chain is given for each. They are budgets, and
\ choosing one from the routine instead of from a line here is dot
\ habu-choose-the-register-a95390ac. The two wide ones are
\ tools/codegen-compare-migrated2.f's measured floors for the same two bodies:
\ a body with control flow holds a register for every value carried across an
\ edge, and COUNT-CH carries a counter and a cursor.
4 constant REGS
8 constant BRANCH-REGS
12 constant WIDE-REGS

public

\ Publish all four the way the engine publishes any definition. Whichever
\ wordlist is open when this runs is where they land, which is how one set of
\ strings becomes two engine-compiled packages.
: PUBLISH-ENGINE ( -- )
   FOLD-C$ EV
   COUNT-CH$ EV
   TERM-TAG$ EV
   TERM-PAY$ EV ;

\ Publish all four through the native chain. Same strings, same order, same
\ wordlist rule: a migration lands where the current wordlist points when it
\ runs, so the caller opens the package it wants them in.
: PUBLISH-CHAIN ( -- )
   FOLD-C$ 1 1 BRANCH-REGS NMIGRATE:DEFINE
   COUNT-CH$ 3 1 WIDE-REGS NMIGRATE:DEFINE
   TERM-TAG$ 1 1 REGS NMIGRATE:DEFINE
   TERM-PAY$ 1 1 REGS NMIGRATE:DEFINE ;

\ ---- the checker's OWN fold, and its callers ---------------------------------
\ The four subjects above are a program this file publishes twice in order to
\ time it. This one is not: SYM-FOLD-C is a word of the engine bin/hb is running
\ (src/core/checker.f), run once per byte of every symbol the checker compares,
\ and its callers are the checker's own - compiled into the binary and never
\ recompiled. That is why the compile-shaped workload used to report nothing
\ whatever the chain did: republishing a word reaches the callers compiled after
\ it, and the checker's callers all came first.
\
\ SO THE BODY IS MIGRATED AND THE CALL SITES ARE MOVED. The definition cannot be
\ published under the checker's own record - a tail may be defined once in a
\ wordlist, so the engine refuses a second SYM-FOLD-C where the first one lives
\ - so it is migrated into a package of its own and
\ src/compiler/native/reach.f moves every call instruction that entered the old
\ code onto the new routine. The seam refuses if it finds no site to move, so
\ this cannot quietly do nothing.
\
\ The source is checker.f's body word for word, with its three hexadecimal
\ literals written in decimal for the reason the four subjects above are: the
\ tape reads a literal's spelling back with the stdlib's decimal reader (dot
\ habu-record-the-engine-79c570ed), and the engine compiles $41 and 65 to the
\ same code.
: CHECKER-FOLD$ ( -- ptr u8 n )
   s" : SYM-FOLD-C ( n -- n ) {: c:n :} c 65 < if c exit then c 90 > if c exit then c 32 or ;" ;

variable REACHED-N

: PUBLISH-CHECKER-FOLD ( -- )
   CHECKER-FOLD$ 1 1 BRANCH-REGS NMIGRATE:DEFINE ;

\ Move the checker's own call sites onto it. The count is kept because it is the
\ fact the compile-shaped row's delta rests on: nothing about that row means
\ anything if this is zero, and the acceptance suite reads it rather than
\ trusting the report's prose.
: REACH-CHECKER-FOLD ( -- )
   s" SYM-FOLD-C" s" HOT-REACH:SYM-FOLD-C" NREACH:REDIRECT REACHED-N ! ;

: REACHED ( -- n )
   REACHED-N @ ;

private

\ ---- the data ---------------------------------------------------------------
\ A linear congruential sequence, the one every textbook writes down, kept inside
\ 31 bits so the values are the same on any host with 64-bit cells. The byte
\ buffer is filled with printable bytes across the case boundary FOLD-C turns on,
\ and the cell buffer with whole words, because a type term is a whole cell whose
\ low three bits are its tag.
1103515245 constant LCG-MUL
12345 constant LCG-ADD
$7FFFFFFF constant LCG-MASK
20260803 constant SEED0

variable SEED

: NEXT ( -- n )
   SEED @ LCG-MUL * LCG-ADD + LCG-MASK and
   dup SEED ! ;

public

4096 constant BYTE-N
2048 constant TERM-N

private

create BYTE-BUF BYTE-N allot
create TERM-BUF TERM-N cells allot

32 constant PRINTABLE-LO
95 constant PRINTABLE-N

public

: FILL-DATA ( -- )
   SEED0 SEED !
   BYTE-N 0 ?do
      NEXT 7 rshift PRINTABLE-N mod PRINTABLE-LO +  i BYTE-BUF + c!
   loop
   TERM-N 0 ?do
      NEXT  i cells TERM-BUF + !
   loop ;

\ The two buffers, as the spans a driver takes. A `create`d word is a `ptr a` so
\ that nothing does arithmetic on it by accident; naming the span it stands for
\ is what these two are for, and the driver bodies below index them the way
\ lib/string.f and lib/vector.f index theirs.
: BYTES$ ( -- ptr u8 n )
   BYTE-BUF BYTE-N ;

: TERMS$ ( -- ptr n n )
   TERM-BUF TERM-N ;

\ A checksum over the generated data, so the acceptance suite can pin the bytes
\ the two arms agree about rather than only the agreement.
: BYTE-SUM ( -- n )
   0
   BYTE-N 0 ?do
      i BYTE-BUF + c@ +
   loop ;

: TERM-SUM ( -- n )
   0
   TERM-N 0 ?do
      i cells TERM-BUF + @ +
   loop ;

private

\ ---- rendering one definition -----------------------------------------------
512 constant DEF-CAP
create DEF-BUF DEF-CAP allot
variable DEF-U

: DEF-RESET ( -- )
   0 DEF-U ! ;

: DEF+ ( ptr u8 n -- ) {: a:ptr u:n :}
   DEF-U @ u + DEF-CAP > if E-WLHOT-CAP throw then
   a  DEF-BUF DEF-U @ +  u STR-LEN BYTE-COPY-LEN
   DEF-U @ u + DEF-U ! ;

public

\ Publish one driver: the definer, the name the caller chose, and the body. The
\ body is the caller's string and is never copied into this file, so the two arms
\ of a workload are compiled from one text.
: DEFINE-AS ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n ba:ptr bu:n :}
   DEF-RESET
   s" : " DEF+
   na nu DEF+
   ba bu DEF+
   DEF-BUF DEF-U @ EV ;

\ ---- the three workload bodies ----------------------------------------------
\ Each is the text that follows the definition's name, so a caller renders it
\ under whatever name it wants and the body is the same characters every time.

\ Every byte of the buffer through FOLD-C, from a loop the OLD emitter compiled
\ in both arms. This is the shape the checker's symbol comparison really has -
\ a fold per byte, called from the comparison's own loop - and the migration can
\ only reach the callee.
: SCAN-BODY$ ( -- ptr u8 n )
   s"  ( ptr u8 n -- n ) {: a:ptr u:n :} 0 u 0 ?do a i + c@ FOLD-C + loop ;" ;

\ The whole buffer through COUNT-CH, once per repetition. The loop that does the
\ work is INSIDE the migrated word, so almost all of this workload's time is in
\ code the migration replaced.
: COUNT-BODY$ ( -- ptr u8 n )
   s"  ( ptr u8 n n -- n ) {: a:ptr u:n r:n :} 0 r 0 ?do a u 101 COUNT-CH + loop ;" ;

\ ---- the same workload at less than total coverage --------------------------
\ COUNT-BODY$ above spends essentially ALL of its time inside the migrated word,
\ so the delta it reports is the migrated word's own speed-up and nothing else.
\ That is one END of a curve, not a figure for what a migration is worth to a
\ program, because no real program is one call in a loop. These two bodies are
\ the middle of the curve, and they get there without a fudge factor: each does
\ the same buffer pass THREE times, and the only thing that changes between them
\ is how many of the three passes go through the subject the migration replaces.
\ The rest go through HOT-FIXED's copy - the same four strings, compiled by the
\ same engine, that the control rows use, and which no migration touches.
\
\ So the fraction of the old arm's work that the migration can reach is exactly
\ two thirds in the first and one third in the second, by construction rather
\ than by calibration: the three passes cost the same as each other in the old
\ arm, because in the old arm all three are the engine's code over the same
\ bytes. Amdahl then says the delta must come out at that fraction of the
\ all-coverage row's delta, and a reader can check the arithmetic against the
\ count row in the same table.
\
\ The unmigrated passes name HOT-FIXED's word OUTRIGHT, while the migrated ones
\ name the subject bare and are resolved by the search order the arm is compiled
\ under, exactly as every other driver here is.
: MIX66-BODY$ ( -- ptr u8 n )
   s"  ( ptr u8 n n -- n ) {: a:ptr u:n r:n :} 0 r 0 ?do a u 101 COUNT-CH + a u 101 COUNT-CH + a u 101 HOT-FIXED:COUNT-CH + loop ;" ;

: MIX33-BODY$ ( -- ptr u8 n )
   s"  ( ptr u8 n n -- n ) {: a:ptr u:n r:n :} 0 r 0 ?do a u 101 COUNT-CH + a u 101 HOT-FIXED:COUNT-CH + a u 101 HOT-FIXED:COUNT-CH + loop ;" ;

\ Every cell of the term buffer split into its tag and its payload. Both
\ subjects are small enough and straight-line enough that the engine copies them
\ into whichever driver it is compiling, so neither arm contains a call - and
\ what the row compares is the two code generators' bodies as the engine copied
\ them.
: TERM-BODY$ ( -- ptr u8 n )
   s"  ( ptr n n -- n ) {: a:ptr u:n :} 0 u 0 ?do a i cells + @ dup TERM-TAG swap TERM-PAY + + loop ;" ;

private

\ ---- the compile-shaped workload's generated source -------------------------
\ A batch of checked definitions, rendered into one source and handed to the
\ engine whole. This is the checker's own hot path at volume: every definition is
\ scanned, its declared effect parsed, its body certified and its calls resolved
\ by name - and a name lookup folds the case of every byte it compares, which is
\ what SYM-FOLD-C is for.
\
\ EACH BATCH GOES INTO A PACKAGE OF ITS OWN because a batch has to be compilable
\ more than once and one name may be defined once in one wordlist. The package
\ names differ by two characters and nothing else, so every run of the batch
\ compiles the same number of definitions from the same body text.
$4000 constant BATCH-CAP
create BATCH-BUF BATCH-CAP allot
variable BATCH-U

public

40 constant BATCH-DEFS          \ definitions in one batch

private

: B+ ( ptr u8 n -- ) {: a:ptr u:n :}
   BATCH-U @ u + BATCH-CAP > if E-WLHOT-CAP throw then
   a  BATCH-BUF BATCH-U @ +  u STR-LEN BYTE-COPY-LEN
   BATCH-U @ u + BATCH-U ! ;

: B-C ( n -- ) {: c:n :}
   BATCH-U @ 1+ BATCH-CAP > if E-WLHOT-CAP throw then
   c BATCH-BUF BATCH-U @ + c!
   BATCH-U @ 1+ BATCH-U ! ;

48 constant DIGIT-ZERO
65 constant LETTER-A

create D2 2 allot

\ Two decimal digits, so every generated name is the same length and no batch is
\ a different number of characters from another.
: B-2D ( n -- ) {: v:n :}
   v 10 / DIGIT-ZERO +  D2 c!
   v 10 mod DIGIT-ZERO +  D2 1 + c!
   D2 2 B+ ;

: B-NAME ( n -- ) {: k:n :}
   s" Q" B+  k B-2D ;

\ The first definition of a batch has no predecessor to name, so it is the one
\ with a body of its own: the fold, spelled out.
: B-FIRST ( -- )
   s" : " B+  0 B-NAME
   s"  ( n -- n ) {: c:n :} c 65 < if c exit then c 90 > if c exit then c 32 or ; " B+ ;

\ Every later definition names the one before it twice, so the batch's cost is
\ dominated by what the checker does with a call: resolve the name, read the
\ effect it published, and unify that effect against the stack it has.
: B-NEXT ( n -- ) {: k:n :}
   s" : " B+  k B-NAME
   s"  ( n -- n ) dup " B+  k 1- B-NAME
   s"  7 and swap " B+  k 1- B-NAME
   s"  3 rshift + ; " B+ ;

public

\ One batch's source, in a package named by the arm's letter and the round.
\ An arm index becomes a letter from A up, so every generated package name is six
\ characters and every batch's source is the same number of bytes whichever arm
\ compiled it. The run file hands out the indices: 0 and 1 are the two arms of
\ the real row, and the pairs above them are its null draws.
0 constant ARM-BEFORE
1 constant ARM-AFTER

: BATCH$ ( n n -- ptr u8 n ) {: arm:n round:n :}
   0 BATCH-U !
   s" package WLQ" B+
   LETTER-A arm + B-C
   round B-2D
   s"  public " B+
   B-FIRST
   BATCH-DEFS 1 ?do i B-NEXT loop
   s" ;package " B+
   BATCH-BUF BATCH-U @ ;

\ Compile one batch. This is the compile-shaped workload's whole body: the
\ engine's interpret path over a generated source, which is what `bin/hb --load`
\ does to every file it reads.
: CHECK-BATCH ( n n -- )
   BATCH$ EV ;

;package
