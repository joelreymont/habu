\ code-reclaim.f - which bytes of the code arena a reclamation may hand back.
\ One concern: the CODE-RECLAIM package in src/habu/xref.f and the floor
\ FORGET-DEFS-FROM computes for it.
\
\ WHAT THIS SUITE HAS TO SHOW, AND WHY IT TAKES A WHOLE FILE. A FORGET retires a
\ suffix of the dictionary RECORD array and hands back a suffix of the CODE
\ arena, and those are two different suffixes. They coincide only while every
\ routine was written in the order its record was made, and this system has two
\ ordinary ways to break that:
\
\   - an `EXPORT` alias publishes a LATE record whose start is an EARLY
\     routine - everything defined between the two sits above it;
\   - a republication (src/compiler/native/publish.f) writes a word's new
\     routine at the top of the arena and leaves its record where it was, so an
\     EARLY record points at LATE code.
\
\ Reading the floor off the record the sweep starts at therefore answered the
\ wrong address in both directions. The alias case is the one that could be
\ shown with nothing but the engine: forgetting the alias handed back the two
\ live words underneath it, the address-keyed records correctly dropped their
\ rows for a span they were told was free, and calling either word afterwards
\ ran whatever the next definition had been compiled into. The republished case
\ needs the chain to build, and is the same defect from the other side.
\
\ SO EVERY CASE HERE ENDS BY CALLING THE WORD. Not "the record still points
\ somewhere" - the record pointed at the right address the whole time, at bytes
\ that had stopped being its routine's. What says the code survived is compiling
\ more definitions over the space the engine now believes is free and then
\ entering the word.
\
\ AND TWO CASES SAY THE FLOOR DID NOT SIMPLY STOP MOVING. A reclamation that
\ never reclaims passes every safety case in this file, so an ordinary forget is
\ measured for the exact address it gives back, and a REPUBLISHED word that is
\ itself forgotten is measured for the same thing plus the publication log row
\ that goes with it.

require lib/test.f
require src/habu/layout.f
require src/compiler/native/publish.f
require test/compiler/native-source-fixture.f
require test/compiler/native-chain-fixture.f

package CRECL-TEST

private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is the only way to compile a definition - a subject, or a filler written over
\ the space a reclamation released - from inside a test.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

4 constant REGS
4 constant INSN-BYTES
0 constant GLOBAL-WID             \ the wordlist an evaluated definition lands in

\ ---- reading the dictionary --------------------------------------------------
: REC ( ptr u8 n -- ptr a )
   XREF-FIND
   dup XREF-FOUND? 0= if s" code-reclaim: subject not found" 76 die then ;

: REC-START ( ptr u8 n -- n )
   REC XREF-START ;

\ The first address past a record's routine. The engine records a length that
\ excludes the trailing return (src/compiler/native/publish.f says why), so the
\ end of the routine is one instruction past what the record reports.
: REC-END ( ptr u8 n -- n )
   REC dup XREF-START swap XREF-LEN + INSN-BYTES + ;

: REC-INDEX ( ptr u8 n -- n )
   XREF-FIND-INDEX ;

: DEFINED? ( ptr u8 n -- bool )
   XREF-FIND XREF-FOUND? ;

\ ---- writing over whatever the engine believes is free -----------------------
\ Four definitions rather than one: the routines this file protects are a few
\ instructions long, so one filler could stop short of the second of them and
\ the case would pass on an accident of size. Each one does real arithmetic so
\ the engine cannot fold it to a single literal push.
: FILL ( -- )
   s" : CR-F1 ( n -- n ) dup 3 * swap 7 + + ;" EV
   s" : CR-F2 ( n -- n ) dup 5 * swap 9 + + ;" EV
   s" : CR-F3 ( n -- n ) dup 11 * swap 13 + + ;" EV
   s" : CR-F4 ( n -- n ) dup 17 * swap 19 + + ;" EV ;

: FILL-FORGET ( -- )
   s" CR-F1" FORGET-DEFS-FROM ;

\ ---- the emission a republication is made out of -----------------------------
\ Copied in shape from test/compiler/native-publish.f, which is where this way
\ of driving the chain is stated: a line of source under the convention a Habu
\ word is entered through, one argument out of data-stack slot zero of the
\ caller's stack and one result back into it.
here CELL 1- and CELL swap - CELL 1- and allot
1 TYPED-BUFFER R-CTX IR-CTX:ctx
1 TYPED-BUFFER R-BLD IR-BUILD:builder

: CC ( -- IR-CTX:ctx )        0 R-CTX @ ;
: BB ( -- IR-BUILD:builder )  0 R-BLD @ ;

128 BUFFER: EM-NAME
variable EM-U

: EM-NAME$ ( -- ptr u8 n )
   EM-NAME EM-U @ ;

: EM-NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   a EM-NAME u STR-LEN BYTE-COPY-LEN
   u EM-U ! ;

\ `NAME dup +` doubles its argument. Every subject below is defined to multiply
\ instead, so the answer alone says which routine the interpreter entered.
128 BUFFER: EM-TEXT
variable EM-TU

: EM-TEXT$ ( -- ptr u8 n )
   EM-TEXT EM-TU @ ;

: EM-TEXT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a EM-TEXT EM-TU @ + u STR-LEN BYTE-COPY-LEN
   EM-TU @ u + EM-TU ! ;

: EM-TEXT! ( -- )
   0 EM-TU !
   EM-NAME$ EM-TEXT+
   s"  dup +" EM-TEXT+ ;

: COMPILE-DOUBLE ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c NSRC:HIR-BUILDER 0 R-BLD !
   CC BB NSRC:MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   EM-TEXT$ NSRC:TEXT!
   CC BB NSRC:TAPE {: tp:IR-ARENA:arena :}
   CC NSRC:LEX
   tp NTAPE:SEAL {: v:IR-ARENA:view :}
   CC BB v p r 1 1 NELAB:COLON drop
   CC BB NSRC:TEXT$ 0 REGS 1 1 NFIX:RUN-HABU ;

: REPUBLISH-BODY ( IR-CTX:ctx -- )
   COMPILE-DOUBLE
   EM-NAME$ GLOBAL-WID NPUB:REPUBLISH ;

\ Republish the named word with a routine that doubles. The whole chain runs
\ inside one context, which is how src/compiler/native/migrate.f drives it.
: REPUBLISH ( ptr u8 n -- )
   EM-NAME!
   EM-TEXT!
   NFIX:BINDING [: REPUBLISH-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- case one: a late record whose routine is early --------------------------
\ The alias record is the last one this file publishes, so the sweep that starts
\ at it retires nothing else at all. Its start is CR-LOW's routine, and CR-HIGH
\ sits above that address - so a floor read off the alias hands back both.
variable A-CP
variable A-LOW
variable A-HIGH
variable A-END

: ALIAS-DIVERGES ( -- )
   s" the alias record starts at the routine of the word it re-exports" T-LABEL
   s" CRECL-ALIAS:CR-LOW" REC-START  s" CRECL-SUBJ:CR-LOW" REC-START  T=

   s" and it is a later record than the word compiled after that one" T-LABEL
   s" CRECL-ALIAS:CR-LOW" REC-INDEX  s" CRECL-SUBJ:CR-HIGH" REC-INDEX  >  TTRUE
   s" CRECL-SUBJ:CR-HIGH" REC-START  s" CRECL-SUBJ:CR-LOW" REC-START  >  TTRUE ;

: ALIAS-CASE ( -- )
   ALIAS-DIVERGES
   cp@ A-CP !
   s" CRECL-SUBJ:CR-LOW" REC-START A-LOW !
   s" CRECL-SUBJ:CR-HIGH" REC-START A-HIGH !
   s" CRECL-SUBJ:CR-HIGH" REC-END A-END !

   s" CRECL-ALIAS:CR-LOW" FORGET-DEFS-FROM

   s" forgetting the alias retires it" T-LABEL
   s" CRECL-ALIAS:CR-LOW" DEFINED? TFALSE

   s" and hands back no byte of the routines that are still somebody's" T-LABEL
   cp@ A-END @ >= TTRUE
   cp@ A-CP @ T=

   s" the two records still point where they did" T-LABEL
   s" CRECL-SUBJ:CR-LOW" REC-START A-LOW @ T=
   s" CRECL-SUBJ:CR-HIGH" REC-START A-HIGH @ T=

   FILL

   \ Through the interpreter, which enters the address the record holds. A
   \ compiled call site would not say this: both routines are small enough for
   \ src/compiler/native/inline.f to copy into a caller, so a call written here
   \ would answer out of this word's own body and never reach the arena at all.
   s" and both words still run their own code" T-LABEL
   s" CRECL-SUBJ:CR-LOW" EV-N 11 T=
   s" CRECL-SUBJ:CR-HIGH" EV-N 22 T=

   FILL-FORGET ;

\ ---- case two: an early record whose routine is late -------------------------
\ CR-REP is defined before CR-GONE and republished afterwards, so its record is
\ the earlier of the two and its routine is above the other's. The sweep starts
\ at CR-GONE, whose start is below the routine CR-REP now runs.
variable R-CP
variable R-START
variable R-END

: REPUB-DEFINE ( -- )
   s" : CR-REP ( n -- n ) 3 * ;" EV
   s" : CR-GONE ( -- n ) 6 ;" EV ;

: REPUB-DIVERGES ( -- )
   s" a republished word keeps its record and moves its routine" T-LABEL
   s" CR-REP" REC-INDEX  s" CR-GONE" REC-INDEX  <  TTRUE
   s" CR-REP" REC-START  s" CR-GONE" REC-START  >  TTRUE

   s" and the record it kept is the routine the interpreter enters" T-LABEL
   s" 5 CR-REP" EV-N 10 T= ;

: REPUB-CASE ( -- )
   REPUB-DEFINE

   s" before the republication the word runs the engine's own code" T-LABEL
   s" 5 CR-REP" EV-N 15 T=

   s" CR-REP" REPUBLISH
   REPUB-DIVERGES

   cp@ R-CP !
   s" CR-REP" REC-START R-START !
   s" CR-REP" REC-END R-END !

   s" CR-GONE" FORGET-DEFS-FROM

   s" forgetting the word below it retires that word" T-LABEL
   s" CR-GONE" DEFINED? TFALSE

   s" and hands back no byte of the republished routine" T-LABEL
   cp@ R-END @ >= TTRUE
   cp@ R-CP @ T=

   s" the record still points at the routine that was published for it" T-LABEL
   s" CR-REP" REC-START R-START @ T=

   FILL

   s" and the word still runs it" T-LABEL
   s" 5 CR-REP" EV-N 10 T=

   FILL-FORGET ;

\ ---- case three: the retired word's own routine still comes back -------------
\ The safety cases above are all passed by a reclamation that never reclaims, so
\ this is the other half: a REPUBLISHED word that is itself forgotten gives back
\ the routine that was published for it, to the byte, and the publication log
\ row that describes that routine goes with it.
variable S-ROWS
variable S-START

: RETIRED-CASE ( -- )
   s" : CR-SELF ( n -- n ) 3 * ;" EV
   s" CR-SELF" REPUBLISH

   s" the seam logged the republication" T-LABEL
   s" CR-SELF" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   NPUB:REPUBLISHED S-ROWS !
   s" CR-SELF" REC-START S-START !

   s" CR-SELF" FORGET-DEFS-FROM

   s" forgetting it gives its routine back to the byte" T-LABEL
   cp@ S-START @ T=

   s" and the row that described that routine goes with it" T-LABEL
   NPUB:REPUBLISHED S-ROWS @ 1- T=
   s" CR-SELF" GLOBAL-WID NPUB:REPUBLISHED? TFALSE ;

\ ---- case four: an ordinary forget is unchanged ------------------------------
\ Nothing here is republished and nothing is aliased, so record order and code
\ order agree and the floor is the retired word's own start. This is the case
\ every FORGET in the tree is, and it is measured for the exact address.
variable P-START

: PLAIN-CASE ( -- )
   s" : CR-P1 ( -- n ) 1 ;" EV
   s" : CR-P2 ( -- n ) 2 ;" EV
   s" : CR-P3 ( -- n ) 3 ;" EV
   s" CR-P2" REC-START P-START !

   s" CR-P2" FORGET-DEFS-FROM

   s" an ordinary forget still gives back the retired word's own routine"
   T-LABEL
   cp@ P-START @ T=

   s" and retires it and everything after it" T-LABEL
   s" CR-P2" DEFINED? TFALSE
   s" CR-P3" DEFINED? TFALSE

   s" while the word below it is untouched" T-LABEL
   s" CR-P1" DEFINED? TTRUE
   s" CR-P1" EV-N 1 T= ;

\ ---- case five: the refusal that makes the rule structural -------------------
\ Every caller of TRUNCATE computes its floor from something else, so the floor
\ is held against the records that survive it rather than taken on the caller's
\ word. A floor at a live routine's own start is the smallest such mistake.
\ THE FLOOR IT ASKS FOR IS CR-REP'S OWN START, which is the exact address the
\ defect handed back: a live word, republished, with a publication log row of
\ its own. So the refusal is measured on all three things a reclamation moves -
\ the pointer, the rows keyed to the span, and the routine itself.
variable G-CP
variable G-ROWS

: REFUSE-BODY ( -- )
   s" CR-REP" REC-START CODE-RECLAIM:TRUNCATE ;

: REFUSE-CASE ( -- )
   cp@ G-CP !
   NPUB:REPUBLISHED G-ROWS !

   s" the routine the refusal names is live, and a record is keyed to it"
   T-LABEL
   s" CR-REP" DEFINED? TTRUE
   s" CR-REP" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   s" a floor at a surviving routine's start is refused by name" T-LABEL
   [: REFUSE-BODY ;] CODE-RECLAIM:E-LIVE TTHROWSQ

   s" a floor above the free slot is still the other refusal" T-LABEL
   [: cp@ INSN-BYTES + CODE-RECLAIM:TRUNCATE ;] CODE-RECLAIM:E-FLOOR TTHROWSQ

   s" neither refusal moved the pointer" T-LABEL
   cp@ G-CP @ T=

   s" nor told a watcher: the row keyed to that routine is untouched" T-LABEL
   NPUB:REPUBLISHED G-ROWS @ T=
   s" CR-REP" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   s" and the routine still answers" T-LABEL
   s" 5 CR-REP" EV-N 10 T= ;

public

: RUN ( -- )
   T-RESET
   ALIAS-CASE
   REPUB-CASE
   RETIRED-CASE
   PLAIN-CASE
   REFUSE-CASE
   T-REPORT ;

;package

\ ---- the two words an alias record is published below ------------------------
\ They are a package's public words because `EXPORT` re-exports one, and they
\ answer different numbers because the whole point of the last assertion is
\ which code the interpreter reached.
\
\ THEY ARE COMPILED HERE, LAST, and not at the top of the file, so that the span
\ a wrong floor gives back is exactly these two routines. Compiled first they
\ would sit under the suite's own words, a wrong floor would hand back the
\ harness along with them, and what the case reported would be whichever word of
\ itself the fillers happened to land on rather than the subject.
package CRECL-SUBJ

public

: CR-LOW ( -- n )
   11 ;

: CR-HIGH ( -- n )
   22 ;

;package

\ The alias record. It is published HERE, after every other record this file
\ makes, so the sweep that starts at it retires nothing but itself - which is
\ what leaves the case about the routines UNDER it and not about the records
\ over it.
package CRECL-ALIAS

public

EXPORT CRECL-SUBJ:CR-LOW

;package

CRECL-TEST:RUN
