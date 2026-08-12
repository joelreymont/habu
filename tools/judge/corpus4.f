\ judge/corpus4.f - the judged rows of tools/codegen-compare-corpus4.f: twelve
\ shapes chosen because somebody had a reason to believe the native chain
\ handles them WORSE than the engine's emitter. One concern: which subject is
\ judged, on which pinned input, against which C twin.
\
\ WHAT THIS FILE STATES AND WHAT IT DOES NOT. It states the twelve subjects,
\ ONE pinned input per subject, the storage each subject reads, and the C symbol
\ that is its twin. It states NOTHING about the programs: the engine compiled
\ them when the corpus file was loaded, and the chain's copies are derived from
\ that same file's bytes by tools/judge/src.f and compiled by
\ tools/judge/chain.f. There is no retyped body here.
\
\ AND IT STATES NOTHING ABOUT WHAT THE CHAIN CANNOT COMPILE. The comparison this
\ replaces kept a hand-written list of those subjects, and a list cannot notice a
\ row that started refusing or one that stopped. Here the chain is asked, every
\ run, and what it answers - the routine, or the code it declined with - is the
\ row's verdict.
\
\ THE INPUT IS WRITTEN ONCE, WHICH IS THE OTHER DUPLICATE GONE. The old harness
\ writes each row's pinned input three times: tools/codegen-compare-cases4.f for
\ the engine, tools/codegen-compare-new4.f for the chain, and
\ tools/codegen-compare-c4.f for the C twin. Three copies of a number is three
\ chances to measure one column on a different program from the other two. Here
\ the numbers are stated once and tools/judge/cost.f builds each column's body
\ from them.
\
\ THE INPUT IS THE LONGEST PATH THROUGH THE SUBJECT, which is the rule
\ tools/codegen-compare-cases4.f set for the same twelve: LADDER is measured at
\ 1000, the input that runs every one of its eight compares, because timing the
\ short way out would measure the first guard and call it a ladder. Every input
\ here is that file's timed input, unchanged.
\
\ THE STORAGE ORDINAL IS WHY A POINTER IS NOT WRITTEN OUT. Two subjects read
\ memory the corpus owns, and the C twins cannot share it - they are a different
\ program, not a second compilation of the same one, so they carry their own,
\ filled from the same constants. A row says WHICH storage it reads and each
\ column resolves that to its own. The stepped cell is also the one subject that
\ is not idempotent, so its ordinal carries the reset a VALUE body runs first;
\ the timing body does not reset, because a reset inside it would be timing a
\ fill as well as a step.
\
\ ONE LIST, THREE PASSES. The twelve rows are written once, in EACH, and the
\ three passes hand it their own body: the bytes, the answers, and the times.
\ The times are measured by running that list more than once and keeping each
\ column's fastest, so the columns are interleaved in time rather than measured
\ in three blocks a scheduler could treat differently.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-compare-cabi.f
require tools/codegen-compare-macho.f
require tools/codegen-compare-clang.f
require tools/codegen-compare-corpus4.f
require tools/codegen-tail-probe.f
require tools/judge/src.f
require tools/judge/chain.f
require tools/judge/row.f
require src/compiler/native/dict.f
require tools/judge/cost.f

package JUDGE-CORPUS4

private

variable REC-CELL
variable STEP-CELL

public

\ The storage a subject reads. `none` for ten of the twelve.
ENUM store DERIVE eq
   none
   record
   stepped
;ENUM

\ The C twins' own copies of the pinned data, reached through the reference
\ library. Public because a generated reference body names them.
\
\ THE TWO POINTERS ARE ASKED FOR ONCE. They are constants of the mapped
\ library, and a generated TIMING body runs its inputs on every one of a
\ quarter of a million repetitions: resolving a symbol and making a foreign
\ call inside that loop put four hundred nanoseconds into both a row and its
\ floor, where it cancelled but left the row's cost as the difference of two
\ large nearly equal numbers. Cached, the input costs a load.
: C-REC ( -- n )
   REC-CELL @ 0<> if REC-CELL @ exit then
   s" hc4_rec_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 dup REC-CELL ! ;

: C-STEP ( -- n )
   STEP-CELL @ 0<> if STEP-CELL @ exit then
   s" hc4_step_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 dup STEP-CELL ! ;

: C-RESET ( -- )
   s" hc4_step_reset" CODEGEN-CABI:FN CODEGEN-CABI:I0 drop ;

private

: SOURCE$ ( -- ptr u8 n )
   s" tools/codegen-compare-corpus4.f" ;

\ A suffix of this corpus's own, because the judge measures the corpora one
\ after another into one dictionary and two of them may spell a subject the same
\ way.
: SUFFIX$ ( -- ptr u8 n )
   s" -J4" ;

\ The package this corpus publishes its subjects in, and therefore the package
\ the derived words are published in too. A derived body may name storage that
\ is PRIVATE to it - the corpus's own cell, which both columns step - and a
\ word compiled anywhere else could not see it. So JUDGE runs with this package
\ open, and the reader that takes a size off a dictionary record is given the
\ qualifier, because that reader resolves a spelling as written.
: QUALIFIER$ ( -- ptr u8 n )
   s" CODEGEN-CORPUS4:" ;

\ ---- the twelve rows, written once -------------------------------------------
\ Each is: the name the corpus publishes it under, the storage it reads, its
\ pinned input, and the C symbol that is its twin.
\ typed-local-lint: allow-bare-local - row is the caller's own body, and a local
\ annotation cannot carry a quotation effect.
: EACH ( [ ptr u8 n JUDGE-CORPUS4:store ptr u8 n ptr u8 n -- ] -- ) {: row :}
   s" CALL-FAN"      JUDGE--CORPUS4-STORE:NONE    s" 7"
      s" hc4_call_fan"      row execute
   s" CALL-FAN-BIG"  JUDGE--CORPUS4-STORE:NONE    s" 7"
      s" hc4_call_fan_big"  row execute
   s" CALL-LOOP-3"   JUDGE--CORPUS4-STORE:NONE    s" 1 2 3 7 CODEGEN-CORPUS4:LOOP-LEN"
      s" hc4_call_loop_3"   row execute
   s" WIDE-ARITY"    JUDGE--CORPUS4-STORE:NONE    s" 1 2 3 4 5 6"
      s" hc4_wide_arity"    row execute
   s" LADDER"        JUDGE--CORPUS4-STORE:NONE    s" 1000"
      s" hc4_ladder"        row execute
   s" PRESSURE-LOOP" JUDGE--CORPUS4-STORE:RECORD  s" CODEGEN-CORPUS4:LOOP-LEN"
      s" hc4_pressure_loop" row execute
   s" CALL-PRESSURE" JUDGE--CORPUS4-STORE:NONE    s" 1 2 3 4 5 6 7 9 CODEGEN-CORPUS4:LOOP-LEN"
      s" hc4_call_pressure" row execute
   s" BIG-CONSTS"    JUDGE--CORPUS4-STORE:NONE    s" CODEGEN-CORPUS4:LOOP-LEN"
      s" hc4_big_consts"    row execute
   s" MANY-LOCALS"   JUDGE--CORPUS4-STORE:NONE    s" 1 2 3 4 5 6 7 8 CODEGEN-CORPUS4:LOOP-LEN"
      s" hc4_many_locals"   row execute
   s" TINY-CALLEE"   JUDGE--CORPUS4-STORE:NONE    s" 0 CODEGEN-CORPUS4:LOOP-LEN"
      s" hc4_tiny_callee"   row execute
   s" FLOAT-MIX"     JUDGE--CORPUS4-STORE:NONE    s" 0 CODEGEN-CORPUS4:LOOP-LEN"
      s" hc4_float_mix"     row execute
   s" STORE-LOAD"    JUDGE--CORPUS4-STORE:STEPPED s" CODEGEN-CORPUS4:LOOP-LEN"
      s" hc4_store_load"    row execute ;

\ ---- the texts a column's body is built from ---------------------------------

$100 constant TXT-CAP
create HABU-IN TXT-CAP allot
create REF-IN TXT-CAP allot
create CALL-TXT TXT-CAP allot
create NAME-TXT TXT-CAP allot
variable HABU-IN-U
variable REF-IN-U
variable CALL-TXT-U
variable NAME-TXT-U

: PUT ( ptr u8 n ptr u8 ptr a -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u TXT-CAP > if E-JUDGE-ROW-CAP throw then
   a dst u STR-LEN BYTE-COPY-LEN
   u lenp ! ;

: ADD ( ptr u8 n ptr u8 ptr a -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   lenp @ u + TXT-CAP > if E-JUDGE-ROW-CAP throw then
   a  dst lenp @ +  u STR-LEN BYTE-COPY-LEN
   lenp @ u + lenp ! ;

\ The pointer a subject's storage is reached by, in each of the two worlds.
: HABU-STORE$ ( JUDGE-CORPUS4:store -- ptr u8 n ) {: st:JUDGE-CORPUS4:store :}
   st MATCH store
      none OF s" " ENDOF
      record OF s" CODEGEN-CORPUS4:REC " ENDOF
      stepped OF s" CODEGEN-CORPUS4:STEP-AT " ENDOF
   ;MATCH ;

: REF-STORE$ ( JUDGE-CORPUS4:store -- ptr u8 n ) {: st:JUDGE-CORPUS4:store :}
   st MATCH store
      none OF s" " ENDOF
      record OF s" JUDGE-CORPUS4:C-REC " ENDOF
      stepped OF s" JUDGE-CORPUS4:C-STEP " ENDOF
   ;MATCH ;

\ What a VALUE body runs before the subject, so that the one subject which steps
\ a cell answers from the pinned fill however many timing passes ran before it.
: HABU-SETUP$ ( JUDGE-CORPUS4:store -- ptr u8 n ) {: st:JUDGE-CORPUS4:store :}
   st MATCH store
      none OF s" " ENDOF
      record OF s" " ENDOF
      stepped OF s" CODEGEN-CORPUS4:S-RESET " ENDOF
   ;MATCH ;

: REF-SETUP$ ( JUDGE-CORPUS4:store -- ptr u8 n ) {: st:JUDGE-CORPUS4:store :}
   st MATCH store
      none OF s" " ENDOF
      record OF s" " ENDOF
      stepped OF s" JUDGE-CORPUS4:C-RESET " ENDOF
   ;MATCH ;

: HABU-IN$ ( -- ptr u8 n )   HABU-IN HABU-IN-U @ ;
: REF-IN$ ( -- ptr u8 n )    REF-IN REF-IN-U @ ;
: CALL$ ( -- ptr u8 n )      CALL-TXT CALL-TXT-U @ ;
: NAME$ ( -- ptr u8 n )      NAME-TXT NAME-TXT-U @ ;

\ Build the two input texts for a row. `valued` adds the setup a value body
\ needs and a timing body must not have.
: INPUTS! ( JUDGE-CORPUS4:store ptr u8 n bool -- )
   {: st:JUDGE-CORPUS4:store a:ptr u:n valued:bool :}
   valued if st HABU-SETUP$ else s" " then HABU-IN HABU-IN-U PUT
   st HABU-STORE$ HABU-IN HABU-IN-U ADD
   a u HABU-IN HABU-IN-U ADD
   valued if st REF-SETUP$ else s" " then REF-IN REF-IN-U PUT
   st REF-STORE$ REF-IN REF-IN-U ADD
   a u REF-IN REF-IN-U ADD ;

\ The name a row is printed and found under: the subject as the corpus publishes
\ it, which is also how the engine's word is spelled.
: QUAL! ( ptr u8 n -- ) {: a:ptr u:n :}
   QUALIFIER$ NAME-TXT NAME-TXT-U PUT
   a u NAME-TXT NAME-TXT-U ADD ;

\ ---- the three columns' call texts -------------------------------------------

: OLD-CALL! ( ptr u8 n -- ) {: a:ptr u:n :}
   QUALIFIER$ CALL-TXT CALL-TXT-U PUT
   a u CALL-TXT CALL-TXT-U ADD ;

\ The chain's word is published in the corpus's package, so a body generated at
\ the top level names it qualified. The engine's word is qualified for the same
\ reason and always was.
: NEW-CALL! ( ptr u8 n -- ) {: a:ptr u:n :}
   QUALIFIER$ CALL-TXT CALL-TXT-U PUT
   a u CALL-TXT CALL-TXT-U ADD
   SUFFIX$ CALL-TXT CALL-TXT-U ADD ;

\ The foreign call that reaches a twin. The arity is the SUBJECT's own, read off
\ its stack comment by tools/judge/src.f, so the empty function the row's floor
\ is measured against marshals exactly what the twin does.
: REF-CALL! ( n -- ) {: arity:n :}
   s" JUDGE-COST:TWIN@ CODEGEN-CABI:I" CALL-TXT CALL-TXT-U PUT
   SB-RESET arity FMT:SB-U SB$ CALL-TXT CALL-TXT-U ADD ;

\ The empty C function of a given arity, spelled the way tools/clang/twins.c
\ spells it.
create EMPTY-NAME 16 allot
variable EMPTY-NAME-U

: EMPTY$ ( n -- ptr u8 n ) {: arity:n :}
   s" hf_i" EMPTY-NAME EMPTY-NAME-U PUT
   SB-RESET arity FMT:SB-U SB$ EMPTY-NAME EMPTY-NAME-U ADD
   EMPTY-NAME EMPTY-NAME-U @ ;

\ ---- pass zero: publishing, which happens at load ----------------------------
\ A migration publishes where the INTERPRETER's current wordlist points when it
\ runs, and executing a word does not move that: `package` is a parsing word
\ read at load. So the derived words are published by a top-level line at the
\ foot of this file, inside the corpus's own package block, exactly as
\ tools/codegen-compare-migrated4.f publishes its column - and for the same
\ reason, which is that a derived body may name storage private to the corpus.
\ What the chain answered about each subject is kept here, because the row it
\ belongs to is opened long afterwards.

16 constant SUB-MAX
create SUB-RC SUB-MAX cells allot
variable SUB-N

: PUBLISH-ROW ( ptr u8 n JUDGE-CORPUS4:store ptr u8 n ptr u8 n -- )
   {: a:ptr u:n st:JUDGE-CORPUS4:store ia:ptr iu:n ta:ptr tu:n :}
   SUB-N @ SUB-MAX >= if E-JUDGE-ROW-CAP throw then
   a u JUDGE-SRC:FIND {: d:n :}
   d 0 < if E-JUDGE-SRC-ROW throw then
   d JUDGE-CHAIN:PUBLISH-CALLING SUB-RC SUB-N @ cells + !
   SUB-N @ 1+ SUB-N ! ;

: SUB-RC@ ( n -- n ) {: j:n :}
   j 0 < j SUB-N @ >= or if E-JUDGE-ROW-INDEX throw then
   SUB-RC j cells + @ ;

\ ---- pass one: the bytes -----------------------------------------------------

variable ROW-IX                    \ which subject of EACH's order a pass is on

: BYTES-ROW ( ptr u8 n JUDGE-CORPUS4:store ptr u8 n ptr u8 n -- )
   {: a:ptr u:n st:JUDGE-CORPUS4:store ia:ptr iu:n ta:ptr tu:n :}
   a u JUDGE-SRC:FIND {: d:n :}
   a u QUAL!
   NAME$ JUDGE-ROW:OPEN {: k:n :}
   k  NAME$ NTAILPROBE:CODE-BYTES  JUDGE-ROW:OLD!
   ROW-IX @ SUB-RC@ {: rc:n :}
   ROW-IX @ 1+ ROW-IX !
   rc 0<> if k rc JUDGE-ROW:REFUSED!
   else k  d JUDGE-CHAIN:SIZE  JUDGE-ROW:NEW! then
   CODEGEN-CLANG:PRESENT? 0= if exit then
   k  ta tu CODEGEN-MACHO:BYTES  JUDGE-ROW:REF! ;

\ ---- pass two: the answers ---------------------------------------------------
\ Every column that ran is valued on the row's own pinned input. This is what
\ makes a time a measurement of the right program: a generated body that
\ compiles is not a body that computes the row, and a column that answers
\ something else is caught here rather than reported as a cost.

: VALUE-ROW ( ptr u8 n JUDGE-CORPUS4:store ptr u8 n ptr u8 n -- )
   {: a:ptr u:n st:JUDGE-CORPUS4:store ia:ptr iu:n ta:ptr tu:n :}
   a u QUAL!
   NAME$ JUDGE-ROW:FIND {: k:n :}
   st ia iu true INPUTS!
   a u OLD-CALL!
   CALL$ NAME$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
   k  0  HABU-IN$ CALL$ JUDGE-COST:VALUE  JUDGE-ROW:OLD-COST!
   k JUDGE-ROW:REFUSED? 0= if
      a u NEW-CALL!
      CALL$ CALL$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
      k  0  HABU-IN$ CALL$ JUDGE-COST:VALUE  JUDGE-ROW:NEW-COST!
   then
   k JUDGE-ROW:COVERED? 0= if exit then
   a u JUDGE-SRC:FIND JUDGE-SRC:IN REF-CALL!
   ta tu CODEGEN-CABI:FN JUDGE-COST:TWIN!
   k  0 0  REF-IN$ CALL$ JUDGE-COST:VALUE  JUDGE-ROW:REF-COST! ;

\ ---- pass three: the times ---------------------------------------------------
\ Run more than once; each column keeps its fastest, through JUDGE-ROW:SAMPLE,
\ which also remembers how far apart a program's two measurements were. A
\ column's first measurement is taken while the others are cold and its last
\ while they are warm, so keeping the best of an interleaved sequence is what
\ stops the order of the columns from being part of the answer.

: TIME-ROW ( ptr u8 n JUDGE-CORPUS4:store ptr u8 n ptr u8 n -- )
   {: a:ptr u:n st:JUDGE-CORPUS4:store ia:ptr iu:n ta:ptr tu:n :}
   a u QUAL!
   NAME$ JUDGE-ROW:FIND {: k:n :}
   st ia iu false INPUTS!
   a u OLD-CALL!
   CALL$ NAME$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
   k  k JUDGE-ROW:OLD-PICOS@ HABU-IN$ CALL$ JUDGE-COST:TIME JUDGE-ROW:SAMPLE
      k JUDGE-ROW:OLD-VALUE@  JUDGE-ROW:OLD-COST!
   k JUDGE-ROW:REFUSED? 0= if
      a u NEW-CALL!
      CALL$ CALL$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
      k  k JUDGE-ROW:NEW-PICOS@ HABU-IN$ CALL$ JUDGE-COST:TIME JUDGE-ROW:SAMPLE
         k JUDGE-ROW:NEW-VALUE@  JUDGE-ROW:NEW-COST!
   then
   k JUDGE-ROW:COVERED? 0= if exit then
   a u JUDGE-SRC:FIND JUDGE-SRC:IN {: arity:n :}
   arity REF-CALL!
   ta tu CODEGEN-CABI:FN JUDGE-COST:TWIN!
   k JUDGE-ROW:REF-PICOS@ REF-IN$ CALL$ JUDGE-COST:TIME JUDGE-ROW:SAMPLE {: picos:n :}
   arity EMPTY$ CODEGEN-CABI:FN JUDGE-COST:TWIN!
   k JUDGE-ROW:REF-FLOOR@ REF-IN$ CALL$ JUDGE-COST:TIME JUDGE-ROW:SAMPLE {: floor:n :}
   k picos floor k JUDGE-ROW:REF-VALUE@ JUDGE-ROW:REF-COST! ;

public

\ How many timing passes a run makes over the list. Two is what an interleave
\ needs: one ordering of the columns and one more of them warm.
2 constant TIME-PASSES

\ Read the corpus source and compile every subject through the chain. Runs at
\ load, from inside the corpus's package.
: PUBLISH-ALL ( -- )
   0 SUB-N !
   SUFFIX$ JUDGE-CHAIN:SUFFIX!
   QUALIFIER$ JUDGE-CHAIN:QUALIFIER!
   SOURCE$ JUDGE-SRC:LOAD
   [: PUBLISH-ROW ;] EACH ;

\ Judge every subject of this corpus: the bytes, then the answers, then the
\ times. The source is read again because the reader holds one file at a time
\ and other corpora are judged between the load that published these words and
\ this measurement.
: JUDGE ( -- )
   SUFFIX$ JUDGE-CHAIN:SUFFIX!
   QUALIFIER$ JUDGE-CHAIN:QUALIFIER!
   SOURCE$ JUDGE-SRC:LOAD
   0 ROW-IX !
   [: BYTES-ROW ;] EACH
   [: VALUE-ROW ;] EACH
   TIME-PASSES 0 ?do [: TIME-ROW ;] EACH loop
   JUDGE-COST:FLOOR JUDGE-COST:FLOOR JUDGE-ROW:SAMPLE JUDGE-ROW:FLOOR! ;

;package

\ The corpus's package, open around the judging, so the derived words are
\ published where the corpus's own private storage is reachable and land beside
\ the words they are compared against. It is written out here because `package`
\ parses its operand: the package a corpus is judged in cannot be a parameter,
\ so it is a line of this file rather than a cell.
package CODEGEN-CORPUS4
public

JUDGE-CORPUS4:PUBLISH-ALL

;package
