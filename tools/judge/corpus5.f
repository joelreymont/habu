\ judge/corpus5.f - the judged rows of tools/codegen-compare-corpus5.f: seven
\ shapes about one decision, a call in tail position. One concern: which
\ subject is judged, on which pinned input, against which C twin.
\
\ WHAT THIS FILE STATES is what tools/judge/corpus4.f states and no more: the
\ subjects, one pinned input each, and the C symbol that is each one's twin.
\ The programs come from the corpus file's own bytes, and what the chain can
\ and cannot compile is measured rather than listed.
\
\ THIS CORPUS READS NO STORAGE. Every subject takes its argument and hands it
\ to a callee, so the storage vocabulary below has one arm. It is still
\ declared, because a row is written the same way in every corpus and one that
\ needed a second arm would add it rather than invent a form of its own.
\
\ SIX OF THE SEVEN LEAVE BY A BRANCH, which is what the corpus exists to
\ measure, and it is why their byte counts are not the whole of their program:
\ a routine that tail-branches carries four bytes and the callee it reaches
\ carries the rest. The judged table says so on those rows rather than adding a
\ shared callee into each of them, which would claim its bytes six times over.
\
\ TAIL-PAIR LEAVES TWO VALUES. A generated body accounts for every value its
\ subject leaves - a timed one drops them, a valued one folds them together
\ with `xor` - so the row is compared on both of its results rather than on
\ whichever was on top.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-compare-cabi.f
require tools/codegen-compare-macho.f
require tools/codegen-compare-clang.f
require tools/codegen-compare-corpus5.f
require tools/codegen-tail-probe.f
require tools/judge/src.f
require tools/judge/chain.f
require tools/judge/row.f
require src/compiler/native/dict.f
require tools/judge/cost.f

package JUDGE-CORPUS5

public

\ The storage a subject reads. This corpus reads none: every row is a call in
\ tail position over an argument. The vocabulary is still declared, because the
\ shape a row is written in is one shape across the corpora and a corpus that
\ needed a second arm would add it rather than invent a form of its own.
ENUM store DERIVE eq
   none
;ENUM

private

: SOURCE$ ( -- ptr u8 n )
   s" tools/codegen-compare-corpus5.f" ;

\ A suffix of this corpus's own, because the judge measures the corpora one
\ after another into one dictionary and two of them may spell a subject the same
\ way.
: SUFFIX$ ( -- ptr u8 n )
   s" -J5" ;

\ The package this corpus publishes its subjects in, and therefore the package
\ the derived words are published in too. A derived body may name storage that
\ is PRIVATE to it - the corpus's own cell, which both columns step - and a
\ word compiled anywhere else could not see it. So JUDGE runs with this package
\ open, and the reader that takes a size off a dictionary record is given the
\ qualifier, because that reader resolves a spelling as written.
: QUALIFIER$ ( -- ptr u8 n )
   s" CODEGEN-CORPUS5:" ;

\ ---- the twelve rows, written once -------------------------------------------
\ Each is: the name the corpus publishes it under, the storage it reads, its
\ pinned input, and the C symbol that is its twin.
\ typed-local-lint: allow-bare-local - row is the caller's own body, and a local
\ annotation cannot carry a quotation effect.
: EACH ( [ ptr u8 n JUDGE-CORPUS5:store ptr u8 n ptr u8 n -- ] -- ) {: row :}
   s" TAIL-BIG"    JUDGE--CORPUS5-STORE:NONE  s" 7"  s" hc5_tail_big"    row execute
   s" TAIL-WORK"   JUDGE--CORPUS5-STORE:NONE  s" 7"  s" hc5_tail_work"   row execute
   s" NONTAIL"     JUDGE--CORPUS5-STORE:NONE  s" 7"  s" hc5_nontail"     row execute
   s" TAIL-MID"    JUDGE--CORPUS5-STORE:NONE  s" 7"  s" hc5_tail_mid"    row execute
   s" TAIL-CHAIN"  JUDGE--CORPUS5-STORE:NONE  s" 7"  s" hc5_tail_chain"  row execute
   s" TAIL-PAIR"   JUDGE--CORPUS5-STORE:NONE  s" 7 9"  s" hc5_tail_pair"  row execute
   s" TAIL-AFTER"  JUDGE--CORPUS5-STORE:NONE  s" 7"  s" hc5_tail_after"  row execute ;

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
: HABU-STORE$ ( JUDGE-CORPUS5:store -- ptr u8 n ) {: st:JUDGE-CORPUS5:store :}
   st MATCH store
      none OF s" " ENDOF
   ;MATCH ;

: REF-STORE$ ( JUDGE-CORPUS5:store -- ptr u8 n ) {: st:JUDGE-CORPUS5:store :}
   st MATCH store
      none OF s" " ENDOF
   ;MATCH ;

\ What a VALUE body runs before the subject, so that the one subject which steps
\ a cell answers from the pinned fill however many timing passes ran before it.
: HABU-SETUP$ ( JUDGE-CORPUS5:store -- ptr u8 n ) {: st:JUDGE-CORPUS5:store :}
   st MATCH store
      none OF s" " ENDOF
   ;MATCH ;

: REF-SETUP$ ( JUDGE-CORPUS5:store -- ptr u8 n ) {: st:JUDGE-CORPUS5:store :}
   st MATCH store
      none OF s" " ENDOF
   ;MATCH ;

: HABU-IN$ ( -- ptr u8 n )   HABU-IN HABU-IN-U @ ;
: REF-IN$ ( -- ptr u8 n )    REF-IN REF-IN-U @ ;
: CALL$ ( -- ptr u8 n )      CALL-TXT CALL-TXT-U @ ;
: NAME$ ( -- ptr u8 n )      NAME-TXT NAME-TXT-U @ ;

\ Build the two input texts for a row. `valued` adds the setup a value body
\ needs and a timing body must not have.
: INPUTS! ( JUDGE-CORPUS5:store ptr u8 n bool -- )
   {: st:JUDGE-CORPUS5:store a:ptr u:n valued:bool :}
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

: PUBLISH-ROW ( ptr u8 n JUDGE-CORPUS5:store ptr u8 n ptr u8 n -- )
   {: a:ptr u:n st:JUDGE-CORPUS5:store ia:ptr iu:n ta:ptr tu:n :}
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
variable OUTS-CELL                 \ how many values the subject being measured leaves

: OUTS ( -- n )
   OUTS-CELL @ ;

\ A foreign call leaves ONE value however many its habu twin leaves: that is
\ the C calling convention and not a property of the row. So a reference body
\ accounts for one, and a subject that leaves more has no single reference
\ answer to be compared with - which the table says rather than hides.
: REF-OUTS ( -- n )
   1 ;

: BYTES-ROW ( ptr u8 n JUDGE-CORPUS5:store ptr u8 n ptr u8 n -- )
   {: a:ptr u:n st:JUDGE-CORPUS5:store ia:ptr iu:n ta:ptr tu:n :}
   a u JUDGE-SRC:FIND {: d:n :}
   a u QUAL!
   NAME$ JUDGE-ROW:OPEN {: k:n :}
   k d JUDGE-SRC:OUT JUDGE-ROW:OUTS!
   k  NAME$ NTAILPROBE:CODE-BYTES  JUDGE-ROW:OLD!
   ROW-IX @ SUB-RC@ {: rc:n :}
   ROW-IX @ 1+ ROW-IX !
   rc 0<> if k rc JUDGE-ROW:REFUSED!
   else k  d JUDGE-CHAIN:SIZE  d JUDGE-CHAIN:TAIL?  JUDGE-ROW:NEW! then
   CODEGEN-CLANG:PRESENT? 0= if exit then
   k  ta tu CODEGEN-MACHO:BYTES  JUDGE-ROW:REF! ;

\ ---- pass two: the answers ---------------------------------------------------
\ Every column that ran is valued on the row's own pinned input. This is what
\ makes a time a measurement of the right program: a generated body that
\ compiles is not a body that computes the row, and a column that answers
\ something else is caught here rather than reported as a cost.

: VALUE-ROW ( ptr u8 n JUDGE-CORPUS5:store ptr u8 n ptr u8 n -- )
   {: a:ptr u:n st:JUDGE-CORPUS5:store ia:ptr iu:n ta:ptr tu:n :}
   a u QUAL!
   a u JUDGE-SRC:FIND JUDGE-SRC:OUT OUTS-CELL !
   NAME$ JUDGE-ROW:FIND {: k:n :}
   st ia iu true INPUTS!
   a u OLD-CALL!
   CALL$ NAME$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
   k  0  HABU-IN$ CALL$ OUTS JUDGE-COST:VALUE  JUDGE-ROW:OLD-COST!
   k JUDGE-ROW:REFUSED? 0= if
      a u NEW-CALL!
      CALL$ CALL$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
      k  0  HABU-IN$ CALL$ OUTS JUDGE-COST:VALUE  JUDGE-ROW:NEW-COST!
   then
   k JUDGE-ROW:COVERED? 0= if exit then
   a u JUDGE-SRC:FIND JUDGE-SRC:IN REF-CALL!
   ta tu CODEGEN-CABI:FN JUDGE-COST:TWIN!
   k  0 0  REF-IN$ CALL$ REF-OUTS JUDGE-COST:VALUE  JUDGE-ROW:REF-COST! ;

\ ---- pass three: the times ---------------------------------------------------
\ Run more than once; each column keeps its fastest, through JUDGE-ROW:SAMPLE,
\ which also remembers how far apart a program's two measurements were. A
\ column's first measurement is taken while the others are cold and its last
\ while they are warm, so keeping the best of an interleaved sequence is what
\ stops the order of the columns from being part of the answer.

: TIME-ROW ( ptr u8 n JUDGE-CORPUS5:store ptr u8 n ptr u8 n -- )
   {: a:ptr u:n st:JUDGE-CORPUS5:store ia:ptr iu:n ta:ptr tu:n :}
   a u QUAL!
   a u JUDGE-SRC:FIND JUDGE-SRC:OUT OUTS-CELL !
   NAME$ JUDGE-ROW:FIND {: k:n :}
   st ia iu false INPUTS!
   a u OLD-CALL!
   CALL$ NAME$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
   k  k JUDGE-ROW:OLD-PICOS@ HABU-IN$ CALL$ OUTS JUDGE-COST:TIME JUDGE-ROW:SAMPLE
      k JUDGE-ROW:OLD-VALUE@  JUDGE-ROW:OLD-COST!
   k JUDGE-ROW:REFUSED? 0= if
      a u NEW-CALL!
      CALL$ CALL$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
      k  k JUDGE-ROW:NEW-PICOS@ HABU-IN$ CALL$ OUTS JUDGE-COST:TIME JUDGE-ROW:SAMPLE
         k JUDGE-ROW:NEW-VALUE@  JUDGE-ROW:NEW-COST!
   then
   k JUDGE-ROW:COVERED? 0= if exit then
   a u JUDGE-SRC:FIND JUDGE-SRC:IN {: arity:n :}
   arity REF-CALL!
   ta tu CODEGEN-CABI:FN JUDGE-COST:TWIN!
   k JUDGE-ROW:REF-PICOS@ REF-IN$ CALL$ REF-OUTS JUDGE-COST:TIME JUDGE-ROW:SAMPLE {: picos:n :}
   arity EMPTY$ CODEGEN-CABI:FN JUDGE-COST:TWIN!
   k JUDGE-ROW:REF-FLOOR@ REF-IN$ CALL$ REF-OUTS JUDGE-COST:TIME JUDGE-ROW:SAMPLE {: floor:n :}
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
package CODEGEN-CORPUS5
public

JUDGE-CORPUS5:PUBLISH-ALL

;package
