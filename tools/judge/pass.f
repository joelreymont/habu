\ judge/pass.f - the measuring every judged corpus shares. One concern: turning
\ ONE stated row - a subject, its pinned input, its C twin - into the four
\ passes the judged table is built from.
\
\ WHY THIS FILE EXISTS. A corpus file used to carry the whole apparatus: the
\ text buffers, the three call texts, the four passes, and the publication.
\ tools/judge/corpus4.f and tools/judge/corpus5.f held that apparatus twice,
\ line for line apart from the row list, and a third corpus would have held it a
\ third time. Two copies of a measurement's machinery is two chances for one
\ corpus to be measured differently from another, and the difference would be
\ reported as a code generator result. Everything that is the same for every
\ corpus lives here; a corpus file states its rows and nothing else.
\
\ HOW A ROW IS STATED. A row is opened, then its parts are appended in the order
\ the subject takes them, then the pass runs:
\
\   s" CALL-FAN" s" hc4_call_fan" JUDGE-PASS:ROW!
\      s" 7" JUDGE-PASS:IN+
\      row execute
\
\ IN+ appends the same text to both worlds. STORE+ appends DIFFERENT text to
\ each, and it is how a pointer crosses: the two programs cannot share memory -
\ the C twin is a different program, not a second compilation of the same one -
\ so each column names its own and the row says which. SETUP+ is what a VALUE
\ body runs before the call, for the subjects that are not idempotent, and READ+
\ is what it runs after, for the subjects whose point is a store.
\
\ WHY THE PARTS ARE APPENDED RATHER THAN NAMED IN ONE ROW. The storage a subject
\ reads sits at a position in its argument list, and that position is the
\ subject's, not a convention: T-SGD! takes its learning rate FIRST and its two
\ buffers after it. A row that named a storage ordinal beside its input could
\ only ever splice the pointer at one fixed place, so it would have to state the
\ input twice - once per world - which is the duplicate this whole tool exists to
\ remove.
\
\ THE REFUSAL IS RECORDED UNDER THE SUBJECT'S NAME. The chain compiles a corpus
\ when its file is loaded and the table is built long afterwards, so what the
\ chain answered has to be kept in between. It is kept against the row's
\ qualified name rather than against its position in a list: a position is a
\ claim that two passes walked the same list in the same order, and a name is no
\ claim at all.
\
\ HOW A REFERENCE ROW IS REACHED. Through the call shapes of
\ tools/codegen-compare-cabi.f. A twin whose arguments are all integers and whose
\ answer is one is reached by ROW!, which reads the shape off the SUBJECT's own
\ arity - `I3` for three - so nobody states a number the source already carries.
\ A twin that takes or answers a double has a shape of its own and no arity to
\ derive it from, so ROW-ABI! takes the spelling: `IID`, `DIII`, `DD`. The row's
\ floor is the empty C function of that same shape, spelled the way
\ tools/clang/twins.c spells it - `hf_` and the shape in lower case - so what a
\ row's marshalling costs is subtracted by a call that marshals the same thing.
\
\ AND THE SHAPE SAYS WHAT THE TWIN ANSWERS. Every name in that file ends in `D`
\ where the answer is a double (`ID`, `IID`, `IIID`, `DD`, `DDD`, `DDDD`) and in
\ `I` or a digit where it is an integer (`DI`, `DIII`, `I0` .. `I9`). So the last
\ character decides whether a reference answer has to be projected to its bits
\ before it can be compared with the engine's, and a shape ending in anything
\ else is refused rather than guessed at.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-compare-cabi.f
require tools/codegen-compare-macho.f
require tools/codegen-compare-clang.f
require tools/codegen-tail-probe.f
require tools/judge/src.f
require tools/judge/chain.f
require tools/judge/row.f
require tools/judge/traffic.f
require src/compiler/native/dict.f
require tools/judge/cost.f

package JUDGE-PASS

\ How many pinned input tuples one row may state. Public because the member
\ that proves a ninth is refused has to ask for one more than there are, and
\ because it is the one number a corpus writing a long row has to know.
public

8 constant IN-MAX

private

$100 constant TXT-CAP             \ bytes of one part of one generated body
32 constant SHAPE-CAP             \ bytes of a C call shape, and of the floor's name
64 constant SUB-MAX               \ subjects the refusal ledger holds
48 constant SUB-NAME-MAX

$44 constant CH-D                 \ the letter a double-answering shape ends in
$49 constant CH-I                 \ the letter an integer-answering shape ends in
$30 constant CH-0
$39 constant CH-9

create HABU-IN IN-MAX TXT-CAP * allot
create REF-IN IN-MAX TXT-CAP * allot
create HABU-IN-LENS IN-MAX cells allot
create REF-IN-LENS IN-MAX cells allot
create IN-REF-OK IN-MAX cells allot       \ this tuple has a counterpart in the C world
create HABU-SET TXT-CAP allot
create REF-SET TXT-CAP allot
create HABU-READ TXT-CAP allot
create REF-READ TXT-CAP allot
create CALL-TXT TXT-CAP allot
create NAME-TXT TXT-CAP allot
create TWIN-TXT TXT-CAP allot
create VAL-TXT TXT-CAP allot
create QUAL-TXT TXT-CAP allot
create SHAPE-TXT SHAPE-CAP allot
create FLOOR-TXT SHAPE-CAP allot

variable IN-N                      \ how many tuples this row states
variable HABU-SET-U
variable REF-SET-U
variable HABU-READ-U
variable REF-READ-U
variable CALL-TXT-U
variable NAME-TXT-U
variable TWIN-TXT-U
variable VAL-TXT-U
variable QUAL-TXT-U
variable SHAPE-TXT-U
variable FLOOR-TXT-U

variable DEF-CELL                  \ which definition of the loaded source this row is
variable OPENED                    \ a row has been stated

: PUT ( ptr u8 n ptr u8 ptr a -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u TXT-CAP > if E-JUDGE-PASS-CAP throw then
   a dst u STR-LEN BYTE-COPY-LEN
   u lenp ! ;

: ADD ( ptr u8 n ptr u8 ptr a -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   lenp @ u + TXT-CAP > if E-JUDGE-PASS-CAP throw then
   a  dst lenp @ +  u STR-LEN BYTE-COPY-LEN
   lenp @ u + lenp ! ;

\ One input tuple's text, in one world. The tuples share one store, so each is
\ a fixed stretch of it and its own length cell.
: IN-AT ( ptr u8 n -- ptr u8 ) {: base:ptr j:n :}
   base j TXT-CAP * + ;

: IN-LEN ( ptr a n -- ptr a ) {: lens:ptr j:n :}
   lens j cells + ;

: HABU-IN$ ( n -- ptr u8 n ) {: j:n :}
   HABU-IN j IN-AT
   HABU-IN-LENS j IN-LEN @ ;

: REF-IN$ ( n -- ptr u8 n ) {: j:n :}
   REF-IN j IN-AT
   REF-IN-LENS j IN-LEN @ ;

: HABU-SET$ ( -- ptr u8 n )   HABU-SET HABU-SET-U @ ;
: REF-SET$ ( -- ptr u8 n )    REF-SET REF-SET-U @ ;
: HABU-READ$ ( -- ptr u8 n )  HABU-READ HABU-READ-U @ ;
: REF-READ$ ( -- ptr u8 n )   REF-READ REF-READ-U @ ;
: CALL$ ( -- ptr u8 n )       CALL-TXT CALL-TXT-U @ ;
: TWIN$ ( -- ptr u8 n )       TWIN-TXT TWIN-TXT-U @ ;
: SHAPE$ ( -- ptr u8 n )      SHAPE-TXT SHAPE-TXT-U @ ;
: FLOOR$ ( -- ptr u8 n )      FLOOR-TXT FLOOR-TXT-U @ ;
: QUAL$ ( -- ptr u8 n )       QUAL-TXT QUAL-TXT-U @ ;

\ Which definition of the loaded corpus source this row's subject is.
: DEF ( -- n )
   OPENED @ 0= if E-JUDGE-PASS-ROW throw then
   DEF-CELL @ ;

\ What a VALUE body runs before the call, and the row's own input, in one text.
\ Built where it is used, because a TIMED body must not carry the setup: a reset
\ inside a timing loop would be timing a fill as well as the subject.
: VAL$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: sa:ptr su:n ia:ptr iu:n :}
   sa su VAL-TXT VAL-TXT-U PUT
   ia iu VAL-TXT VAL-TXT-U ADD
   VAL-TXT VAL-TXT-U @ ;

public

\ The row this pass is about, as the table prints and finds it: the subject
\ spelled the way its corpus publishes it, which is how the engine's word is
\ spelled and, with the corpus's suffix, how the chain's derived word is.
: NAME$ ( -- ptr u8 n )
   NAME-TXT NAME-TXT-U @ ;

private

\ ---- the refusal ledger ------------------------------------------------------
\ What the chain answered about each subject, kept from the load that compiled it
\ to the pass that prints it, under the subject's own qualified name.

SUB-MAX SUB-NAME-MAX * BUFFER: SUB-NAMES
create SUB-LENS SUB-MAX cells allot
create SUB-CODES SUB-MAX cells allot
variable SUB-N

: SUB-AT ( n -- ptr u8 )
   SUB-NAME-MAX * SUB-NAMES + ;

: SUB-NAME$ ( n -- ptr u8 n ) {: k:n :}
   k SUB-AT
   SUB-LENS k cells + @ ;

: SUB-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1
   SUB-N @ 0 ?do
      i SUB-NAME$ a u STR= if drop i then
   loop ;

\ Record what the chain answered about the open row. A name already in the
\ ledger is a refusal: two rows under one name would leave the table reading one
\ subject's refusal against another subject's bytes.
: RC! ( n -- ) {: rc:n :}
   NAME$ {: a:ptr u:n :}
   a u SUB-FIND 0 >= if E-JUDGE-PASS-ROW throw then
   SUB-N @ SUB-MAX >= if E-JUDGE-PASS-CAP throw then
   u SUB-NAME-MAX > if E-JUDGE-PASS-CAP throw then
   a  SUB-N @ SUB-AT  u STR-LEN BYTE-COPY-LEN
   u SUB-LENS SUB-N @ cells + !
   rc SUB-CODES SUB-N @ cells + !
   SUB-N @ 1+ SUB-N ! ;

: RC@ ( -- n )
   NAME$ SUB-FIND {: k:n :}
   k 0 < if E-JUDGE-PASS-ROW throw then
   SUB-CODES k cells + @ ;

\ ---- the three columns' call texts -------------------------------------------

\ The engine's word, which is the subject under its own qualified spelling.
: OLD-CALL! ( -- )
   NAME$ CALL-TXT CALL-TXT-U PUT ;

\ The chain's word, which is that spelling with the corpus's suffix.
: NEW-CALL! ( -- )
   NAME$ CALL-TXT CALL-TXT-U PUT
   JUDGE-CHAIN:SUFFIX$ CALL-TXT CALL-TXT-U ADD ;

\ The foreign call that reaches a twin, through the shape the row was opened
\ with.
: REF-CALL! ( -- )
   s" JUDGE-COST:TWIN@ CODEGEN-CABI:" CALL-TXT CALL-TXT-U PUT
   SHAPE$ CALL-TXT CALL-TXT-U ADD ;

\ ---- the shape a reference row is reached through ----------------------------

\ Does this shape's twin answer a double? The last character of the name says so,
\ which is why it is read rather than stated a second time.
: SHAPE-REAL? ( -- bool )
   SHAPE$ {: a:ptr u:n :}
   u 0= if E-JUDGE-PASS-SHAPE throw then
   u 1- a + c@ {: last:n :}
   last CH-D = if true exit then
   last CH-I = if false exit then
   last CH-0 < last CH-9 > or if E-JUDGE-PASS-SHAPE throw then
   false ;

\ The empty C function of this shape, spelled the way tools/clang/twins.c spells
\ it: `hf_` and the shape in lower case.
: FLOOR-NAME! ( -- )
   SHAPE$ {: a:ptr u:n :}
   SB-RESET
   s" hf_" SB-APPEND
   u 0 ?do i a + c@ ASCII-LOWER SB-APPEND-C loop
   SB$ FLOOR-TXT FLOOR-TXT-U PUT ;

: SHAPE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if E-JUDGE-PASS-SHAPE throw then
   u SHAPE-CAP > if E-JUDGE-PASS-CAP throw then
   a SHAPE-TXT u STR-LEN BYTE-COPY-LEN
   u SHAPE-TXT-U !
   SHAPE-REAL? drop
   FLOOR-NAME! ;

\ The all-integer shape of a twin that takes the subject's own arguments and
\ answers one value: `I` and the arity the subject's stack comment declares.
: SHAPE-FROM-ARITY! ( -- )
   SB-RESET
   s" I" SB-APPEND
   DEF JUDGE-SRC:IN FMT:SB-U
   SB$ SHAPE! ;

\ ---- opening a row -----------------------------------------------------------

: CLEAR ( -- )
   1 IN-N !
   IN-MAX 0 ?do
      0 HABU-IN-LENS i IN-LEN !
      0 REF-IN-LENS i IN-LEN !
      1 IN-REF-OK i IN-LEN !
   loop
   0 HABU-SET-U !   0 REF-SET-U !
   0 HABU-READ-U !  0 REF-READ-U !
   0 SHAPE-TXT-U !  0 FLOOR-TXT-U ! ;

: OPEN-ROW ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n ta:ptr tu:n :}
   QUAL-TXT-U @ 0= if E-JUDGE-PASS-ROW throw then
   CLEAR
   QUAL$ NAME-TXT NAME-TXT-U PUT
   a u NAME-TXT NAME-TXT-U ADD
   ta tu TWIN-TXT TWIN-TXT-U PUT
   a u JUDGE-SRC:FIND {: d:n :}
   d 0 < if E-JUDGE-SRC-ROW throw then
   d DEF-CELL !
   true OPENED ! ;

: STATED ( -- )
   OPENED @ 0= if E-JUDGE-PASS-ROW throw then ;

public

\ How many timing passes a run makes over a corpus's rows. Two is what an
\ interleave needs: one ordering of the columns and one more of them warm.
2 constant TIME-PASSES

\ The corpus the rows that follow belong to: the source file the subjects are
\ read out of, the suffix its derived words carry, and the package it publishes
\ them in. Every corpus states all three, because no default for any of them
\ would fail loudly - it would measure one corpus as another.
: CORPUS! ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: sa:ptr su:n ua:ptr uu:n qa:ptr qu:n :}
   ua uu JUDGE-CHAIN:SUFFIX!
   qa qu JUDGE-CHAIN:QUALIFIER!
   qa qu QUAL-TXT QUAL-TXT-U PUT
   false OPENED !
   sa su JUDGE-SRC:LOAD ;

\ Open a row whose C twin takes the subject's arguments as integers and answers
\ one.
: ROW! ( ptr u8 n ptr u8 n -- )
   OPEN-ROW
   SHAPE-FROM-ARITY! ;

\ Open a row whose C twin takes or answers a double, and therefore has a call
\ shape no arity can derive.
: ROW-ABI! ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: a:ptr u:n ta:ptr tu:n sa:ptr su:n :}
   a u ta tu OPEN-ROW
   sa su SHAPE! ;

\ The same text in both worlds: a number, or a name both columns resolve to the
\ same value. It joins the tuple ALSO last opened.
: IN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   STATED
   IN-N @ 1- {: j:n :}
   a u  HABU-IN j IN-AT  HABU-IN-LENS j IN-LEN  ADD
   a u  REF-IN j IN-AT  REF-IN-LENS j IN-LEN  ADD ;

\ Text that differs between the two worlds, which is how a pointer crosses: the
\ twin owns its own copy of the pinned data and reaches it by its own name.
: STORE+ ( ptr u8 n ptr u8 n -- ) {: ha:ptr hu:n ra:ptr ru:n :}
   STATED
   IN-N @ 1- {: j:n :}
   ha hu  HABU-IN j IN-AT  HABU-IN-LENS j IN-LEN  ADD
   ra ru  REF-IN j IN-AT  REF-IN-LENS j IN-LEN  ADD ;

\ Text for the habu columns that the C world has no counterpart for: an input
\ over storage tools/clang/twins.c does not carry. The tuple is still compared
\ across the two habu columns - which is the head-to-head this tool exists for -
\ and the reference column simply has nothing to be asked about it. That is the
\ same policy the byte column already runs on a subject with no twin and on a
\ host with no compiler, one input at a time instead of one row at a time, and
\ the tally prints how many tuples the reference did reach so a comparison not
\ made cannot read as one made and passed.
: HABU-ONLY+ ( ptr u8 n -- ) {: a:ptr u:n :}
   STATED
   IN-N @ 1- {: j:n :}
   a u  HABU-IN j IN-AT  HABU-IN-LENS j IN-LEN  ADD
   0 IN-REF-OK j IN-LEN ! ;

\ Does the reference column have a program for this tuple?
: IN-REF-OK? ( n -- bool ) {: j:n :}
   IN-REF-OK j IN-LEN @ 0<> ;

\ ANOTHER PINNED INPUT FOR THE SAME SUBJECT. Everything IN+ and STORE+ append
\ after this joins a new tuple, and the row is VALUED on every tuple it states -
\ each one through all three columns, which must agree about it. It is TIMED on
\ the first alone: a cost is about one program on one input, and timing a row on
\ four would report the mean of four different programs' work under one name.
\
\ WHY A ROW WANTS MORE THAN ONE. The pinned input runs the longest path through
\ the subject; the arms it does NOT take are exactly where a code generator can
\ be wrong and agree about everything anybody measured. An empty span, a miss, a
\ zero, a negative turn count: each is one line here and one whole arm of the
\ compiled routine.
: ALSO ( -- )
   STATED
   IN-N @ IN-MAX >= if E-JUDGE-PASS-INPUTS throw then
   IN-N @ 1+ IN-N ! ;

\ What a VALUE body runs before the call, for a subject that is not idempotent.
: SETUP+ ( ptr u8 n ptr u8 n -- ) {: ha:ptr hu:n ra:ptr ru:n :}
   STATED
   ha hu HABU-SET HABU-SET-U ADD
   ra ru REF-SET REF-SET-U ADD ;

\ What a VALUE body runs after the call, for a subject whose point is a store.
\ It leaves ONE cell, which is folded into the row's answer, so the comparison is
\ about the memory the subject wrote as well as the value it left.
: READ+ ( ptr u8 n ptr u8 n -- ) {: ha:ptr hu:n ra:ptr ru:n :}
   STATED
   ha hu HABU-READ HABU-READ-U ADD
   ra ru REF-READ REF-READ-U ADD ;

\ ---- pass zero: publishing, which happens at load ----------------------------

\ Compile this row's subject through the chain and record what the chain
\ answered. Runs at load, from inside the corpus's own package, because a
\ migration publishes where the interpreter's wordlist points and a derived body
\ may name storage private to that corpus.
: PUBLISH ( -- )
   DEF JUDGE-CHAIN:PUBLISH-CALLING RC! ;

\ ---- pass one: the bytes -----------------------------------------------------

: BYTES ( -- )
   NAME$ JUDGE-ROW:OPEN {: k:n :}
   k DEF JUDGE-SRC:OUT JUDGE-ROW:OUTS!
   k  NAME$ NTAILPROBE:CODE-BYTES  JUDGE-ROW:OLD!
   k  NAME$ JUDGE-TRAFFIC:COUNT  JUDGE-ROW:OLD-TRAFFIC!
   RC@ {: rc:n :}
   rc 0<> if k rc JUDGE-ROW:REFUSED!
   else
      k  DEF JUDGE-CHAIN:SIZE  DEF JUDGE-CHAIN:TAIL?  JUDGE-ROW:NEW!
      NEW-CALL!
      k  CALL$ JUDGE-TRAFFIC:COUNT  JUDGE-ROW:NEW-TRAFFIC!
   then
   CODEGEN-CLANG:PRESENT? 0= if exit then
   k  TWIN$ CODEGEN-MACHO:BYTES  JUDGE-ROW:REF! ;

\ ---- pass two: the answers ---------------------------------------------------
\ Every column that ran is valued on the row's own pinned input. This is what
\ makes a time a measurement of the right program: a generated body that compiles
\ is not a body that computes the row, and a column that answers something else
\ is caught here rather than reported as a cost.

private

\ How many values the subject leaves, and which projection the one on top needs -
\ both read off its own stack comment, because a generated body has to account
\ for every value and neither a double nor a flag is a cell until it is
\ projected. More than one value needing a projection has no answer here: the
\ projection reaches the top of the stack and nothing under it.
: SUB-OUTS ( -- n )
   DEF JUDGE-SRC:OUT ;

: SUB-PROJ ( -- n )
   DEF JUDGE-SRC:REALS {: reals:n :}
   DEF JUDGE-SRC:FLAGS {: flags:n :}
   reals flags + 0= if JUDGE-COST:P-CELL exit then
   SUB-OUTS 1 <> reals flags + 1 <> or if E-JUDGE-COST-FOLD throw then
   reals 1 = if JUDGE-COST:P-REAL exit then
   JUDGE-COST:P-FLAG ;

\ A foreign call leaves ONE value however many its habu twin leaves: that is the
\ C calling convention and not a property of the row. So a reference body
\ accounts for one, and a subject that leaves more has no single reference answer
\ to be compared with - which the table says rather than hides.
: REF-OUTS ( -- n )
   1 ;

: REF-PROJ ( -- n )
   SHAPE-REAL? if JUDGE-COST:P-REAL else JUDGE-COST:P-CELL then ;

\ Does this row read the memory its subject wrote?
: WITNESSED? ( -- bool )
   HABU-READ-U @ 0<> ;

: OLD-VALUE ( n n -- ) {: k:n j:n :}
   OLD-CALL!
   CALL$ NAME$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
   k  0  HABU-SET$ j HABU-IN$ VAL$  CALL$  SUB-OUTS SUB-PROJ
      JUDGE-COST:VALUE  JUDGE-ROW:OLD-COST!
   WITNESSED? 0= if exit then
   k  HABU-SET$ j HABU-IN$ VAL$  CALL$  HABU-READ$  SUB-OUTS
      JUDGE-COST:WITNESS  JUDGE-ROW:OLD-WITNESS! ;

: NEW-VALUE ( n n -- ) {: k:n j:n :}
   NEW-CALL!
   CALL$ CALL$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
   k  0  HABU-SET$ j HABU-IN$ VAL$  CALL$  SUB-OUTS SUB-PROJ
      JUDGE-COST:VALUE  JUDGE-ROW:NEW-COST!
   WITNESSED? 0= if exit then
   k  HABU-SET$ j HABU-IN$ VAL$  CALL$  HABU-READ$  SUB-OUTS
      JUDGE-COST:WITNESS  JUDGE-ROW:NEW-WITNESS! ;

: REF-VALUE ( n n -- ) {: k:n j:n :}
   REF-CALL!
   TWIN$ CODEGEN-CABI:FN JUDGE-COST:TWIN!
   k  0 0  REF-SET$ j REF-IN$ VAL$  CALL$  REF-OUTS REF-PROJ
      JUDGE-COST:VALUE  JUDGE-ROW:REF-COST!
   WITNESSED? 0= if exit then
   k  REF-SET$ j REF-IN$ VAL$  CALL$  REF-READ$  REF-OUTS
      JUDGE-COST:WITNESS  JUDGE-ROW:REF-WITNESS! ;

\ ---- the inputs past the first ----------------------------------------------
\ The first pinned input is RECORDED in the row, because the artifact prints
\ what it made the columns answer and the row's cost is about that program. The
\ others are COMPARED and not recorded: what they are for is the arms the first
\ input does not take, and the whole of that question is whether the three
\ columns still agree. A disagreement on any of them makes the row disagree,
\ which is the one condition that makes a whole row worthless.
\
\ THE ANSWERS ARE NOT COMMITTED, and that is deliberate. A recorded answer is a
\ number somebody has to keep in step by hand the day a corpus subject changes;
\ what these inputs establish is that three independently compiled programs
\ compute the SAME thing on them, which nothing has to remember.

: ALT-HABU ( ptr u8 n n -- n ) {: ca:ptr cu:n j:n :}
   HABU-SET$ j HABU-IN$ VAL$  ca cu  SUB-OUTS SUB-PROJ JUDGE-COST:VALUE ;

: ALT-HABU-WITNESS ( ptr u8 n n -- n ) {: ca:ptr cu:n j:n :}
   HABU-SET$ j HABU-IN$ VAL$  ca cu  HABU-READ$ SUB-OUTS JUDGE-COST:WITNESS ;

: ALT-REF ( n -- n ) {: j:n :}
   REF-CALL!
   TWIN$ CODEGEN-CABI:FN JUDGE-COST:TWIN!
   REF-SET$ j REF-IN$ VAL$  CALL$  REF-OUTS REF-PROJ JUDGE-COST:VALUE ;

: ALT-REF-WITNESS ( n -- n ) {: j:n :}
   REF-CALL!
   TWIN$ CODEGEN-CABI:FN JUDGE-COST:TWIN!
   REF-SET$ j REF-IN$ VAL$  CALL$  REF-READ$ REF-OUTS JUDGE-COST:WITNESS ;

\ The chain's answer against the engine's, on one input past the first.
: ALT-NEW-AGREES? ( n n -- bool ) {: k:n j:n :}
   k JUDGE-ROW:REFUSED? if true exit then
   OLD-CALL!
   CALL$ NAME$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
   CALL$ j ALT-HABU {: old:n :}
   WITNESSED? if CALL$ j ALT-HABU-WITNESS else 0 then {: old-wit:n :}
   NEW-CALL!
   CALL$ CALL$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
   CALL$ j ALT-HABU {: new:n :}
   WITNESSED? if CALL$ j ALT-HABU-WITNESS else 0 then {: new-wit:n :}
   old new <> if false exit then
   old-wit new-wit = ;

\ And the C twin's, on the same input. A subject leaving more than one value has
\ no single reference answer to be compared with, for the reason REF-OUTS gives.
: ALT-REF-AGREES? ( n n -- bool ) {: k:n j:n :}
   k JUDGE-ROW:COVERED? 0= if true exit then
   j IN-REF-OK? 0= if true exit then
   OLD-CALL!
   CALL$ NAME$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
   CALL$ j ALT-HABU {: old:n :}
   WITNESSED? if CALL$ j ALT-HABU-WITNESS else 0 then {: old-wit:n :}
   WITNESSED? if
      j ALT-REF-WITNESS old-wit <> if false exit then
   then
   k JUDGE-ROW:MULTI? if true exit then
   j ALT-REF old = ;

: ALT-VALUE ( n n -- ) {: k:n j:n :}
   k j ALT-NEW-AGREES? 0= if k JUDGE-ROW:ALT-BAD! exit then
   k j ALT-REF-AGREES? 0= if k JUDGE-ROW:ALT-BAD! then ;

\ How many of this row's tuples the reference column has a program for. A row
\ with no twin at all reaches none of them.
: REF-TUPLES ( n -- n ) {: k:n :}
   k JUDGE-ROW:COVERED? 0= if 0 exit then
   0
   IN-N @ 0 ?do
      i IN-REF-OK? if 1+ then
   loop ;

public

: VALUE ( -- )
   NAME$ JUDGE-ROW:FIND {: k:n :}
   k IN-N @ JUDGE-ROW:INPUTS!
   k  k REF-TUPLES  JUDGE-ROW:REF-INPUTS!
   k 0 OLD-VALUE
   k JUDGE-ROW:REFUSED? 0= if k 0 NEW-VALUE then
   k JUDGE-ROW:COVERED? if k 0 REF-VALUE then
   IN-N @ 1 ?do k i ALT-VALUE loop ;

\ ---- pass three: the times ---------------------------------------------------
\ Run more than once; each column keeps its fastest, through JUDGE-ROW:SAMPLE,
\ which also remembers how far apart a program's two measurements were. A
\ column's first measurement is taken while the others are cold and its last
\ while they are warm, so keeping the best of an interleaved sequence is what
\ stops the order of the columns from being part of the answer.

private

: OLD-TIME ( n -- ) {: k:n :}
   OLD-CALL!
   CALL$ NAME$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
   k  k JUDGE-ROW:OLD-PICOS@ 0 HABU-IN$ CALL$ SUB-OUTS JUDGE-COST:TIME
      JUDGE-ROW:SAMPLE  k JUDGE-ROW:OLD-VALUE@  JUDGE-ROW:OLD-COST! ;

: NEW-TIME ( n -- ) {: k:n :}
   NEW-CALL!
   CALL$ CALL$ NDICT:CALL-TARGET JUDGE-COST:COLUMN-CK
   k  k JUDGE-ROW:NEW-PICOS@ 0 HABU-IN$ CALL$ SUB-OUTS JUDGE-COST:TIME
      JUDGE-ROW:SAMPLE  k JUDGE-ROW:NEW-VALUE@  JUDGE-ROW:NEW-COST! ;

: REF-TIME ( n -- ) {: k:n :}
   REF-CALL!
   TWIN$ CODEGEN-CABI:FN JUDGE-COST:TWIN!
   k JUDGE-ROW:REF-PICOS@ 0 REF-IN$ CALL$ REF-OUTS JUDGE-COST:TIME
      JUDGE-ROW:SAMPLE {: picos:n :}
   FLOOR$ CODEGEN-CABI:FN JUDGE-COST:TWIN!
   k JUDGE-ROW:REF-FLOOR@ 0 REF-IN$ CALL$ REF-OUTS JUDGE-COST:TIME
      JUDGE-ROW:SAMPLE {: floor:n :}
   k picos floor k JUDGE-ROW:REF-VALUE@ JUDGE-ROW:REF-COST! ;

public

: TIME ( -- )
   NAME$ JUDGE-ROW:FIND {: k:n :}
   k OLD-TIME
   k JUDGE-ROW:REFUSED? 0= if k NEW-TIME then
   k JUDGE-ROW:COVERED? 0= if exit then
   k REF-TIME ;

\ The measurement's own floor: the timing word run over an empty quotation, which
\ is what every habu column's number carries in common.
: FLOOR ( -- )
   JUDGE-COST:FLOOR JUDGE-COST:FLOOR JUDGE-ROW:SAMPLE JUDGE-ROW:FLOOR! ;

;package
