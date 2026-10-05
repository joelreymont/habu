\ checker-completion.f - the spellings that bind at a cursor, as the checker
\ selects them (src/core/checker.f CHECKER-CURSOR!, CHECKER-RESOLVE:EACH-VISIBLE).
\ It reads checker internals, so it is a WHITEBOX-SUITE row and runs on the
\ unsealed engine (test/whitebox-engine.f):
\   hb-whitebox --load test/checker-completion.f
\
\ The verifier arms a cursor in the next body it checks (VERIFY-CURSOR-OFF) and
\ asks which spellings bind at a top-level one (VERIFY-EACH-VISIBLE-OFF). The
\ declarations replay as the verifier reads them (VERIFY:SOURCE-BUF-IN-SCOPE),
\ so each body is checked on the verifier's own path. The text the checker
\ reads is the definition's tokens from its name on, each followed by one
\ space (src/habu/verify-source.f BODY-APPEND), so in a single-spaced source
\ a cursor sits at its source byte less the end of the `: ` before the name.
\
\ How this can fail, each one a case below:
\ - the cursor never fires, or fires at the wrong token: in the gap before a
\   token, inside it, after the last one;
\ - the prefix holds bytes past the cursor, or misses bytes before it;
\ - it fires on the definition's name, or inside its signature;
\ - an arm outlives its check, a body's or a does> clause's, after a refusal,
\   a visitor's throw or a use handler's throw, so the next body fires at a
\   stale cursor or the next arm is refused;
\ - a nested arm or a nested enumeration changes the outer one;
\ - a local is offered inside a quotation, or for a tick or `is` target;
\ - two locals that differ only in case merge, or a spelling a local holds is
\   offered as the word that local hides;
\ - a refused spelling (one two used publics export) or a tombstone is offered;
\ - the definition's own name is offered before it is declared;
\ - enumerating changes the body's verdict, its refusal or its uses, or grows
\   the store;
\ - a word is offered twice, once for each source that names it;
\ - a qualified prefix offers a private name, or a bare prefix offers bare a
\   public of a package not in use;
\ - a bare prefix, a package's stem or its edge colon misses a qualified
\   spelling a qualified prefix offers there, or one only the dictionary holds;
\ - a candidate's location is not its selected declaration's: a located word
\   offered without its row's visit and bytes, a word the selection hides
\   lending its location, a refused redeclaration moving it, or a local, an
\   engine word, a word declared unarmed or one at a rewound row's offset
\   offered with anything but 0 0 0;
\ - an engine word the body has not used yet is missing;
\ - a word is not spelled as it was declared: a located word as its
\   declaration wrote it, a qualified one as the prefix's qualifier, else its
\   package's, and the declared tail, an engine word as the dictionary holds
\   it; or a word the selection hides lends its spelling to the one it
\   selects;
\ - a refused redeclaration renames the word it failed to replace, or a
\   spelling dies with the bytes it was armed from.
\
\ A coverage limit, not an exact spelling: a name the checker generates with
\ no declaration of its own (a suffix registration) keeps the store's folded
\ spelling, so no case here pins one.

require lib/test.f
require src/habu/verify-source.f
require test/checker-decl-locs-lib.f

\ A public does> definer this file loads: the dictionary holds its clause's
\ record, QALDEF;does, in QALD's public wordlist, and the store holds no
\ symbol for it (case 12).
package QALD
public
: QALDEF ( n -- ) create , does> ( -- n ) @ ;
;package

package NAVL

private

\ ---- what one check offered ---------------------------------------------------
\ The visitor counts every spelling it is given and keeps the ones that start
\ with the case's filter, with their location. A filter names a handful of
\ fixture words, so the table is the fixture's size; a spelling past it is
\ counted in KEPT-OVER, which every case asserts is 0.
32 constant KEPT-CAP
48 constant KEPT-W
create KEPT-NAMES KEPT-CAP KEPT-W * allot
create KEPT-LENS KEPT-CAP cells allot
create KEPT-VISITS KEPT-CAP cells allot
create KEPT-STARTS KEPT-CAP cells allot
create KEPT-ENDS KEPT-CAP cells allot
variable KEPT-N
variable KEPT-OVER
variable CALLS
PTR-VARIABLE FILTER-A   variable FILTER-U

: KEPT-RESET ( ptr u8 n -- )
   FILTER-U !  FILTER-A !
   0 KEPT-N !  0 KEPT-OVER !  0 CALLS ! ;

\ A U starts with the filter, folded.
: FILTERED? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u FILTER-U @ < IF false EXIT THEN
   a FILTER-U @ FILTER-A @ FILTER-U @ CORE-STR=CI ;

: KEEP ( ptr u8 n n n n -- )
   {: a:ptr u:n v:n s:n e:n :}
   1 CALLS +!
   a u FILTERED? 0= IF EXIT THEN
   KEPT-N @ KEPT-CAP >=  u KEPT-W >  or IF 1 KEPT-OVER +! EXIT THEN
   a  KEPT-NAMES KEPT-N @ KEPT-W * +  u BYTE-COPY
   u  KEPT-LENS KEPT-N @ cells + !
   v  KEPT-VISITS KEPT-N @ cells + !
   s  KEPT-STARTS KEPT-N @ cells + !
   e  KEPT-ENDS KEPT-N @ cells + !
   1 KEPT-N +! ;

: KEPT$ ( n -- ptr u8 n )
   {: i:n :}
   KEPT-NAMES i KEPT-W * +  KEPT-LENS i cells + @ ;

\ The first kept spelling equal to A U, byte for byte, or -1.
: KEPT-AT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   KEPT-N @ 0 ?DO
      a u i KEPT$ CORE-STR= IF i UNLOOP EXIT THEN
   LOOP
   -1 ;

\ How many kept spellings equal A U folded.
: KEPT-CI ( ptr u8 n -- n )
   {: a:ptr u:n :}
   0  KEPT-N @ 0 ?DO
      a u i KEPT$ CORE-STR=CI IF 1 + THEN
   LOOP ;

\ The first kept spelling equal to A U folded, or -1: for a word whose case
\ the store does not keep, so no case pins one.
: KEPT-CI-AT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   KEPT-N @ 0 ?DO
      a u i KEPT$ CORE-STR=CI IF i UNLOOP EXIT THEN
   LOOP
   -1 ;

\ A U was kept once in any case, and spelled exactly so.
: ONCE ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u KEPT-CI 1 =  a u KEPT-AT 0 >=  and ;

\ ---- assertions -----------------------------------------------------------------
\ Every case kept what its filter named.
: KEPT-ALL ( -- )
   s" the filter's spellings fit the table" T-LABEL
   KEPT-OVER @ 0 T= ;

\ Kept spelling I carries the location V S E.
: KEPT-LOC ( n n n n -- )
   {: i:n v:n s:n e:n :}
   KEPT-VISITS i cells + @ v T=
   KEPT-STARTS i cells + @ s T=
   KEPT-ENDS i cells + @ e T= ;

\ NAME was offered once in any case, spelled exactly so, at V S E.
: PLACED ( ptr u8 n n n n -- )
   {: a:ptr u:n v:n s:n e:n :}
   a u T-LABEL
   a u KEPT-CI 1 T=
   a u KEPT-AT {: i:n :}
   i 0 >= TTRUE
   i 0 < IF EXIT THEN
   i v s e KEPT-LOC ;

\ NAME was offered once, exactly so spelled, with no location: an engine word,
\ which no declaration on this path located.
: UNPLACED ( ptr u8 n -- )
   0 0 0 PLACED ;

\ NAME was offered once, exactly so spelled, where its newest row was declared.
: OFFERED ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u ROW CHECKER-REC-DECL-AT {: v:n s:n e:n found:bool :}
   a u T-LABEL
   found TTRUE
   a u v s e PLACED ;

\ NAME was offered once in any case, with no location: a word declared with no
\ spelling armed, whose case the store does not keep, so no case pins it.
: FOLDED ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u T-LABEL
   a u KEPT-CI 1 T=
   a u KEPT-CI-AT {: i:n :}
   i 0 >= TTRUE
   i 0 < IF EXIT THEN
   i 0 0 0 KEPT-LOC ;

\ NAME was not offered, in any case.
: ABSENT ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u T-LABEL
   a u KEPT-CI 0 T= ;

\ The local NAME was offered, exactly so spelled, with no location.
: LOCAL ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u T-LABEL
   a u KEPT-AT {: i:n :}
   i 0 >= TTRUE
   i 0 < IF EXIT THEN
   i 0 0 0 KEPT-LOC ;

\ ---- a replay with the cursor armed ---------------------------------------------
PTR-VARIABLE TEXT-A   variable TEXT-U
: REPLAY ( -- ) TEXT-A @ TEXT-U @ VERIFY:SOURCE-BUF-IN-SCOPE ;

\ Replay TEXT unarmed; the replay's throw code, 0 when it ran through.
: PLAIN ( ptr u8 n -- n )
   TEXT-U !  TEXT-A !
   [: REPLAY ;] catch ;

\ Replay TEXT with the cursor armed at checked-text byte AT and VISIT the
\ visitor; the replay's throw code. Nothing disarms here: an arm the check
\ failed to release makes the next case's arm throw.
: ARMED ( ptr u8 n n [ ptr u8 n n n n -- ] -- n )
   CURSOR!  PLAIN ;

\ Replay TEXT with NAME's declaration armed at visit V, bytes S to E, as the
\ verifier arms a registrar call in a file it visits; the replay's throw code.
: NAMED-AT ( ptr u8 n ptr u8 n n n n -- n )
   {: na:ptr nu:n a:ptr u:n v:n s:n e:n :}
   na nu v s e ARM
   a u PLAIN
   DISARM ;

\ NAMED-AT the armed location.
: NAMED ( ptr u8 n ptr u8 n -- n )
   VISIT AT-START AT-END NAMED-AT ;

\ ARMED, with NAME's declaration armed too: the word TEXT declares keeps its
\ spelling.
: DECLARED ( ptr u8 n ptr u8 n n [ ptr u8 n n n n -- ] -- n )
   CURSOR!  NAMED ;

\ The offset of the first NEEDLE in TEXT, or -1.
: FIND$ ( ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n b:ptr v:n :}
   u v - 1 + 0 ?DO
      a i + v b v CORE-STR= IF i UNLOOP EXIT THEN
   LOOP
   -1 ;

\ The checked-text byte K bytes into the first NEEDLE of the one-line source
\ TEXT: the checked text starts at the name after the first `: `.
: AT ( ptr u8 n ptr u8 n n -- n )
   {: a:ptr u:n b:ptr v:n k:n :}
   a u b v FIND$ k +  a u s" : " FIND$ 2 + - ;

\ ---- the fixture ----------------------------------------------------------------
\ Two more locations: the package's DSCITEM is declared at one, so it is told
\ from the global dscitem it hides, which takes the armed location; a refused
\ redeclaration is armed at the other.
8 constant DSC-VISIT    120 constant DSC-START   127 constant DSC-END
9 constant RE-VISIT     130 constant RE-START    140 constant RE-END

\ CMPA and CMPB are located, so a body's calls of them are published uses.
: FIXTURE ( -- )
   s" CMPA" s" : CMPA ( -- n ) 1 ;" [: ;] ARMED-REPLAY
   s" CMPB" s" : CMPB ( -- n ) 2 ;" [: ;] ARMED-REPLAY
   s" ITEM" s" : ITEM ( -- n ) 3 ;" [: ;] ARMED-REPLAY
   s" CMPDF" s" defer CMPDF ( -- n )" [: ;] ARMED-REPLAY
   s" : CMPGONE ( -- n ) 4 ; undefine CMPGONE" VERIFY:SOURCE-BUF-IN-SCOPE
   s" CMPPRI" s" package CMPP : CMPPRI ( -- n ) 5 ; ;package" [: ;] ARMED-REPLAY
   s" CMPPUB" s" package CMPP public : CMPPUB ( -- n ) 6 ; ;package" [: ;] ARMED-REPLAY
   s" package CMPX public : CMPAMB ( -- n ) 7 ; ;package" VERIFY:SOURCE-BUF-IN-SCOPE
   s" package CMPY public : CMPAMB ( -- n ) 8 ; ;package" VERIFY:SOURCE-BUF-IN-SCOPE
   s" dscitem" s" : dscitem ( -- n ) 9 ;" [: ;] ARMED-REPLAY
   s" DSCITEM" s" package DSCP : DSCITEM ( -- n ) 10 ; ;package"
   DSC-VISIT DSC-START DSC-END [: ;] PLACED-REPLAY
   s" package DSCQ : byte-copy ( -- n ) 14 ; ;package" VERIFY:SOURCE-BUF-IN-SCOPE ;

\ ---- 1. a body token -------------------------------------------------------------
: PARTIAL$ ( -- ptr u8 n ) s" : CMPC ( -- n ) CMPA CMPB + ;" ;

: CASE-PARTIAL ( -- )
   s" cmp" KEPT-RESET
   s" a partial token's check runs through" T-LABEL
   PARTIAL$  PARTIAL$ s" CMPB" 2 AT  [: KEEP ;] ARMED 0 T=
   KEPT-ALL
   s" CMPA" OFFERED
   s" CMPB" OFFERED
   s" CMPDF" VISIT AT-START AT-END PLACED
   s" CMPC" ABSENT
   s" CMPGONE" ABSENT
   s" CMPPUB" ABSENT
   s" CMPPRI" ABSENT
   s" CMPAMB" ABSENT
   s" a package not in use offers its public behind its name" T-LABEL
   s" CMPP:CMPPUB" VISIT AT-START AT-END PLACED
   s" CMPP:CMPPRI" ABSENT ;

\ The gap before a token and the end of the text offer every visible name.
: CASE-GAP ( -- )
   s" cmp" KEPT-RESET
   s" CMPC1"  s" : CMPC1 ( -- n ) CMPA CMPB + ;" 2dup s" CMPB" 0 AT  [: KEEP ;] DECLARED
   s" the gap's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPA" OFFERED
   s" CMPB" OFFERED
   s" cmp" KEPT-RESET
   s" : CMPC2 ( -- n ) CMPA CMPB + ;" 2dup s" ;" 0 AT  [: KEEP ;] ARMED
   s" the end's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPA" OFFERED
   s" CMPB" OFFERED
   s" CMPC1" OFFERED ;

\ The name and the signature are not body tokens.
: CASE-NOT-BODY ( -- )
   s" cmp" KEPT-RESET
   s" : CMPC3 ( -- n ) CMPA ;" 2dup s" CMPC3" 2 AT  [: KEEP ;] ARMED
   s" the name's check runs through" T-LABEL 0 T=
   s" a cursor in the name offers nothing" T-LABEL
   CALLS @ 0 T=
   s" : CMPC4 ( -- n ) CMPA ;" 2dup s" --" 1 AT  [: KEEP ;] ARMED
   s" the signature's check runs through" T-LABEL 0 T=
   s" a cursor in the signature offers nothing" T-LABEL
   CALLS @ 0 T= ;

\ ---- 2. an engine word the body has not used --------------------------------------
: CASE-ENGINE ( -- )
   s" du" KEPT-RESET
   s" : CMPN ( n -- n n ) dup ;" 2dup s" dup" 2 AT  [: KEEP ;] ARMED
   s" the engine word's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" dup" UNPLACED
   s" byte-cop" KEPT-RESET
   s" : CMPN2 ( n -- n ) ;" 2dup s" ;" 0 AT  [: KEEP ;] ARMED
   s" the empty body's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" BYTE-COPY" UNPLACED ;

\ ---- 3. locals -------------------------------------------------------------------
\ Locals keep their case, and a word's spelling a local holds is the local.
: CASE-LOCAL-CASE ( -- )
   s" it" KEPT-RESET
   s" : CMPK ( n n -- n ) {: item:n ITEM:n :} item ITEM + ;" 2dup s" ITEM +" 2 AT
   [: KEEP ;] ARMED
   s" the locals' check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" item" LOCAL
   s" ITEM" LOCAL
   s" the word ITEM is not offered as a third item" T-LABEL
   s" item" KEPT-CI 2 T= ;

: CASE-LOCAL ( -- )
   s" cmp" KEPT-RESET
   s" : CMPL ( n -- n ) {: CMPLOC:n :} CMPLOC ;" 2dup s" CMPLOC ;" 2 AT
   [: KEEP ;] ARMED
   s" the local's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPLOC" LOCAL
   s" CMPA" OFFERED ;

\ Inside a quotation, and for a tick or `is` target, no local binds.
: CASE-NO-LOCAL ( -- )
   s" cmp" KEPT-RESET
   s" : CMPQ ( n -- n ) {: CMPLOC:n :} [: CMPA ;] drop CMPLOC ;" 2dup s" CMPA ;]" 2 AT
   [: KEEP ;] ARMED
   s" the quotation's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPA" OFFERED
   s" cmploc" ABSENT
   s" cmp" KEPT-RESET
   s" : CMPT ( n -- n ) {: CMPLOC:n :} ['] CMPA drop CMPLOC ;" 2dup s" ['] CMPA" 6 AT
   [: KEEP ;] ARMED
   s" the tick's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPA" OFFERED
   s" cmploc" ABSENT
   s" cmp" KEPT-RESET
   s" : CMPI ( n -- n ) {: CMPLOC:n :} [: CMPA ;] is CMPDF CMPLOC ;" 2dup s" is CMPDF" 5 AT
   [: KEEP ;] ARMED
   s" the is target's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPDF" VISIT AT-START AT-END PLACED
   s" cmploc" ABSENT ;

\ ---- 4. what the scope refuses, and qualified names ------------------------------
: CASE-AMBIGUOUS ( -- )
   s" cmp" KEPT-RESET
   s" using CMPX using CMPY : CMPU ( -- n ) CMPA ; ;using ;using" 2dup s" CMPA ;" 2 AT
   [: KEEP ;] ARMED
   s" the using scope's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPA" OFFERED
   s" CMPAMB" ABSENT
   s" each package's public is offered behind its name" T-LABEL
   s" CMPX:CMPAMB" FOLDED
   s" CMPY:CMPAMB" FOLDED ;

: CASE-QUALIFIED ( -- )
   s" cmpp:" KEPT-RESET
   s" : CMPV ( -- n ) CMPP:CMPPUB ;" 2dup s" CMPP:CMPPUB" 7 AT  [: KEEP ;] ARMED
   s" the qualified call's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPP:CMPPUB" VISIT AT-START AT-END PLACED
   s" CMPP:CMPPRI" ABSENT ;

\ ---- 5. a top-level cursor --------------------------------------------------------
PTR-VARIABLE TOP-A   variable TOP-U
: TOP-ASK ( -- )
   TOP-A @ TOP-U @ CHECKER-OWNER-ABI:VISIBLE-TOP [: KEEP ;] EACH-VISIBLE ;

\ The spellings that bind at top level under PREFIX, counted against the store.
: TOP ( ptr u8 n -- )
   TOP-U !  TOP-A !
   SYM-N @ {: n0:n :}
   s" " [: TOP-ASK ;] VERIFY:SOURCE-BUF-THEN-IN-SCOPE
   s" a top-level enumeration grows no store" T-LABEL
   SYM-N @ n0 T= ;

: CASE-TOP ( -- )
   s" cmp" KEPT-RESET
   s" CM" TOP
   KEPT-ALL
   s" CMPA" OFFERED
   s" CMPDF" VISIT AT-START AT-END PLACED
   s" CMPGONE" ABSENT
   s" CMPPUB" ABSENT
   s" CMPAMB" ABSENT
   s" cmpp:" KEPT-RESET
   s" CMPP:CM" TOP
   KEPT-ALL
   s" CMPP:CMPPUB" VISIT AT-START AT-END PLACED
   s" CMPP:CMPPRI" ABSENT ;

\ ---- 6. the observer changes nothing ------------------------------------------------
\ Two bodies of the same layout, one checked unarmed and one armed: the same
\ verdict, the same published uses at the same bytes, and the same store growth.
variable USE-N   variable USE-SUM

: COUNT-USE ( n n n n n -- )
   {: s:n e:n v:n ds:n de:n :}
   1 USE-N +!  s e + USE-SUM +! ;

: COUNTED ( -- ) [: COUNT-USE ;] [: REPLAY ;] WITH-USES ;

\ Replay TEXT counting its uses; its throw code, uses, byte sum and store growth.
: OBSERVED ( ptr u8 n -- n n n n )
   TEXT-U !  TEXT-A !
   0 USE-N !  0 USE-SUM !
   SYM-N @ {: n0:n :}
   [: COUNTED ;] catch  USE-N @  USE-SUM @  SYM-N @ n0 - ;

: CASE-OBSERVER ( -- )
   s" cmp" KEPT-RESET
   s" : CMPE ( -- n ) CMPA CMPB + ;" OBSERVED {: rc0:n n0:n sum0:n grew0:n :}
   s" the unarmed body offers nothing" T-LABEL
   CALLS @ 0 T=
   s" : CMPF ( -- n ) CMPA CMPB + ;" 2dup s" CMPB" 2 AT [: KEEP ;] CURSOR!
   OBSERVED {: rc1:n n1:n sum1:n grew1:n :}
   s" the unarmed body certifies" T-LABEL
   rc0 0 T=
   s" the armed body certifies" T-LABEL
   rc1 0 T=
   s" both publish their two calls" T-LABEL
   n0 2 T=
   n1 2 T=
   s" at the same bytes" T-LABEL
   sum1 sum0 T=
   s" the store grows the same" T-LABEL
   grew1 grew0 T=
   s" the armed body offered its candidates" T-LABEL
   s" CMPA" ONCE TTRUE ;

\ The same for a refused body: the same refusal, uses and growth.
: CASE-OBSERVER-REFUSED ( -- )
   s" cmp" KEPT-RESET
   s" : CMPE2 ( -- n ) CMPA CMPB ;" OBSERVED {: rc0:n n0:n sum0:n grew0:n :}
   s" the unarmed refused body offers nothing" T-LABEL
   CALLS @ 0 T=
   s" : CMPF2 ( -- n ) CMPA CMPB ;" 2dup s" CMPB" 2 AT [: KEEP ;] CURSOR!
   OBSERVED {: rc1:n n1:n sum1:n grew1:n :}
   s" the unarmed body is refused" T-LABEL
   rc0 0 T<>
   s" the armed body is refused the same" T-LABEL
   rc1 rc0 T=
   s" both publish the same uses" T-LABEL
   n1 n0 T=
   s" at the same bytes" T-LABEL
   sum1 sum0 T=
   s" the store grows the same" T-LABEL
   grew1 grew0 T=
   s" the armed refused body offered its candidates" T-LABEL
   s" CMPA" ONCE TTRUE ;

\ ---- 7. one arm at a time, released on every exit ----------------------------------
: CASE-NESTED-ARM ( -- )
   s" a second arm while one is armed is refused by name" T-LABEL
   5 [: KEEP ;] CURSOR!
   [: 6 [: KEEP ;] CURSOR! ;] E-CURSOR-NESTED TTHROWSQ
   -1 [: KEEP ;] CURSOR!
   s" cmp" KEPT-RESET
   s" : CMPG ( -- n ) CMPA ;" PLAIN
   s" the disarmed check runs through" T-LABEL 0 T=
   s" the disarmed check offers nothing" T-LABEL
   CALLS @ 0 T= ;

variable NEST-ARM   variable NEST-ENUM
: NEST-ARM-TRY ( -- ) 5 [: KEEP ;] CURSOR! ;
: NEST-ENUM-TRY ( -- ) s" CM" CHECKER-OWNER-ABI:VISIBLE-TOP [: KEEP ;] EACH-VISIBLE ;

\ A visitor that tries to arm and to enumerate before it keeps its spelling.
: NESTING ( ptr u8 n n n n -- )
   [: NEST-ARM-TRY ;] catch NEST-ARM !
   [: NEST-ENUM-TRY ;] catch NEST-ENUM !
   KEEP ;

: CASE-NESTED ( -- )
   s" cmp" KEPT-RESET
   0 NEST-ARM !  0 NEST-ENUM !
   s" : CMPJ ( -- n ) CMPA ;" 2dup s" CMPA ;" 2 AT  [: NESTING ;] ARMED
   s" the nesting visitor's check runs through" T-LABEL 0 T=
   s" an arm inside the visitor is refused by name" T-LABEL
   NEST-ARM @ E-CURSOR-NESTED T=
   s" an enumeration inside the visitor is refused by name" T-LABEL
   NEST-ENUM @ E-CURSOR-NESTED T=
   KEPT-ALL
   s" CMPA" OFFERED ;

\ A visitor that keeps its spelling and then throws.
: THROWING ( ptr u8 n n n n -- )
   KEEP  1 0 / drop ;

: CASE-RELEASE ( -- )
   s" cmp" KEPT-RESET
   s" : CMPR ( -- n ) CMPA CMPNOPE ;" 2dup s" CMPA" 2 AT  [: KEEP ;] ARMED
   s" a refused body throws its refusal" T-LABEL 0 T<>
   s" a refused body still offers what binds" T-LABEL
   s" CMPA" ONCE TTRUE
   s" cmp" KEPT-RESET
   s" : CMPS ( -- n ) CMPA ;" PLAIN
   s" the next body after a refusal runs through" T-LABEL 0 T=
   s" the arm did not outlive the refusal" T-LABEL
   CALLS @ 0 T=
   s" : CMPW ( -- n ) CMPA ;" 2dup s" CMPA" 2 AT  [: THROWING ;] ARMED
   s" the visitor's throw leaves the check" T-LABEL E-DIV-ZERO T=
   s" cmp" KEPT-RESET
   s" : CMPX1 ( -- n ) CMPA ;" PLAIN
   s" the next body after a throw runs through" T-LABEL 0 T=
   s" the arm did not outlive the throw" T-LABEL
   CALLS @ 0 T=
   s" : CMPY1 ( -- n ) CMPA ;" 2dup s" CMPA" 2 AT  [: KEEP ;] ARMED
   s" a new arm after a throw runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPA" OFFERED ;

\ A use handler that throws, as the owner's handler of published uses can.
: USE-THROWS ( n n n n n -- )
   2drop 2drop drop  1 0 / drop ;

: USES-THROW ( -- ) [: USE-THROWS ;] [: REPLAY ;] WITH-USES ;

\ The use handler's throw leaves the check after the arm is released: its
\ candidates are published, and the next arm, disarm and body check find no
\ arm left owned.
: CASE-USE-THROW ( -- )
   s" cmp" KEPT-RESET
   s" : CMPH ( -- n ) CMPA ;" 2dup s" CMPA" 2 AT  [: KEEP ;] CURSOR!
   TEXT-U !  TEXT-A !
   s" the use handler's throw leaves the check" T-LABEL
   [: USES-THROW ;] catch E-DIV-ZERO T=
   KEPT-ALL
   s" CMPA" OFFERED
   s" an arm after the handler's throw is taken" T-LABEL
   [: 5 [: KEEP ;] CURSOR! ;] catch 0 T=
   s" and so is a disarm" T-LABEL
   [: -1 [: KEEP ;] CURSOR! ;] catch 0 T=
   s" cmp" KEPT-RESET
   s" : CMPH2 ( -- n ) CMPA ;" PLAIN
   s" the next body after the handler's throw runs through" T-LABEL 0 T=
   s" it fires no stale visitor" T-LABEL
   CALLS @ 0 T= ;

\ The same throw from a does> clause, which the verifier checks after the
\ definer's body (CHECKER-SOURCE-DOES!). The body's use arms the cursor, so
\ the clause takes the arm; the clause's use throws. The clause's checked text
\ is `@ CMPB + `, so its byte 4 is in CMPB after `CM`.
variable DOES-USES
: DOES-USE ( n n n n n -- )
   1 DOES-USES +!
   DOES-USES @ 1 > IF USE-THROWS EXIT THEN
   2drop 2drop drop
   4 [: KEEP ;] CURSOR! ;

: DOES-USES-THROW ( -- ) [: DOES-USE ;] [: REPLAY ;] WITH-USES ;

: CASE-DOES-USE-THROW ( -- )
   s" cmp" KEPT-RESET
   0 DOES-USES !
   s" : CMPMK ( n -- ) CMPA + create , does> ( -- n ) @ CMPB + ;" TEXT-U !  TEXT-A !
   s" the clause's use handler throw leaves the check" T-LABEL
   [: DOES-USES-THROW ;] catch E-DIV-ZERO T=
   s" the body's use armed the clause, whose use threw" T-LABEL
   DOES-USES @ 2 T=
   KEPT-ALL
   s" CMPB" OFFERED
   s" an arm after the clause's handler throw is taken" T-LABEL
   [: 5 [: KEEP ;] CURSOR! ;] catch 0 T=
   s" and so is a disarm" T-LABEL
   [: -1 [: KEEP ;] CURSOR! ;] catch 0 T=
   s" cmp" KEPT-RESET
   s" : CMPH3 ( -- n ) CMPA ;" PLAIN
   s" the next body after the clause's throw runs through" T-LABEL 0 T=
   s" it fires no stale visitor" T-LABEL
   CALLS @ 0 T= ;

\ ---- 8. the selection spells what it binds ----------------------------------------
\ An earlier global dscitem, the open package's DSCITEM and a body's local
\ dscitem: the local and the package's word are offered, each as written, and
\ the global the package's word hides lends neither its spelling nor its
\ location.
: DISC$ ( -- ptr u8 n )
   s" package DSCP : DSCB ( n -- n ) {: dscitem:n :} dscitem DSCITEM + ; ;package" ;

: CASE-DISCRIMINATOR ( -- )
   s" dsc" KEPT-RESET
   DISC$  DISC$ s" DSCITEM +" 4 AT  [: KEEP ;] ARMED
   s" the discriminator's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" dscitem" LOCAL
   s" the package's DSCITEM is offered as written, where the package declared it" T-LABEL
   s" DSCITEM" KEPT-AT {: i:n :}
   i 0 >= TTRUE
   i 0 < IF EXIT THEN
   i DSC-VISIT DSC-START DSC-END KEPT-LOC
   s" not where the global it hides was declared" T-LABEL
   s" dscitem" ROW CHECKER-REC-DECL-AT {: v:n s:n e:n found:bool :}
   found TTRUE
   v VISIT T=
   KEPT-VISITS i cells + @ v T<>
   s" nothing else is spelled dscitem" T-LABEL
   s" dscitem" KEPT-CI 2 T= ;

\ The open package's byte-copy, declared with no spelling armed, hides the
\ engine's BYTE-COPY: what is offered is the package's word, with no location,
\ and the hidden engine record does not lend it its spelling.
: HIDE$ ( -- ptr u8 n ) s" package DSCQ : DSCH ( -- n ) BYTE-COPY ; ;package" ;

: CASE-HIDDEN ( -- )
   s" byte-c" KEPT-RESET
   HIDE$  HIDE$ s" BYTE-COPY ;" 6 AT  [: KEEP ;] ARMED
   s" the hiding word's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" byte-copy" FOLDED
   s" the hidden engine word lends it no spelling" T-LABEL
   s" BYTE-COPY" KEPT-AT 0 < TTRUE ;

\ A refused redeclaration renames and moves nothing: its row goes, and its
\ spelling and location with it.
: CASE-REDECLARED ( -- )
   s" a redeclaration in another case is refused" T-LABEL
   s" cmpa" s" : cmpa ( -- n ) 12 ;" RE-VISIT RE-START RE-END NAMED-AT 0 T<>
   s" cmpa" KEPT-RESET
   s" CM" TOP
   KEPT-ALL
   s" CMPA" VISIT AT-START AT-END PLACED ;

\ ---- 9. declarations the verifier arms itself --------------------------------------
\ Composed with a visit, the verifier arms each declaration as it reads it: a
\ DEFTYPE's two converters each by its own name, a qualified EXPORT by the tail
\ it is kept under, and a trust row keeps the name its declaration wrote.
: VISITED$ ( -- ptr u8 n )
   s\" DEFTYPE CmpCol\npackage CMPEA public : CMPEXA ( -- n ) 15 ; ;package\npackage CMPEB public EXPORT CMPEA:CMPEXA ;package\n: CmpTr ( -- n ) 16 ;\ns\" cmptr\" s\" -- n\" trust\n" ;

: COMPOSED ( -- ) VISITED$ s" cmp-visited.f" VERIFY:SOURCE-COMPOSE-IN-SCOPE ;

: CASE-VISITED ( -- )
   s" the composition runs through" T-LABEL
   [: COMPOSED ;] catch 0 T=
   s" >cmpcol" KEPT-RESET
   s" >CMPC" TOP
   KEPT-ALL
   s" >CmpCol" OFFERED
   s" cmpcol>" KEPT-RESET
   s" CMPCOL>" TOP
   KEPT-ALL
   s" CmpCol>N" OFFERED
   s" cmpeb:" KEPT-RESET
   s" CMPEB:CMPE" TOP
   KEPT-ALL
   s" CMPEB:CMPEXA" OFFERED
   s" cmptr" KEPT-RESET
   s" CMPT" TOP
   KEPT-ALL
   s" CmpTr" OFFERED ;

\ A required file's declaration keeps the spelling it was armed with after the
\ loader unmaps that file's bytes, when its scan ends: the cursor sits in the
\ body of the subject that required it (test/checker-completion-dep.f).
: DEP-SUBJECT$ ( -- ptr u8 n )
   s\" require test/checker-completion-dep.f\n: CMPDU ( -- n ) CmpDep ;\n" ;

: DEP-COMPOSED ( -- )
   DEP-SUBJECT$ s" test/cmp-dep-subject.f" VERIFY:SOURCE-COMPOSE-IN-SCOPE ;

: CASE-DEPENDENCY ( -- )
   s" cmpdep" KEPT-RESET
   DEP-SUBJECT$ s" CmpDep ;" 4 AT  [: KEEP ;] CURSOR!
   s" the subject's composition runs through" T-LABEL
   [: DEP-COMPOSED ;] catch 0 T=
   KEPT-ALL
   s" CmpDep" OFFERED ;

\ ---- 10. a rewind, then the same offset -------------------------------------------
\ A neutral scope's rewind takes a located row back, and the next word the
\ store retains, declared unarmed, lands at its offset: it is offered with no
\ location, where a row the rewind left would lend it one.
variable REW-ROW

: REWOUND ( -- )
   s" CMPRW" s" : CMPRW ( -- n ) 17 ;" [: ;] ARMED-REPLAY
   s" CMPRW" ROW REW-ROW ! ;

: CASE-REWOUND ( -- )
   [: REWOUND ;] IN-NEUTRAL
   s" : CMPRX ( -- n ) 18 ;" VERIFY:SOURCE-BUF-IN-SCOPE
   s" the next word takes the rewound offset" T-LABEL
   s" CMPRX" ROW REW-ROW @ T=
   s" cmpr" KEPT-RESET
   s" : CMPRY ( -- n ) CMPRX ;" 2dup s" CMPRX" 2 AT  [: KEEP ;] ARMED
   s" the rewound offset's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" cmprx" FOLDED
   s" cmprw" ABSENT ;

\ ---- 11. qualified spellings under any prefix ---------------------------------------
\ A bare prefix, a package's stem and its edge colon offer the package's publics
\ behind its name, as the qualified prefix does: each as its selection spells
\ and locates it - a replacement as its own declaration wrote it, where that
\ declaration was - and nothing the selection refuses: a private word, a
\ tombstone, or a public's own name inside its body. The qualifier is the one
\ the prefix typed, else the package's own spelling.
10 constant QRE-VISIT   150 constant QRE-START   160 constant QRE-END

: QAL-FIXTURE ( -- )
   s" QALPUB" s" package QALP public : QALPUB ( -- n ) 30 ; ;package" [: ;] ARMED-REPLAY
   s" package QALP public : QALGONE ( -- n ) 31 ; undefine QALGONE ;package" VERIFY:SOURCE-BUF-IN-SCOPE
   s" package QALP : QALPRI ( -- n ) 32 ; ;package" VERIFY:SOURCE-BUF-IN-SCOPE
   s" QALRE" s" package QALP public : QALRE ( -- n ) 33 ; ;package" [: ;] ARMED-REPLAY
   s" qalre" s" package QALP public undefine QALRE : qalre ( -- n ) 34 ; ;package"
   QRE-VISIT QRE-START QRE-END [: ;] PLACED-REPLAY
   s" QalW" s" package QalPk public : QalW ( -- n ) 35 ; ;package" [: ;] ARMED-REPLAY ;

\ What every prefix of QALP:QALPUB offers behind QALP.
: QAL-SEEN ( -- )
   KEPT-ALL
   s" QALP:QALPUB" VISIT AT-START AT-END PLACED
   s" QALP:qalre" QRE-VISIT QRE-START QRE-END PLACED
   s" QALP:QALGONE" ABSENT
   s" QALP:QALPRI" ABSENT ;

: SELF$ ( -- ptr u8 n ) s" package QALP public : QALSELF ( -- n ) QALP:QALPUB ; ;package" ;

\ Inside a package its name qualifies a global too, as the walk binds it: the
\ prefix K bytes into TEXT's QALP:CMPA offers that spelling, the global's own.
: ALIAS-AT ( ptr u8 n n -- )
   {: a:ptr u:n k:n :}
   s" qalp:cmpa" KEPT-RESET
   a u  a u s" QALP:CMPA" k AT  [: KEEP ;] ARMED
   s" the open package's body's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" QALP:CMPA" VISIT AT-START AT-END PLACED ;

: CASE-QUALIFIED-ANY ( -- )
   QAL-FIXTURE
   s" qalp:" KEPT-RESET  s" Q" TOP  QAL-SEEN
   s" qalp:" KEPT-RESET  s" QALP" TOP  QAL-SEEN
   s" qalp:" KEPT-RESET  s" QALP:" TOP  QAL-SEEN
   s" qalp:" KEPT-RESET  s" QALP:Q" TOP  QAL-SEEN
   s" qalp:qal" KEPT-RESET
   SELF$  SELF$ s" QALP:QALPUB" 1 AT  [: KEEP ;] ARMED
   s" the public's body's check runs through" T-LABEL 0 T=
   QAL-SEEN
   s" QALP:QALSELF" ABSENT
   s" package QALP public : QALAL1 ( -- n ) QALP:CMPA ; ;package" 1 ALIAS-AT
   s" package QALP public : QALAL2 ( -- n ) QALP:CMPA ; ;package" 6 ALIAS-AT
   s" qalpk:" KEPT-RESET  s" QA" TOP
   KEPT-ALL
   s" QalPk:QalW" VISIT AT-START AT-END PLACED
   s" qalpk:" KEPT-RESET  s" QALPK:" TOP
   KEPT-ALL
   s" QALPK:QalW" VISIT AT-START AT-END PLACED
   s" qalpk:" KEPT-RESET  s" qalpk:Q" TOP
   KEPT-ALL
   s" qalpk:QalW" VISIT AT-START AT-END PLACED ;

\ ---- 12. a public only the dictionary holds -----------------------------------------
\ QALD:QALDEF;does, which the store holds no symbol for, is offered under a
\ bare prefix, the package's edge colon and a qualified prefix, as the
\ dictionary spells it, with no location; its definer, which has a symbol too,
\ once.
: CASE-DICTIONARY ( -- )
   s" the store holds no symbol for the clause" T-LABEL
   s" QALD" s" QALDEF;does" CHECKER-PUBLIC-SYM? 0 T=
   s" qald:" KEPT-RESET  s" QA" TOP
   KEPT-ALL
   s" QALD:QALDEF;does" UNPLACED
   s" QALD:QALDEF" UNPLACED
   s" qald:" KEPT-RESET  s" QALD:" TOP
   KEPT-ALL
   s" QALD:QALDEF;does" UNPLACED
   s" qald:" KEPT-RESET  s" qald:QALDEF;" TOP
   KEPT-ALL
   s" qald:QALDEF;does" UNPLACED ;

: CASES ( -- )
   FIXTURE
   CASE-PARTIAL
   CASE-GAP
   CASE-NOT-BODY
   CASE-ENGINE
   CASE-LOCAL-CASE
   CASE-LOCAL
   CASE-NO-LOCAL
   CASE-AMBIGUOUS
   CASE-QUALIFIED
   CASE-TOP
   CASE-OBSERVER
   CASE-OBSERVER-REFUSED
   CASE-NESTED-ARM
   CASE-NESTED
   CASE-RELEASE
   CASE-USE-THROW
   CASE-DOES-USE-THROW
   CASE-DISCRIMINATOR
   CASE-HIDDEN
   CASE-REDECLARED
   CASE-VISITED
   CASE-DEPENDENCY
   CASE-REWOUND
   CASE-QUALIFIED-ANY
   CASE-DICTIONARY ;

\ A replay's names bind only while a scope holds the checker's overlay open, so
\ the fixture and every case run in one neutral checker scope, as a verifier
\ child runs its composition (tools/check-verify-child.f SCOPED).
: MAIN ( -- )
   T-RESET
   [: CASES ;] IN-NEUTRAL
   T-REPORT ;

MAIN

;package
