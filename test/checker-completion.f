split-str
\ checker-completion.f - the spellings that bind at a cursor, as the checker
\ selects them.
\   bin/hb --load test/checker-completion.f

require src/lib/test.f
require src/lib/fs.f
require src/lib/string.f
require src/habu/verify-source.f

\ A public does> definer this file loads: the dictionary holds its clause's
\ record, QALDEF;does, in QALD's public wordlist.
package QALD
public
: QALDEF ( n -- ) create , does> ( -- n ) @ ;
;package

\ CMQA's part CmqB, whose name the top-level package CMQB shares, as the engine
\ makes them: the dictionary holds the part's row, CMQA:CmqB (cases 10 to 12).
package CMQB
public
: QPT ( -- n ) 40 ;
;package

package CMQA
package CmqB
: QPV ( -- n ) 41 ;
public
: QPW ( -- n ) 42 ;
;package
;package

package NAVL

private

\ A filter names a handful of fixture words, so the table is the fixture's
\ size; a spelling or absolute path past its width is counted in KEPT-OVER.
32 constant KEPT-CAP
48 constant KEPT-W
256 constant KEPT-PATH-W
create KEPT-NAMES KEPT-CAP KEPT-W * allot
create KEPT-LENS KEPT-CAP cells allot
create KEPT-PATHS KEPT-CAP KEPT-PATH-W * allot
create KEPT-PATH-LENS KEPT-CAP cells allot
create KEPT-STARTS KEPT-CAP cells allot
create KEPT-ENDS KEPT-CAP cells allot
variable KEPT-N
variable KEPT-OVER
variable CALLS
PTR-VARIABLE FILTER-A   variable FILTER-U

: KEPT-RESET ( str -- )
   FILTER-U !  FILTER-A !
   0 KEPT-N !  0 KEPT-OVER !  0 CALLS ! ;

: FILTERED? ( str -- bool )
   {: a:^ u:n :}
   u FILTER-U @ < IF false EXIT THEN
   a FILTER-U @ FILTER-A @ FILTER-U @ CORE-STR=CI ;

\ Whether NAME passes the filter and fits the next row, which then holds it.
: KEPT-NAME? ( str -- bool )
   {: a:^ u:n :}
   1 CALLS +!
   a u FILTERED? 0= IF false EXIT THEN
   KEPT-N @ KEPT-CAP >=  u KEPT-W >  or IF 1 KEPT-OVER +! false EXIT THEN
   a  KEPT-NAMES KEPT-N @ KEPT-W * +  u BYTE-COPY
   u  KEPT-LENS KEPT-N @ cells + !
   true ;

\ The next row's bytes S to E; the row is kept.
: KEPT-SPAN ( n n -- )
   {: s:n e:n :}
   s  KEPT-STARTS KEPT-N @ cells + !
   e  KEPT-ENDS KEPT-N @ cells + !
   1 KEPT-N +! ;

\ A spelling VERIFY:ON-CANDIDATE offers, with the path of its declaration's
\ file and where its declaring token starts and ends there.
: KEEP ( ^u8 n ^u8 n n n -- )
   {: a:^ u:n p:^ pu:n s:n e:n :}
   a u KEPT-NAME? 0= IF EXIT THEN
   pu KEPT-PATH-W > IF 1 KEPT-OVER +! EXIT THEN
   p  KEPT-PATHS KEPT-N @ KEPT-PATH-W * +  pu BYTE-COPY
   pu  KEPT-PATH-LENS KEPT-N @ cells + !
   s e KEPT-SPAN ;

: KEPT$ ( n -- str )
   {: i:n :}
   KEPT-NAMES i KEPT-W * +  KEPT-LENS i cells + @ ;

: KEPT-PATH$ ( n -- str )
   {: i:n :}
   KEPT-PATHS i KEPT-PATH-W * +  KEPT-PATH-LENS i cells + @ ;

: KEPT-AT ( str -- n )
   {: a:^ u:n :}
   KEPT-N @ 0 ?DO
      a u i KEPT$ CORE-STR= IF i UNLOOP EXIT THEN
   LOOP
   -1 ;

: KEPT-CI ( str -- n )
   {: a:^ u:n :}
   0  KEPT-N @ 0 ?DO
      a u i KEPT$ CORE-STR=CI IF 1 + THEN
   LOOP ;

: KEPT-ALL ( -- )
   s" the filter's spellings fit the table" T-LABEL
   KEPT-OVER @ 0 T= ;

: KEPT-LOC ( n ^u8 n n n -- )
   {: i:n p:^ pu:n s:n e:n :}
   i KEPT-PATH$ p pu T$=
   KEPT-STARTS i cells + @ s T=
   KEPT-ENDS i cells + @ e T= ;

: ABSENT ( ^u8 n -- )
   {: a:^ u:n :}
   a u T-LABEL
   a u KEPT-CI 0 T= ;

: FIND$ ( str str -- n )
   {: a:^ u:n b:^ v:n :}
   u v - 1 + 0 ?DO
      a i + v b v CORE-STR= IF i UNLOOP EXIT THEN
   LOOP
   -1 ;

\ A composed case's subject: the declarations every case sees, then its body,
\ at one path, so a declaration's location is its token's bytes here.
: SUBJECT-PATH$ ( -- str ) s" cmp-subject.f" ;
1024 constant SUBJECT-CAP
SUBJECT-CAP BUFFER: SUBJECT-BUF
variable SUBJECT-N

\ Append TEXT and a newline to the subject.
: LINE+ ( str -- )
   {: a:^ u:n :}
   SUBJECT-N @ u + 1 + SUBJECT-CAP > IF
      s" checker-completion: a subject longer than SUBJECT-CAP" EXIT-STATUS:FULL die
   THEN
   a  SUBJECT-BUF SUBJECT-N @ +  u BYTE-COPY
   10  SUBJECT-BUF SUBJECT-N @ u + +  c!
   u 1 + SUBJECT-N +! ;

: SUBJECT-OF ( str -- str )
   0 SUBJECT-N !
   s" : CMPA ( -- n ) 1 ; : CMPB ( -- n ) 2 ; : ITEM ( -- n ) 3 ;" LINE+
   s" defer CMPDF ( -- n )" LINE+
   s" : CMPGONE ( -- n ) 4 ; undefine CMPGONE" LINE+
   s" package CMPP : CMPPRI ( -- n ) 5 ;" LINE+
   s" public : CMPPUB ( -- n ) 6 ; ;package" LINE+
   s" package CMPX public : CMPAMB ( -- n ) 7 ; ;package" LINE+
   s" package CMPY public : CMPAMB ( -- n ) 8 ; ;package" LINE+
   LINE+
   SUBJECT-BUF SUBJECT-N @ ;

\ Byte K into the last NEEDLE in TEXT, which a subject's body holds.
: CURSOR-AT ( str str n -- n )
   {: a:^ u:n b:^ v:n k:n :}
   -1  u v - 1 + 0 ?DO
      a i + v b v CORE-STR= IF drop i THEN
   LOOP
   k + ;

PTR-VARIABLE RUN-A   variable RUN-U

: SUBJECT-COMPOSE ( -- )
   RUN-A @ RUN-U @ SUBJECT-PATH$ 2dup VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;

: UNKEPT ( ^u8 n ^u8 n n n -- ) 2drop 2drop 2drop ;

\ Run Q, which composes TEXT at SUBJECT-PATH$, in a neutral scope of its own so
\ no case sees another's declarations, each spelling offered to V; the throw
\ code.
: COMPOSE-RUN ( str [ -- ] [ ^u8 n ^u8 n n n -- ] -- n )
   {: a:^ u:n q v :}
   a RUN-A !  u RUN-U !
   v is VERIFY:ON-CANDIDATE
   [: q CHECK:SCOPED ;] catch
   \ No word reads a defer's action: UNKEPT drops each spelling, as the default.
   ['] UNKEPT is VERIFY:ON-CANDIDATE ;

\ Compose TEXT, each spelling offered kept; the throw code.
: COMPOSE-KEPT ( str -- n )
   ['] SUBJECT-COMPOSE ['] KEEP COMPOSE-RUN ;

\ COMPOSE-KEPT with the cursor at byte AT.
: COMPOSE-AT ( str n -- n )
   VERIFY:CURSOR!  COMPOSE-KEPT ;

\ Compose BODY, then PREFIX alone on a top-level line with the cursor at its
\ end. A prefix that names nothing is rejected there, so no code is read.
: TOP-AT ( str str -- )
   {: b:^ bu:n p:^ pu:n :}
   b bu SUBJECT-OF 2drop  p pu LINE+
   SUBJECT-BUF SUBJECT-N @  SUBJECT-N @ 1 -  COMPOSE-AT drop ;

\ Whether byte AT of TEXT is a blank or past either end: a token's edge.
: EDGE? ( str n -- bool )
   {: a:^ u:n at:n :}
   at 0 <  at u >=  or IF true EXIT THEN
   a at + c@ 33 < ;

\ Where the first token spelled TOKEN starts in TEXT at or after byte FROM.
: TOKEN-AT ( str str n -- n )
   {: a:^ u:n b:^ v:n from:n :}
   u v - 1 + from ?DO
      a i + v b v CORE-STR=
      a u i 1 - EDGE? and  a u i v + EDGE? and
      IF i UNLOOP EXIT THEN
   LOOP
   -1 ;

\ Where the first token spelled TOKEN at or after byte FROM of the last
\ subject composed starts and ends.
: SPAN-FROM ( str n -- n n )
   {: t:^ tu:n from:n :}
   RUN-A @ RUN-U @ t tu from TOKEN-AT  dup tu + ;

\ The spelling after its last colon.
: TAIL ( str -- str )
   {: a:^ u:n :}
   u 0 ?DO
      a u 1 - i - + c@ 58 = IF a u i - + i UNLOOP EXIT THEN
   LOOP
   a u ;

\ NAME was offered exactly so spelled, from PATH's bytes S to E.
: SPELLED ( str str n n -- )
   {: a:^ u:n p:^ pu:n s:n e:n :}
   a u T-LABEL
   a u KEPT-AT {: i:n :}
   i 0 >= TTRUE
   i 0 < IF EXIT THEN
   i p pu s e KEPT-LOC ;

\ NAME was offered once, exactly so spelled, from PATH's bytes S to E.
: OFFERED ( str str n n -- )
   {: a:^ u:n p:^ pu:n s:n e:n :}
   a u p pu s e SPELLED
   a u T-LABEL
   a u KEPT-CI 1 T= ;

\ NAME was offered at its declaring token in the last subject composed: the
\ first token after the first AFTER spelled as NAME's tail.
: PLACED-AFTER ( str str -- )
   {: a:^ u:n f:^ fu:n :}
   a u SUBJECT-PATH$
   a u TAIL  RUN-A @ RUN-U @ f fu FIND$ fu +  SPAN-FROM
   OFFERED ;

: PLACED ( str -- )
   s" " PLACED-AFTER ;

\ NAME was offered as a local is: exactly so spelled, with no path or bytes.
: LOCAL ( ^u8 n -- )
   s" " 0 0 SPELLED ;

\ NAME was offered once, exactly so spelled, with no path or bytes: a word
\ only the dictionary holds.
: UNPLACED ( ^u8 n -- )
   s" " 0 0 OFFERED ;

\ How this can fail, each one a case below:
\ - the cursor never fires, or fires at the wrong token: in the gap before a
\   token, inside it, after the last one;
\ - the prefix holds bytes past the cursor, or misses bytes before it;
\ - it fires on the definition's name, or inside its signature;
\ - a cursor outlives its composition, after a refused body, a visitor's throw,
\   a use handler's throw or a scan stopped before the cursor, so the next
\   composition offers with none set or the next cursor is refused;
\ - a cursor a visitor sets fires in the open composition, or the next one
\   loses it;
\ - a refused body misses what binds;
\ - a local is missed inside a quotation, or offered for a tick or `is` target;
\ - two locals that differ only in case merge, or a spelling a local holds is
\   offered as the word that local hides;
\ - a refused spelling (one two used publics export) or a tombstone is offered;
\ - the definition's own name is offered before it is declared;
\ - the cursor changes the composition's throw, its census or its uses;
\ - a word is offered twice, once for each source that names it;
\ - a qualified prefix offers a private name, or a bare prefix offers bare a
\   public of a package not in use;
\ - a bare prefix or a package's edge colon misses a qualified spelling a
\   qualified prefix offers there, or one only the dictionary holds;
\ - a part's public is missed behind its own name inside its package or the
\   part, or offered outside them, behind the parent's path or a top-level
\   package's name the part shadows; or a part a subject opens is a top-level
\   package of its name;
\ - a candidate's location is not its selected declaration's: a declared word
\   offered without its path and bytes, a word the selection hides lending its
\   location, a refused redeclaration moving it, or a local or a word only the
\   dictionary holds offered with one;
\ - an engine word the body has not used yet is missing;
\ - a word is not spelled as it was declared: a declared word as its
\   declaration wrote it, a qualified one as the prefix's qualifier, else its
\   package's, and the declared tail, a word only the dictionary holds as the
\   dictionary holds it; or a word the selection hides lends its spelling to
\   the one it selects;
\ - a refused redeclaration renames the word it failed to replace, or a
\   spelling dies with the bytes it was declared in;
\ - a word a neutral scope took back is offered in the next composition.
\ A coverage limit, not an exact spelling: a name the checker generates with
\ no declaration of its own (a suffix registration) keeps the store's folded
\ spelling, so no case here pins one.

\ 1. A body token.
: CASE-PARTIAL ( -- )
   s" cmp" KEPT-RESET
   s" : CMPC ( -- n ) CMPA CMPB + ;" SUBJECT-OF
   2dup s" CMPB" 2 CURSOR-AT COMPOSE-AT
   s" a partial token's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPGONE" ABSENT
   s" CMPPUB" ABSENT
   s" a package not in use offers its public behind its name" T-LABEL
   s" CMPP:CMPPUB" PLACED
   s" CMPP:CMPPRI" ABSENT ;

\ The gap before a token and the end of the text offer every visible name.
: CASE-GAP ( -- )
   s" cmp" KEPT-RESET
   s" : CMPC1 ( -- n ) CMPA CMPB + ;" SUBJECT-OF
   2dup s" CMPB" 0 CURSOR-AT COMPOSE-AT
   s" the gap's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPA" PLACED
   s" CMPB" PLACED
   s" cmp" KEPT-RESET
   s" : CMPC1 ( -- n ) CMPA CMPB + ; : CMPC2 ( -- n ) CMPA CMPB + ;" SUBJECT-OF
   2dup s" ;" 0 CURSOR-AT COMPOSE-AT
   s" the end's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPA" PLACED
   s" CMPB" PLACED
   s" CMPC1" PLACED ;

: CASE-NOT-BODY ( -- )
   s" cmp" KEPT-RESET
   s" : CMPC3 ( -- n ) CMPA ;" SUBJECT-OF  2dup s" CMPC3" 2 CURSOR-AT COMPOSE-AT
   s" the name's check runs through" T-LABEL 0 T=
   s" a cursor in the name offers nothing" T-LABEL
   CALLS @ 0 T=
   s" : CMPC4 ( -- n ) CMPA ;" SUBJECT-OF  2dup s" --" 1 CURSOR-AT COMPOSE-AT
   s" the signature's check runs through" T-LABEL 0 T=
   s" a cursor in the signature offers nothing" T-LABEL
   CALLS @ 0 T= ;

\ An engine word the body has not used yet is offered as the dictionary spells
\ it, with no location.
: CASE-ENGINE ( -- )
   s" du" KEPT-RESET
   s" : CMPN ( n -- n n ) dup ;" SUBJECT-OF  2dup s" dup ;" 2 CURSOR-AT COMPOSE-AT
   s" the engine word's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" dup" UNPLACED
   s" byte-cop" KEPT-RESET
   s" : CMPN2 ( n -- n ) ;" SUBJECT-OF  2dup s" ;" 0 CURSOR-AT COMPOSE-AT
   s" the empty body's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" BYTE-COPY" UNPLACED ;

\ 2. Locals.
\ Locals keep their case, and a word's spelling a local holds is the local.
: CASE-LOCAL-CASE ( -- )
   s" it" KEPT-RESET
   s" : CMPK ( n n -- n ) {: item:n ITEM:n :} item ITEM + ;" SUBJECT-OF
   2dup s" ITEM +" 2 CURSOR-AT COMPOSE-AT
   s" the locals' check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" item" LOCAL
   s" ITEM" LOCAL
   s" the word ITEM is not offered as a third item" T-LABEL
   s" item" KEPT-CI 2 T= ;

: CASE-LOCAL ( -- )
   s" cmp" KEPT-RESET
   s" : CMPL ( n -- n ) {: CMPLOC:n :} CMPLOC ;" SUBJECT-OF
   2dup s" CMPLOC ;" 2 CURSOR-AT COMPOSE-AT
   s" the local's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPLOC" LOCAL
   s" CMPA" PLACED ;

\ Inside a quotation a local binds as outside it.
: CASE-QUOT-LOCAL ( -- )
   s" cmp" KEPT-RESET
   s" : CMPQ ( n -- n ) {: CMPLOC:n :} [: CMPA ;] drop CMPLOC ;" SUBJECT-OF
   2dup s" CMPA ;]" 2 CURSOR-AT COMPOSE-AT
   s" the quotation's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPA" PLACED
   s" CMPLOC" LOCAL ;

\ For a tick or `is` target no local binds.
: CASE-NO-LOCAL ( -- )
   s" cmp" KEPT-RESET
   s" : CMPT ( n -- n ) {: CMPLOC:n :} ['] CMPA drop CMPLOC ;" SUBJECT-OF
   2dup s" ['] CMPA" 6 CURSOR-AT COMPOSE-AT
   s" the tick's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPA" PLACED
   s" cmploc" ABSENT
   s" cmp" KEPT-RESET
   s" : CMPI ( n -- n ) {: CMPLOC:n :} [: CMPA ;] is CMPDF CMPLOC ;" SUBJECT-OF
   2dup s" is CMPDF" 5 CURSOR-AT COMPOSE-AT
   s" the is target's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPDF" PLACED
   s" cmploc" ABSENT ;

\ 3. What the scope refuses, and qualified names.
: CASE-AMBIGUOUS ( -- )
   s" cmp" KEPT-RESET
   s" using CMPX using CMPY : CMPU ( -- n ) CMPA ; ;using ;using" SUBJECT-OF
   2dup s" CMPA ;" 2 CURSOR-AT COMPOSE-AT
   s" the using scope's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPA" PLACED
   s" CMPAMB" ABSENT
   s" each package's public is offered behind its name" T-LABEL
   s" CMPX:CMPAMB" PLACED
   s" CMPY:CMPAMB" s" package CMPY" PLACED-AFTER ;

: CASE-QUALIFIED ( -- )
   s" cmpp:" KEPT-RESET
   s" : CMPV ( -- n ) CMPP:CMPPUB ;" SUBJECT-OF
   2dup s" CMPP:CMPPUB ;" 7 CURSOR-AT COMPOSE-AT
   s" the qualified call's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPP:CMPPUB" PLACED
   s" CMPP:CMPPRI" ABSENT ;

\ 4. A top-level cursor.
: CASE-TOP ( -- )
   s" cmp" KEPT-RESET
   s" CMPA drop" SUBJECT-OF  2dup s" CMPA drop" 2 CURSOR-AT COMPOSE-AT
   s" the top level's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPA" PLACED
   s" CMPDF" PLACED
   s" CMPGONE" ABSENT
   s" CMPPUB" ABSENT
   s" CMPAMB" ABSENT
   s" cmpp:" KEPT-RESET
   s" CMPP:CMPPUB drop" SUBJECT-OF  2dup s" CMPP:CMPPUB drop" 7 CURSOR-AT COMPOSE-AT
   s" the qualified top level's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPP:CMPPUB" PLACED
   s" CMPP:CMPPRI" ABSENT ;

\ 5. A refused body, and what the cursor leaves.
: CASE-RELEASE ( -- )
   s" cmp" KEPT-RESET
   s" : CMPR ( -- n ) CMPA CMPNOPE ;" SUBJECT-OF
   2dup s" CMPA CMPNOPE" 2 CURSOR-AT COMPOSE-AT
   s" a refused body throws its refusal" T-LABEL 0 T<>
   s" a refused body still offers what binds" T-LABEL
   s" CMPA" PLACED ;

\ Whatever ended the last composition, no cursor outlives it: the next one,
\ with none set, offers nothing, and a new cursor fires.
: NEXT-UNARMED ( -- )
   s" cmp" KEPT-RESET
   s" : CMPS ( -- n ) CMPA CMPB + ;" SUBJECT-OF COMPOSE-KEPT
   s" the next composition runs through" T-LABEL 0 T=
   s" no cursor outlived the last composition" T-LABEL
   CALLS @ 0 T=
   s" : CMPS ( -- n ) CMPA CMPB + ;" SUBJECT-OF
   2dup s" CMPA" 2 CURSOR-AT COMPOSE-AT
   s" a new cursor's composition runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPA" PLACED ;

: THROWING ( ^u8 n ^u8 n n n -- )
   KEEP  E-DIV-ZERO throw ;

\ The subject byte whose use USE-THROWS throws at.
variable THROW-AT

: USE-THROWS ( n n str n n n n -- )
   {: s:n e:n p:^ pu:n ds:n de:n sym:n v:n :}
   s THROW-AT @ = IF E-DIV-ZERO throw THEN ;

: USE-DROPS ( n n str n n n n -- )
   2drop 2drop 2drop 2drop ;

: CASE-OUTLIVE ( -- )
   s" cmp" KEPT-RESET
   s" : CMPR2 ( -- n ) CMPA CMPNOPE ;" SUBJECT-OF
   2dup s" CMPA CMPNOPE" 2 CURSOR-AT COMPOSE-AT
   s" the refused body's composition throws" T-LABEL 0 T<>
   NEXT-UNARMED
   s" cmp" KEPT-RESET
   s" : CMPW ( -- n ) CMPA ;" SUBJECT-OF  2dup s" CMPA ;" 2 CURSOR-AT VERIFY:CURSOR!
   ['] SUBJECT-COMPOSE ['] THROWING COMPOSE-RUN
   s" the visitor's throw leaves the composition" T-LABEL E-DIV-ZERO T=
   NEXT-UNARMED
   s" cmp" KEPT-RESET
   s" : CMPH ( -- n ) CMPA ;" SUBJECT-OF
   2dup s" CMPA ;" 0 CURSOR-AT THROW-AT !
   2dup s" CMPA ;" 2 CURSOR-AT
   ['] USE-THROWS is VERIFY:ON-USE
   COMPOSE-AT
   ['] USE-DROPS is VERIFY:ON-USE
   s" the use handler's throw leaves the composition" T-LABEL E-DIV-ZERO T=
   NEXT-UNARMED
   \ The duplicate CMPA stops the scan before the cursor, which never fires.
   s" cmp" KEPT-RESET
   s" : CMPA ( -- n ) 9 ; CMPB drop" SUBJECT-OF
   2dup s" CMPB drop" 2 CURSOR-AT COMPOSE-AT
   s" the stopped scan throws" T-LABEL 0 T<>
   s" before its cursor" T-LABEL
   CALLS @ 0 T=
   NEXT-UNARMED ;

TYPED-VARIABLE NEST-TRIED bool
variable NEST-AT

\ A visitor that sets the cursor at NEST-AT, once.
: NEST-CURSOR ( ^u8 n ^u8 n n n -- )
   NEST-TRIED @ IF KEEP EXIT THEN  true NEST-TRIED !  NEST-AT @ VERIFY:CURSOR!  KEEP ;

: NEST-TEXT$ ( -- str )
   s" : CMPJ ( -- n ) CMPA ; : CMPJ2 ( -- n ) CMPB ;" SUBJECT-OF ;

\ The throw code and how many candidates NEST-TEXT$ offers with the cursor at
\ CMPA, through visitor V.
: NESTED-CALLS ( [ ^u8 n ^u8 n n n -- ] -- n n )
   {: v :}
   s" cmp" KEPT-RESET  false NEST-TRIED !
   NEST-TEXT$ 2dup s" CMPA ;" 2 CURSOR-AT VERIFY:CURSOR!
   ['] SUBJECT-COMPOSE v COMPOSE-RUN  CALLS @ ;

: CASE-NESTED ( -- )
   ['] KEEP NESTED-CALLS {: rc0:n n0:n :}
   s" the plain visitor's composition runs through" T-LABEL rc0 0 T=
   NEST-TEXT$ s" CMPB ;" 2 CURSOR-AT NEST-AT !
   ['] NEST-CURSOR NESTED-CALLS {: rc2:n n2:n :}
   s" the cursor-setting visitor's composition runs through" T-LABEL rc2 0 T=
   s" the outer cursor is unchanged" T-LABEL
   n2 n0 T=
   KEPT-ALL
   s" CMPA" PLACED
   s" CMPB" PLACED
   s" cmp" KEPT-RESET
   NEST-TEXT$ COMPOSE-KEPT
   s" the next composition runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPB" PLACED
   s" CMPJ" PLACED ;

variable USE-N   variable USE-SUM

: COUNT-USE ( n n str n n n n -- )
   {: s:n e:n p:^ pu:n ds:n de:n sym:n v:n :}
   1 USE-N +!  s e + USE-SUM +! ;

\ The cursor changes nothing the composition answers: TEXT composed with the
\ cursor at AT and with none throws the same, certifies and defers the same
\ and publishes the same uses at the same bytes; the throw code.
: OBSERVED ( str n -- n )
   {: a:^ u:n at:n :}
   s" cmp" KEPT-RESET
   ['] COUNT-USE is VERIFY:ON-USE
   0 USE-N !  0 USE-SUM !
   a u COMPOSE-KEPT  VERIFY:CENSUS {: rc0:n c0:n d0:n :}
   USE-N @ USE-SUM @ {: n0:n sum0:n :}
   s" the composition with no cursor offers nothing" T-LABEL
   CALLS @ 0 T=
   0 USE-N !  0 USE-SUM !
   a u at COMPOSE-AT  VERIFY:CENSUS {: rc1:n c1:n d1:n :}
   ['] USE-DROPS is VERIFY:ON-USE
   s" the cursor's composition throws the same" T-LABEL  rc1 rc0 T=
   s" certifies the same" T-LABEL  c1 c0 T=
   s" and defers the same" T-LABEL  d1 d0 T=
   s" publishes as many uses" T-LABEL  USE-N @ n0 T=
   s" at the same bytes" T-LABEL  USE-SUM @ sum0 T=
   KEPT-ALL
   s" CMPA" PLACED
   rc0 ;

: CASE-OBSERVER ( -- )
   s" : CMPE ( -- n ) CMPA CMPB + ;" SUBJECT-OF
   2dup s" CMPB +" 2 CURSOR-AT OBSERVED
   s" the certified body certifies" T-LABEL 0 T=
   s" : CMPE2 ( -- n ) CMPA CMPB ;" SUBJECT-OF
   2dup s" CMPB ;" 2 CURSOR-AT OBSERVED
   s" the refused body is refused" T-LABEL 0 T<> ;

\ 6. The selection spells what it binds.
\ A global dscitem, the open package's DSCITEM and a body's local dscitem: the
\ local and the package's word are offered, each as written, and the global
\ the package's word hides lends neither its spelling nor its location.
: DISC$ ( -- str )
   s" : dscitem ( -- n ) 9 ; package DSCP : DSCITEM ( -- n ) 10 ; : DSCB ( n -- n ) {: dscitem:n :} dscitem DSCITEM + ; ;package" ;

: CASE-DISCRIMINATOR ( -- )
   s" dsc" KEPT-RESET
   DISC$ SUBJECT-OF  2dup s" DSCITEM +" 4 CURSOR-AT COMPOSE-AT
   s" the discriminator's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" dscitem" LOCAL
   s" the package's DSCITEM is offered as written, where the package declared it" T-LABEL
   s" DSCITEM" SUBJECT-PATH$ s" DSCITEM" 0 SPAN-FROM SPELLED
   s" nothing else is spelled dscitem" T-LABEL
   s" dscitem" KEPT-CI 2 T= ;

\ The open package's byte-copy hides the engine's BYTE-COPY: what is offered is
\ the package's word, and the hidden engine record lends it no spelling.
: HIDE$ ( -- str )
   s" package DSCQ : byte-copy ( -- n ) 14 ; : DSCH ( -- n ) BYTE-COPY ; ;package" ;

: CASE-HIDDEN ( -- )
   s" byte-c" KEPT-RESET
   HIDE$ SUBJECT-OF  2dup s" BYTE-COPY ;" 6 CURSOR-AT COMPOSE-AT
   s" the hiding word's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" byte-copy" PLACED
   s" the hidden engine word lends it no spelling" T-LABEL
   s" BYTE-COPY" KEPT-AT 0 < TTRUE ;

\ A refused redeclaration neither moves nor renames the word it failed to
\ replace: in a multi-error window, which goes past it as the verifier child's
\ does, CMPA is offered as its first declaration wrote it, where that is.
variable REJECTS

: MULTI-COMPOSE ( -- )
   CHECK:MULTI-ERR-BEGIN
   [: SUBJECT-COMPOSE ;] catch {: rc:n :}
   CHECK:MULTI-ERR-END REJECTS !
   rc 0<> IF rc throw THEN ;

: CASE-REDECLARED ( -- )
   s" cmpa" KEPT-RESET
   s" : cmpa ( -- n ) 12 ; : CMPRD ( -- n ) CMPA ;" SUBJECT-OF
   2dup s" CMPA ;" 2 CURSOR-AT VERIFY:CURSOR!
   REJECTION:CAPTURE
   ['] MULTI-COMPOSE ['] KEEP COMPOSE-RUN
   REJECTION:BUFFER-OFF
   s" the window goes past the redeclaration" T-LABEL 0 T=
   s" and rejects it alone" T-LABEL
   REJECTS @ 1 T=
   REJECTION:BUFFER$ s" error[E-DUPLICATE-DEFINITION]" STR:STARTS-WITH? TTRUE
   KEPT-ALL
   s" CMPA" PLACED ;

\ 7. Declarations the composition locates.
\ A qualified EXPORT is offered by the tail it is kept under, at its operand.
: VISITED$ ( -- str )
   s" package CMPEA public : CMPEXA ( -- n ) 15 ; ;package package CMPEB public EXPORT CMPEA:CMPEXA ;package CMPEB:CMPEXA drop" ;

: CASE-VISITED ( -- )
   s" cmpeb:" KEPT-RESET
   VISITED$ SUBJECT-OF  2dup s" CMPEB:CMPEXA drop" 10 CURSOR-AT COMPOSE-AT
   s" the composition runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPEB:CMPEXA" SUBJECT-PATH$ s" CMPEA:CMPEXA" 0 SPAN-FROM OFFERED ;

\ A required file's declaration keeps the spelling it was declared with after
\ the loader unmaps that file's bytes, when its scan ends: the cursor sits in
\ the body of the subject that required it.
: DEP-PATH$ ( -- str ) s" test/checker-completion-dep.f" ;
512 constant DEP-CAP
DEP-CAP BUFFER: DEP-BUF

\ Where CmpDep's declaring token starts and ends in the dependency's bytes.
: DEP-SPAN ( -- n n )
   DEP-PATH$ DEP-BUF DEP-CAP FS:READ-ALL {: n:n :}
   DEP-BUF n s" CmpDep" 0 TOKEN-AT  dup 6 + ;

\ The dependency's path as a load resolves it from the tree's root, where this
\ test runs.
: DEP-CANON$ ( -- str )
   DEP-PATH$ SOURCE-ROOT:CANONICAL drop ;

: CASE-DEPENDENCY ( -- )
   s" cmpdep" KEPT-RESET
   s" require test/checker-completion-dep.f : CMPDU ( -- n ) CmpDep ;" SUBJECT-OF
   2dup s" CmpDep ;" 4 CURSOR-AT COMPOSE-AT
   s" the subject's composition runs through" T-LABEL 0 T=
   KEPT-ALL
   DEP-SPAN {: s:n e:n :}
   s" CmpDep" DEP-CANON$ s e OFFERED ;

\ A neutral scope's rewind takes back what its composition declared: the next
\ composition offers its own word, where it declared it, and not the rewound
\ one.
: CASE-REWOUND ( -- )
   s" cmpr" KEPT-RESET
   s" : CMPRW ( -- n ) 17 ;" SUBJECT-OF COMPOSE-KEPT
   s" the rewound word's composition runs through" T-LABEL 0 T=
   s" : CMPRX ( -- n ) 18 ; : CMPRY ( -- n ) CMPRX ;" SUBJECT-OF
   2dup s" CMPRX ;" 2 CURSOR-AT COMPOSE-AT
   s" the next composition runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CMPRX" PLACED
   s" CMPRW" ABSENT ;

\ 8. Qualified spellings under any prefix.
\ A bare prefix offers the package's publics behind its name, as the qualified
\ prefix does: each as its selection spells and locates it - a replacement as
\ its own declaration wrote it, where that declaration was - and nothing the
\ selection refuses: a private word, a tombstone, or a public's own name inside
\ its body. The qualifier is the one the prefix typed, else the package's own.
: QAL$ ( -- str )
   s\" package QALP public : QALPUB ( -- n ) 30 ; : QALGONE ( -- n ) 31 ; undefine QALGONE\n: QALRE ( -- n ) 33 ; undefine QALRE : qalre ( -- n ) 34 ;\nprivate : QALPRI ( -- n ) 32 ; ;package package QalPk public : QalW ( -- n ) 35 ; ;package" ;

\ Inside a package its name qualifies a global too, as the walk binds it: the
\ prefix K bytes into the QALP:CMPA of a body in QALP offers that spelling, the
\ global's own.
: ALIAS-AT ( n -- )
   {: k:n :}
   s" qalp:cmpa" KEPT-RESET
   s" package QALP public : QALAL ( -- n ) QALP:CMPA ; ;package" SUBJECT-OF
   2dup s" QALP:CMPA" k CURSOR-AT COMPOSE-AT
   s" the open package's body's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" QALP:CMPA" PLACED ;

\ Under the top-level prefix P, QalPk's QalW is offered spelled NAME, where it
\ is declared.
: QALW-AT ( str str -- )
   {: p:^ pu:n a:^ u:n :}
   s" qalpk:" KEPT-RESET
   QAL$ p pu TOP-AT
   KEPT-ALL
   a u PLACED ;

: CASE-QUALIFIED-ANY ( -- )
   s" qalp:" KEPT-RESET
   QAL$ s" Q" TOP-AT
   KEPT-ALL
   s" QALP:QALPUB" PLACED
   s" QALP:qalre" PLACED
   s" QALP:QALGONE" ABSENT
   s" QALP:QALPRI" ABSENT
   s" qalp:qal" KEPT-RESET
   s" package QALP public : QALPUB ( -- n ) 30 ; : QALSELF ( -- n ) QALP:QALPUB ; ;package" SUBJECT-OF
   2dup s" QALP:QALPUB ;" 1 CURSOR-AT COMPOSE-AT
   s" the public's body's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" QALP:QALPUB" PLACED
   s" QALP:QALSELF" ABSENT
   1 ALIAS-AT
   6 ALIAS-AT
   s" QA" s" QalPk:QalW" QALW-AT
   s" QALPK:" s" QALPK:QalW" QALW-AT
   s" qalpk:Q" s" qalpk:QalW" QALW-AT ;

\ 9. A public only the dictionary holds.
\ QALD:QALDEF;does is offered under a bare prefix, the package's edge colon
\ and a qualified prefix, as the dictionary spells it, with no location; its
\ definer once.
: CASE-DICTIONARY ( -- )
   s" qald:" KEPT-RESET  s" " s" QA" TOP-AT
   KEPT-ALL
   s" QALD:QALDEF;does" UNPLACED
   s" QALD:QALDEF" UNPLACED
   s" qald:" KEPT-RESET  s" " s" QALD:" TOP-AT
   KEPT-ALL
   s" QALD:QALDEF;does" UNPLACED
   s" qald:" KEPT-RESET  s" " s" qald:QALDEF;" TOP-AT
   KEPT-ALL
   s" qald:QALDEF;does" UNPLACED ;

\ 10. A part's words.
\ Inside CMQA a qualifier CmqB names its part, which shadows the top-level
\ CMQB there: the part's public is offered behind the part's name, as its row
\ spells it, and neither the part's private nor CMQB's public is.
: PART-IN ( n -- )
   {: k:n :}
   s" cmqb:" KEPT-RESET
   s" package CMQA : CMQU ( -- n ) CmqB:QPW ; ;package" SUBJECT-OF
   2dup s" CmqB:QPW" k CURSOR-AT COMPOSE-AT
   s" the package's body's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CmqB:QPW" UNPLACED
   s" CmqB:QPV" ABSENT
   s" CmqB:QPT" ABSENT ;

: CASE-PART-IN ( -- )
   2 PART-IN  5 PART-IN  6 PART-IN ;

\ Outside CMQA, CMQB: offers CMQB's public and no spelling reaches the part.
: PART-OUT ( -- )
   KEPT-ALL
   s" CMQB:QPT" UNPLACED
   s" CMQB:QPW" ABSENT
   s" CMQB:QPV" ABSENT ;

\ The prefix K bytes into CMQB:QPT, in a body of CMQC.
: PART-OTHER ( n -- )
   {: k:n :}
   s" cmqb:" KEPT-RESET
   s" package CMQC : CMQX ( -- n ) CMQB:QPT ; ;package" SUBJECT-OF
   2dup s" CMQB:QPT" k CURSOR-AT COMPOSE-AT
   s" the other package's body's check runs through" T-LABEL 0 T=
   PART-OUT ;

\ Under the top-level prefix P, CMQB: offers what PART-OUT says.
: PART-TOP ( str -- )
   {: p:^ pu:n :}
   s" cmqb:" KEPT-RESET  s" " p pu TOP-AT  PART-OUT ;

\ CMQA: offers nothing at top level.
: PART-PATH ( str -- )
   {: p:^ pu:n :}
   s" cmqa:" KEPT-RESET  s" " p pu TOP-AT
   KEPT-ALL
   s" nothing behind CMQA" T-LABEL
   KEPT-N @ 0 T= ;

: CASE-PART-OUT ( -- )
   s" CM" PART-TOP
   s" CMQB:" PART-TOP
   s" CMQB:Q" PART-TOP
   s" CMQA:" PART-PATH
   2 PART-OTHER  5 PART-OTHER  6 PART-OTHER ;

\ 11. A part a subject opens.
\ A subject's `package CmqB` in CMQA opens the part CMQA:CmqB, no top-level
\ CMQB.
: PART-OPENS$ ( -- str )
   s" package CMQA package CmqB public : QPR ( -- n ) 43 ; ;package : CMQU4 ( -- n ) CmqB:QPR ; ;package" ;

: CASE-PART-REPLAY ( -- )
   s" cmqb:qpr" KEPT-RESET
   PART-OPENS$ SUBJECT-OF  2dup s" CmqB:QPR" 6 CURSOR-AT COMPOSE-AT
   s" the package's body's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CmqB:QPR" PLACED
   s" cmqb:qpr" KEPT-RESET  PART-OPENS$ s" CMQB:Q" TOP-AT
   KEPT-ALL
   s" CMQB:QPR" ABSENT ;

\ 12. Inside the part its words are the open package's: bare, and its public
\ behind its own name, which qualifies a global too, as the engine's find binds
\ it.
: IN-PART ( str -- str )
   {: a:^ u:n :}
   0 SUBJECT-N !
   s" package CMQA package CmqB" LINE+  a u LINE+  s" ;package ;package" LINE+
   SUBJECT-BUF SUBJECT-N @ ;

: CASE-PART-OPEN ( -- )
   s" qp" KEPT-RESET
   s" : CMQS1 ( -- n ) QPV QPW + ;" IN-PART  2dup s" QPW +" 2 CURSOR-AT COMPOSE-AT
   s" the part's body's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" QPW" UNPLACED
   s" QPV" UNPLACED
   s" QPT" ABSENT
   s" cmqb:qp" KEPT-RESET
   s" : CMQS2 ( -- n ) QPV QPW + ;" IN-PART  2dup s" QPW +" 0 CURSOR-AT COMPOSE-AT
   s" the part's gap's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CmqB:QPW" UNPLACED
   s" CmqB:QPV" ABSENT
   s" CmqB:QPT" ABSENT
   s" cmqb:dup" KEPT-RESET
   s" : CMQS3 ( n -- n n ) CmqB:dup ;" IN-PART  2dup s" CmqB:dup" 1 CURSOR-AT COMPOSE-AT
   s" the qualified global's check runs through" T-LABEL 0 T=
   KEPT-ALL
   s" CmqB:dup" UNPLACED ;

\ Each case composes in a scope of its own.
: MAIN ( -- )
   T-RESET
   CASE-PARTIAL
   CASE-GAP
   CASE-NOT-BODY
   CASE-ENGINE
   CASE-LOCAL-CASE
   CASE-LOCAL
   CASE-QUOT-LOCAL
   CASE-NO-LOCAL
   CASE-AMBIGUOUS
   CASE-QUALIFIED
   CASE-TOP
   CASE-RELEASE
   CASE-OUTLIVE
   CASE-NESTED
   CASE-OBSERVER
   CASE-DISCRIMINATOR
   CASE-HIDDEN
   CASE-REDECLARED
   CASE-VISITED
   CASE-DEPENDENCY
   CASE-REWOUND
   CASE-QUALIFIED-ANY
   CASE-DICTIONARY
   CASE-PART-IN
   CASE-PART-OUT
   CASE-PART-REPLAY
   CASE-PART-OPEN ;

MAIN

public

: REPORT ( -- ) T-REPORT ;

;package

NAVL:REPORT
