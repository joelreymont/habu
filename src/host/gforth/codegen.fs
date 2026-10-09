\ src/host/gforth/codegen.fs - the Gforth platform's codegen (docs/bootstrap.md
\ stages 1-2; docs/architecture.md "The codegen is one pass over the checked
\ events"). Under the check hook a colon definition is captured without
\ running a token, checked with the checker's source tape armed, and compiled
\ in one pass over the events the tape told, through a handler per event
\ kind. It reads only the tape's events (CHECKER-TAPE) and the owner ABI's
\ rows (src/core/checker-owner-abi.f), each found by its spelling once the
\ checker has loaded (CG-RESOLVE). A value of n cells is n Gforth cells.
\ TRUSTED: bodies, and every body read before the hook is installed, keep the
\ reader's own compile (reader.fs BODY-LOOP). reader.fs requires this file
\ before HB-COLON.

\ ---- faults -----------------------------------------------------------------
\ A fact this codegen cannot compile is a host refusal, named (REFUSE-RC).
: CG-DIE ( c-addr u -- ) s" hb: gforth codegen: " ERR ERR REFUSE-RC COMPILE-DIE ;

\ ---- the checker's names (CG-RESOLVE) ----------------------------------------
\ Each row is a word answering what a Habu spelling's record answers once the
\ checker has loaded: a constant's value or, for CK-XT, the word's xt.
variable CK-LINK
: CK-ROW ( xt? "word" "spelling" -- )
   create  here CK-LINK @ , CK-LINK !  ,  0 ,  parse-name string,
   does> 2 cells + @ ;
: CK ( "word" "spelling" -- ) false CK-ROW ;
: CK-XT ( "word" "spelling" -- ) true CK-ROW ;
: CK-RESOLVE ( row -- )
   dup 3 cells + count 2dup LFIND ?dup 0= if
      s" hb: gforth codegen: no checker name " ERR ERR REFUSE-RC COMPILE-DIE then
   nip nip @  over cell+ @ 0= if execute then  swap 2 cells + ! ;

CK K-NAME CHECKER-TAPE:K-NAME                  CK K-INT CHECKER-TAPE:K-INT
CK K-REAL CHECKER-TAPE:K-REAL                  CK K-STRING CHECKER-TAPE:K-STRING
CK K-CHAR CHECKER-TAPE:K-CHAR                  CK K-COUNTED CHECKER-TAPE:K-COUNTED-STRING
CK K-PRINTED CHECKER-TAPE:K-PRINTED-STRING     CK K-CONTROL CHECKER-TAPE:K-CONTROL
CK K-LOCAL-DECL CHECKER-TAPE:K-LOCAL-DECL      CK K-LOCAL-REF CHECKER-TAPE:K-LOCAL-REF
CK K-LOCAL-CLOSE CHECKER-TAPE:K-LOCAL-CLOSE    CK K-CONSTRUCT CHECKER-TAPE:K-CONSTRUCT
CK K-MATCH CHECKER-TAPE:K-MATCH                CK K-MATCH-ARM CHECKER-TAPE:K-MATCH-ARM
CK K-MATCH-OF CHECKER-TAPE:K-MATCH-OF          CK K-MATCH-END CHECKER-TAPE:K-MATCH-END
CK K-TICK CHECKER-TAPE:K-TICK                  CK K-IS CHECKER-TAPE:K-IS
CK K-OPERAND CHECKER-TAPE:K-OPERAND            CK-XT TAPE-EVENT CHECKER-TAPE:EVENT
CK TAPE-INSTALL-OFF CHECKER-OWNER-ABI:TAPE-INSTALL-OFF
CK TAPE-ARM-OFF CHECKER-OWNER-ABI:TAPE-ARM-OFF
CK TAPE-DISARM-OFF CHECKER-OWNER-ABI:TAPE-DISARM-OFF
CK CALL-BINDING-OFF CHECKER-OWNER-ABI:CALL-BINDING-OFF
CK CALL-CELLS-OFF CHECKER-OWNER-ABI:CALL-CELLS-OFF
CK WF-W-AT-OFF CHECKER-OWNER-ABI:WF-W-AT-OFF
CK MATCH-CELLS-OFF CHECKER-OWNER-ABI:EFFECT-MATCH-CELLS-OFF
CK FAMILY-NAME-OFF CHECKER-OWNER-ABI:FAMILY-NAME-OFF
CK LOCAL-WIDTH-OFF CHECKER-OWNER-ABI:LOCAL-WIDTH-OFF
CK BOUND-KIND CHECKER-OWNER-ABI:BOUND-KIND     CK BOUND-RECORD CHECKER-OWNER-ABI:BOUND-RECORD
CK BOUND-CTL CHECKER-OWNER-ABI:BOUND-CTL       CK BOUND-IN CHECKER-OWNER-ABI:BOUND-IN
CK BOUND-OUT CHECKER-OWNER-ABI:BOUND-OUT       CK BOUND-DICT CHECKER-OWNER-ABI:BOUND-DICT
CK BOUND-INTRINSIC CHECKER-OWNER-ABI:BOUND-INTRINSIC
CK CTL-CORE-OP CTL-CORE-OP

\ The target owner's slot for an owner-ABI field (reader.fs TGT-XT).
: OWNER ( off -- xt ) dup TGT-XT ?dup if nip exit then
   s" hb: gforth codegen: no owner slot " ERR ERR-N REFUSE-RC COMPILE-DIE ;
: CALL-BINDING ( ord -- row size ) CALL-BINDING-OFF OWNER execute ;
: CALL-CELLS ( ord -- cin cout ) CALL-CELLS-OFF OWNER execute ;
: WF-W-AT ( off pos -- w ) WF-W-AT-OFF OWNER execute ;
: MATCH-CELLS ( ord -- n ) MATCH-CELLS-OFF OWNER execute ;
: FAMILY-NAME ( fam -- c-addr u ) FAMILY-NAME-OFF OWNER execute ;
: LOCAL-WIDTH ( seq -- w ) LOCAL-WIDTH-OFF OWNER execute ;

\ ---- the events of one definition ---------------------------------------------
\ One row per tape ordinal: the event's kind and two arguments, the reported
\ bytes (a copy), whether it is the definition's name token, a local's width,
\ the token's offset in the scanned text, and a transport's operand widths,
\ the top operand's first. Every report spans at least one captured byte and
\ its delimiter, so the body buffer bounds both tables.
4 constant XPORT-MAX                   \ the most values a transport takes (2swap, 2over)
8 XPORT-MAX + constant EV-CELLS
BODYBUF-CAP 2/ 1+ constant EV-CAP
EV-CAP EV-CELLS * cells allocate throw constant EVS
BODYBUF-CAP allocate throw constant POOL
variable EVN  variable POOLN  variable SCAN0  variable DONE-V  variable DONE-SEEN
: EV ( i -- e ) EV-CELLS * cells EVS + ;
: EV-KIND ( e -- a ) ;
: EV-A0 ( e -- a ) cell+ ;
: EV-A1 ( e -- a ) 2 cells + ;
: EV-STR ( e -- a ) 3 cells + ;        \ two cells: c-addr u
: EV-FIRST ( e -- a ) 5 cells + ;
: EV-W ( e -- a ) 6 cells + ;
: EV-OFF ( e -- a ) 7 cells + ;
: EV-XW ( e -- a ) 8 cells + ;         \ XPORT-MAX cells
: EV$ ( e -- c-addr u ) EV-STR 2@ ;
: POOL-SAVE ( c-addr u -- c-addr' u )
   POOLN @ over + BODYBUF-CAP > if 2drop s" token bytes overflow" CG-DIE then
   tuck POOL POOLN @ + swap move  POOL POOLN @ + swap  dup POOLN +! ;
: NEW-ROW ( -- e )
   EVN @ EV-CAP >= if s" event table full" CG-DIE then
   EVN @ EV dup EV-CELLS cells erase  1 EVN +! ;

create TRANSPORTS
   s" dup" string, s" drop" string, s" swap" string, s" over" string,
   s" nip" string, s" tuck" string, s" rot" string, s" -rot" string,
   s" 2dup" string, s" 2drop" string, s" 2swap" string, s" 2over" string, 0 c,
: TRANSPORT? ( c-addr u -- flag )
   TRANSPORTS begin dup c@ while
      >r 2dup r@ count NAME= if 2drop r> drop true exit then  r> count +
   repeat drop 2drop false ;

\ The tape's observer (CHECKER-TAPE:INSTALL). Each report takes the next
\ ordinal, so a row's index is its ordinal. The events are told only inside
\ DONE and only for the scan being told, so an accepted scan's rows are filled
\ there, with each local's width (LOCAL-WIDTH-OFF) and each transport
\ operand's (WF-W-AT-OFF, keyed by the token's offset and the operand's
\ position, the top's 0), which the checker answers until its next check.
: CG-SCAN ( c-addr u -- ) 2drop  EVN @ SCAN0 ! ;
: CG-TOKEN ( c-addr u off span kind first -- )
   >r 2drop  NEW-ROW tuck EV-OFF !  >r  POOL-SAVE r@ EV-STR 2!  r> r> swap EV-FIRST ! ;
: ROW-FILL ( ix -- )
   dup TAPE-EVENT execute {: ix kind a0 a1 :}  ix EV {: e :}
   kind e EV-KIND !  a0 e EV-A0 !  a1 e EV-A1 !
   kind K-LOCAL-DECL = kind K-LOCAL-REF = or if a0 LOCAL-WIDTH e EV-W ! then
   kind K-NAME = if e EV$ TRANSPORT? if
      XPORT-MAX 0 ?do e EV-OFF @ i WF-W-AT  e EV-XW i cells + ! loop then then ;
: CG-DONE ( c-addr u verdict -- )
   DONE-V !  2drop  true DONE-SEEN !
   DONE-V @ -1 = if EVN @ SCAN0 @ ?do i ROW-FILL loop then ;
: VERDICT-CK ( -- )                    \ the scan just run told an accepted verdict
   DONE-SEEN @ 0= DONE-V @ -1 <> or if s" no accepted verdict on the tape" CG-DIE then
   false DONE-SEEN ! ;
1 constant TAPE-BY                     \ the installer identity CHECKER-TAPE:INSTALLED-BY answers
variable RESOLVED
: TAPE-OPEN ( -- )
   RESOLVED @ 0= if s" checker names not resolved" CG-DIE then
   TAPE-BY ['] CG-SCAN ['] CG-TOKEN ['] CG-DONE TAPE-INSTALL-OFF OWNER execute
   0 EVN !  0 POOLN !  0 SCAN0 !  false DONE-SEEN !
   TAPE-ARM-OFF OWNER execute ;
: TAPE-CLOSE ( -- ) TAPE-DISARM-OFF OWNER execute ;

\ ---- the walk ----------------------------------------------------------------
\ Gforth's control-flow stack is its data stack, so the walk keeps its state
\ in variables and holds no cell across a handler.
variable WI                            \ the row being compiled, its ordinal
: EVT ( -- e ) WI @ EV ;
: EVT-A0 ( -- x ) EVT EV-A0 @ ;
: EVT-A1 ( -- x ) EVT EV-A1 @ ;
: EVT$ ( -- c-addr u ) EVT EV$ ;
: AT-DIE ( c-addr u -- )
   s" hb: gforth codegen: " ERR ERR  s"  at '" ERR EVT$ ERR  s" ' in " ERR DREC-NAME ERR
   REFUSE-RC COMPILE-DIE ;
: H-NOTHING ( -- ) ;
: H-LIT ( -- ) EVT-A0 postpone literal ;
: H-STRING ( -- ) EVT$ postpone sliteral ;
: H-PRINTED ( -- ) EVT$ postpone sliteral ['] HB-TYPE compile, ;
: H-COUNTED ( -- ) EVT$ CSTR-LIT ;

\ The control markers by the checker's code, CF-TOK?'s order (checker.f:19516).
\ `;match` (9) closes a MATCH, which the tape tells as K-MATCH-END instead.
create CTL-XTS
   ' KC-[: ,     ' KC-;] ,     ' KC-IF ,      ' KC-ELSE ,    ' KC-THEN ,
   ' KC-CASE ,   ' KC-OF ,     ' KC-ENDOF ,   ' KC-ENDCASE , 0 ,
   ' KC-BEGIN ,  ' KC-UNTIL ,  ' KC-AGAIN ,   ' KC-WHILE ,   ' KC-REPEAT ,
   ' KC-DO ,     ' KC-?DO ,    ' KC-LOOP ,    ' KC-+LOOP ,   ' KC-I ,
   ' KC-J ,      ' KC-EXIT ,   ' KC-LEAVE ,   ' KC-UNLOOP ,  ' KC-RECURSE ,
here CTL-XTS - cell / constant CTL-N
: H-CONTROL ( -- )
   EVT-A0 dup 0 CTL-N within 0= if drop s" control code" AT-DIE then
   cells CTL-XTS + @ ?dup 0= if s" control code" AT-DIE then execute ;

\ Locals: a value of w cells is w Gforth locals, %L<seq>.<cell> (reader.fs
\ LNAME). A group is declared at its `:}`, its last local first and each top
\ cell first, since Gforth's (local) gives its first name the top cell.
create GRP LOC-RECS 2* cells allot  variable GRPN
: H-LOCAL-DECL ( -- )
   GRPN @ LOC-RECS >= if s" local group" AT-DIE then
   EVT-A0 EVT EV-W @  GRPN @ 2* cells GRP + 2!  1 GRPN +! ;
: LDECLARE ( seq w -- ) {: seq w :} w 0 ?do seq w 1- i - LNAME (local) loop ;
: H-LOCAL-CLOSE ( -- )
   GRPN @ 0 ?do GRPN @ 1- i - 2* cells GRP + 2@ LDECLARE loop  0 0 (local)  0 GRPN ! ;
: H-LOCAL-REF ( -- )
   EVT-A0 EVT EV-W @ {: seq w :}
   w 0 ?do
      seq i LNAME rec-local dup translate-none = if drop s" local" AT-DIE then execute
   loop ;

\ Construct and MATCH. A variant value is its fields, its pad cells, then its
\ tag on top (habu2.f EM-ADT-CON-PUSHES); a MATCH is a case on the tag whose
\ arms drop the tag and the arm's pads, and whose fallthrough is the bad-tag
\ death (habu2.f C-DIE-BAD-TAG): "hb: bad <family> tag" on fd 2, exit 85.
85 constant BAD-TAG-RC                 \ src/core/engine-error.f:8 ENGINE-ERROR:BAD-TAG
: BAD-TAG ( tag c-addr u -- ) s" hb: bad " ERR ERR s"  tag" BAD-TAG-RC RC-DIE ;
variable VTAG  variable VPADS
: H-CONSTRUCT ( -- )
   WI @ MATCH-CELLS 0 max EVT-A1 + 0 ?do 0 postpone literal loop  EVT-A0 postpone literal ;
: H-MATCH ( -- ) KC-CASE ;
: H-MATCH-ARM ( -- ) EVT-A0 VTAG !  EVT-A1 VPADS ! ;
: H-MATCH-OF ( -- )
   WI @ MATCH-CELLS dup 0< if drop VPADS @ then {: pads :}
   VTAG @ postpone literal KC-OF  pads 0 ?do postpone drop loop ;
: H-MATCH-END ( -- )
   EVT-A0 FAMILY-NAME save-mem postpone 2literal ['] BAD-TAG compile, KC-ENDCASE ;

\ ['] and is name the record the checker bound (-1: none).
: TARGET-REC ( -- rec ) EVT-A0 dup -1 = if drop s" tick target" AT-DIE then REC ;
: H-TICK ( -- ) TARGET-REC @ postpone literal ;
: H-IS ( -- )                          \ reader.fs HB-IS: store into the dispatch cell
   TARGET-REC @ dup DEFER? 0= if drop s" is target" AT-DIE then
   >body @ postpone literal postpone ! ;

\ ---- calls -------------------------------------------------------------------
\ A core op whose instantiated cells differ from its stored effect moves whole
\ values: a transport as the permutation its primitive makes of markers, each
\ value as wide as the checker's width fact at its token says (EV-XW, the
\ facts habu2.f EM-P2-QUERY-1 reads frozen in the certificate that
\ CHECKER-CERT:PRODUCE makes right after DONE); >r r> r@ 2>r 2r> 2r@ as one
\ block of the cells they move (reader.fs RT-N>R). Any other callee is one
\ entry for every instantiation, as the native compiler stages it
\ (elaborate.f DO-WORD-CALL), and a row variable's cells pass beneath it.
variable CROW  variable CIN  variable COUT
: ROW@ ( field -- x ) cells CROW @ + @ ;
\ A permutation's table, sized from its call row: n input cells, m output
\ cells, each output cell's source, then the n cells PERMUTE parks the inputs in.
: PERM-NEW ( n m -- tbl )
   2dup + 2 + cells allocate throw {: n m tbl :}  n tbl !  m tbl cell+ !  tbl ;
: PERM-SRC ( i tbl -- addr ) swap 2 + cells + ;
: PERMUTE ( x*n tbl -- y*m )
   {: tbl :}  tbl @  tbl cell+ @  dup tbl PERM-SRC {: n m park :}
   n 0 ?do  park n 1- i - cells + !  loop
   m 0 ?do  i tbl PERM-SRC @ cells park + @  loop ;
: PERM-LIT ( tbl -- ) postpone literal ['] PERMUTE compile, ;
$5EED0000 constant MARK0
: MARK-RUN ( xt n m -- vout k )        \ vout: the input value each of k outputs is
   {: xt n m :}  depth {: d0 :}
   n 0 ?do MARK0 i + loop
   xt catch if s" marker run threw" AT-DIE then
   depth d0 - {: k :}
   k 0< k m > or if s" marker run depth" AT-DIE then
   k cells allocate throw {: vout :}
   k 0 ?do
      MARK0 - dup 0 n within 0= if s" marker run: not a permutation" AT-DIE then
      vout k 1- i - cells + !
   loop  vout k ;
: XW ( v bin -- w ) swap - 1- cells EVT EV-XW + @ ;   \ value v's cells, the deepest's 0
: SPLIT ( bin cin -- starts )          \ each value's first cell, the deepest's first
   {: bin cin :}
   bin XPORT-MAX > if s" transport values" AT-DIE then
   bin cells allocate throw  0 {: starts n :}
   bin 0 ?do  n starts i cells + !  i bin XW n + to n  loop
   n cin <> if s" transport cells" AT-DIE then  starts ;
: SHUFFLE ( xt bin cin -- )
   {: xt bin cin :}
   bin cin SPLIT {: starts :}
   xt bin COUT @ MARK-RUN {: vout k :}
   cin COUT @ PERM-NEW  0 {: tbl m :}
   k 0 ?do
      i cells vout + @  dup bin XW 0 ?do
         m COUT @ >= if s" transport cells out" AT-DIE then
         dup cells starts + @ i +  m tbl PERM-SRC !  m 1+ to m
      loop drop
   loop
   m COUT @ <> if s" transport cells out" AT-DIE then
   starts free throw  vout free throw  tbl PERM-LIT ;
: RS-MOVE ( xt w -- ) postpone literal compile, ;   \ w cells as one block
: CALL-REC ( -- rec )                  \ the record a dictionary binding names
   BOUND-RECORD ROW@ dup -1 = if drop s" call with no record" AT-DIE then REC ;
\ Any other core op whose cells differ (execute, catch) differs by a row
\ variable, and is a call like any other.
: VALUE-MOVE ( -- moved? )
   EVT$ TRANSPORT? if
      BOUND-KIND ROW@ BOUND-DICT <> if s" transport with no record" AT-DIE then
      CALL-REC @  BOUND-IN ROW@  CIN @  SHUFFLE true exit then
   EVT$ s" >r" NAME= EVT$ s" 2>r" NAME= or if ['] RT-N>R CIN @ RS-MOVE true exit then
   EVT$ s" r>" NAME= EVT$ s" 2r>" NAME= or if ['] RT-NR> COUT @ RS-MOVE true exit then
   EVT$ s" r@" NAME= EVT$ s" 2r@" NAME= or if ['] RT-NR@ COUT @ RS-MOVE true exit then
   false ;
: CORE-WIDE? ( -- flag )
   CIN @ 0< if false exit then
   CIN @ BOUND-IN ROW@ <>  COUT @ BOUND-OUT ROW@ <> or
   BOUND-CTL ROW@ CTL-CORE-OP and 0<> and ;
: CALLEE ( -- )                        \ the call's one entry
   BOUND-KIND ROW@ BOUND-DICT = if CALL-REC COMPILE-REC exit then
   BOUND-KIND ROW@ BOUND-INTRINSIC = if
      EVT$ KW-C KW-FIND ?dup 0= if s" intrinsic with no row" AT-DIE then execute exit then
   s" binding kind" AT-DIE ;
: H-CALL ( -- )
   WI @ CALL-BINDING 0= if drop s" no binding" AT-DIE then CROW !
   WI @ CALL-CELLS COUT ! CIN !
   CORE-WIDE? if VALUE-MOVE if exit then then
   CALLEE
   WI @ MATCH-CELLS dup 0> if          \ a wide constructor: its pads under the tag
      postpone >r 0 ?do 0 postpone literal loop postpone r> else drop then ;
: H-NAME ( -- ) EVT EV-FIRST @ if exit then H-CALL ;

\ ---- the handler per kind (CG-RESOLVE) ----------------------------------------
32 constant KINDS-CAP
create HANDLERS KINDS-CAP cells allot
: HANDLER! ( xt kind -- )
   dup 0 KINDS-CAP within 0= if s" event kind out of range" CG-DIE then
   cells HANDLERS + dup @ if s" two event kinds share a value" CG-DIE then ! ;
: KIND-HANDLER ( kind -- xt )
   dup 0 KINDS-CAP within if cells HANDLERS + @ ?dup if exit then else drop then
   s" event kind" AT-DIE ;
\ Run once the checker has loaded (boot.fs), before its hook is installed.
: CG-RESOLVE ( -- )
   CK-LINK @ begin ?dup while dup CK-RESOLVE @ repeat
   HANDLERS KINDS-CAP cells erase
   ['] H-NAME K-NAME HANDLER!             ['] H-LIT K-INT HANDLER!
   ['] H-LIT K-REAL HANDLER!              ['] H-STRING K-STRING HANDLER!
   ['] H-LIT K-CHAR HANDLER!              ['] H-COUNTED K-COUNTED HANDLER!
   ['] H-PRINTED K-PRINTED HANDLER!       ['] H-CONTROL K-CONTROL HANDLER!
   ['] H-LOCAL-DECL K-LOCAL-DECL HANDLER! ['] H-LOCAL-REF K-LOCAL-REF HANDLER!
   ['] H-LOCAL-CLOSE K-LOCAL-CLOSE HANDLER!
   ['] H-CONSTRUCT K-CONSTRUCT HANDLER!   ['] H-MATCH K-MATCH HANDLER!
   ['] H-MATCH-ARM K-MATCH-ARM HANDLER!   ['] H-MATCH-OF K-MATCH-OF HANDLER!
   ['] H-MATCH-END K-MATCH-END HANDLER!   ['] H-TICK K-TICK HANDLER!
   ['] H-IS K-IS HANDLER!                 ['] H-NOTHING K-OPERAND HANDLER!
   true RESOLVED ! ;
variable WEND                          \ the row the walk stops before
: WALK ( from to -- )
   WEND !  WI !
   begin WI @ WEND @ < while  EVT EV-KIND @ KIND-HANDLER execute  1 WI +!  repeat ;
\ A definer with a signature is checked in two scans, its clause's first
\ (CHECK-SPLIT), and Gforth compiles its head, then does>, then the clause.
: SPLIT? ( -- flag ) TSIG-U-CELL D@ 0<> DOESB-CELL D@ 0<> and ;
variable HEAD0                         \ a definer's first head row: its clause's row count
: CG-WALK ( -- )
   0 GRPN !
   SPLIT? if HEAD0 @ EVN @ WALK  DOES-COMPILE  0 HEAD0 @ WALK exit then
   0 EVN @ WALK ;

\ ---- the capture (habu1.f EMIT-BCAP) -----------------------------------------
\ The body text the hook reads, token by token, with nothing run. Each token
\ resolves as the compile loop resolves it (reader.fs RUN-COMPILE), so a token
\ nothing defines dies E-UNDEFINED at the token, and a parsing keyword reads
\ and walls its operand as it does when compiled. A local is a name only in
\ its scope (reader.fs CLOC, CF-SCOPE). Construct and MATCH operands are
\ consumed before resolution, as habu2.f EM-COMPILE-ADT-MODE consumes them
\ (CMODE: 1 construct's family, 2 its variant, 3 match's family, 4 a variant
\ or ;match, 5 the arm's `of`); a MATCH opens a frame at its family and each
\ arm one at its `of`, which `;match` and the arm's `endof` close.
variable CMODE
: CAP-STRING ( c-addr u -- )           \ a string keyword's payload, walled, not kept
   over c@ FOLD [char] c = >r  nip 3 = if ESC-TEXT else STR-TEXT then
   nip r> if CSTR-CHECK then drop ;
: CAP-RESOLVE ( c-addr u -- )
   2dup KW-C KW-FIND if CF-SCOPE exit then
   2dup NUM-PARSE nip nip if 2drop exit then
   2dup LOOKUP-REC ?dup 0= if UNDEF-DIE then
   >FLAGS @ DNAME-IMM and if
      s" hb: gforth has no immediate word in a checked body: " ERR ERR
      REFUSE-RC COMPILE-DIE then
   2drop ;
: CAP-MODE ( c-addr u -- )             \ a construct or MATCH operand
   CMODE @ 1 = if BCS 2 CMODE ! exit then
   CMODE @ 2 = if BCS 0 CMODE ! exit then
   CMODE @ 3 = if BCS 0 FR-PUSH 4 CMODE ! exit then
   CMODE @ 4 = if 2dup BCS s" ;match" NAME= if FR-POP 0 else 5 then CMODE ! exit then
   BCS ARM FR-PUSH 0 CMODE ! ;
: CAP-TOKEN ( c-addr u -- )
   CMODE @ if CAP-MODE exit then
   2dup s" does>" NAME= if DOES-TAKE exit then
   2dup s" {:" str= if 2drop LOCALS-READ drop exit then
   2dup BCS
   2dup LOCAL? if QUOT-REF 2drop exit then
   2dup + {: a u e :}
   a u STRING-WORD? if a u CAP-STRING e true CAPTURE-TAIL exit then
   a u s" [char]" NAME= if s" [char]" DEF-NAME 2drop e false CAPTURE-TAIL exit then
   a u s" [']" NAME= if TICK-REC nip nip DNAME-INT REC-GATE drop e false CAPTURE-TAIL exit then
   a u s" is" NAME= if IS-TARGET drop e false CAPTURE-TAIL exit then
   a u s" construct" NAME= if 1 CMODE ! exit then
   a u s" match" NAME= if 3 CMODE ! exit then
   a u s" endof" NAME= FR-TOP ARM = and if 4 CMODE ! then
   a u CAP-RESOLVE ;
: CAPTURE ( -- )
   0 CMODE !
   begin NEXT-TOKEN 2dup s" ;" str= 0= while
      2dup COMMENT if 2drop else CAP-TOKEN then
   repeat 2drop ;

\ ---- the checked colon definition --------------------------------------------
\ The check runs with the tape armed. A definer with a signature is checked as
\ native's first check, which decides acceptance, checks one (habu2.f
\ EM-COMPILE-PUBLISH-TRUSTED): its clause against the created signature
\ (reader.fs CHECK-DOES), then its head under the hook, whose record takes the
\ clause's created effect (checker.f DOES-EFF-TAKE). The clause thus binds its
\ names before the head's is pending. The native compile tier's head-first
\ order (compiler.f CHECK-DOES-SPLIT) runs only once a program is accepted. The
\ clause's rows come first on the tape; an armed unit keeps every scan's rows
\ (checker.f REC-RESET), so the walk reads them after the head's check. Any
\ other body goes to the hook whole, does> and all, as native hands a body with
\ no signature (habu2.f EM-COMPILE-PUBLISH). Each check answers whether the
\ body is kept. A refused or failed body closes Gforth's definition uncounted.
: CHECK-SPLIT ( -- kept? )
   CHECK-DOES VERDICT-CK  EVN @ HEAD0 !
   BODY DEFINER-LEN HOOK-CELL D@ execute 0= if DIE-DOES then  VERDICT-CK true ;
\ A zero verdict over a body with a signature dies (habu2.f
\ C-CALL-CHECK-DEFINER); over one with none it drops the definition and the
\ load goes on (EM-COMPILE-PUBLISH-HOOKED's rejected arm).
: CHECK-WHOLE ( -- kept? )
   BODY$ HOOK-CELL D@ execute 0= if TSIG-U-CELL D@ if DIE-DOES then false exit then
   VERDICT-CK true ;
: CG-CHECK ( -- kept? )
   TAPE-OPEN  SPLIT? if ['] CHECK-SPLIT else ['] CHECK-WHOLE then  catch  TAPE-CLOSE throw ;
: CG-ABORT ( colon-sys n -- ) >r CLOSE-SYS 2drop  CLEAR-DEF  r> throw ;
: CG-COLON ( "name" -- )
   0 DEF-OPEN
   ['] CAPTURE catch ?dup if CG-ABORT then
   ['] CG-CHECK catch ?dup if CG-ABORT then
   0= if CLOSE-SYS 2drop  CLEAR-DEF exit then
   CG-WALK
   CLOSE-SYS 0= if CS-MISMATCH throw then
   PEND-CELL D@ !  REC-PUBLISH  REC-WIDE  CLEAR-DEF ;
