\ definers.f - the definition heads of the interpret loop written in Habu:
\ `:`, `kernel:` and `trusted:`, as the engine's EM-INTERPRET-COLON and
\ C-TRUSTED (habu2.f) open a definition, and the capture of the body that
\ follows one; `cast:` and `linear:`, which declare and publish at once;
\ `immediate`; and the definers `create`, `variable`, `constant` and `defer`,
\ whose bodies NCOMP compiles as they are read. STEP (src/habu/interpret.f) asks
\ COMPILING? after a comment and DEFINE? after the package keywords.
\
\ A head opens record NDICT through def-open, unpublished, and the definition
\ stays pending: each body token is captured into BODYBUF, as the engine's
\ tier 1 captures it, a stack-neutral parsing immediate among them runs as it
\ is read, `does>` splits the body and takes the signature of the words it
\ creates, and `;` compiles the body whole and ends the definition through
\ def-close. That is tier 1. Tier 0 compiles each token as it is read, with
\ the engine's JIT: the head ends through jit-open and each body token, `;`
\ among them, goes to jit-token.
\
\ The head refuses in the engine's order and with its text. A definition
\ writer's own refusal exits without a word (src/habu/prims.f), so every
\ refusal the engine reports is made here first. Unlike the engine's head,
\ this one leaves no PROT window open: def-open and jit-open close their own.

require lib/prelude.f
require lib/string.f
require lib/source-syntax.f
require src/core/checker.f
require src/core/does-clause.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/dict.f
require src/compiler/native/compiler.f
require src/habu/outer.f
require src/habu/packages.f

package OUTER

private

\ Tier 1's capture reads locals before syntax. These cells describe lexical
\ lifetime only; LOCF is the JIT's physical frame and CF its patch stack.
LOC-RECS TYPED-BUFFER BODY-LOCAL-DEPTH n
variable BODY-DEPTH
variable BODY-RAW-NEXT
variable TOK-BODY-DEPTH
variable TOK-BODY-RAW

: BODY-LEX-RESET ( -- )
   0 BODY-DEPTH !  0 BODY-RAW-NEXT ! ;

76 constant DEF-RC-P2-NEST       \ habu2.f EM-INTERPRET-COLON: a `:` while pass 2 runs
76 constant DEF-RC-BAD-SIG       \ habu2.f C-SIG-BAD: `trusted:` or `does>` with no signature
71 constant DEF-RC-BODY-FULL     \ habu2.f EM-BODY-CAP-DIE: the body capture is full
72 constant DEF-RC-PD-FULL       \ habu2.f C-PD-DIE-FULL: no pending slot holds the defer

\ ---- the definition writers, each a TRUSTED: boundary ------------------------
TRUSTED: DEF-OPEN ( ptr u8 n n n n -- ) def-open ;
TRUSTED: DEF-APPEND ( ptr u8 n -- ) body-append ;
TRUSTED: DEF-TRUST-SIG ( ptr u8 n -- ) trust-sig! ;
TRUSTED: DEF-CREATED-SIG ( ptr u8 n -- ) created-sig! ;
TRUSTED: DEF-CLOSE ( -- ) def-close BODY-LEX-RESET ;
TRUSTED: DEF-ABORT ( -- ) def-abort BODY-LEX-RESET ;
TRUSTED: DEF-IMM-MARK ( -- ) imm-mark ;
TRUSTED: DEF-CAST ( -- ) def-cast ;
TRUSTED: DEF-MIN-IN ( n n -- ) min-in-mark ;
TRUSTED: DEF-JIT-OPEN ( -- ) jit-open ;
TRUSTED: DEF-JIT-TOKEN ( -- ) jit-token ;

\ ---- tier 0 ------------------------------------------------------------------
\ Nothing of this loop's is on the stack when jit-token runs: what the token
\ leaves there is the program's, as after a word the loop runs.
: DEF-TIER-0? ( -- bool )
   NCOMP-DISPATCH:DEF-TIER-CELL CELL@ 0= ;

\ A `:` while the JIT's pass 2 reads a body again refuses first, naming the
\ token, as the engine's head does; `trusted:` is never refused for it.
: DEF-P2-NEST ( -- )
   P2-CELL CELL@ 0= if exit then
   s" hb: nested definition in pass 2: " SAY
   DEF-RC-P2-NEST PKG-FAIL ;

\ ---- the body capture (habu1.f EMIT-BCAP, habu2.f EM-BODY-CAP-DIE) ------------
\ The capture's first token, the definition's name, which a head seeds: the
\ bytes up to a space, a zero byte or BODYLEN (habu2.f EMIT-DIAGDEF).
: DEF-CAPTURED-NAME ( -- ptr u8 n )
   data-base BODYBUF-OFF + BYTE-VIEW BODYLEN-CELL CELL@ {: b:ptr len:n :}
   0 begin
      dup len < if b over + c@ dup BLANK <> swap 0<> and else false then
   while 1+ repeat
   {: u:n :}
   b u ;

\ A capture that would take the body past BODYBUF-CAP ends the definition,
\ naming the ceiling, the definition and the size it needed.
: DEF-BODY-FULL ( n -- ) {: need:n :}
   s" hb: definition body text full at " SAY
   BODYBUF-CAP DIGITS$ SAY
   s"  bytes: " SAY
   DEF-CAPTURED-NAME SAY
   s"  needs " SAY
   need DIGITS$ SAY
   DEF-RC-BODY-FULL THROW-AT ;

\ The source ended inside the pending definition: refused, named and located
\ at its name token (layout.f PENDTKA-CELL), as habu2.f LSRCEND refuses.
: DEF-ENDED ( -- )
   PENDTKA-CELL CELL@ INP-CELL CELL!
   s" hb: source ended inside definition: " SAY
   DEF-CAPTURED-NAME SAY
   RC-UNCLOSED THROW-AT ;

\ The bytes and one space join the body.
: DEF-CAPTURE ( ptr u8 n -- ) {: a:ptr u:n :}
   BODYLEN-CELL CELL@ u + 1 + {: need:n :}
   need BODYBUF-CAP > if need DEF-BODY-FULL then
   a u DEF-APPEND ;

\ ---- the head's room (habu2.f EM-INTERPRET-COLON, C-TRUSTED) -----------------
\ CP at the code ceiling and no record slot left each refuse, naming the
\ keyword. Both comparisons are signed, as the engine's head makes them.
: DEF-ROOM ( -- )
   cp@ dbase@ REGION + PKG-CODE-RESERVE - >= if
      s" hb: code space full at: " SAY PKG-RC-CODE-FULL PKG-FAIL
   then
   ndict@ DICT-CAP >= if
      s" hb: dictionary full at: " SAY PKG-RC-DICT-FULL PKG-FAIL
   then ;

\ The name. With the input at its end `:` and `kernel:` refuse with the
\ engine's colon text, `trusted:` as the reader keyword it is (habu2.f
\ C-DIE-KEYWORD-NAME).
: DEF-NAME ( bool -- ) {: trusted:bool :}
   trusted if s" trusted:" OPERAND exit then
   TOKEN if exit then
   s" hb: : missing definition name after " SAY RC-NO-NAME THROW-AT ;

\ ---- the qualifier (habu2.f EMIT-QUALIFY-DEF) --------------------------------
\ The wordlist NAME:tail goes into: NAME's public one, from its namespace row.
\ With no row a new one is made, as a qualified definition makes it: a public
\ wid and no private one.
: DEF-NAMESPACE ( ptr u8 n -- n ) {: a:ptr q:n :}
   a q XREF-NAMESPACE-WL WL-PROBE {: row:ptr :}
   row XREF-FOUND? if row XREF-PKG-PUBLIC exit then
   PKG-DICT-ROOM
   a q PKG-CODE-ROOM
   a q false PKG-NS-RECORD XREF-REC XREF-PKG-PUBLIC ;

\ A selected unit owns its definitions: a name with a colon anywhere in it,
\ at an edge too, refuses before the qualifier could send it to another
\ package (habu2.f C-UNIT-DEF-NAME-GUARD).
: DEF-UNIT-GUARD ( -- )
   UNIT? 0= if exit then
   TOKEN$ 0 COLON-AT 0 >= if UNIT-REFUSE then ;

\ The tail the record is named and the wordlist it goes into. DEF-TKA and
\ DEF-TKL take the name token, which the refusals after them name, as TKA and
\ TKL still do: the engine moves those to the tail and back, and this head
\ names the tail where the engine names it. A bare name goes into the current
\ wordlist; the qualifier is the first colon at neither edge (one at either
\ edge leaves the whole token a bare name: `:a:b` is bare), and a second colon
\ after it refuses.
: DEF-QUALIFY ( -- ptr u8 n n )
   DEF-UNIT-GUARD
   SEAL-GUARD
   TKA-CELL CELL@ DEF-TKA-CELL CELL!
   TKL-CELL CELL@ DEF-TKL-CELL CELL!
   TOKEN$ {: a:ptr u:n :}
   a u 0 COLON-AT {: q:n :}
   q 1 <  q 1+ u >=  or if a u get-current exit then
   a u q 1+ COLON-AT 0 >= if PKG-RC-CONTEXT PKG-FAIL then
   a q 1+ ZPTR+  u q - 1-  a q DEF-NAMESPACE ;

\ ---- the compile-keyword wall (habu2.f EMIT-DEF-KW-GUARD) ---------------------
\ The keywords a body's compiler reads before it looks a word up
\ (EM-COMPILE-KEYWORDS): a word named by one could never be called from a body.
: DEF-KEYWORDS ( -- ptr u8 n )
   S\" if then else begin until again while repeat case of endof endcase construct match s\q c\q .\q s\\\q c\\\q .\\\q ['] [char] does> [: is ;] do loop i >r r> r@ exit recurse ?do +loop j leave unloop {:" ;

\ A design may define none of the engine's dispatch-row names. The twelve
\ admitted syntax rows keep the ordinary reserved-name error; every other
\ row is outside the vocabulary before dictionary duplicate checks run.
: DEF-DESIGN-KEYWORDS ( -- ptr u8 n )
   S\" if else then {: :} s\q package public private ;package using ;using" ;

\ The interpret dispatch rows, plus kernel: and ;match, which the ARM row walk
\ compares inside their constructs. Ordinary words such as require and include
\ may be published by a package even though a definition cannot take their
\ names at this checked head boundary.
: DEF-INTERPRET-KEYWORDS ( -- ptr u8 n )
   S\" kernel: trusted: cast: linear: immediate create variable constant defer export char ' ;match" ;

: DEF-NAME-ONLY-KEYWORDS ( -- ptr u8 n )
   s" : 2constant to require include sumtype variant ;variant ;sumtype" ;

: DEF-OP-KEYWORDS ( -- ptr u8 n )
   s" + - * and or xor lshift rshift dup drop swap over nip = <> < > <= >= 1+ 1- 0= 0< negate invert f+ f- f* f/" ;

: DEF-P2-KEYWORDS ( -- ptr u8 n )
   s" tuck rot -rot 2dup 2drop 2swap 2over 2>r 2r> 2r@ @ !" ;

\ The index past the keyword that starts at s in the table.
: DEF-KEYWORD-END ( ptr u8 n n -- n ) {: t:ptr tu:n s:n :}
   s begin dup tu < if t over + c@ BLANK <> else false then while 1+ repeat ;

: DEF-IN-TABLE? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n t:ptr tu:n :}
   0 begin dup tu < while
      {: s:n :}
      t tu s DEF-KEYWORD-END {: e:n :}
      a u  t s ZPTR+ e s -  FOLDED= if true exit then
      e 1+
   repeat
   drop false ;

: DEF-KEYWORD? ( ptr u8 n -- bool )
   DEF-KEYWORDS DEF-IN-TABLE? ;

: DEF-DESIGN-KEYWORD? ( ptr u8 n -- bool )
   DEF-DESIGN-KEYWORDS DEF-IN-TABLE? ;

: DEF-ROW-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u DEF-KEYWORD? if true exit then
   a u DEF-INTERPRET-KEYWORDS DEF-IN-TABLE? if true exit then
   a u DEF-OP-KEYWORDS DEF-IN-TABLE? if true exit then
   a u DEF-P2-KEYWORDS DEF-IN-TABLE? ;

: DEF-NAME-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u DEF-ROW-KEYWORD? if true exit then
   a u DEF-NAME-ONLY-KEYWORDS DEF-IN-TABLE? ;

\ The x86-64 policy writer asks this captured interpreter once per package,
\ before setting its admission bit. Use the same dispatch-row table that guards
\ names in a sealed definition, including pass-2 rows.
public
: POLICY-KEYWORD ( n -- ptr u8 n bool ) {: wid:n :}
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST wid = if
         rec XREF-NAME$ {: a:ptr u:n :}
         a u DEF-DESIGN-KEYWORD? a u DEF-ROW-KEYWORD? or if
            a u true unloop exit
         then
      then
   loop
   s" " false ;

: INSTALL-POLICY ( -- )
   ['] POLICY-KEYWORD data-base POLICY-ABI:KEYWORD-CELL + xt! ;
private

: DEF-KEYWORD-REFUSE ( ptr u8 n -- ) {: a:ptr u:n :}
   s" hb: compile keyword cannot be a definition name: " SAY
   a u SAY RC-REJECT THROW-AT ;

: DEF-POLICY-REFUSE ( ptr u8 n -- ) {: a:ptr u:n :}
   s" hb: not in vocabulary: " SAY
   a u SAY ENGINE-ERROR:POLICY THROW-AT ;

\ Matched as the engine matches its keywords, with only the tail's A-Z folded.
: DEF-WALL ( ptr u8 n -- ) {: a:ptr u:n :}
   POLICY-NDICT-CELL CELL@ 0<> if
      a u DEF-DESIGN-KEYWORD? if a u DEF-KEYWORD-REFUSE then
      a u DEF-NAME-KEYWORD? if a u DEF-POLICY-REFUSE then
      exit
   then
   a u DEF-KEYWORD? if a u DEF-KEYWORD-REFUSE then ;

\ ---- the signature (habu2.f C-SIG-START, C-SIG-END, C-SIG-CAPTURE-TSIG) -------
\ A signature starts at the first byte past the blanks when that byte is `(`,
\ whole token or not: where it starts, and whether one does.
: DEF-SIG-START ( -- ptr u8 bool )
   INP-CELL ADDR@ INE-CELL ADDR@ {: p:ptr e:ptr :}
   p e BLANKS-END {: s:ptr :}
   s e < if s c@ OPEN-COMMENT = else false then {: open:bool :}
   s open ;

\ The byte past the `)` that closes the signature at s, and whether one does;
\ with none, the input's end.
: DEF-SIG-END ( ptr u8 -- ptr u8 bool ) {: s:ptr :}
   INE-CELL ADDR@ {: e:ptr :}
   s begin dup e < if dup c@ CLOSE-COMMENT <> else false then while 1 + repeat
   {: c:ptr :}
   c e < if c 1 + true exit then
   e false ;

\ INP passes the signature, trust-sig! takes the text inside its parentheses
\ and the capture all of it. The inner length is the engine's, two less than
\ the whole: one less than the text for a signature the input ends inside.
: DEF-SIG-TAKE ( ptr u8 ptr u8 -- ) {: s:ptr end:ptr :}
   end INP-CELL ADDR!
   s 1 + end s - 2 - DEF-TRUST-SIG
   s end s - DEF-CAPTURE ;

\ `:` takes one if it is there, to the input's end if it is not closed.
: DEF-MAYBE-SIG ( -- )
   DEF-SIG-START {: s:ptr open:bool :}
   open 0= if exit then
   s DEF-SIG-END drop {: end:ptr :}
   s end DEF-SIG-TAKE ;

\ A signature that must be there, opened and closed, or the refusal names the
\ token: where it starts and the byte past it. A head's signature that the
\ input ends before or inside is the source ending inside the definition
\ (habu2.f C-PARSE-TRUST-SIG).
: DEF-SIG-SPAN ( bool -- ptr u8 ptr u8 ) {: head:bool :}
   DEF-SIG-START {: s:ptr open:bool :}
   head s INE-CELL ADDR@ = and if DEF-ENDED then
   open 0= if DEF-RC-BAD-SIG PKG-FAIL then
   s DEF-SIG-END {: end:ptr closed:bool :}
   closed 0= if head if DEF-ENDED then DEF-RC-BAD-SIG PKG-FAIL then
   s end ;

\ `trusted:` needs one, and the refusal names the definition.
: DEF-REQUIRED-SIG ( -- )
   true DEF-SIG-SPAN DEF-SIG-TAKE ;

\ ---- the head ----------------------------------------------------------------
\ Tier 1 compiles through the entry the AOT seed installed; with none the
\ boot is broken, and the process ends (habu2.f NCOMP-EMIT:LOAD).
: DEF-DISPATCH ( -- )
   NCOMP-DISPATCH:XT-CELL CELL@ 0<> if exit then
   s" hb: native compiler dispatch unset" ENGINE-ERROR:AOT-SEED FAIL-CLOSED ;

\ The tail and its wordlist pass the wall, the room, the duplicate test and the
\ seal's protected wordlists, then the name's code room, before def-open
\ writes anything (habu2.f EMIT-QUALIFY-DEF, EMIT-STORE-DEF-NAME), then opens
\ the record with its kind and the tier whose close ends it: the live tier for
\ a colon body, 1 for a body NCOMP compiles as it is read. The refusals name
\ the token, but the wall and the code room name the tail.
: DEF-RECORD ( ptr u8 n n n n -- ) {: a:ptr u:n wid:n kind:n tier:n :}
   a u DEF-WALL
   PKG-DICT-ROOM
   a u wid WL-PROBE XREF-FOUND? if
      s" duplicate definition: " SAY PKG-RC-DUPLICATE PKG-FAIL
   then
   wid PKG-OPEN-WID
   a u PKG-CODE-ROOM
   a u wid kind tier DEF-OPEN ;

\ The name opens a pending record with the capture it seeds. `trusted:` then
\ sets the trusted cell and needs a signature; `:` takes one if it is there.
\ The record's tier then picks the compiler: tier 0's JIT opens the body.
: DEF-HEAD ( bool -- ) {: trusted:bool :}
   trusted 0= if DEF-P2-NEST then
   TASK-GUARD
   DEF-ROOM
   trusted DEF-NAME
   0 BODYLEN-CELL CELL!
   TOKEN$ DEF-CAPTURE
   DEF-QUALIFY 0 tier@ DEF-RECORD
   0 LOCN-CELL CELL!  0 LOCF-CELL CELL!
   BODY-LEX-RESET
   trusted if
      1 TRUSTED-CELL CELL!
      DEF-REQUIRED-SIG
   else
      DEF-MAYBE-SIG
   then
   DEF-TIER-0? if DEF-JIT-OPEN exit then
   DEF-DISPATCH ;

\ ---- a string in the body (habu2.f NCOMP-EMIT:CAPTURE-STRING) -----------------
\ After a string keyword the body takes the literal's text as written, from one
\ byte past the keyword through its closing quote, in one capture. The scan
\ comes first, so a literal with no closing quote or with a bad escape refuses
\ as the top-level keyword does, before any of it is captured. A counted
\ string's length waits for `;`, which compiles the body.
: DEF-TEXT ( -- )
   TEXT {: a:ptr u:n :}
   a u 1+ DEF-CAPTURE ;

: DEF-ESC-TEXT ( -- )
   ESC-SCAN drop {: s:ptr q:ptr :}
   q PAST
   s q s - 1+ DEF-CAPTURE ;

\ The engine's six string keywords, matched as LITERAL? matches them.
: DEF-STRING-TEXT ( -- )
   S\" s\q" TOKEN-IS? if DEF-TEXT exit then
   S\" c\q" TOKEN-IS? if DEF-TEXT exit then
   S\" .\q" TOKEN-IS? if DEF-TEXT exit then
   S\" s\\\q" TOKEN-IS? if DEF-ESC-TEXT exit then
   S\" c\\\q" TOKEN-IS? if DEF-ESC-TEXT exit then
   S\" .\\\q" TOKEN-IS? if DEF-ESC-TEXT then ;

\ A body admits a local before looking up a dictionary word. Tier 0's JIT
\ fills LOCNAMES while compiling `{:`; tier 1 captures each declaration here,
\ then the native compiler reads it from BODYBUF at `;`.
: BODY-LOCAL? ( -- bool )
   TOKEN$ {: a:ptr u:n :}
   LOCN-CELL CELL@ 0 ?do
      data-base LOCNAMES + i LOC-REC * + BYTE-VIEW {: row:ptr :}
      row CELL-VIEW @ u = if
         a u row 8 + u STR= if true unloop exit then
      then
   loop
   false ;

: BODY-LOCAL-ONE ( -- )
   TOKEN$ {: a:ptr u:n :}
   LOCN-CELL CELL@ {: idx:n :}
   idx LOC-RECS >= if
      s" hb: more than 64 locals in one definition: " SAY
      TOKEN$ SAY RC-REJECT THROW-AT
   then
   a u 0 COLON-AT {: colon:n :}
   colon 0 < if u else colon then {: len:n :}
   len LOC-NAME-CAP > if
      s" hb: local name over 16 bytes: " SAY
      TOKEN$ SAY RC-REJECT THROW-AT
   then
   data-base LOCNAMES + idx LOC-REC * + BYTE-VIEW {: row:ptr :}
   len row CELL-VIEW !
   a row 8 + len BYTE-COPY
   BODY-DEPTH @ idx BODY-LOCAL-DEPTH !
   idx 1+ LOCN-CELL CELL! ;

: BODY-LOCAL-GROUP ( -- )
   begin TOKEN while
      TOKEN$ DEF-CAPTURE
      s" :}" TOKEN-IS? if exit then
      BODY-LOCAL-ONE
   repeat ;

\ A closer discards exactly the locals declared in its block. A later rejected
\ token can restore the count without reconstructing any row.
: BODY-BLOCK-DROP ( -- )
   begin
      LOCN-CELL CELL@ 0 > if
         LOCN-CELL CELL@ 1- BODY-LOCAL-DEPTH @ BODY-DEPTH @ >=
      else false then
   while
      LOCN-CELL CELL@ 1- LOCN-CELL CELL!
   repeat ;

: BODY-BLOCK-STEP ( -- )
   LOCN-CELL CELL@ 0= if exit then
   TOKEN$ SOURCE-SYNTAX:BLOCK-OPENER? if
      BODY-DEPTH @ 1+ BODY-DEPTH ! exit
   then
   s" else" TOKEN-IS? if BODY-BLOCK-DROP exit then
   TOKEN$ SOURCE-SYNTAX:BLOCK-CLOSER? if
      BODY-BLOCK-DROP  BODY-DEPTH @ 1- BODY-DEPTH !
   then ;

\ The JIT's sealed row and the tier-1 capture admit the same body vocabulary.
\ `;` is handled before the capture at both tiers; a tier-0 JIT call sees it
\ here first. A miss reaches the five design body keywords only after local,
\ dictionary and numeric lookup, as the native capture does.
: BODY-POLICY ( -- )
   BODY-LOCAL? if exit then
   TOKEN$ NUMBER {: v:n flt:bool num:bool range:bool :}
   range if UNDEFINED then
   POLICY-NDICT-CELL CELL@ 0= if exit then
   num if exit then
   TOKEN$ FIND-SCOPE {: rec:ptr :}
   rec XREF-FOUND? if rec POLICY-CHECK-REC exit then
   TOKEN$ FIND {: used:ptr :}
   used XREF-FOUND? if used POLICY-CHECK-REC exit then
   s" ;" TOKEN-IS? if exit then
   s" if" TOKEN-IS? if exit then
   s" else" TOKEN-IS? if exit then
   s" then" TOKEN-IS? if exit then
   S\" s\q" TOKEN-IS? if exit then
   s" {:" TOKEN-IS? if exit then
   POLICY-REFUSE ;

\ ---- an immediate in the body (habu2.f NCOMP-EMIT:CAPTURE-IMMEDIATE) ----------
\ A captured token LFIND resolves to an immediate word runs now when the
\ checker calls it a stack-neutral parsing immediate (parse-imm): it may read
\ the input after it or end the definition, and the compiler passes over it at
\ `;`. Any other immediate waits in the capture for the compiler. The engine
\ closes the code window over the head's unit before it asks the checker; here
\ that window is closed already: def-open closes its own, and evaluate's
\ return closes the one an engine head opened. The engine finds
\ NEUTRAL-PARSE-IMM? by name as it asks, and exits 70 naming it when no checker
\ is loaded; this file binds it as it loads, after the engine's checker.
: DEF-IMMEDIATE ( -- n bool )
   TOKEN$ FIND-SCOPE {: rec:ptr :}
   rec XREF-FOUND? 0= if 0 false exit then
   rec XREF-FLAGS DNAME-IMM and 0= if 0 false exit then
   rec POLICY-CHECK-REC
   rec XREF-START  TOKEN$ NEUTRAL-PARSE-IMM? ;

\ The engine's cells hold the preflight's and the compiler entry's xts raw;
\ these views state their effects: the checker's (src/core/check-hook.f
\ PREFLIGHT) and NCOMP:COMPILE's.
TRUSTED: DEF-AS-PREFLIGHT ( n -- [ ptr u8 n ptr u8 n bool -- ] ) ;
TRUSTED: DEF-AS-COMPILER ( n -- [ ptr u8 n -- ] ) ;

\ An armed checker's preflight gets the body so far, the token and the trusted
\ cell first; one armed without a preflight is refused (habu2.f
\ C-CALL-COMPILE-IMMEDIATE, LPREFMISS).
: DEF-PREFLIGHT ( -- )
   HOOK-CELL CELL@ 0= if exit then
   COMPILE-PREFLIGHT-CELL CELL@ {: xt:n :}
   xt 0= if
      S\" hb: compile preflight hook missing\n" SAY RC-REJECT throw
   then
   data-base BODYBUF-OFF + BYTE-VIEW BODYLEN-CELL CELL@ TOKEN$ TRUSTED-CELL CELL@ 0<>
   xt DEF-AS-PREFLIGHT execute ;

\ The xt waits on the return stack while the unit hook and the preflight run,
\ and the stack's floor holds after the word, as after a word the loop runs.
TRUSTED: DEF-RUN ( n -- )
   >r 0 UNIT-EV-IMMEDIATE UNIT-EVENT drop
   DEF-PREFLIGHT r> execute-floor FLOORED ;

: DEF-IMMEDIATE? ( -- bool )
   DEF-IMMEDIATE if DEF-RUN true exit then
   drop false ;

\ ---- `does>` in the body (habu2.f NCOMP-EMIT:CAPTURE-DOES) -------------------
\ `does>` ends a defining word's own part: DOESB takes the capture's length
\ with `does>` in it, where `;` splits the body, and the signature after it,
\ which must be there, opened and closed, is the effect of each word the
\ definer creates. The signature joins no capture: INP passes it and
\ created-sig! takes a copy of its inside at here, where it outlives the input
\ (C-PARSE-CREATED-SIG). A second `does>` refuses naming `does>` (C-DIE-DOES),
\ and a missing or open signature names the token as spelled (C-SIG-BAD).

: DEF-CREATED ( -- )
   false DEF-SIG-SPAN {: s:ptr end:ptr :}
   end INP-CELL ADDR!
   s 1 + end s - 2 - COPY-HERE DEF-CREATED-SIG ;

\ The pending parent is record NDICT. Its clause will be published under the
\ parent's name plus ;does in the same wordlist. Check the live records before
\ the signature copy or split changes any state, including a folded-case hit.
: DEF-DOES-NAME ( -- )
   ndict@ XREF-REC {: parent:ptr :}
   parent XREF-NAME$ {: stem:ptr stemu:n :}
   parent XREF-WORDLIST {: wid:n :}
   DOES-CLAUSE:SUFFIX$ {: suffix:ptr sufu:n :}
   ndict@ 0 ?do
      i XREF-REC {: row:ptr :}
      row XREF-WORDLIST wid = if
         row XREF-NAME$ {: name:ptr size:n :}
         size stemu sufu + = if
            name stemu stem stemu STR=CI
            name stemu + sufu suffix sufu STR=CI and if
               s" duplicate definition: " SAY
               stem stemu SAY suffix sufu SAY
               PKG-RC-DUPLICATE THROW-AT
            then
         then
      then
   loop ;

: DEF-DOES ( -- )
   DOESB-CELL CELL@ 0<> if s" does>" SAY RC-REJECT THROW-AT then
   DEF-DOES-NAME
   DEF-CREATED
   BODYLEN-CELL CELL@ DOESB-CELL CELL! ;

\ The engine reads `does>` as its keyword, with the token's A-Z folded, unless
\ LFIND finds a word of that spelling that is not immediate, which the body
\ calls instead.
: DEF-CALL? ( -- bool )
   TOKEN$ FIND-SCOPE {: rec:ptr :}
   rec XREF-FOUND? 0= if false exit then
   rec XREF-FLAGS DNAME-IMM and 0= ;

: DEF-DOES? ( -- bool )
   s" does>" TOKEN-IS? 0= if false exit then
   DEF-CALL? if false exit then
   DEF-DOES true ;

\ ---- the buffer's own definition (habu2.f C-DEF-SOURCE-CLOSE, C-DEF-SOURCE-END)
\ A definition closes in the buffer that opened it. ENTRY-PEND is the record
\ pending as the buffer OUTER:INTERPRET reads began (habu1.f EVAL-ENTER,
\ layout.f EVAL-FRAME:PEND); a pending record other than it is the buffer's
\ own. A throw out of the buffer, these refusals included, puts the entry
\ record back in PEND-CELL (OUTER:INTERPRET): the buffer's own definition is
\ abandoned, so no later `;` publishes it, and an outer one stays pending, as
\ habu2.f EM-EVAL-THROW-RECOVER leaves them. Unlike the engine's, this loop
\ puts no CP, NDICT or DP back (habu-roll-back-failed-64bf2ba5).
variable ENTRY-PEND

\ A `;` that would close the record pending as the buffer began, one an outer
\ buffer opened, is refused at the `;`, naming the definition.
: DEF-SOURCE-CLOSE ( -- )
   PEND-CELL CELL@ ENTRY-PEND @ <> if exit then
   s" hb: source closed a definition it did not open: " SAY
   DEF-CAPTURED-NAME SAY
   RC-UNCLOSED THROW-AT ;

\ At the buffer's end a record the buffer opened is refused.
: DEF-SOURCE-END ( -- )
   PEND-CELL CELL@ ENTRY-PEND @ = if exit then
   DEF-ENDED ;

\ An evaluate begun inside this native definition owns only its new tokens.
\ The checker reads the body prefix for binding, leaving the definition's
\ effect, open scopes and publication to its eventual `;`. A refusal lands in
\ evaluate's existing frame, whose token mark removes only the failed token.
TRUSTED: DEF-PREFIX-BIND ( -- )
   ENTRY-PEND @ 0= if exit then
   data-base BODYBUF-OFF + BYTE-VIEW
   BODYLEN-CELL CELL@ TOKBODY-CELL CELL@
   CHECKER-PREFIX:BIND? {: rollback:n ok:bool :}
   ok 0= if
      \ A parser operand can roll back its opener as well as the operand.
      \ The token mark must then restore the state before that opener.
      rollback TOKBODY-CELL CELL@ < if 0 TOK-BODY-RAW ! then
      rollback TOKBODY-CELL CELL! UNDEFINED
   then ;

\ ---- `;` (habu2.f NCOMP-EMIT:EM-COMPILE) --------------------------------------
\ `;`, the one byte, ends the definition and joins no capture. The body goes
\ to the compiler entry the AOT seed installed, as the engine's tier-1 `;`
\ hands it (NCOMP-EMIT:LOAD), checked again here since a body immediate can
\ have cleared it. The compiler checks, compiles and publishes the record, or
\ throws with the definition still pending, as the engine's does. On its
\ return def-close closes the provenance window native and clears what
\ def-open set.
: DEF-COMPILE ( ptr u8 n -- )
   DEF-DISPATCH NCOMP-DISPATCH:XT-CELL CELL@ DEF-AS-COMPILER execute ;

: DEF-SEMI? ( -- bool )
   s" ;" TOKEN-IS? 0= if false exit then
   DEF-SOURCE-CLOSE
   [: data-base BODYBUF-OFF + BYTE-VIEW BODYLEN-CELL CELL@ DEF-COMPILE ;] catch
   {: rc:n :}
   rc E-NCOMP-REPORTED = if DEF-ABORT true exit then
   rc 0<> if rc throw then
   DEF-CLOSE
   true ;

\ ---- the body ----------------------------------------------------------------
\ While a definition is pending every token is its body's (habu2.f EM-COMMENT):
\ tier 0's JIT compiles it, or tier 1 ends it at `;`, or captures the token,
\ runs it if it is a neutral immediate, splits the body at `does>`, or takes a
\ string keyword's text (NCOMP-EMIT:EM-COMPILE).
: COMPILING? ( -- bool )
   PEND-CELL CELL@ 0= if false exit then
   DEF-TIER-0? if BODY-POLICY DEF-JIT-TOKEN true exit then
   BODY-RAW-NEXT @ if
      0 BODY-RAW-NEXT !
      TOKEN$ DEF-CAPTURE  DEF-PREFIX-BIND  true exit
   then
   DEF-SEMI? if true exit then
   BODY-LOCAL? if
      TOKEN$ DEF-CAPTURE  DEF-PREFIX-BIND  true exit
   then
   BODY-POLICY
   TOKEN$ DEF-CAPTURE
   s" {:" TOKEN-IS? if BODY-LOCAL-GROUP DEF-PREFIX-BIND true exit then
   BODY-BLOCK-STEP
   DEF-IMMEDIATE? if true exit then
   DEF-DOES? if true exit then
   DEF-STRING-TEXT
   TOKEN$ SOURCE-SYNTAX:PARSING-KEYWORD? if -1 BODY-RAW-NEXT ! then
   DEF-PREFIX-BIND
   true ;

\ ---- identity declarers (habu2.f C-IDENTITY) ----------------------------------
\ `cast: NAME ( in -- out )` declares a checked retype: a record of kind
\ DKIND:CAST whose code is the identity, published at once, with no body and
\ no `;`. It refuses in the engine's order and with its text: a live task, the
\ room, the name, then the signature, which must be there, opened and closed,
\ and is read before the qualifier. With a check hook the checker proves the
\ retype legal (checker.f CAST-CERTIFY) before def-cast counts the record, so
\ a refused cast leaves none counted. The engine emits the code before it
\ asks; its refusal inside evaluate rolls the code back, which this loop
\ cannot do yet, so here the code waits for the checker.

\ With the input at its end the token cells still hold the keyword, which the
\ refusal names (C-CAST-DIE-NO-NAME).
: CAST-NAME ( -- )
   TOKEN if exit then
   s" hb: cast: missing name after " SAY TOKEN$ SAY RC-NO-NAME THROW-AT ;

: LINEAR-NAME ( -- )
   s" linear:" OPERAND ;

\ The owner record stores raw execution tokens; these views state the two
\ signatures called here.
TRUSTED: DEF-AS-CAST ( n -- [ ptr u8 n ptr u8 n -- ] ) ;
TRUSTED: DEF-AS-COUNT ( n -- [ -- n ] ) ;

\ The certifying operation gets the name and signature: the active owner's,
\ then the target owner's unless it is the same one (habu2.f DECL-OWNER).
: IDENTITY-REGISTER ( ptr u8 n n -- ) {: sa:ptr su:n off:n :}
   HOOK-CELL CELL@ 0= if exit then
   NCOMP-DISPATCH:DECL-CELL off PKG-OPERATION {: own:n :}
   own 0<> if DEF-CAPTURED-NAME sa su own DEF-AS-CAST execute then
   off PKG-TARGET {: target:n :}
   target 0= if exit then
   target own = if exit then
   DEF-CAPTURED-NAME sa su target DEF-AS-CAST execute ;

\ A checker word the engine finds by name in the global wordlist as it asks;
\ with none the process ends naming it (habu2.f C-FIND-GLOBAL).
: DEF-GLOBAL ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u 0 WL-PROBE {: rec:ptr :}
   rec XREF-FOUND? 0= if a u RC-REJECT FAIL-CLOSED then
   rec XREF-START ;

\ The checker's facts for the record just counted (habu2.f
\ EM-REC-WIDE-PUBLISH): rec-wide-publish marks it wide when its effect is,
\ and the certified minimum input arity rec-min-in@ gives is stored unless 0.
: IDENTITY-FACTS ( -- )
   HOOK-CELL CELL@ 0= if exit then
   s" rec-wide-publish" DEF-GLOBAL PKG-AS-ACTION execute
   s" rec-min-in@" DEF-GLOBAL DEF-AS-COUNT execute {: mi:n :}
   mi 0= if exit then
   ndict@ 1- mi DEF-MIN-IN ;

: CAST-HEAD ( -- )
   TASK-GUARD
   DEF-ROOM
   CAST-NAME
   0 BODYLEN-CELL CELL!
   TOKEN$ DEF-CAPTURE
   false DEF-SIG-SPAN {: s:ptr end:ptr :}
   end INP-CELL ADDR!
   s end s - DEF-CAPTURE
   DEF-QUALIFY DKIND:CAST 1 DEF-RECORD
   s 1 + end s - 2 - {: sa:ptr su:n :}
   sa su DEF-TRUST-SIG
   sa su NCOMP-DISPATCH:DECL-CAST-OFF IDENTITY-REGISTER
   DEF-CAST
   IDENTITY-FACTS ;

\ A qualified linear name is refused by the certifier before DEF-QUALIFY can
\ create its namespace. A second colon is left to DEF-QUALIFY's syntax refusal.
: LINEAR-QUALIFIED? ( -- bool )
   TOKEN$ {: a:ptr u:n :}
   a u 0 COLON-AT {: q:n :}
   q 0 <= q 1+ u >= or if false exit then
   a u q 1+ COLON-AT 0< ;

: LINEAR-HEAD ( -- )
   TASK-GUARD
   DEF-ROOM
   LINEAR-NAME
   0 BODYLEN-CELL CELL!
   TOKEN$ DEF-CAPTURE
   false DEF-SIG-SPAN {: s:ptr end:ptr :}
   end INP-CELL ADDR!
   s end s - DEF-CAPTURE
   s 1 + end s - 2 - {: sa:ptr su:n :}
   LINEAR-QUALIFIED? if
      sa su NCOMP-DISPATCH:DECL-LINEAR-OFF IDENTITY-REGISTER
      TOKEN$ SAY RC-REJECT THROW-AT
   then
   DEF-QUALIFY DKIND:CAST 1 DEF-RECORD
   sa su DEF-TRUST-SIG
   sa su NCOMP-DISPATCH:DECL-LINEAR-OFF IDENTITY-REGISTER
   DEF-CAST
   IDENTITY-FACTS ;

\ ---- the definers (habu2.f EMIT-CREATE, INTERP-EMIT C-CREATE, C-VARIABLE, C-CONSTANT)
\ A definer's word is whole when it is read. Its record opens with the stamp a
\ mention of the word folds to, DKIND:ADDR for a DATA address or DKIND:VAL for
\ a decided number, which also makes it the record `does-patch` reads (LASTC)
\ and NCOMP's at either tier, as the engine's definers are native at both;
\ NCOMP compiles and publishes the body that pushes that cell, and def-close
\ ends the definition. The engine's check hook then reads the name and the
\ keyword, and the word's effect is registered as raw storage: `-- ptr a` for
\ `create` and `variable`, `-- a` for `constant`, never a checked effect,
\ which could not state it (habu2.f LASTC-TRUST).

\ The head refuses while a task is live, then needs a name, naming kw as the
\ engine bakes it: `variable` is the engine's `create` with a cell allotted
\ after it. The capture is seeded with the name for the hook.
: DEF-FIXED-HEAD ( ptr u8 n -- ) {: kw:ptr u:n :}
   TASK-GUARD
   kw u OPERAND
   0 BODYLEN-CELL CELL!
   TOKEN$ DEF-CAPTURE ;

\ The owner record holds raw execution tokens; these views state the two
\ signatures called here: the check hook's, and that of a registrar taking a
\ name and a signature, trust-raw's and trust-decl's.
TRUSTED: DEF-AS-HOOK ( n -- [ ptr u8 n -- n ] ) ;
TRUSTED: DEF-AS-NAME-SIG ( n -- [ ptr u8 n ptr u8 n -- ] ) ;

\ The keyword joins the capture, and an armed check hook reads it, its verdict
\ dropped (habu2.f C-DEFHOOK).
: DEF-HOOK ( ptr u8 n -- )
   DEF-CAPTURE
   HOOK-CELL CELL@ {: xt:n :}
   xt 0= if exit then
   data-base BODYBUF-OFF + BYTE-VIEW BODYLEN-CELL CELL@ xt DEF-AS-HOOK execute drop ;

\ The active checker's trust-raw; without one, with the check hook armed, the
\ target checker's, and with neither the process ends naming it, as a word
\ published unsealed would be (habu2.f LASTC-TRUST:FIND-ACTIVE, FIND-RAW). 0:
\ no checker is armed and nothing is registered.
: DEF-RAW-FIRST ( n -- n ) {: own:n :}
   own 0<> if own exit then
   HOOK-CELL CELL@ 0= if 0 exit then
   NCOMP-DISPATCH:DECL-RAW-OFF PKG-TARGET {: target:n :}
   target 0= if s" trust-raw" RC-REJECT FAIL-CLOSED then
   target ;

\ The name the capture holds and the raw signature sig, to the first
\ registrar, then to the target checker's unless the active owner's field
\ holds the same operation (LASTC-TRUST:FIND-TARGET, DECL-OWNER:SKIP-SAME).
: DEF-RAW ( ptr u8 n -- ) {: sig:ptr su:n :}
   NCOMP-DISPATCH:DECL-CELL NCOMP-DISPATCH:DECL-RAW-OFF PKG-OPERATION {: own:n :}
   own DEF-RAW-FIRST {: first:n :}
   first 0= if exit then
   DEF-CAPTURED-NAME sig su first DEF-AS-NAME-SIG execute
   NCOMP-DISPATCH:DECL-CELL CELL@ 0= if exit then
   NCOMP-DISPATCH:DECL-RAW-OFF PKG-TARGET {: target:n :}
   target 0= if exit then
   target own = if exit then
   DEF-CAPTURED-NAME sig su target DEF-AS-NAME-SIG execute ;

\ The DATA address a created word's body pushes, as the number NCOMP takes.
TRUSTED: DEF-HERE ( -- n ) here ;

\ `create` rounds the data field up to a cell, as the engine's create does,
\ before the body takes its address.
: DEF-CREATE-ROW ( -- )
   s" create" DEF-FIXED-HEAD
   DEF-QUALIFY DKIND:ADDR 1 DEF-RECORD
   align
   DEF-HERE NDICT:FIXED-ADDR NCOMP:COMPILE-FIXED
   DEF-CLOSE ;

\ A compiled defining word reaches CREATE through the engine primitive. ARM's
\ BCREATE calls the same record writer without the outer keyword's hook and
\ raw-effect registration; those belong to the interpreting CREATE only.
: DEF-CREATE ( -- )
   DEF-CREATE-ROW
   s" create" DEF-HOOK
   s" -- ptr a" DEF-RAW ;

: DEF-VARIABLE ( -- )
   DEF-CREATE
   1 cells allot ;

\ `constant` takes the program's top cell after its record opens, where the
\ engine's pops it. With none the engine reads below its stack and faults
\ (rc 102); this loop names the underflow, as after a word.
variable DEF-VALUE

: DEF-CONSTANT ( -- )
   s" constant" DEF-FIXED-HEAD
   DEF-QUALIFY DKIND:VAL 1 DEF-RECORD
   depth 0= if s" E-UNDERFLOW: " REFUSE then
   [: DEF-VALUE ! ;] TAKE-N
   DEF-VALUE @ NDICT:FIXED-VAL NCOMP:COMPILE-FIXED
   DEF-CLOSE
   s" constant" DEF-HOOK
   s" -- a" DEF-RAW ;

\ ---- `defer` (habu2.f C-DEFER) ------------------------------------------------
\ `defer NAME ( in -- out )` declares a word that calls the execution token in
\ its dispatch cell, an aligned DATA cell that starts with `defer-unset`'s and
\ that `is` in a body re-points. It refuses in the engine's order and with its
\ text: a live task, the room, the name, then the signature, which must be
\ there, opened and closed, then `defer-unset`, whose cell is allotted before
\ the qualifier, as the engine's is. The checker gets the effect before NCOMP
\ compiles the body, which asks it whether the defer returns; NPUB lays the
\ trailer after the routine and def-close ends the definition. The engine
\ registers after it publishes, which nothing observes: trust-decl never
\ consults the dictionary (habu2.f DEF-TRUST).

\ With the input at its end the token cells still hold the keyword, which the
\ refusal names (DEFER-DIAG:DIE-NO-NAME, rc $4A).
: DEFER-NAME ( -- )
   TOKEN if exit then
   s" hb: defer: missing name after " SAY TOKEN$ SAY RC-NO-NAME THROW-AT ;

\ `defer-unset`'s execution token, found as LFIND finds it; with none the
\ refusal is the bare name token (C-DEFER-FIND-UNSET, rc $46).
: DEFER-UNSET-XT ( -- n )
   s" defer-unset" FIND-SCOPE {: rec:ptr :}
   rec XREF-FOUND? 0= if TOKEN$ SAY RC-REJECT THROW-AT then
   rec XREF-START ;

\ The cell holds an execution token, so xt! declares it one, as the engine's
\ C-DEFER-CELL marks it: a snapshot moves it with the code region.
TRUSTED: DEF-XT! ( n n -- ) xt! ;

: DEFER-CELL ( -- n )
   DEFER-UNSET-XT {: xt:n :}
   align
   DEF-HERE {: cell:n :}
   1 cells allot
   xt cell DEF-XT!
   cell ;

\ ---- the pre-trust hold (habu2.f C-PRETRUST-READY?, C-PD-CAPTURE) ------------
\ A target owner without both trust-decl and checker-defer cannot register a
\ defer yet: a build's src/core/checker.f declares defers before `: TRUST`, and
\ the build reads it through this loop. The defer's name and signature wait in
\ the next pending slot (src/habu/layout.f PD-*) until checker.f's
\ DRAIN-PRETRUST replays both registrations to the target owner.
\ With both owner cells zero the engine's loop still holds the defer, while
\ NCOMP below refuses it ("ncomp: cannot compile"): no checker knows its
\ effect. No build reaches that state: native-runtime.f BIND-OWNER sets both
\ cells at boot, and checker.f CHECKER-RESET-SOURCE zeroes only the target's.
: DEFER-READY? ( -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-OFF PKG-TARGET 0<>
   NCOMP-DISPATCH:DECL-DEFER-OFF PKG-TARGET 0<> and ;

\ A full table, or a name or signature past its slot field, ends the process
\ naming the defer.
: PD-FULL ( -- )
   s" hb: pre-trust defer table full: " SAY TOKEN$ SAY
   NL 1 DEF-RC-PD-FULL FAIL-CLOSED ;

\ The u bytes at a into the slot field at DATA offset off, their count into
\ the cell at lenoff.
: PD-PUT ( ptr u8 n n n n -- ) {: a:ptr u:n cap:n lenoff:n off:n :}
   u cap > if PD-FULL then
   u lenoff CELL!
   a data-base off + BYTE-VIEW u BYTE-COPY ;

: PD-HOLD ( ptr u8 n -- ) {: sig:ptr su:n :}
   PD-TABLE-OFF CELL@ {: count:n :}
   count PD-CAP >= if PD-FULL then
   PD-TABLE-OFF PD-SLOTS-REL + count PD-SLOT * + {: slot:n :}
   DEF-CAPTURED-NAME PD-NAME-CAP slot PD-NLEN-OFF + slot PD-NAME-OFF + PD-PUT
   sig su PD-SIG-CAP slot PD-SLEN-OFF + slot PD-SIG-OFF + PD-PUT
   count 1 + PD-TABLE-OFF CELL! ;

\ The active owner's operation at off, then the target owner's, 0 when the
\ active owner's field holds the same one (DECL-OWNER:FIND, SKIP-SAME).
: DEF-OWNERS ( n -- n n ) {: off:n :}
   NCOMP-DISPATCH:DECL-CELL off PKG-OPERATION {: own:n :}
   off PKG-TARGET {: target:n :}
   own  target own = if 0 else target then ;

\ The name the capture holds and signature sig to each trust-decl
\ (DEF-TRUST:REGISTER), then the name to each checker-defer
\ (C-CALL-CHECKER-DEFER). Before the target owner is ready only the active
\ owner's two are called, and the pending slot holds the defer for the
\ target's.
: DEFER-REGISTER ( ptr u8 n -- ) {: sig:ptr su:n :}
   DEFER-READY? {: ready:bool :}
   NCOMP-DISPATCH:DECL-EFFECT-OFF DEF-OWNERS {: own:n target:n :}
   own 0<> if DEF-CAPTURED-NAME sig su own DEF-AS-NAME-SIG execute then
   ready target 0<> and if DEF-CAPTURED-NAME sig su target DEF-AS-NAME-SIG execute then
   NCOMP-DISPATCH:DECL-DEFER-OFF DEF-OWNERS {: down:n dtarget:n :}
   down 0<> if DEF-CAPTURED-NAME down PKG-AS-NAME-ACTION execute then
   ready dtarget 0<> and if DEF-CAPTURED-NAME dtarget PKG-AS-NAME-ACTION execute then
   ready 0= if sig su PD-HOLD then ;

: DEF-DEFER ( -- )
   TASK-GUARD
   DEF-ROOM
   DEFER-NAME
   0 BODYLEN-CELL CELL!
   TOKEN$ DEF-CAPTURE
   false DEF-SIG-SPAN {: s:ptr end:ptr :}
   end INP-CELL ADDR!
   s end s - DEF-CAPTURE
   DEFER-CELL {: cell:n :}
   DEF-QUALIFY 0 1 DEF-RECORD
   s 1 + end s - 2 - {: sa:ptr su:n :}
   sa su DEF-TRUST-SIG
   sa su DEFER-REGISTER
   cell NCOMP:FIXED-DEFER NCOMP:COMPILE-FIXED
   DEF-CLOSE ;

\ ---- the definition keywords --------------------------------------------------
\ The engine's `:` is the one byte, `kernel:` its synonym; `trusted:`, `cast:`, `linear:`,
\ `immediate` and the definers are matched as LITERAL? matches its keywords.
\ `immediate` marks the newest record, whatever it is, and refuses nothing, as
\ the engine's does (habu2.f C-IMMEDIATE).
: DEFINE? ( -- bool )
   s" :" TOKEN-IS? if false DEF-HEAD true exit then
   s" kernel:" TOKEN-IS? if false DEF-HEAD true exit then
   s" trusted:" TOKEN-IS? if true DEF-HEAD true exit then
   s" cast:" TOKEN-IS? if CAST-HEAD true exit then
   s" linear:" TOKEN-IS? if LINEAR-HEAD true exit then
   s" immediate" TOKEN-IS? if DEF-IMM-MARK true exit then
   s" create" TOKEN-IS? if DEF-CREATE true exit then
   s" variable" TOKEN-IS? if DEF-VARIABLE true exit then
   s" constant" TOKEN-IS? if DEF-CONSTANT true exit then
   s" defer" TOKEN-IS? if DEF-DEFER true exit then
   false ;

public

: INSTALL-CREATE ( -- )
   ['] DEF-CREATE-ROW data-base CREATEP-CELL + xt! ;

;package
