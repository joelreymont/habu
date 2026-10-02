\ definers.f - the definition heads of the interpret loop written in Habu:
\ `:`, `kernel:` and `trusted:`, as the engine's EM-INTERPRET-COLON and
\ C-TRUSTED (habu2.f) open a definition, and the capture of the body that
\ follows one; `cast:`, which declares and publishes at once, as C-CAST does;
\ and `immediate`. STEP (src/habu/interpret.f) asks COMPILING? after a comment
\ and DEFINE? after the package keywords.
\
\ A head opens record NDICT through def-open, unpublished, and the definition
\ stays pending: each body token is captured into BODYBUF, as the engine's
\ tier 1 captures it, a stack-neutral parsing immediate among them runs as it
\ is read, `does>` splits the body and takes the signature of the words it
\ creates, and `;` compiles the body whole and ends the definition through
\ def-close. Only tier 1 is read here. The engine's tier 0 compiles each token
\ as it reads it (its JIT), so at tier 0 a head or a body token is refused.
\
\ The head refuses in the engine's order and with its text. A definition
\ writer's own refusal exits without a word (src/habu/prims.f), so every
\ refusal the engine reports is made here first. Unlike the engine's head,
\ this one leaves no PROT window open: def-open closes its own.

require lib/prelude.f
require src/core/checker.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/dict.f
require src/habu/outer.f
require src/habu/packages.f

package OUTER

private

76 constant DEF-RC-TIER-0        \ the code of the engine head's first refusal, a pass-2 nesting
76 constant DEF-RC-BAD-SIG       \ habu2.f C-SIG-BAD: `trusted:` or `does>` with no signature
71 constant DEF-RC-BODY-FULL     \ habu2.f EM-BODY-CAP-DIE: the body capture is full

\ ---- the definition writers, each a TRUSTED: boundary ------------------------
TRUSTED: DEF-OPEN ( ptr u8 n n n -- ) def-open ;
TRUSTED: DEF-APPEND ( ptr u8 n -- ) body-append ;
TRUSTED: DEF-TRUST-SIG ( ptr u8 n -- ) trust-sig! ;
TRUSTED: DEF-CREATED-SIG ( ptr u8 n -- ) created-sig! ;
TRUSTED: DEF-CLOSE ( -- ) def-close ;
TRUSTED: DEF-IMM-MARK ( -- ) imm-mark ;
TRUSTED: DEF-CAST ( -- ) def-cast ;
TRUSTED: DEF-MIN-IN ( n n -- ) min-in-mark ;

\ ---- tier 0 ------------------------------------------------------------------
\ The engine's tier 0 compiles each body token as it reads it, with its JIT,
\ which this loop cannot call yet (habu-hook-tier-0-96e33c29): a head or a body
\ token at tier 0 refuses, naming the token, where the engine's head makes its
\ first refusal.
: DEF-TIER-0 ( -- )
   s" hb: tier 0 is not in the Habu loop: " SAY
   DEF-RC-TIER-0 PKG-FAIL ;

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
   a q XREF-NAMESPACE-WL FIND-PROBE {: row:ptr :}
   row XREF-FOUND? if row XREF-PKG-PUBLIC exit then
   PKG-DICT-ROOM
   a q PKG-CODE-ROOM
   a q false PKG-NS-RECORD XREF-REC XREF-PKG-PUBLIC ;

\ The tail the record is named and the wordlist it goes into. DEF-TKA and
\ DEF-TKL take the name token, which the refusals after them name, as TKA and
\ TKL still do: the engine moves those to the tail and back, and this head
\ names the tail where the engine names it. A bare name goes into the current
\ wordlist; the qualifier is the first colon at neither edge, as FIND-SPLIT
\ reads it, and a second colon refuses.
: DEF-QUALIFY ( -- ptr u8 n n )
   SEAL-GUARD
   TKA-CELL CELL@ DEF-TKA-CELL CELL!
   TKL-CELL CELL@ DEF-TKL-CELL CELL!
   TOKEN$ {: a:ptr u:n :}
   a u FIND-SPLIT {: q:n :}
   q FIND-BAD = if PKG-RC-CONTEXT PKG-FAIL then
   q FIND-BARE = if a u get-current exit then
   a q 1+ ZPTR+  u q - 1-  a q DEF-NAMESPACE ;

\ ---- the compile-keyword wall (habu2.f EMIT-DEF-KW-GUARD) ---------------------
\ The keywords a body's compiler reads before it looks a word up
\ (EM-COMPILE-KEYWORDS): a word named by one could never be called from a body.
: DEF-KEYWORDS ( -- ptr u8 n )
   S\" if then else begin until again while repeat case of endof endcase construct match s\q c\q .\q s\\\q c\\\q .\\\q ['] [char] does> [: is ;] do loop i >r r> r@ exit recurse ?do +loop j leave unloop {:" ;

\ The index past the keyword that starts at s in the table.
: DEF-KEYWORD-END ( ptr u8 n n -- n ) {: t:ptr tu:n s:n :}
   s begin dup tu < if t over + c@ BLANK <> else false then while 1+ repeat ;

: DEF-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   DEF-KEYWORDS {: t:ptr tu:n :}
   0 begin dup tu < while
      {: s:n :}
      t tu s DEF-KEYWORD-END {: e:n :}
      a u  t s ZPTR+ e s -  FOLDED= if true exit then
      e 1+
   repeat
   drop false ;

\ Matched as the engine matches its keywords, with only the tail's A-Z folded.
: DEF-WALL ( ptr u8 n -- ) {: a:ptr u:n :}
   a u DEF-KEYWORD? 0= if exit then
   s" hb: compile keyword cannot be a definition name: " SAY
   a u SAY RC-REJECT THROW-AT ;

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
\ token: where it starts and the byte past it.
: DEF-SIG-SPAN ( -- ptr u8 ptr u8 )
   DEF-SIG-START {: s:ptr open:bool :}
   open 0= if DEF-RC-BAD-SIG PKG-FAIL then
   s DEF-SIG-END {: end:ptr closed:bool :}
   closed 0= if DEF-RC-BAD-SIG PKG-FAIL then
   s end ;

\ `trusted:` needs one, and the refusal names the definition.
: DEF-REQUIRED-SIG ( -- )
   DEF-SIG-SPAN DEF-SIG-TAKE ;

\ ---- the head ----------------------------------------------------------------
\ Tier 1 compiles through the entry the AOT seed installed; with none the
\ boot is broken, and the process ends (habu2.f NCOMP-EMIT:LOAD).
: DEF-DISPATCH ( -- )
   NCOMP-DISPATCH:XT-CELL CELL@ 0<> if exit then
   S\" hb: native compiler dispatch unset\n" ENGINE-ERROR:AOT-SEED FAIL-CLOSED ;

\ The tail and its wordlist pass the wall, the room, the duplicate test and the
\ seal's protected wordlists, then the name's code room, before def-open
\ writes anything (habu2.f EMIT-QUALIFY-DEF, EMIT-STORE-DEF-NAME), then opens
\ the record with its kind. The refusals name the token, but the wall and the
\ code room name the tail.
: DEF-RECORD ( ptr u8 n n n -- ) {: a:ptr u:n wid:n kind:n :}
   a u DEF-WALL
   PKG-DICT-ROOM
   a u wid FIND-PROBE XREF-FOUND? if
      s" duplicate definition: " SAY PKG-RC-DUPLICATE PKG-FAIL
   then
   wid PKG-OPEN-WID
   a u PKG-CODE-ROOM
   a u wid kind DEF-OPEN ;

\ The name opens a pending record with the capture it seeds. `trusted:` then
\ sets the trusted cell and needs a signature; `:` takes one if it is there.
: DEF-HEAD ( bool -- ) {: trusted:bool :}
   NCOMP-DISPATCH:TIER-CELL CELL@ 0= if DEF-TIER-0 then
   TASK-GUARD
   DEF-ROOM
   trusted DEF-NAME
   0 BODYLEN-CELL CELL!
   TOKEN$ DEF-CAPTURE
   DEF-QUALIFY 0 DEF-RECORD
   trusted if
      1 TRUSTED-CELL CELL!
      DEF-REQUIRED-SIG
   else
      DEF-MAYBE-SIG
   then
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
   rec XREF-START  TOKEN$ NEUTRAL-PARSE-IMM? ;

\ An armed checker's preflight gets the body so far, the token and the trusted
\ cell first; one armed without a preflight is refused (habu2.f
\ C-CALL-COMPILE-IMMEDIATE, LPREFMISS).
TRUSTED: DEF-PREFLIGHT ( -- )
   HOOK-CELL CELL@ 0= if exit then
   COMPILE-PREFLIGHT-CELL CELL@ 0= if
      S\" hb: compile preflight hook missing\n" SAY RC-REJECT throw
   then
   data-base BODYBUF-OFF + BODYLEN-CELL CELL@ TOKEN$ TRUSTED-CELL CELL@
   COMPILE-PREFLIGHT-CELL CELL@ execute ;

\ The xt waits on the return stack while the preflight runs, and the stack's
\ floor holds after the word, as after a word the loop runs.
TRUSTED: DEF-RUN ( n -- )
   >r DEF-PREFLIGHT r> execute-floor FLOORED ;

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
   DEF-SIG-SPAN {: s:ptr end:ptr :}
   end INP-CELL ADDR!
   s 1 + end s - 2 - COPY-HERE DEF-CREATED-SIG ;

: DEF-DOES ( -- )
   DOESB-CELL CELL@ 0<> if s" does>" SAY RC-REJECT THROW-AT then
   BODYLEN-CELL CELL@ DOESB-CELL CELL!
   DEF-CREATED ;

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

\ ---- `;` (habu2.f NCOMP-EMIT:EM-COMPILE) --------------------------------------
\ `;`, the one byte, ends the definition and joins no capture. The body goes
\ to the compiler entry the AOT seed installed, as the engine's tier-1 `;`
\ hands it (NCOMP-EMIT:LOAD), checked again here since a body immediate can
\ have cleared it. The compiler checks, compiles and publishes the record, or
\ throws with the definition still pending, as the engine's does. On its
\ return def-close closes the provenance window native and clears what
\ def-open set.
TRUSTED: DEF-COMPILE ( ptr u8 n -- )
   DEF-DISPATCH NCOMP-DISPATCH:XT-CELL CELL@ execute ;

: DEF-SEMI? ( -- bool )
   s" ;" TOKEN-IS? 0= if false exit then
   data-base BODYBUF-OFF + BYTE-VIEW BODYLEN-CELL CELL@ DEF-COMPILE
   DEF-CLOSE
   true ;

\ ---- the body ----------------------------------------------------------------
\ While a definition is pending every token is its body's (habu2.f EM-COMMENT):
\ tier 1 ends it at `;`, or captures the token, runs it if it is a neutral
\ immediate, splits the body at `does>`, or takes a string keyword's text
\ (NCOMP-EMIT:EM-COMPILE).
: COMPILING? ( -- bool )
   PEND-CELL CELL@ 0= if false exit then
   NCOMP-DISPATCH:DEF-TIER-CELL CELL@ 0= if DEF-TIER-0 then
   DEF-SEMI? if true exit then
   TOKEN$ DEF-CAPTURE
   DEF-IMMEDIATE? if true exit then
   DEF-DOES? if true exit then
   DEF-STRING-TEXT
   true ;

\ ---- `cast:` (habu2.f C-CAST) --------------------------------------------------
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

\ The owner record stores raw execution tokens; these views state the two
\ signatures called here.
TRUSTED: DEF-AS-CAST ( n -- [ ptr u8 n ptr u8 n -- ] ) ;
TRUSTED: DEF-AS-COUNT ( n -- [ -- n ] ) ;

\ The checker's cast operation gets the name and the signature: the active
\ owner's, then the target owner's unless it is the same one (habu2.f
\ DEF-TRUST:REGISTER-CAST, DECL-OWNER). Either may throw the cast's refusal.
: CAST-REGISTER ( ptr u8 n -- ) {: sa:ptr su:n :}
   HOOK-CELL CELL@ 0= if exit then
   NCOMP-DISPATCH:DECL-CELL NCOMP-DISPATCH:DECL-CAST-OFF PKG-OPERATION {: own:n :}
   own 0<> if DEF-CAPTURED-NAME sa su own DEF-AS-CAST execute then
   NCOMP-DISPATCH:DECL-CAST-OFF PKG-TARGET {: target:n :}
   target 0= if exit then
   target own = if exit then
   DEF-CAPTURED-NAME sa su target DEF-AS-CAST execute ;

\ A checker word the engine finds by name in the global wordlist as it asks;
\ with none the process ends naming it (habu2.f C-FIND-GLOBAL).
: DEF-GLOBAL ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u 0 FIND-PROBE {: rec:ptr :}
   rec XREF-FOUND? 0= if a u RC-REJECT FAIL-CLOSED then
   rec XREF-START ;

\ The checker's facts for the record just counted (habu2.f
\ EM-REC-WIDE-PUBLISH): rec-wide-publish marks it wide when its effect is,
\ and the certified minimum input arity rec-min-in@ gives is stored unless 0.
: CAST-FACTS ( -- )
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
   DEF-SIG-SPAN {: s:ptr end:ptr :}
   end INP-CELL ADDR!
   s end s - DEF-CAPTURE
   DEF-QUALIFY DKIND:CAST DEF-RECORD
   s 1 + end s - 2 - {: sa:ptr su:n :}
   sa su DEF-TRUST-SIG
   sa su CAST-REGISTER
   DEF-CAST
   CAST-FACTS ;

\ ---- the definition keywords --------------------------------------------------
\ The engine's `:` is the one byte, `kernel:` its synonym; `trusted:`, `cast:`
\ and `immediate` are matched as LITERAL? matches its keywords. `immediate`
\ marks the newest record, whatever it is, and refuses nothing, as the
\ engine's does (habu2.f C-IMMEDIATE).
: DEFINE? ( -- bool )
   s" :" TOKEN-IS? if false DEF-HEAD true exit then
   s" kernel:" TOKEN-IS? if false DEF-HEAD true exit then
   s" trusted:" TOKEN-IS? if true DEF-HEAD true exit then
   s" cast:" TOKEN-IS? if CAST-HEAD true exit then
   s" immediate" TOKEN-IS? if DEF-IMM-MARK true exit then
   false ;

;package
