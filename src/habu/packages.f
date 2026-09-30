\ packages.f - the package keywords of the interpret loop written in Habu:
\ `package`, `public`, `private`, `;package`, `using`, `;using` and `export`,
\ as the engine's C-PACKAGE to C-EXPORT (habu2.f) read them. STEP
\ (src/habu/interpret.f) asks PACKAGE? after the literal keywords.
\
\ The dictionary and the package cells are sealed state: after the seal a
\ store into the friend arena (CUR, WIDN, DEF-WL, PKG-*) exits 83. A namespace
\ row and its wids come from namespace-record and namespace-private, the scope
\ from package-scope!, an alias from alias-record (src/habu/prims.f, the
\ definition writers) and CUR from set-current. The using band and DEF-TKA and
\ DEF-TKL are not sealed and are written here directly. A writer's own refusal
\ exits without a word, so every refusal the engine reports is made here
\ first, in the engine's order and with its text.

require lib/prelude.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/dict.f
require src/habu/outer.f

package OUTER

private

\ The engine's codes for these refusals (habu2.f): $4B a keyword out of its
\ context or a colon in a package name (C-PACKAGE-FAIL), $4C a long name past
\ the code ceiling (C-DIE-CODE-FULL), $4D no record slot left (C-QUALIFY-CAP)
\ and $4E a tail the current wordlist already holds (C-DUP-DEF-FAIL).
75 constant PKG-RC-CONTEXT
76 constant PKG-RC-CODE-FULL
77 constant PKG-RC-DICT-FULL
78 constant PKG-RC-DUPLICATE

\ The code ceiling's distance below the end of the region (habu2.f
\ DEFWRITE:NAME-ROOM).
$4000 constant PKG-CODE-RESERVE

\ ---- the definition writers, each a TRUSTED: boundary ------------------------
TRUSTED: PKG-NS-RECORD ( ptr u8 n bool -- n ) namespace-record ;
TRUSTED: PKG-NS-PRIVATE ( n -- ) namespace-private ;
TRUSTED: PKG-SCOPE! ( n n -- ) package-scope! ;
TRUSTED: PKG-ALIAS ( ptr u8 n n n -- ) alias-record ;

\ A record's index: records sit DREC bytes apart from dbase@.
TRUSTED: PKG-INDEX ( ptr n -- n ) dbase@ - DREC / ;

\ ---- the refusals ------------------------------------------------------------
\ The token, then the engine's compile-die tail (habu2.f C-PACKAGE-FAIL).
: PKG-FAIL ( n -- )
   TOKEN$ SAY THROW-AT ;

\ The keyword's operand. With the input at its end the token cells still hold
\ the keyword, which the refusal names.
: PKG-NAME ( -- )
   TOKEN if exit then
   RC-NO-NAME PKG-FAIL ;

\ No record slot left (habu2.f C-QUALIFY-CAP). The refusal names what DEF-TKA
\ and DEF-TKL hold, which `package` never writes: for it, what the last
\ definition or `export` left there.
: PKG-DICT-ROOM ( -- )
   ndict@ DICT-CAP < if exit then
   s" hb: dictionary full at: " SAY
   DEF-TKA-CELL ADDR@ DEF-TKL-CELL CELL@ SAY
   PKG-RC-DICT-FULL THROW-AT ;

\ A name past DNAME-INL bytes is copied to CP, 4-aligned, below the code
\ ceiling (habu2.f DEFWRITE:NAME-ROOM). The comparisons are unsigned, so a CP
\ already at the ceiling refuses.
: PKG-FITS? ( n -- bool ) {: u:n :}
   u DNAME-INL U> 0= if true exit then
   dbase@ REGION + PKG-CODE-RESERVE - {: ceil:n :}
   ceil cp@ U> 0= if false exit then
   ceil cp@ - {: room:n :}
   room u U>  room u 3 + -4 and U>  and ;

: PKG-CODE-ROOM ( ptr u8 n -- ) {: a:ptr u:n :}
   u PKG-FITS? if exit then
   s" hb: code space full at: " SAY
   a u SAY PKG-RC-CODE-FULL THROW-AT ;

\ ---- the checker's declaration operations (habu2.f DECL-OWNER) ---------------
\ The operation at off in the owner record a dispatch cell names, 0 when there
\ is no record or the field is empty: the engine then skips the call, where
\ src/compiler/native/checker-owner.f FIELD would refuse.
: PKG-OPERATION ( n n -- n ) {: cell:n off:n :}
   data-base cell + 0 ptr-field @ {: rec:ptr :}
   rec 0= if 0 exit then
   rec off + CELL-VIEW @ ;

\ The owner record stores raw execution tokens; these views state the two
\ signatures called here.
TRUSTED: PKG-AS-ACTION ( n -- [ -- ] ) ;
TRUSTED: PKG-AS-NAME-ACTION ( n -- [ ptr u8 n -- ] ) ;

\ `package`, `public`, `private` and `;package` notify the target checker
\ alone (habu2.f C-CALL-CHECKER-PACKAGE and its siblings).
: PKG-TARGET ( n -- n )
   NCOMP-DISPATCH:TARGET-DECL-CELL swap PKG-OPERATION ;

: PKG-NOTIFY ( n -- )
   PKG-TARGET {: xt:n :}
   xt 0= if exit then
   xt PKG-AS-ACTION execute ;

: PKG-NOTIFY-NAME ( n -- )
   PKG-TARGET {: xt:n :}
   xt 0= if exit then
   TOKEN$ xt PKG-AS-NAME-ACTION execute ;

\ `using` notifies the active checker, then the target checker unless its
\ operation is the same one (habu2.f C-CALL-CHECKER-USING). Each records the
\ name at the using depth before it grows.
: PKG-NOTIFY-USING ( -- )
   NCOMP-DISPATCH:DECL-CELL NCOMP-DISPATCH:DECL-USING-OFF PKG-OPERATION
   {: own:n :}
   own 0<> if TOKEN$ own PKG-AS-NAME-ACTION execute then
   NCOMP-DISPATCH:DECL-USING-OFF PKG-TARGET {: target:n :}
   target 0= if exit then
   target own = if exit then
   TOKEN$ target PKG-AS-NAME-ACTION execute ;

\ `export` asks the checker by name instead (habu2.f C-CALL-CHECKER-EXPORT):
\ checker-export in the global wordlist, and without one the process ends
\ with that name.
: PKG-CHECK-EXPORT ( -- )
   s" checker-export" 0 FIND-PROBE {: rec:ptr :}
   rec XREF-FOUND? 0= if s" checker-export" RC-REJECT FAIL-CLOSED then
   TOKEN$ rec XREF-START PKG-AS-NAME-ACTION execute ;

\ ---- package, public, private, ;package (habu2.f C-PACKAGE to C-END-PACKAGE) -
\ The namespace row the token names, or XREF-NULL.
: PKG-ROW ( -- ptr n )
   TOKEN$ XREF-NAMESPACE-WL FIND-PROBE ;

\ Once the engine is sealed, a sealed package's name ends the process, and so
\ does a package whose public wordlist is protected: the token is the whole
\ diagnostic (habu2.f C-PACKAGE-SEAL-GUARD).
: PKG-SEALED? ( -- bool )
   TOKEN$ CHECKER-SEALED-PKG? if true exit then
   PKG-ROW {: row:ptr :}
   row XREF-FOUND? 0= if false exit then
   row XREF-PKG-PUBLIC PROTECTED? ;

: PKG-SEAL-GUARD ( -- )
   SEAL-NDICT@ 0= if exit then
   PKG-SEALED? 0= if exit then
   TOKEN$ ENGINE-ERROR:SEAL-PACKAGE FAIL-CLOSED ;

\ An existing row keeps its wids and gains a private one only when it has none.
: PKG-REOPEN ( ptr n -- n ) {: row:ptr :}
   row PKG-INDEX {: ix:n :}
   row XREF-PKG-PRIVATE 0= if ix PKG-NS-PRIVATE then
   ix ;

\ The index of the row `package` opens (habu2.f C-PACKAGE-ENSURE). A colon
\ anywhere in the name is refused; a new row takes a public and a private wid.
: PKG-ENSURE ( -- n )
   TOKEN$ 0 FIND-COLON 0 >= if PKG-RC-CONTEXT PKG-FAIL then
   PKG-ROW {: row:ptr :}
   row XREF-FOUND? if row PKG-REOPEN exit then
   PKG-DICT-ROOM
   TOKEN$ PKG-CODE-ROOM
   TOKEN$ true PKG-NS-RECORD ;

\ The package opens on its private wordlist. The using depth it opens at is
\ the one `;package` restores.
: PKG-PACKAGE ( -- )
   TASK-GUARD
   PKG-PUB-CELL CELL@ 0<> if PKG-RC-CONTEXT PKG-FAIL then
   PKG-NAME
   NCOMP-DISPATCH:DECL-PACKAGE-OFF PKG-NOTIFY-NAME
   PKG-SEAL-GUARD
   PKG-ENSURE {: ix:n :}
   USE-DEPTH-CELL CELL@ USE-PKG-SAVE-CELL CELL!
   ix get-current PKG-SCOPE!
   PKG-PRI-CELL CELL@ set-current ;

\ `public` and `private` select a wordlist of the open package.
: PKG-SECTION ( n n -- ) {: cell:n off:n :}
   TASK-GUARD
   cell CELL@ 0= if PKG-RC-CONTEXT PKG-FAIL then
   off PKG-NOTIFY
   cell CELL@ set-current ;

: PKG-END-PACKAGE ( -- )
   TASK-GUARD
   PKG-PUB-CELL CELL@ 0= if PKG-RC-CONTEXT PKG-FAIL then
   NCOMP-DISPATCH:DECL-END-PACKAGE-OFF PKG-NOTIFY
   PKG-PARENT-CELL CELL@ set-current
   -1 0 PKG-SCOPE!
   USE-PKG-SAVE-CELL CELL@ USE-DEPTH-CELL CELL! ;

\ ---- using, ;using (habu2.f C-USING, C-END-USING) ----------------------------
\ A refusal's message, the token and the compile-die tail.
: PKG-USING-FAIL ( ptr u8 n n -- ) {: a:ptr u:n rc:n :}
   a u SAY TOKEN$ SAY rc THROW-AT ;

\ The public wordlist of the package the operand names.
: PKG-USED-WID ( -- n )
   TOKEN 0= if
      s" hb: using: missing package name" SAY
      ENGINE-ERROR:USING-NO-NAME THROW-AT
   then
   TOKEN$ 0 FIND-COLON 0 >= if
      s" hb: using: package name must not contain ':': "
      ENGINE-ERROR:USING-BAD-NAME PKG-USING-FAIL
   then
   PKG-ROW {: row:ptr :}
   row XREF-FOUND? 0= if
      s" hb: using: unknown package: " ENGINE-ERROR:USING-UNKNOWN PKG-USING-FAIL
   then
   row XREF-PKG-PUBLIC ;

\ The wid joins the used publics at the current depth; the checkers record the
\ name at that depth before it grows.
: PKG-USING ( -- )
   TASK-GUARD
   PKG-USED-WID {: wid:n :}
   FIND-USE-DEPTH {: d:n :}
   d USE-MAX >= if
      s" hb: using: too many concurrent usings: "
      ENGINE-ERROR:USING-OVERFLOW PKG-USING-FAIL
   then
   wid  d cells USE-WIDS-OFF +  CELL!
   PKG-NOTIFY-USING
   d 1+ USE-DEPTH-CELL CELL! ;

: PKG-END-USING ( -- )
   TASK-GUARD
   FIND-USE-DEPTH {: d:n :}
   d 0= if
      s" hb: ;using without an open using" SAY
      ENGINE-ERROR:USING-UNBALANCED THROW-AT
   then
   d 1- USE-DEPTH-CELL CELL! ;

\ ---- export (habu2.f C-EXPORT) -----------------------------------------------
\ The record the operand names as the engine's LFIND resolves it: the open
\ package, then the global wordlist, never a used public.
: PKG-SOURCE ( -- ptr n )
   TOKEN$ {: a:ptr u:n :}
   a u FIND-SPLIT {: q:n :}
   q FIND-BAD = if XREF-NULL exit then
   q FIND-BARE = if a u NDICT:OPEN-PRI FIND-OPEN exit then
   a u q FIND-QUALIFIED ;

\ The name the alias takes: the operand's tail, after a qualifier.
: PKG-TAIL ( -- ptr u8 n )
   TOKEN$ {: a:ptr u:n :}
   a u FIND-SPLIT {: q:n :}
   q 0< if a u exit then
   a q 1+ ZPTR+  u q - 1- ;

\ After the seal a protected wordlist takes no record: the engine names its
\ guard and the definition, then exits (habu2.f LSTOREDEFNAME).
: PKG-OPEN-WID ( n -- ) {: wid:n :}
   SEAL-NDICT@ 0= if exit then
   wid PROTECTED? 0= if exit then
   s" hb: cannot publish into protected word: " SAY
   TOKEN$ SAY
   NL 1 ENGINE-ERROR:SEAL-PACKAGE FAIL-CLOSED ;

\ The source must not be internal: an alias would carry its body past the
\ interpret gate without the DNAME-INT mark, so the gate's refusal is made here
\ (alias-record refuses the source too). DEF-TKA and DEF-TKL take the operand,
\ which the refusals after them name.
: PKG-EXPORT-SOURCE ( -- ptr n )
   SEAL-GUARD
   PKG-SOURCE {: src:ptr :}
   src XREF-FOUND? 0= if RC-REJECT PKG-FAIL then
   src XREF-FLAGS DNAME-INT and 0<> if
      s" hb: internal engine word: " REFUSE
   then
   TKA-CELL CELL@ DEF-TKA-CELL CELL!
   TKL-CELL CELL@ DEF-TKL-CELL CELL!
   src ;

\ In a package, publish an existing word under its tail into the current
\ wordlist: its code, its immediate, wide and certified-input bits. At top
\ level the name is consumed and nothing else happens: there `export` is
\ hb-build's directive.
: PKG-EXPORT ( -- )
   TASK-GUARD
   PKG-PUB-CELL CELL@ 0= if PKG-NAME exit then
   PKG-NAME
   PKG-EXPORT-SOURCE {: src:ptr :}
   PKG-DICT-ROOM
   PKG-TAIL {: ta:ptr tu:n :}
   ta tu get-current FIND-PROBE XREF-FOUND? if
      s" duplicate definition: " SAY PKG-RC-DUPLICATE PKG-FAIL
   then
   PKG-CHECK-EXPORT
   get-current PKG-OPEN-WID
   ta tu PKG-CODE-ROOM
   ta tu src PKG-INDEX get-current PKG-ALIAS ;

\ ---- the package keywords ----------------------------------------------------
\ The engine's EM-INTERPRET-DEFINE-KEYWORDS rows for packages, matched as
\ LITERAL? matches its own.
: PACKAGE? ( -- bool )
   s" package" TOKEN-IS? if PKG-PACKAGE true exit then
   s" public" TOKEN-IS? if
      PKG-PUB-CELL NCOMP-DISPATCH:DECL-PUBLIC-OFF PKG-SECTION true exit
   then
   s" private" TOKEN-IS? if
      PKG-PRI-CELL NCOMP-DISPATCH:DECL-PRIVATE-OFF PKG-SECTION true exit
   then
   s" ;package" TOKEN-IS? if PKG-END-PACKAGE true exit then
   s" using" TOKEN-IS? if PKG-USING true exit then
   s" ;using" TOKEN-IS? if PKG-END-USING true exit then
   s" export" TOKEN-IS? if PKG-EXPORT true exit then
   false ;

;package
