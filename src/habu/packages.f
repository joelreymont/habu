\ packages.f - the package keywords of the Habu interpret loop, the twins of
\ habu2.f C-PACKAGE to C-EXPORT; interpret.f STEP asks PACKAGE? after the
\ literal keywords. After the seal a store into the friend arena (CUR, WIDN,
\ DEF-WL, PKG-*) exits 83: rows, scope and aliases go through the prims.f
\ writers and CUR through set-current; the using band, DEF-TKA, DEF-TKL and
\ layout.f PKG-OUTSIDE-CELL are not sealed and are written here. A writer
\ refuses silently, so the words here refuse first where they can. It also
\ exports OUTER:WITHOUT-PACKAGE, which runs a quotation as at top level.

require src/lib/prelude.f
require src/habu/boot-cells.f
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

: PKG-INDEX ( ptr n -- n ) BYTE-VIEW NULL-PTR BYTE-VIEW - dbase@ - DREC / ;

\ The token, then the engine's compile-die tail (habu2.f C-PACKAGE-FAIL).
: PKG-FAIL ( n -- )
   TOKEN$ ERROR-TEXT:TYPE THROW-AT ;

\ The keyword's operand. With the input at its end the token cells still hold
\ the keyword, which the refusal names.
: PKG-NAME ( -- )
   TOKEN if exit then
   RC-NO-NAME PKG-FAIL ;

\ No record slot left (habu2.f C-QUALIFY-CAP). The refusal names what DEF-TKA
\ and DEF-TKL hold, which `package` never writes: for it, what the last
\ definition or `export` left there.
: PKG-DICT-FULL ( -- )
   s" hb: dictionary full at: " ERROR-TEXT:TYPE
   DEF-TKA-CELL ADDR@ DEF-TKL-CELL CELL@ ERROR-TEXT:TYPE
   PKG-RC-DICT-FULL THROW-AT ;

: PKG-DICT-ROOM ( -- )
   ndict@ DICT-CAP < if exit then
   PKG-DICT-FULL ;

\ A long name copied to CP takes one whole target code slot. Both names an
\ export writes must fit before its checker declaration becomes pending.
: PKG-CODE-SLOT ( -- n )
   HB-TARGET-LINUX-X86-64? if 16 else 4 then ;

: PKG-CODE-SPAN ( n -- n )
   dup DNAME-INL <= if drop 0 exit then
   PKG-CODE-SLOT {: slot:n :}
   slot 1- + slot negate and ;

: PKG-FITS-AFTER? ( n n -- bool ) {: u:n used:n :}
   u DNAME-INL U> 0= if true exit then
   dbase@ REGION + PKG-CODE-RESERVE - {: ceil:n :}
   ceil cp@ U> 0= if false exit then
   ceil cp@ - {: room:n :}
   room used U> 0= if false exit then
   room used - {: left:n :}
   left u PKG-CODE-SPAN U> ;

: PKG-CODE-ROOM-AFTER ( ptr u8 n n -- ) {: a:ptr u:n used:n :}
   u used PKG-FITS-AFTER? if exit then
   s" hb: code space full at: " ERROR-TEXT:TYPE
   a u ERROR-TEXT:TYPE PKG-RC-CODE-FULL THROW-AT ;

: PKG-CODE-ROOM ( ptr u8 n -- ) 0 PKG-CODE-ROOM-AFTER ;

\ The checker's declaration operations (habu2.f DECL-OWNER).
\ The operation at off in the owner record a dispatch cell names, 0 when there
\ is no record or the field is empty: the engine then skips the call, where
\ src/compiler/native/checker-owner.f FIELD would refuse.
: PKG-OPERATION ( ptr ptr u8 n -- n )
   {: cell:ptr off:n :}
   cell @ {: rec:ptr :}
   rec 0= if 0 exit then
   rec off + CELL-VIEW @ ;

\ The owner record stores raw execution tokens; these views state the two
\ signatures called here.
CAST: PKG-AS-ACTION ( n -- [ -- ] )
CAST: PKG-AS-NAME-ACTION ( n -- [ ptr u8 n -- ] )
CAST: PKG-AS-EXPORT-ACTION ( n -- [ ptr u8 n n -- ] )

\ `package`, `public`, `private` and `;package` notify the target checker
\ alone (habu2.f C-CALL-CHECKER-PACKAGE and its siblings).
: PKG-TARGET ( n -- n )
   NCOMP-DISPATCH:TARGET-DECL-A swap PKG-OPERATION ;

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
   NCOMP-DISPATCH:DECL-A NCOMP-DISPATCH:DECL-USING-OFF PKG-OPERATION
   {: own:n :}
   own 0<> if TOKEN$ own PKG-AS-NAME-ACTION execute then
   NCOMP-DISPATCH:DECL-USING-OFF PKG-TARGET {: target:n :}
   target 0= if exit then
   target own = if exit then
   TOKEN$ target PKG-AS-NAME-ACTION execute ;

\ An actual export names the source checker's declaration callback. A direct
\ CHECKER-EXPORT call only adds a checker row and has no engine alias to bind.
: PKG-CHECK-EXPORT ( -- )
   NCOMP-DISPATCH:DECL-A NCOMP-DISPATCH:DECL-EXPORT-OFF PKG-OPERATION
      {: xt:n :}
   xt 0= if s" checker-export" RC-REJECT FAIL-CLOSED then
   TOKEN$ xt PKG-AS-NAME-ACTION execute ;

: PKG-NOTIFY-EXPORT ( ptr u8 n n -- )
   {: operand:ptr operand-u:n ord:n :}
   NCOMP-DISPATCH:TARGET-DECL-A @
      dup NULL-PTR = if drop exit then {: owner:ptr :}
   owner 8 - CELL-VIEW @ CHECKER-OWNER-ABI:EXPORT-ACCEPT-OFF CELL + < if exit then
   owner CHECKER-OWNER-ABI:EXPORT-ACCEPT-OFF + CELL-VIEW @ {: xt:n :}
   xt 0= if exit then
   operand operand-u ord xt PKG-AS-EXPORT-ACTION execute ;

\ The engine's twins: habu2.f C-PACKAGE to C-END-PACKAGE.
: PKG-ROW ( -- ptr n )
   TOKEN$ BOOT:NAMESPACE-WL WL-PROBE ;

\ A package whose record carries DNAME-SEALED ends the process when named
\ again (SEALED-FAIL); a package is sealed only by the last file that opens it.
: PKG-SEALED? ( -- bool )
   PKG-ROW {: row:ptr :}
   row XREF-FOUND? 0= if false exit then
   row XREF:SEALED? ;

: PKG-SEAL-GUARD ( -- )
   PKG-SEALED? 0= if exit then
   TOKEN$ SEALED-FAIL ;

\ Under the design seal a package whose row predates the seal is refused: the
\ located line and ENGINE-ERROR:POLICY (habu2.f C-PACKAGE-POLICY-GUARD).
: PKG-POLICY-GUARD ( -- )
   POLICY-NDICT-CELL CELL@ {: seal:n :}
   seal 0= if exit then
   PKG-ROW {: row:ptr :}
   row XREF-FOUND? 0= if exit then
   row PKG-INDEX seal >= if exit then
   s" hb: policy: package " ERROR-TEXT:TYPE TOKEN$ ERROR-TEXT:TYPE
   s"  predates the seal" ERROR-TEXT:TYPE ENGINE-ERROR:POLICY THROW-AT ;

\ An existing row keeps its wids and gains a private one only when it has none.
: PKG-REOPEN ( ptr n -- n ) {: row:ptr :}
   row PKG-INDEX {: ix:n :}
   row XREF-PKG-PRIVATE 0= if ix namespace-private then
   ix ;

public
\ A file object's link gives package record n the private wordlist its file made;
\ n outside the dictionary, a non-package or one holding one is E-COMP-STATE.
: PACKAGE-PRIVATE ( n -- )
   {: ix:n :}
   ix 0 <  ix ndict@ >=  or if E-COMP-STATE throw then
   ix XREF-REC XREF-WORDLIST BOOT:NAMESPACE-WL <> if E-COMP-STATE throw then
   ix XREF-REC XREF-PKG-PRIVATE 0<> if E-COMP-STATE throw then
   ix namespace-private ;
private

\ The index of the row `package` opens (habu2.f C-PACKAGE-ENSURE); a new row
\ takes a public and a private wid.
: PKG-ENSURE ( -- n )
   PKG-ROW {: row:ptr :}
   row XREF-FOUND? if row PKG-REOPEN exit then
   OVERLAY-GUARD
   PKG-DICT-ROOM
   TOKEN$ PKG-CODE-ROOM
   TOKEN$ true namespace-record ;

\ namespace-record's name refusals, as E-COMP-STATE: a colon at either edge and a
\ second colon.
: PKG-PATH-CK ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u 0 COLON-AT {: q:n :}
   q 0 < if exit then
   q 0=  q 1+ u =  or if E-COMP-STATE throw then
   a u q 1+ COLON-AT 0 < 0= if E-COMP-STATE throw then ;

public
\ A file object's link records package a u, which must lie in the caller's buffer, with
\ its private wordlist right after the public one when the flag is set, as PKG-ENSURE does.
: PACKAGE-RECORD ( ptr u8 n bool -- n )
   {: a:ptr u:n own:bool :}
   u 1 < if E-COMP-STATE throw then
   a u PKG-PATH-CK
   PKG-DICT-ROOM
   a u PKG-CODE-ROOM
   a u BOOT:NAMESPACE-WL WL-PROBE XREF-FOUND? if
      s" duplicate definition: " ERROR-TEXT:TYPE
      a u ERROR-TEXT:TYPE PKG-RC-DUPLICATE THROW-AT
   then
   a u own namespace-record ;
private

\ CHECKER-RESOLVE:PART-GUARD and PART-PATH's E-PKG-CONTEXT is the engine's
\ `package` refusal; any other code goes on.
: PKG-CONTEXT ( n -- ) {: rc:n :}
   rc 0= if exit then
   rc E-PKG-CONTEXT = if PKG-RC-CONTEXT PKG-FAIL then
   rc throw ;

\ Inside package A the token cells name `A:<token>` (habu2.f C-PACKAGE-PATH).
: PKG-PATH ( -- )
   TOKEN$ CHECKER-RESOLVE:PART-PATH TKL-CELL CELL! TKA-CELL ADDR! ;

\ The package opens on its private wordlist, in this buffer. The using depth
\ it opens at is the one `;package` restores, a part's its package's. The name
\ goes to the unit hook first, whose nonzero answer skips the rest of the input
\ instead (habu2.f C-PACKAGE).
: PKG-PACKAGE ( -- )
   TASK-GUARD
   [: CHECKER-RESOLVE:PART-GUARD ;] catch PKG-CONTEXT
   PKG-NAME
   0 UNIT-EV-PACKAGE UNIT-EVENT 0<> if INE-CELL CELL@ INP-CELL CELL! exit then
   TOKEN$ 0 COLON-AT 0 >= if PKG-RC-CONTEXT PKG-FAIL then
   ['] PKG-PATH catch PKG-CONTEXT
   PKG-POLICY-GUARD
   NCOMP-DISPATCH:DECL-PACKAGE-OFF PKG-NOTIFY-NAME
   PKG-SEAL-GUARD
   PKG-ENSURE {: ix:n :}
   USE-DEPTH-CELL CELL@ USE-PKG-SAVE-CELL CELL!
   ix get-current package-scope!
   PKG-PRI-CELL CELL@ set-current
   0 PKG-OUTSIDE-CELL CELL! ;

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
   ENCLOSING:REC CELL@ {: outer:n :}
   outer 0= if -1 0 else outer XREF-ADDR>INDEX ENCLOSING:PARENT CELL@ then
   package-scope!
   USE-PKG-SAVE-CELL CELL@ USE-DEPTH-CELL CELL! ;

: PKG-SEAL ( -- )
   TASK-GUARD
   PKG-PUB-CELL CELL@ 0= if PKG-RC-CONTEXT PKG-FAIL then
   PKG-REC-CELL CELL@ XREF-ADDR>REC XREF:SEAL ;

\ XREF:WID-SEALED?, with the open package's two wids read from its record and
\ the global and types wordlists, no package's, answered without its walk.
: PKG-WID-SEALED? ( n -- bool ) {: wid:n :}
   wid FIRST-DYNAMIC-WID < if false exit then
   wid BOOT:GLOBAL-WL =  wid BOOT:TYPES-WL =  or if false exit then
   PKG-PUB-CELL CELL@ 0<> if
      wid PKG-PUB-CELL CELL@ =  wid PKG-PRI-CELL CELL@ =  or if
         PKG-REC-CELL CELL@ XREF-ADDR>REC XREF:SEALED? exit
      then
   then
   wid XREF:WID-SEALED? ;

\ A throw puts the package scope back (habu1.f B-EVAL, habu2.f LEVALREC).
\ The enclosing package's row and parent wordlist, the open package's row and
\ parent wordlist, the current wordlist and the open package's using floor:
\ what the engine's evaluate frame keeps of the package scope beside the using
\ depth (PKGSNAP, EVAL-FRAME:ENCLOSING).
: PKG-STATE ( -- n n n n n n )
   ENCLOSING:REC CELL@  ENCLOSING:PARENT CELL@
   PKG-REC-CELL CELL@  PKG-PARENT-CELL CELL@  get-current  USE-PKG-SAVE-CELL CELL@ ;

\ Put back the state PKG-STATE read, as the engine's recovery does after a
\ throw, and have the checker re-read the restored scope. The sealed writers
\ run only when the failed input changed the scope. A part opens only inside
\ its enclosing package, so that package opens first.
: PKG-RECOVER ( n n n n n n -- )
   {: outer-rec:n outer-parent:n rec:n parent:n cur:n floor:n :}
   floor USE-PKG-SAVE-CELL CELL!
   ENCLOSING:REC CELL@ outer-rec <>  ENCLOSING:PARENT CELL@ outer-parent <> or
   PKG-REC-CELL CELL@ rec <> or  PKG-PARENT-CELL CELL@ parent <> or
   get-current cur <> or if
      outer-rec 0<> if outer-rec XREF-ADDR>INDEX outer-parent package-scope! then
      rec 0= if -1 0 package-scope! else rec XREF-ADDR>INDEX parent package-scope! then
      cur set-current
   then
   NCOMP-DISPATCH:DECL-PKG-RESYNC-OFF PKG-NOTIFY ;

\ The engine's twins: habu2.f C-USING and C-END-USING.
: PKG-USING-FAIL ( ptr u8 n n -- ) {: a:ptr u:n rc:n :}
   a u ERROR-TEXT:TYPE TOKEN$ ERROR-TEXT:TYPE rc THROW-AT ;

: PKG-USED-WID ( -- n )
   TOKEN 0= if
      s" hb: using: missing package name" ERROR-TEXT:TYPE
      ENGINE-ERROR:USING-NO-NAME THROW-AT
   then
   TOKEN$ 0 COLON-AT 0 >= if
      s" hb: using: package name must not contain ':': "
      ENGINE-ERROR:USING-BAD-NAME PKG-USING-FAIL
   then
   TOKEN$ CHECKER-RESOLVE:HEAD-ROW {: row:ptr :}
   row XREF-FOUND? 0= if
      s" hb: using: unknown package: " ENGINE-ERROR:USING-UNKNOWN PKG-USING-FAIL
   then
   row XREF-PKG-PUBLIC ;

\ The using depth the buffer the Habu loop reads may close down to, as the
\ engine's evaluate frame keeps it (layout.f EVAL-FRAME:USE-FLOOR): INTERPRET starts
\ it at the depth the buffer enters at, and a buffer below it, because a
\ `;package` closed a package opened before the buffer, lowers it to meet the
\ first using it opens there.
variable USE-FLOOR

\ The wid joins the used publics at the current depth; the checkers record the
\ name at that depth before it grows.
: PKG-USING ( -- )
   TASK-GUARD
   PKG-USED-WID {: wid:n :}
   USE-DEPTH-CELL CELL@ {: d:n :}
   d USE-MAX >= if
      s" hb: using: too many concurrent usings: "
      ENGINE-ERROR:USING-OVERFLOW PKG-USING-FAIL
   then
   d USE-FLOOR @ < if d USE-FLOOR ! then
   wid  d cells USE-WIDS-OFF +  CELL!
   PKG-NOTIFY-USING
   d 1+ USE-DEPTH-CELL CELL! ;

\ In an open package the most recent using must be the package's own: one
\ opened before `package` sits at or below the depth `;package` restores, so
\ closing it here would come undone there. Nor may a buffer close a using its
\ includer opened, at or below the buffer's floor: it would come back open
\ when the buffer ends.
: PKG-END-USING ( -- )
   TASK-GUARD
   USE-DEPTH-CELL CELL@ {: d:n :}
   d 0= if
      s" hb: ;using without an open using" ERROR-TEXT:TYPE
      ENGINE-ERROR:USING-UNBALANCED THROW-AT
   then
   PKG-PUB-CELL CELL@ 0<>  d USE-PKG-SAVE-CELL CELL@ <=  and if
      s" hb: ;using would close a using opened outside the package" ERROR-TEXT:TYPE
      ENGINE-ERROR:USING-OUTER THROW-AT
   then
   d USE-FLOOR @ <= if
      s" hb: ;using would close a using opened outside the file" ERROR-TEXT:TYPE
      ENGINE-ERROR:USING-OUTER THROW-AT
   then
   d 1- USE-DEPTH-CELL CELL! ;

\ The engine's twin: habu2.f C-EXPORT.
\ The name the alias takes: the operand's tail after its first colon, or the
\ whole operand when it has none or one at either edge (habu2.f C-EXPORT-TAIL!).
: PKG-TAIL ( -- ptr u8 n )
   TOKEN$ {: a:ptr u:n :}
   a u 0 COLON-AT {: q:n :}
   q 1 <  q 1+ u >=  or if a u exit then
   a q 1+ ZPTR+  u q - 1- ;

\ The source is the record the operand names as the engine's LFIND resolves it
\ (FIND-SCOPE), never a used public's. It must not be internal: an alias would
\ carry its body past the interpret gate without the DNAME-INT mark, so the
\ gate's refusal is made here (alias-record refuses the source too). DEF-TKA
\ and DEF-TKL take the operand, which the refusals after them name.
: PKG-EXPORT-SOURCE ( -- ptr n )
   TOKEN$ FIND-SCOPE {: src:ptr :}
   src XREF-FOUND? 0= if RC-REJECT PKG-FAIL then
   src XREF-FLAGS DNAME-INT and 0<> if
      s" hb: internal engine word: " REFUSE
   then
   TKA-CELL CELL@ DEF-TKA-CELL CELL!
   TKL-CELL CELL@ DEF-TKL-CELL CELL!
   src ;

\ The record index of the source's does> clause, which its export carries
\ (habu2.f DOES-REC "the clause an export carries"), or -1 for a source that
\ is not a definer.
: PKG-CLAUSE ( ptr n -- n ) {: src:ptr :}
   src PKG-INDEX {: k:n :}
   k XREF-DOES-COMPANION? if k 1+ exit then
   -1 ;

\ The export and its clause each take an occurrence, so two must remain before
\ the checker hears of the export (habu2.f C-EXPORT-CLAUSE-ROOM): the
\ allocation's first cell is the last number issued, and -1 the last one that
\ can be, so fewer than two remain after -2 or -1 (habu1.f EM-DEF-OCC:LROOM).
: PKG-OCC-ROOM ( -- )
   data-base DEF-OCC:PTR-CELL + 0 ptr-field @ CELL-VIEW @ {: last:n :}
   last -2 =  last -1 =  or if DEF-OCC:E-EXHAUSTED throw then ;

\ The clause takes the slot above the export's own, and its name must not be
\ live in the current wordlist (habu2.f C-EXPORT-CLAUSE-ROOM).
: PKG-CLAUSE-ROOM ( n -- ) {: k:n :}
   PKG-OCC-ROOM
   ndict@ 1+ DICT-CAP < 0= if PKG-DICT-FULL then
   k XREF-REC XREF-NAME$ {: a:ptr u:n :}
   a u get-current WL-PROBE XREF-FOUND? if
      s" duplicate definition: " ERROR-TEXT:TYPE
      a u ERROR-TEXT:TYPE PKG-RC-DUPLICATE THROW-AT
   then ;

\ Twin of habu2.f C-EXPORT-CLAUSE-RECORD.
: PKG-CLAUSE-ALIAS ( n -- ) {: k:n :}
   k XREF-REC XREF-NAME$ {: a:ptr u:n :}
   a u k get-current alias-record ;

public
\ A file object's link publishes name a u, which must lie in the caller's buffer, for record
\ src in wordlist wid. Its own refusals are E-COMP-STATE; room and duplicate ones are PKG-EXPORT's.
: ALIAS-RECORD ( ptr u8 n n n -- )
   {: a:ptr u:n src:n wid:n :}
   u 1 <  wid 0 <  or if E-COMP-STATE throw then
   SEAL-NDICT@ 0<>  wid XREF-WID-PROTECTED?  and if E-COMP-STATE throw then
   src 0 <  src ndict@ >=  or if E-COMP-STATE throw then
   src XREF-REC {: rec:ptr :}
   rec XREF-WORDLIST {: swid:n :}
   swid XREF:PACKAGE-WL?  swid XREF:RETIRED-WL?  or if E-COMP-STATE throw then
   rec XREF-FLAGS DNAME-INT and 0<> if E-COMP-STATE throw then
   PKG-DICT-ROOM
   a u PKG-CODE-ROOM
   a u wid WL-PROBE XREF-FOUND? if
      s" duplicate definition: " ERROR-TEXT:TYPE
      a u ERROR-TEXT:TYPE PKG-RC-DUPLICATE THROW-AT
   then
   a u src wid alias-record ;
private

\ In a package, publish an existing word under its tail into the current
\ wordlist: its code, its immediate, wide and certified-input bits, and a
\ does> definer's clause in the slot above. At top level the name is consumed
\ and nothing else happens: there `export` is hb-build's directive.
: PKG-EXPORT ( -- )
   TASK-GUARD
   PKG-PUB-CELL CELL@ 0= if PKG-NAME exit then
   PKG-NAME
   OVERLAY-GUARD
   PKG-EXPORT-SOURCE {: src:ptr :}
   TOKEN$ {: operand:ptr operand-u:n :}
   get-current PKG-WID-SEALED? if operand operand-u SEALED-FAIL then
   PKG-DICT-ROOM
   PKG-TAIL {: ta:ptr tu:n :}
   ta tu get-current WL-PROBE XREF-FOUND? if
      s" duplicate definition: " ERROR-TEXT:TYPE PKG-RC-DUPLICATE PKG-FAIL
   then
   src PKG-CLAUSE {: clause:n :}
   clause 0 >= if clause PKG-CLAUSE-ROOM then
   ta tu PKG-CODE-ROOM
   clause 0 >= if
      clause XREF-REC XREF-NAME$ tu PKG-CODE-SPAN PKG-CODE-ROOM-AFTER
   then
   PKG-CHECK-EXPORT
   ndict@ {: ord:n :}
   ta tu src PKG-INDEX get-current alias-record
   clause 0 >= if clause PKG-CLAUSE-ALIAS then
   operand operand-u ord PKG-NOTIFY-EXPORT ;

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
   s" seal-package" TOKEN-IS? if PKG-SEAL true exit then
   s" using" TOKEN-IS? if PKG-USING true exit then
   s" ;using" TOKEN-IS? if PKG-END-USING true exit then
   s" export" TOKEN-IS? if PKG-EXPORT true exit then
   false ;

\ No package open, and the checker told so.
: PKG-CLEAR ( -- )
   -1 0 package-scope!
   NCOMP-DISPATCH:DECL-PKG-RESYNC-OFF PKG-NOTIFY ;

public

\ Q runs as at top level: no package open, current where the outer package
\ opened. The scope comes back on either exit, Q's throw rethrown (LINKER:WITH).
: WITHOUT-PACKAGE ( [ -- ] -- )
   {: q :}
   PKG-STATE {: outer-rec:n outer-parent:n rec:n parent:n cur:n floor:n :}
   PKG-OUTSIDE-CELL CELL@ {: outside:n :}
   PKG-CLEAR
   outer-rec 0<> if outer-parent set-current else
      rec 0<> if parent set-current then
   then
   q catch {: rc:n :}
   outer-rec outer-parent rec parent cur floor PKG-RECOVER
   outside PKG-OUTSIDE-CELL CELL!
   rc 0<> if rc throw then ;

;package
