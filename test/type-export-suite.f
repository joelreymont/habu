\ type-export-suite.f — checker-level EXPORT alias suite (CHECKER-EXPORT, dot
\ habu-compiler-pkg-re-688212c1). A WHITEBOX-SUITE row:
\ CHECKER-EXPORT and the record queries are
\ checker internals, which the unsealed engine binds by their recorded rows. The
\ gate runs it on that engine, which test/whitebox-engine.f builds. Direct
\ checker operations execute inside real engine package blocks so package
\ authority comes from the live package:
\     <unsealed engine> --load test/type-export-suite.f
\ Covers: cross-package alias fidelity (one scheme, two names, source
\ untouched), private->public promotion, defer + control-flag copy, quotation
\ scheme fidelity, the CHECKER-EXPORT rejects the engine keyword's native walls
\ pre-empt but the static scanner (src/habu/verify-source.f) meets directly (no
\ package, undefined, private from a closed package, malformed qualification,
\ sealed-system source, duplicate/self-export), scope/candidate rollback of the
\ alias rows, and private->public promotion through the engine keyword. The
\ keyword's dual-name execution, checked callers and primitive-source reject
\ (E-EXPORT-PRIM, the one CHECKER-EXPORT reject the keyword reaches) are
\ test/export-package.f's.
\ A failure prints F<index> + detail; T-REPORT exits 1 on any fail.

require src/lib/test.f
require src/core/checker-owner-extension.f
require src/habu/xref.f
require src/habu/interpret.f

T-RESET

require src/habu/verify-source.f   \ VERIFY:CANDIDATE-IN-SCOPE: the certify path's verdict

variable FOUNDF   variable TC
variable P-SYMN   variable P-SYMU   variable P-DEPTH
variable EXPORT-CP
create BASE-CONTRACT 4096 allot
variable BASE-CONTRACT-U
package XTC
private
CAST: CONTRACT-XT ( n -- [ n -- ptr u8 n bool ] )
: CONTRACT$ ( ptr u8 n -- ptr u8 n bool )
   XREF-FIND-INDEX
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
      CHECKER-OWNER-EXT:RECORD-CONTRACT-OFF + CELL-VIEW @
      CONTRACT-XT execute ;
public
: SAVE-CONTRACT ( ptr u8 n -- )
   CONTRACT$ {: bytes:ptr size:n found:bool :}
   found TTRUE
   found 0= if exit then
   size 4096 <= TTRUE
   size 4096 > if exit then
   bytes BASE-CONTRACT size BYTE-COPY
   size BASE-CONTRACT-U ! ;
: SAME-CONTRACT ( ptr u8 n -- )
   CONTRACT$ {: bytes:ptr size:n found:bool :}
   found TTRUE
   found 0= if exit then
   size BASE-CONTRACT-U @ T=
   bytes size BASE-CONTRACT BASE-CONTRACT-U @ STR= TTRUE ;
: HAS-CONTRACT ( ptr u8 n -- )
   CONTRACT$ nip nip TTRUE ;
: ALIAS-ROOM ( -- n ) HB-TARGET-LINUX-X86-64? if 16 else 8 then ;
: PAIR-ROOM ( -- n ) HB-TARGET-LINUX-X86-64? if 48 else 32 then ;
;package
\ The probes below are checked: on the whitebox engine a body naming a
\ checker-internal word binds that word's recorded row.
\
\ An alias CHECKER-EXPORT records is a fact of the checker's store: outside a
\ replay the operation publishes no engine record, so compiled code cannot call
\ one. The probes therefore read the alias's own record (CHECKER-RECORD-SYM?).
\ A checked caller of an alias asks the certify path (VERIFY:CANDIDATE-IN-SCOPE),
\ which binds through the engine's lookup: while a neutral pair is open the
\ checker overlay publishes the alias as a record, as it does the static
\ scanner's aliases. So a section that asks the certify path records its alias
\ and asks inside one pair, and finalizes it: the store keeps the alias, the
\ engine's record goes with the overlay. A package made inside the pair would be
\ refused (ENGINE-ERROR:OVERLAY-OPEN), so the section makes it first and
\ reopens it there.
: TWX-FIND-DEFER ( ptr u8 n -- bool ) CHECKER-RECORD-SYM? DFER-FIND-SYM ;
: TWX-CTL-FLAGS ( ptr u8 n -- n ) CHECKER-RECORD-SYM? CTL-FLAGS-SYM ;



\ ---------------------------------------------------------------------------
\ 1. cross-package alias fidelity: EXPORT xps:XP-INC into xpd publishes the
\    SAME scheme under xpd:XP-INC; the source record is untouched; a wrong
\    declared sig through the alias still rejects.
\ ---------------------------------------------------------------------------
package XPS
public
: XP-INC ( n -- n ) 1 + ;
: XP-LONG-NAMED-INCREMENT ( n -- n ) 1 + ;
: XP-LONG-NAMED-MAKER ( n -- ) create , does> ( -- n ) @ 1 + ;
;package

package XPD
;package

CHECKER-SCOPE-START-NEUTRAL
package XPD
public
s" xps:XP-INC" CHECKER-EXPORT
;package
s" xpd:XP-INC" CHECKER-FIND-USIG FOUNDF !  FOUNDF @ -1 T=
s" xps:XP-INC" CHECKER-FIND-USIG FOUNDF !  FOUNDF @ -1 T=
s" XPU1 ( n -- n ) xpd:XP-INC" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" XPU2 ( n -- n ) xps:XP-INC" CHECK! -1 T=
s" XPU3 ( -- n ) xpd:XP-INC" VERIFY:CANDIDATE-IN-SCOPE 0 T=
s" XPU4 ( n -- n n ) xpd:XP-INC" VERIFY:CANDIDATE-IN-SCOPE 0 T=
CHECKER-SCOPE-FINALIZE
\ the alias has no engine record, so a live caller binds nothing (unresolvable)
s" XPU1L ( n -- n ) xpd:XP-INC" CHECK-QUIET-CANDIDATE! 1 T=

\ ---------------------------------------------------------------------------
\ 2. private->public promotion: a bare-name source resolves through the open
\    package's private scope and publishes under the public tail.
\ ---------------------------------------------------------------------------
package XPP
: XP-HID ( n -- n ) 2 + ;
;package
CHECKER-SCOPE-START-NEUTRAL
package XPP
public
s" XP-HID" CHECKER-EXPORT
;package
s" xpp:XP-HID" CHECKER-FIND-USIG FOUNDF !  FOUNDF @ -1 T=
s" XPU5 ( n -- n ) xpp:XP-HID" VERIFY:CANDIDATE-IN-SCOPE -1 T=
CHECKER-SCOPE-FINALIZE

\ ---------------------------------------------------------------------------
\ 3. defer + control flags ride the alias: the defer flag, and the source's
\    whole control word - the throw edge, the dead path after it and the
\    effect's provenance (EFFECT-EXTERNAL) - but no identity.
\ ---------------------------------------------------------------------------
package XPF
public
defer XP-DEF ( n -- n )
: XP-THR ( -- ) 7 throw ;
;package

package XPF2
public
s" xpf:XP-DEF" CHECKER-EXPORT
s" xpf:XP-THR" CHECKER-EXPORT
;package
s" xpf2:XP-DEF" TWX-FIND-DEFER FOUNDF !  FOUNDF @ -1 T=
s" xpf2:XP-THR" TWX-CTL-FLAGS CTL-THROW and CTL-THROW T=
s" xpf2:XP-THR" TWX-CTL-FLAGS  s" xpf:XP-THR" TWX-CTL-FLAGS T=
s" xpf2:XP-DEF" TWX-CTL-FLAGS EFFECT-EXTERNAL invert and 0 T=
s" xpf2:XP-DEF" TWX-CTL-FLAGS EFFECT-EXTERNAL and EFFECT-EXTERNAL T=
s" xpf:XP-DEF" TWX-CTL-FLAGS EFFECT-EXTERNAL and EFFECT-EXTERNAL T=
s" xpf2:XP-THR" TWX-FIND-DEFER FOUNDF !  FOUNDF @ 0 T=

\ ---------------------------------------------------------------------------
\ 4. quotation scheme fidelity: a higher-order sig survives the alias copy;
\    a wrong quotation argument through the alias rejects.
\ ---------------------------------------------------------------------------
package XPQ
public
: XP-HOF ( [ n -- n ] n -- n ) swap execute ;
;package
package XPQ2
;package
CHECKER-SCOPE-START-NEUTRAL
package XPQ2
public
s" xpq:XP-HOF" CHECKER-EXPORT
;package
s" XPU6 ( n -- n ) [: 1 + ;] swap xpq2:XP-HOF" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" XPU7 ( n -- n ) [: + ;] swap xpq2:XP-HOF" VERIFY:CANDIDATE-IN-SCOPE 0 T=
CHECKER-SCOPE-FINALIZE

\ ---------------------------------------------------------------------------
\ 5. rejects. Every fail-closed path throws its named code; catch restores
\    the pre-call ( a u ) under the code.
\ ---------------------------------------------------------------------------
\ no open package.
s" XP-INC" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-NO-PACKAGE T=
package XPR
public
\ undefined bare + qualified names.
s" XP-NOPE" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-UNDEFINED T=
s" xps:XP-NOPE" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-UNDEFINED T=
\ private word from a CLOSED package: qualified lookup is public-only.
s" xps:XP-PRIV" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-UNDEFINED T=
\ malformed qualification (double colon / edge colon) never resolves.
s" xps:XP:BAD" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-UNDEFINED T=
s" :XP-INC" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-UNDEFINED T=
\ re-export FROM a sealed system package (latch is sealed in this process).
s" tfam:list" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-SEALED T=
s" type:of" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-SEALED T=
s" match:arm" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-SEALED T=
s" engine-error:bad-tag" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-SEALED T=
s" engine-error:bad-tag" ' CHECKER-LBUF-NAME-OK? catch TC ! 2drop
TC @ CHECKER-REJECT-RC T=
\ duplicate tail in the current section.
s" xps:XP-INC" CHECKER-EXPORT
s" xps:XP-INC" ' CHECKER-EXPORT catch TC ! 2drop  TC @ $4E T=
;package
\ self-export in the same section is the duplicate case.
package XPZ
public
: XP-SELF ( n -- n ) ;
s" XP-SELF" ' CHECKER-EXPORT catch TC ! 2drop  TC @ $4E T=
;package

\ the private source used by the closed-package reject above really is private:
\ record it AFTER the reject probes so the earlier lookup could not see it, then
\ prove a private record still does not resolve via the public qualifier.
package XPS
: XP-PRIV ( n -- n ) ;
;package
package XPR2
public
s" xps:XP-PRIV" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-UNDEFINED T=
;package

\ ---------------------------------------------------------------------------
\ 6. scope rollback: the alias's sym/effect rows retire with the frame; the
\    watermarks (SYM-N, sym string pool) restore exactly.
\ ---------------------------------------------------------------------------
SYM-N @ P-SYMN !   SYM-STR-U @ P-SYMU !
CHECKER-SCOPE-START
   package XRB
   public
   s" xps:XP-INC" CHECKER-EXPORT
   s" xrb:XP-INC" CHECKER-FIND-USIG FOUNDF !  FOUNDF @ -1 T=
   ;package
CHECKER-SCOPE-DONE
SYM-N @ P-SYMN @ T=
SYM-STR-U @ P-SYMU @ T=
s" xrb:XP-INC" CHECKER-FIND-USIG FOUNDF !  FOUNDF @ 0 T=

\ ---------------------------------------------------------------------------
\ 7. candidate rollback: alias effect, defer flag, and control flags all
\    retire; the frame depth balances.
\ ---------------------------------------------------------------------------
RBF-DEPTH @ P-DEPTH !
CHECK-CANDIDATE-START
   package XRB2
   public
   s" xpf:XP-DEF" CHECKER-EXPORT
   s" xpf:XP-THR" CHECKER-EXPORT
   s" xrb2:XP-DEF" TWX-FIND-DEFER FOUNDF !  FOUNDF @ -1 T=
   s" xrb2:XP-DEF" TWX-CTL-FLAGS EFFECT-EXTERNAL and EFFECT-EXTERNAL T=
   s" xrb2:XP-THR" TWX-CTL-FLAGS CTL-THROW and CTL-THROW T=
   ;package
0 CHECK-CANDIDATE-DONE drop
RBF-DEPTH @ P-DEPTH @ T=
s" xrb2:XP-DEF" CHECKER-FIND-USIG FOUNDF !  FOUNDF @ 0 T=
s" xrb2:XP-DEF" TWX-FIND-DEFER FOUNDF !  FOUNDF @ 0 T=
s" xrb2:XP-DEF" TWX-CTL-FLAGS 0 T=
s" xrb2:XP-THR" TWX-CTL-FLAGS 0 T=

\ ---------------------------------------------------------------------------
\ 8. private->public promotion through the engine keyword: a bare private name
\    resolves in the open package and runs under its public tail.
\ ---------------------------------------------------------------------------
package XPG
: XPG-HID ( n -- n ) 3 + ;
public
EXPORT XPG-HID
;package
4 XPG:XPG-HID 7 T=
s" xpg:XPG-HID" XTC:HAS-CONTRACT

\ Both entry paths publish an engine alias after the checker records its effect.
s" package XPE public EXPORT xps:XP-INC ;package" evaluate-closed
s" xpe:XP-INC" XTC:HAS-CONTRACT

\ A direct checker declaration has no pending engine record. It must not
\ attach its alias or cast effect to the next word the engine publishes.
package XPC
public
: BASE ( -- n ) 42 ;
;package
s" xpc:BASE" XTC:SAVE-CONTRACT
package XPC
public
s" xps:XP-INC" CHECKER-EXPORT
: AFTER-EXPORT ( -- n ) 42 ;
s" GHOST-CAST" s" n -- n" CHECKER-DEFCAST
: AFTER-CAST ( -- n ) 42 ;
;package
s" xpc:AFTER-EXPORT" XTC:SAME-CONTRACT
s" xpc:AFTER-CAST" XTC:SAME-CONTRACT

\ A caught OUTER export with no room for its long alias must leave the next
\ engine definition linked to its own checker declaration, after CP recovers.
package XPC
public
cp@ EXPORT-CP !
dbase@ REGION + $4000 - XTC:ALIAS-ROOM - cp!
s" EXPORT xps:XP-LONG-NAMED-INCREMENT" ' OUTER:INTERPRET catch TC ! 2drop
TC @ 76 T=
EXPORT-CP @ cp!
s" xpc:XP-LONG-NAMED-INCREMENT" XREF-FIND XREF-FOUND? TFALSE
: AFTER-FAILED-EXPORT ( -- n ) 42 ;
;package
s" xpc:AFTER-FAILED-EXPORT" XTC:SAME-CONTRACT

\ The long alias fits alone, but its does> clause does not fit after it.
package XPC
public
cp@ EXPORT-CP !
dbase@ REGION + $4000 - XTC:PAIR-ROOM - cp!
s" EXPORT xps:XP-LONG-NAMED-MAKER" ' OUTER:INTERPRET catch TC ! 2drop
TC @ 76 T=
EXPORT-CP @ cp!
s" xpc:XP-LONG-NAMED-MAKER" XREF-FIND XREF-FOUND? TFALSE
s" xpc:XP-LONG-NAMED-MAKER;does" XREF-FIND XREF-FOUND? TFALSE
: AFTER-FAILED-CLAUSE ( -- n ) 42 ;
;package
s" xpc:AFTER-FAILED-CLAUSE" XTC:SAME-CONTRACT

\ ---------------------------------------------------------------------------
\ report: "test: ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
T-REPORT
