split-str
\ definition.f - the compiler-neutral step around one pending definition's
\ lowering: staging, the checker scan and verdict, name and arity checks,
\ publication by kind, the pending callback stub, and retraction on refusal.
\ src/habu/t0.f calls it on every target.

require src/core/exit-status.f
require src/lib/prelude.f
require src/lib/error.f
require src/lib/string.f
require src/habu/boot-cells.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/workspace.f
require src/core/checker.f
require src/core/check-walk.f
require src/core/check-hook.f
require src/compiler/native/checker-owner.f
require src/compiler/native/dict.f
require src/compiler/native/emission.f
require src/compiler/native/publish.f
require src/compiler/native/callback-stub.f

package NDEFINITION

public

\ Everything the run needs, parked in cells: the whole run is one quotation under catch (compiler.f RUN).
0 CWORK-VARIABLE M-VERDICT n                   \ the verdict the recorded scan reached
0 CWORK-VARIABLE M-ROW-REJECTED bool           \ the checker rejected a TRUSTED: definition's row
0 CWORK-VARIABLE M-DOES-FRAME n                \ checker-owned transaction spans a split compilation
0 CWORK-VARIABLE M-DOES n                      \ byte split after `does> `, or zero
0 CWORK-VARIABLE M-DOES-SIG ^u8
0 CWORK-VARIABLE M-DOES-SIG-U n
0 CWORK-VARIABLE M-DOES-FUN n                  \ hidden clause function ordinal
0 CWORK-VARIABLE M-DOES-OFF n                  \ published clause's byte offset, or zero
0 CWORK-VARIABLE M-FIXED-VAL n                  \ the pending definer's value or dispatch cell
0 CWORK-VARIABLE M-OPEN n                      \ a compilation is running

\ The bytes from a `does>` keyword's start to its clause: the keyword and the
\ blank M-DOES points just past.
6 constant DOES-SPAN

\ The definition's name is its source spelling, whole, which stays put until the
\ next definition is recorded: no copy that a longer name could run past.
0 CWORK-VARIABLE NAME-A ^u8
0 CWORK-VARIABLE NAME-U n

\ How PUBLISH-IT publishes the emission: a pending body (its `does>` clause,
\ when M-DOES is set), a defer body through M-FIXED-VAL's cell, or a raw body.
0 constant PUB-PENDING
1 constant PUB-DEFER
2 constant PUB-RAW
0 CWORK-VARIABLE M-PUB n

\ The published emission's function offsets, which a publication callback reads
\ after the backend has cleared NEMIT.
0 CWORK-VARIABLE PUB-FUNS n
DYNAMIC-BUFFER PUB-OFF n

private

EXPORT CHECK:DOES-PUBLISHED
EXPORT CHECK:CLOSER-ARM
EXPORT CHECK:ROW-DECLARED
EXPORT CHECK:CHECK-UNJUDGED!
EXPORT CHECK:CHECKER-NATIVE-DOES-BEGIN
EXPORT CHECK:CHECK-DOES-DIN-CELLS
EXPORT CHECK:CHECK-DOES-DOUT-CELLS
EXPORT CHECK:CHECK-UNRECORDED-DIN
EXPORT CHECK:CHECK-UNRECORDED-DOUT
EXPORT CHECK:CHECKER-NATIVE-DOES-COMMIT

0 CWORK-VARIABLE M-SRC ^u8
0 CWORK-VARIABLE M-SRC-U n
0 CWORK-VARIABLE M-ROWS n                      \ where the checker store ended at STAGE-UNIT
0 CWORK-VARIABLE M-HEAD-RECOVERY bool          \ a split definition's head bound a recovery row
-1 CWORK-VARIABLE M-CB-SLOT n                   \ -1 outside a callback accessor definition
0 CWORK-VARIABLE M-CB-THUNK n
0 CWORK-VARIABLE M-CB-ENTRY n

public

: SRC$ ( -- ^u8 n )
   M-SRC @ M-SRC-U @ ;

: DOES-BYTE@ ( -- n )
   data-base DOESB-CELL + @ ;

: NAME$ ( -- str )
   NAME-A @ NAME-U @ ;

\ A compiler entry reached while a compilation runs would stage one definition
\ over another's state.
: IDLE-CK ( -- )
   M-OPEN @ 0<> if E-COMP-STATE throw then ;

: STUB-PENDING? ( -- bool )
   M-CB-SLOT @ 0 >= ;

\ The next definition is an accessor that owns one anonymous C entry after
\ its ordinary return. The entry is published with that definition's emission.
: CALLBACK-STUB! ( n n -- ) {: k:n thunk:n :}
   IDLE-CK
   STUB-PENDING? if E-COMP-STATE throw then
   k M-CB-SLOT !  thunk M-CB-THUNK !  0 M-CB-ENTRY ! ;

: CALLBACK-STUB@ ( -- n )
   M-CB-ENTRY @ dup 0= if E-COMP-STATE throw then ;

\ A compiler arms NCBSTUB before the accessor's first walk, so each emission
\ carries the stub; STUB-TAKE reads the host's placed entry.
: STUB-ARM ( -- )
   STUB-PENDING? if M-CB-SLOT @ M-CB-THUNK @ NCBSTUB:SET then ;

: STUB-TAKE ( -- )
   STUB-PENDING? if NCBSTUB:ENTRY@ M-CB-ENTRY ! then ;

: STUB-END ( -- )
   -1 M-CB-SLOT ! ;

private

: DOES-SIG-FIELD ( -- ^^u8 )
   data-base TCSIG-A-CELL CELL / ptr-field ;

: TRUSTED? ( -- bool )
   data-base TRUSTED-CELL + @ 0<> ;

: TRUST-SIG-A-FIELD ( -- ^^u8 )
   data-base TSIG-A-CELL CELL / ptr-field ;

: TRUST-SIG$ ( -- str )
   TRUST-SIG-A-FIELD @  data-base TSIG-U-CELL + @ ;

: CHECK-PARENT ( str -- n )
   {: a:^ u:n :}
   TRUSTED? if -1 CHECKER-OWNER:BIND-REGIME!
      a u CHECK-UNJUDGED! exit then
   0 CHECKER-OWNER:BIND-REGIME!
   a u CHECK:DEFINITION ;

public

: CHECK-SOURCE ( -- n )
   SRC$ CHECK-PARENT ;

private

\ A refused clause is reported as CHECK:DEFINITION reports a refused body: the
\ checker's reason, then its reject status, which the run names the
\ definition under. The clause is named by the caller, from its open unit.
: CLAUSE-CK ( n str -- )
   CHECKER-OWNER:DOES-REPORT -1 <> if EXIT-STATUS:REJECTED throw then ;

\ The engine records the split just past the `does> ` it consumed, so a cut
\ that is not just past a `does> ` in this source means the source and the
\ pending definition disagree.
: DOES-CUT-CK ( -- )
   M-SRC @ {: src:^ :}
   M-DOES @ {: cut:n :}
   cut DOES-SPAN < cut M-SRC-U @ > or if E-NFEED-CUT throw then
   src cut DOES-SPAN - + DOES-SPAN 1- s" does>" STR=CI 0= if E-NFEED-CUT throw then
   src cut 1- + c@ 32 <> if E-NFEED-CUT throw then ;

EXPORT CHECKER-EFFECT-AUTHORITY:RECOVERY-USED?
EXPORT CHECKER-TAPE:ADVANCE

public

\ The `does>` split's head. The caller advances its own record past the
\ `does>` token and checks the clause (CHECK-DOES-CLAUSE).
: CHECK-DOES-HEAD ( -- )
   DOES-CUT-CK
   CHECKER-NATIVE-DOES-BEGIN
   1 M-DOES-FRAME !
   M-DOES @ {: cut:n :}
   M-SRC @ cut DOES-SPAN - CHECK-PARENT
   TRUSTED? if drop else -1 <> if E-COMP-VERDICT throw then then
   \ The clause's scan clears the checker's recovery flag, so keep the head's.
   RECOVERY-USED? M-HEAD-RECOVERY ! ;

\ The `does>` clause, named for a refusal by the unit's name. The checker's
\ token ordinal first steps past the `does>` keyword between the two bodies.
: CHECK-DOES-CLAUSE ( str -- n )
   {: na:^ nu:n :}
   ADVANCE
   M-DOES @ {: cut:n :}
   M-SRC @ cut +  M-SRC-U @ cut -  {: ca:^ cu:n :}
   ca cu CLOSER-ARM
   ca cu  M-DOES-SIG @ M-DOES-SIG-U @
   CHECKER-OWNER:DOES-CHECK na nu CLAUSE-CK
   CHECK-DOES-DIN-CELLS 0 <  CHECK-DOES-DOUT-CELLS 0 <  or if E-COMP-ARITY throw then
   -1 ;

public

\ A TRUSTED: definition's row, which no scan certified, and whether the checker
\ rejected it. Run once the scan has recorded the name's exact spelling from
\ the tape: the pending dictionary record holds only a qualified name's tail.
: DECLARE-ROW ( -- bool )
   TRUSTED? 0= if false exit then
   NAME$ TRUST-SIG$ ROW-DECLARED ;

private

EXPORT CHECK:CHECKER-RETRACT-ROWS
EXPORT CHECK:CHECKER-ROWS-END
EXPORT CHECK:MULTI-ERR?
\ A multi-error run whose reader recovers from E-COMP-RECOVERY: that reader
\ sets RECOVER-CELL around the compile it calls (layout.f). Under any other
\ reader a refusal is E-COMP-VERDICT.
: RECOVERING? ( -- bool )
   MULTI-ERR?
   data-base NCOMP-DISPATCH:RECOVER-CELL + @ 0<> and ;

public

\ A recovering reader takes E-COMP-RECOVERY for a rejected or uncheckable body
\ without does>, a recovery-row binding, or a rejected TRUSTED: row.
: VERDICT-CK ( -- )
   TRUSTED? 0= if M-VERDICT @ -1 <> if
      M-DOES @ 0= M-VERDICT @ 0 = M-VERDICT @ 1 = or and if
         RECOVERING? if E-COMP-RECOVERY throw then
      then
      E-COMP-VERDICT throw
   then then
   RECOVERING? if
      RECOVERY-USED? M-HEAD-RECOVERY @ or
      if E-COMP-RECOVERY throw then
      M-ROW-REJECTED @ if E-COMP-RECOVERY throw then
   then ;

private

\ ---- which word the source published -----------------------------------------
\ The pending record is the unpublished slot the count still points at, which is
\ exactly the slot publish.f will commit.
: REC-INDEX ( -- n )
   ndict@ ;

EXPORT CHECKER-RESOLVE:HEAD-ROW

: QUALIFIED-RECORD-NAME? ( ^n str n -- bool )
   {: rec:^ a:^ u:n split:n :}
   rec  a split 1+ ZPTR+  u split - 1-  XREF-MATCH? 0= if false exit then
   a split HEAD-ROW
   dup XREF-FOUND? 0= if drop false exit then
   XREF-PKG-PUBLIC  rec XREF-WORDLIST  = ;

: RECORD-NAME? ( ^n str -- bool )
   {: rec:^ a:^ u:n :}
   a u XREF-QUAL-INDEX {: split:n :}
   split 0 < if rec a u XREF-MATCH? exit then
   rec a u split QUALIFIED-RECORD-NAME? ;

\ The compiler owns one pending record, so the dictionary count must not move.
: PUBLISHED-NONE ( n -- ) {: before:n :}
   ndict@ before <> if E-COMP-NAME throw then ;

public

: SOURCE-PUBLICATION-CK ( n -- ) {: before:n :}
   before PUBLISHED-NONE ;

\ The tape chose the pending name before a later tentative record could overwrite
\ its slot. An ordinary record stores a bare tail even when the source token was
\ qualified, so that spelling also has to name the namespace whose public WID
\ owns the record. Matching only the tail would let another package impersonate
\ this definition.
: RECORD-NAME-CK ( -- )
   REC-INDEX XREF-REC  NAME$  RECORD-NAME? 0= if
      E-COMP-NAME throw
   then ;

\ ---- what the definition takes and leaves ------------------------------------
\ The arity is asked with the definition's source spelling, NAME$: bare for an
\ author's private definition, qualified when a public definition names a
\ different wordlist. A TRUSTED: effect is registered under that same spelling.
\
\ ARITY-NONE is refused here. Only a definition with no recordable
\ declaration reaches the refusal (no signature, an unparsable one, or one with
\ a scope scheme): CHECK:DEFINITION has refused every uncertified body. After a
\ certified scan nothing reaches it either: a package opened and closed by the source is
\ E-COMP-NAME, an unsigned body answers its inferred effect, and one whose
\ effect is unrecorded (W-EFFECT-NOT-RECORDED) answers the checker's
\ unrecorded input and output cells; no row carries them for callers.
: ARITY-CK ( -- )
   NAME$ NDICT:SPELL-ARITY drop NDICT:ARITY-NONE <> if exit then
   CHECK-UNRECORDED-DIN 0 <  CHECK-UNRECORDED-DOUT 0 <  or
   if E-COMP-ARITY throw then ;

private

\ ---- publication and refusal -------------------------------------------------
\ Room for the offsets is made before the commit, so nothing past it throws.
: KEEP-OFFSETS ( -- )
   NEMIT:FUNCTIONS {: n:n :}
   n PUB-OFF-RESERVE
   n 0 ?do i NEMIT:FUNCTION-OFFSET@ i PUB-OFF ! loop
   n PUB-FUNS ! ;

public

: PUBLISH-IT ( -- )
   KEEP-OFFSETS
   M-PUB @ PUB-RAW = if NPUB:PUBLISH-RAW exit then
   M-PUB @ PUB-DEFER = if M-FIXED-VAL @ NPUB:PUBLISH-PENDING-DEFER exit then
   M-DOES @ 0<> if
      M-DOES-FUN @ NEMIT:FUNCTION-OFFSET@ M-DOES-OFF !
      M-DOES-FUN @ NPUB:PUBLISH-PENDING-DOES
      0 M-DOES-FRAME ! exit
   then
   NPUB:PUBLISH-PENDING ;

private

\ Drops a refused definition's rows from M-ROWS on, or its does> frame, so a
\ redefinition is no duplicate.
: RETRACT ( -- )
   M-DOES @ 0<> if
      M-DOES-FRAME @ 0<> if
         NULL-PTR 0 false DOES-PUBLISHED
         0 M-DOES-FRAME !
      then
      exit
   then
   M-ROWS @ CHECKER-RETRACT-ROWS ;

EXPORT POINTER-STORAGE:RESERVED-PTR-U8-CELL
TKA-CELL RESERVED-PTR-U8-CELL READ-AT

public

\ The record entering at xt has no host code: a cross-build's host routine for
\ it ends here, placed at the token the interpreter read last.
: TARGET-ERROR ( n -- )
   XREF:ENTRY-REC {: r:n :}
   r -1 = if E-COMP-STATE throw then
   r XREF-REC XREF-NAME$ {: na:^ nu:n :}
   s" E-TARGET-WORD" REJECTION:OPEN
   s" a build cannot run target word `" REJECTION:EMIT-STR
   na nu REJECTION:EMIT-STR  96 REJECTION:EMIT1
   SRCLOC:TOKEN$ REJECTION:AT  s" runs a target word" REJECTION:EMIT-STR
   REJECTION:HELP
   s" define `" REJECTION:EMIT-STR  na nu REJECTION:EMIT-STR
   s" ` in a `host` section if the build runs it, or call it only from target words"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The `;` that ends the definition: the token the engine read last, whose
\ address in the input places a refusal there on its line.
: CLOSE$ ( -- str ) READ-AT @ data-base TKL-CELL + @ ;

private

\ A recovery-bound head's scan marks its record (checker.f RECOVERY-RECORD)
\ unless deferred; a TRUSTED: head's newer TRUST-DECL row has none.
: HEAD-MARKED? ( -- bool )
   M-HEAD-RECOVERY @ TRUSTED? 0= and ;

public

\ Rolls back a unit whose compile ended with code rc, and throws nothing: the
\ compile throws rc in its catch's own unit, which keeps a carried record.
: SETTLE-UNIT ( n -- )
   {: rc:n :}
   rc E-COMP-RECOVERY = if
      \ A HEAD-MARKED? definer keeps its rows, as a withheld colon word does;
      \ every other withheld definer is rolled back.
      M-DOES-FRAME @ 0<> if
         NULL-PTR 0 HEAD-MARKED? DOES-PUBLISHED
         HEAD-MARKED? if CHECKER-NATIVE-DOES-COMMIT then
         0 M-DOES-FRAME !
      then
      exit
   then
   rc 0 <> if RETRACT then ;

: STAGE-UNIT ( str -- )
   {: sa su:n :}
   0 CHECKER-OWNER:BIND-REGIME!
   sa M-SRC ! su M-SRC-U !
   0 M-VERDICT !
   false M-ROW-REJECTED !
   CHECKER-ROWS-END M-ROWS !
   0 M-DOES-FRAME !
   DOES-BYTE@ M-DOES !
   DOES-SIG-FIELD @ M-DOES-SIG !
   data-base TCSIG-U-CELL + @ M-DOES-SIG-U !
   false M-HEAD-RECOVERY !
   -1 M-DOES-FUN !
   0 M-DOES-OFF !
   PUB-PENDING M-PUB !
   0 M-FIXED-VAL !
   0 NAME-U ! ;

seal-package

;package
