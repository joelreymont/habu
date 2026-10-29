\ definition.f - the compiler-neutral step around one pending definition's
\ lowering: staging, the checker scan and verdict, name and arity checks,
\ publication by kind, the pending callback stub, and retraction on refusal.
\ Tier 1 (src/compiler/native/compiler.f, NCOMP) and tier 0 (src/habu/t0.f)
\ both call it. It requires no tier-1 package (tape, feed, elaborator, HIR,
\ backend), so tier 0 can require it on every target.

require src/lib/prelude.f
require src/lib/errors.f
require src/lib/string.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/workspace.f
require src/compiler/native/checker-owner.f
require src/compiler/native/host.f
require src/compiler/native/dict.f
require src/compiler/native/fetch.f
require src/compiler/native/emission.f
require src/compiler/native/publish.f
require src/compiler/native/callback-stub.f

package NDEFINITION

public

\ Everything the run needs, parked: a quotation cannot read the enclosing word's
\ locals and the whole run is one quotation.
0 CWORK-VARIABLE M-IN n
0 CWORK-VARIABLE M-OUT n
0 CWORK-VARIABLE M-VERDICT n                   \ the verdict the recorded scan reached
0 CWORK-VARIABLE M-REQUIRED bool                  \ this definition's build-required hookless scan
0 CWORK-VARIABLE M-DOES-FRAME n                \ checker-owned transaction spans a split compilation
0 CWORK-VARIABLE M-DOES n                      \ byte split after `does> `, or zero
0 CWORK-VARIABLE M-DOES-SIG ptr u8
0 CWORK-VARIABLE M-DOES-SIG-U n
0 CWORK-VARIABLE M-DOES-IN n
0 CWORK-VARIABLE M-DOES-OUT n
0 CWORK-VARIABLE M-DOES-GIN n                  \ where the clause's argument row's values end
0 CWORK-VARIABLE M-DOES-GOUT n                 \ where the clause's result row's values end
0 CWORK-VARIABLE M-DOES-FUN n                  \ hidden clause function ordinal
0 CWORK-VARIABLE M-DOES-OFF n                  \ published clause's byte offset, or zero
0 CWORK-VARIABLE M-FIXED-VAL n                  \ the pending definer's value or dispatch cell
0 CWORK-VARIABLE M-OPEN n                      \ a compilation is running

\ The bytes from a `does>` keyword's start to its clause: the keyword and the
\ blank M-DOES points just past.
6 constant DOES-SPAN

\ The definition's name is its source spelling, whole, which stays put until the
\ next definition is recorded: no copy that a longer name could run past.
0 CWORK-VARIABLE NAME-A ptr u8
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

0 CWORK-VARIABLE M-SRC ptr u8
0 CWORK-VARIABLE M-SRC-U n
0 CWORK-VARIABLE M-UNJUDGED n                  \ a hook-cell-empty scan's verdict, else -1
0 CWORK-VARIABLE M-ROWS n                      \ where the checker store ended at STAGE-UNIT
0 CWORK-VARIABLE M-HEAD-RECOVERY bool          \ a split definition's head bound a recovery row
-1 CWORK-VARIABLE M-CB-SLOT n                   \ -1 outside a callback accessor definition
0 CWORK-VARIABLE M-CB-THUNK n
0 CWORK-VARIABLE M-CB-ENTRY n

public

: SRC$ ( -- ptr u8 n )
   M-SRC @ M-SRC-U @ ;

: DOES-BYTE@ ( -- n )
   data-base DOESB-CELL + @ ;

: NAME$ ( -- ptr u8 n )
   NAME-A @ NAME-U @ ;

\ A compiler entry reached while a compilation runs would stage one definition
\ over another's state.
: IDLE-CK ( -- )
   M-OPEN @ 0<> if E-NCOMP-STATE throw then ;

: STUB-PENDING? ( -- bool )
   M-CB-SLOT @ 0 >= ;

\ The next definition is an accessor that owns one anonymous C entry after
\ its ordinary return. The entry is published with that definition's emission.
: CALLBACK-STUB! ( n n -- ) {: k:n thunk:n :}
   IDLE-CK
   STUB-PENDING? if E-NCOMP-STATE throw then
   k M-CB-SLOT !  thunk M-CB-THUNK !  0 M-CB-ENTRY ! ;

: CALLBACK-STUB@ ( -- n )
   M-CB-ENTRY @ dup 0= if E-NCOMP-STATE throw then ;

\ A compiler arms NCBSTUB before it seals the accessor's emission, takes the
\ stub's placed entry after, and ends the pending stub when the compile ends.
: STUB-ARM ( -- )
   STUB-PENDING? if M-CB-SLOT @ M-CB-THUNK @ NCBSTUB:SET then ;

: STUB-TAKE ( -- )
   STUB-PENDING? if NCBSTUB:ENTRY@ M-CB-ENTRY ! then ;

: STUB-END ( -- )
   -1 M-CB-SLOT ! ;

private

: DOES-SIG-FIELD ( -- ptr ptr u8 )
   data-base TCSIG-A-CELL CELL / ptr-field ;

: TRUSTED? ( -- bool )
   data-base TRUSTED-CELL + @ 0<> ;

: TRUST-SIG-A-FIELD ( -- ptr ptr u8 )
   data-base TSIG-A-CELL CELL / ptr-field ;

: TRUST-SIG$ ( -- ptr u8 n )
   TRUST-SIG-A-FIELD @  data-base TSIG-U-CELL + @ ;

\ TRUST-DECL is deliberately unavailable to checked code. Register only after
\ the scan has recorded the exact source spelling from the tape: the pending
\ dictionary record holds only the bare tail of a qualified definition.
TRUSTED: REGISTER-TRUST ( ptr u8 n ptr u8 n -- )
   CHECKER-OWNER:DECLARED-EFFECT ;

\ Certification is the engine's hook cell; the scan is the owner's. A hook in
\ the cell owns the verdict, and the live checker's own hook scans. Tier 0
\ reads the cell per definition, so `0 set-check` takes certification off and
\ installing another hook moves it. With the cell empty an ordinary definition
\ is published uncertified; the owner's scan still fills the tape and reports
\ its verdict. A native build requires that verdict while LOGICAL-RESET has
\ cleared the hook and the target's check-hook.f is not loaded.
\ The hook cell stores a raw execution token; this view states the scan's signature.
CAST: AS-HOOK ( n -- [ ptr u8 n -- n ] )

\ The two tiers agree on the empty cell: tier 0 also publishes the definition
\ uncertified with its declaration as its row (habu2.f EM-COMPILE-PUBLISH-HOOKED
\ reads HOOK-CELL).
: CERTIFYING? ( -- bool )
   check@ 0 <> ;

70 constant RC-REJECT                 \ the check hook's reject status (check-hook.f CHECK-RC)

\ A build-required scan refuses before the declaration is recorded; otherwise
\ KEEP-ARITY refuses only when no declaration could be recorded. The reason is
\ printed here because a does> head's scan is replaced by its clause's before
\ KEEP-ARITY runs.
: CHECK-HOOKLESS ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u CHECKER-OWNER:CHECK-UNJUDGED {: v:n :}
   v -1 <> if CHECKER-OWNER:REPORT then
   M-REQUIRED @ v -1 <> and if RC-REJECT throw then
   v M-UNJUDGED ! ;

: CHECK-PARENT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   CHECKER-OWNER:RESET-REPORT
   TRUSTED? if -1 CHECKER-OWNER:BIND-REGIME!
      a u CHECKER-OWNER:CHECK-UNJUDGED exit then
   check@ {: hook:n :}
   hook 0= if -1 CHECKER-OWNER:BIND-REGIME!
      a u CHECK-HOOKLESS -1 exit then
   0 CHECKER-OWNER:BIND-REGIME!
   a u hook AS-HOOK execute ;

public

: CHECK-SOURCE ( -- n )
   SRC$ CHECK-PARENT
   SRC$ 0 NFETCH:CAPTURE ;

private

\ A refused clause is reported as the check hook reports a refused body: the
\ checker's reason, then the hook's reject status, which the run names the
\ definition under. The clause is named by the caller, from its open unit.
: CLAUSE-CK ( n ptr u8 n -- )
   CHECKER-OWNER:DOES-REPORT -1 <> if RC-REJECT throw then ;

\ The engine records the split just past the `does> ` it consumed, so a cut
\ that is not just past a `does> ` in this source means the source and the
\ pending definition disagree.
: DOES-CUT-CK ( -- )
   M-SRC @ {: src:ptr :}
   M-DOES @ {: cut:n :}
   cut DOES-SPAN < cut M-SRC-U @ > or if E-NFEED-CUT throw then
   src cut DOES-SPAN - + DOES-SPAN 1- s" does>" STR=CI 0= if E-NFEED-CUT throw then
   src cut 1- + c@ 32 <> if E-NFEED-CUT throw then ;

public

\ The `does>` split's head. The caller advances its own record past the
\ `does>` token and checks the clause (CHECK-DOES-CLAUSE).
: CHECK-DOES-HEAD ( -- )
   DOES-CUT-CK
   CHECKER-OWNER:DOES-BEGIN
   1 M-DOES-FRAME !
   M-DOES @ {: cut:n :}
   M-SRC @ cut DOES-SPAN - CHECK-PARENT
   TRUSTED? CERTIFYING? 0= or if drop else -1 <> if E-NCOMP-VERDICT throw then then
   CHECKER-OWNER:RECOVERY-USED? M-HEAD-RECOVERY !   \ the clause's scan starts its own
   M-SRC @ cut DOES-SPAN - 0 NFETCH:CAPTURE ;

\ The `does>` clause, named for a refusal by the unit's name. The checker's
\ token ordinal first steps past the `does>` keyword between the two bodies.
: CHECK-DOES-CLAUSE ( ptr u8 n -- n )
   {: na:ptr nu:n :}
   CHECKER-OWNER:TAPE-ADVANCE
   M-DOES @ {: cut:n :}
   M-SRC @ cut +  M-SRC-U @ cut -  M-DOES-SIG @ M-DOES-SIG-U @
   CHECKER-OWNER:DOES-CHECK na nu CLAUSE-CK
   M-SRC @ cut + M-SRC-U @ cut - cut NFETCH:CAPTURE
   CHECKER-OWNER:DOES-IN M-DOES-IN !
   CHECKER-OWNER:DOES-OUT M-DOES-OUT !
   M-DOES-IN @ 0 < M-DOES-OUT @ 0 < or if E-NCOMP-ARITY throw then
   \ Where the clause's values end, read here because the checker captured
   \ the clause's rows for this split and the next one replaces them. A row
   \ wider than a cell is placed from these boundaries as a definition's own
   \ row is; a row the checker could not separate into cells answers
   \ GLUE-UNKNOWN and the elaborator refuses the definer over it.
   NDICT:DOES-GLUE M-DOES-GOUT ! M-DOES-GIN !
   -1 ;

private

\ With the hook cell empty a definition's row is its declaration, as at tier 0
\ (checker.f CHECKER-DECLARED-ROW!): recorded after the scan, its report and the
\ capture, whose state the recorder would clobber, and before KEEP-ARITY, which
\ then answers from it. The scan's own row - certified, deferred or a multi-error
\ recovery fact - wins, since the recorder skips a symbol a live row states. A
\ definition with no signature records nothing, as at tier 0.
: DECLARE-HOOKLESS ( -- )
   TRUST-SIG$ {: sa:ptr su:n :}
   su 0= if exit then
   NAME$ sa su CHECKER-OWNER:DECLARED-ROW ;

public

\ The row a scan that did not certify leaves: a TRUSTED: declaration, or with
\ the hook cell empty the declared one. Run once the scan has named the unit.
: DECLARE-ROW ( -- )
   TRUSTED? if NAME$ TRUST-SIG$ REGISTER-TRUST exit then
   CERTIFYING? 0= if DECLARE-HOOKLESS then ;

private

\ A multi-error run whose reader recovers from E-NCOMP-RECOVERY: that reader
\ sets RECOVER-CELL around the compile it calls (layout.f). Under any other
\ reader a refusal is E-NCOMP-VERDICT, which SETTLE-UNIT reports.
: RECOVERING? ( -- bool )
   CHECKER-OWNER:MULTI-ERROR?
   data-base NCOMP-DISPATCH:RECOVER-CELL + @ 0<> and ;

public

\ Under a recovering reader a definition the checker refused, or any
\ definition, trusted or not, whose body or does> clause bound a refused
\ declaration's recovery row, is counted and abandoned unpublished:
\ E-NCOMP-RECOVERY ends it without a second report.
: VERDICT-CK ( -- )
   TRUSTED? 0= CERTIFYING? and if M-VERDICT @ -1 <> if
      M-DOES @ 0= M-VERDICT @ 0 = M-VERDICT @ 1 = or and if
         RECOVERING? if E-NCOMP-RECOVERY throw then
      then
      E-NCOMP-VERDICT throw
   then then
   RECOVERING? if
      CHECKER-OWNER:RECOVERY-USED? M-HEAD-RECOVERY @ or if E-NCOMP-RECOVERY throw then
   then ;

private

\ ---- which word the source published -----------------------------------------
\ The pending record is the unpublished slot the count still points at, which is
\ exactly the slot publish.f will commit.
: REC-INDEX ( -- n )
   ndict@ ;

: QUALIFIED-RECORD-NAME? ( ptr n ptr u8 n n -- bool )
   {: rec:ptr a:ptr u:n split:n :}
   rec  a split 1+ ZPTR+  u split - 1-  XREF-MATCH? 0= if false exit then
   a split XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? 0= if drop false exit then
   XREF-PKG-PUBLIC  rec XREF-WORDLIST  = ;

: RECORD-NAME? ( ptr n ptr u8 n -- bool )
   {: rec:ptr a:ptr u:n :}
   a u XREF-QUAL-INDEX {: split:n :}
   split 0 < if rec a u XREF-MATCH? exit then
   rec a u split QUALIFIED-RECORD-NAME? ;

\ The compiler owns one pending record, so the dictionary count must not move.
: PUBLISHED-NONE ( n -- ) {: before:n :}
   ndict@ before <> if E-NCOMP-NAME throw then ;

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
      E-NCOMP-NAME throw
   then ;

\ ---- what the definition takes and leaves ------------------------------------
\ The arity is the checker's answer, from the same dict.f reader every callee's
\ arity comes from, so the routine's contract and its calls share one
\ authority. It asks for cells, what NELAB:COLON checks the body's value vector
\ against; dict.f EFF-CELLS states the width rule.
\
\ It is asked with the source spelling KEEP-TAPE-NAME kept: bare for an
\ author's private definition, qualified when a public definition names a
\ different wordlist. A TRUSTED: effect is registered under that same spelling.
\
\ ARITY-NONE is refused here: passed on as -1, NELAB:COLON would refuse it as
\ E-NELAB-ARITY and name the wrong thing. Only a definition with no recordable
\ declaration reaches the refusal (no signature, an unparsable one, or one with
\ a scope scheme): a hook has refused every uncertified body, and with the cell
\ empty DECLARE-HOOKLESS recorded the declaration. The scan has printed why, so
\ the refusal takes the check hook's reject status. After a certified scan
\ nothing reaches it either: a package opened and closed by the source is
\ E-NCOMP-NAME and an unsigned body answers its inferred effect.
: KEEP-ARITY ( -- )
   NAME$ NDICT:SPELL-ARITY {: din:n dout:n :}
   din NDICT:ARITY-NONE = if
      M-UNJUDGED @ -1 <> if RC-REJECT throw then
      E-NCOMP-ARITY throw
   then
   din M-IN !  dout M-OUT ! ;

private

: HOST-DECL? ( -- bool )
   NAME$ CHECKER-OWNER:QUERY 0= if false exit then
   CHECKER-OWNER:STACK-STABLE? 0= if false exit then
   CHECKER-OWNER:DIN-N M-IN @ <>
   CHECKER-OWNER:DOUT-N M-OUT @ <> or if false exit then
   CHECKER-OWNER:DIN-CELLS M-IN @ <>
   CHECKER-OWNER:DOUT-CELLS M-OUT @ <> or if false exit then
   M-IN @ 0 ?do
      i CHECKER-OWNER:DIN-SLOT 0<>
      i CHECKER-OWNER:DIN-CON 0 <= or if false unloop exit then
   loop
   M-OUT @ 0 ?do
      i CHECKER-OWNER:DOUT-SLOT 0<>
      i CHECKER-OWNER:DOUT-CON 0 <= or if false unloop exit then
   loop
   true ;

: HOST-ROOT-CONTRACT ( -- )
   HOST-DECL? 0= if exit then
   NHOST:SOURCE-CONTRACT-START
   M-IN @ 0 ?do i i CHECKER-OWNER:DIN-CON NHOST:SOURCE-CONTRACT-IN loop
   M-OUT @ 0 ?do i i CHECKER-OWNER:DOUT-CON NHOST:SOURCE-CONTRACT-OUT loop
   NHOST:SOURCE-CONTRACT-DONE ;

public

\ The arity of each of the definition's functions, root first. A `does>`
\ clause's is the split's; every other function past the root is a quotation,
\ whose arity the compiler that built it answers by function ordinal.
: HOST-ARITIES ( n [ n -- n n ] -- )
   {: funs:n q :}
   M-IN @ M-OUT @ NHOST:SOURCE-ARITY+
   HOST-ROOT-CONTRACT
   funs 1 ?do
      M-DOES @ 0<> i M-DOES-FUN @ = and if
         M-DOES-IN @ M-DOES-OUT @
      else
         i q execute
      then
      NHOST:SOURCE-ARITY+
   loop ;

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

\ A refusal after the scan truncates the checker store to M-ROWS, its end at
\ STAGE-UNIT (checker.f CHECKER-RETRACT-ROWS): the definition is rolled back, so
\ its row goes too, or the next definition of the name would be a duplicate.
\ Nothing goes when nothing was recorded (a declaration that did not parse or
\ had a scope scheme), and the refusal keeps its own code for a catch, TRUSTED:
\ or not.
\ The cut takes exactly the definition's rows because the caller opens
\ CHECKER-OWNER:WRITE-WINDOW after VERDICT-CK and before anything it runs can
\ call back (NCOMP WORK); inside the window a callback that would record a row
\ is refused E-NCOMP-STATE. Rows recorded before STAGE-UNIT (a twin in another
\ scope, a CHECK! row, a parsing immediate's) stay. No word outlives the cut
\ pointing at a cut row: source a callback evaluates while the definition is
\ pending is its body's capture (definers.f COMPILING?), where a `;` is refused
\ (rc 74). Under a hook a scan that did not certify was refused before this and
\ the checker rolled it back itself, so the cut finds nothing. A does> definer
\ rolls back through its own checker frame (DOES-FINISH).
: RETRACT ( -- )
   M-DOES @ 0<> if
      M-DOES-FRAME @ 0<> if
         NULL-PTR 0 false CHECKER-OWNER:DOES-FINISH
         0 M-DOES-FRAME !
      then
      exit
   then
   M-ROWS @ CHECKER-OWNER:RETRACT-ROWS ;

: ERROR-TEXT ( ptr u8 n -- ) {: a:ptr u:n :}
   2 a u write drop ;

\ The compiler's refusal names the token it stopped at.
: REPORT-FAILURE ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" ncomp: cannot compile " ERROR-TEXT
   NAME$ ERROR-TEXT
   u 0 > if s"  at " ERROR-TEXT a u ERROR-TEXT then
   S\" \n" ERROR-TEXT ;

: REPORT-UNDEFINED-TICK ( ptr u8 n -- )
   s" E-UNDEFINED: " ERROR-TEXT
   ERROR-TEXT
   S\" \n" ERROR-TEXT ;

public

\ How a run ends, from the code its entry threw, the code the compilation
\ reached, whether that refusal is a tick whose operand no record answers, and
\ the token the compiler refused at.
: SETTLE-UNIT ( n n bool ptr u8 n -- )
   {: entry-rc:n rc:n undefined:bool a:ptr u:n :}
   entry-rc 0<> if
      M-DOES-FRAME @ 0<> if NULL-PTR 0 false CHECKER-OWNER:DOES-FINISH then
      entry-rc throw
   then
   rc E-NCOMP-RECOVERY = if rc throw then
   undefined if
      a u REPORT-UNDEFINED-TICK
      RETRACT RC-REJECT throw
   then
   rc 0 <> if
      rc RC-REJECT <> CHECKER-OWNER:JSON-REPORTED? 0= or
      if a u REPORT-FAILURE then
      RETRACT rc throw
   then ;

\ The unit's state when its body reaches the compiler.
: STAGE-UNIT ( ptr u8 n bool -- )
   {: sa su:n required:bool :}
   required M-REQUIRED !
   0 CHECKER-OWNER:BIND-REGIME!
   NFETCH:RELEASE
   sa M-SRC ! su M-SRC-U !
   0 M-IN ! 0 M-OUT !
   TRUSTED? CERTIFYING? 0= or NHOST:SOURCE-BEGIN
   0 M-VERDICT !
   -1 M-UNJUDGED !
   CHECKER-OWNER:ROWS-END M-ROWS !
   0 M-DOES-FRAME !
   DOES-BYTE@ M-DOES !
   DOES-SIG-FIELD @ M-DOES-SIG !
   data-base TCSIG-U-CELL + @ M-DOES-SIG-U !
   0 M-DOES-IN ! 0 M-DOES-OUT !
   false M-HEAD-RECOVERY !
   NDICT:GLUE-NONE M-DOES-GIN !  NDICT:GLUE-NONE M-DOES-GOUT !
   -1 M-DOES-FUN !
   0 M-DOES-OFF !
   PUB-PENDING M-PUB !
   0 M-FIXED-VAL !
   0 NAME-U ! ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
