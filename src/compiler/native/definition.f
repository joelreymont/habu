\ definition.f - the compiler-neutral step of compiling one pending definition.
\
\ Every native compiler compiles a pending definition the same way around its
\ own lowering: stage the unit, scan the body or its `does>` split through the
\ checker owner, enforce the verdict, record the row a scan did not certify,
\ check the recorded name and arity, state the root's host facts, publish the
\ emission by kind, and on a refusal report it and retract the rows the
\ definition recorded. This package holds that step and the state it reads,
\ including the pending callback stub: CALLBACK-STUB! makes the next
\ definition a callback accessor, whose seal appends the stub.
\ Tier 1 (src/compiler/native/compiler.f, NCOMP) calls it around its tape,
\ elaborator and backend. It requires no tier-1 package (tape, feed, elaborator,
\ HIR, backend), so tier 0 (src/habu/t0.f) can require it on every target.

require src/lib/prelude.f
require src/lib/errors.f
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

\ CERTIFICATION IS THE ENGINE'S HOOK CELL, AND THE SCAN IS THE OWNER'S.
\ Two questions were one call before this: `LOWER-CERT-HOOK:HOOK` by name both
\ scanned the source (the tape is filled from inside a scan) and enforced a
\ verdict, and it did so in the checker instance this file was compiled into.
\ Tier 0 reads the hook CELL per definition, which is why `0 set-check` takes
\ certification off it and installing another hook moves it; tier 1 could not be
\ moved at all.
\
\ So: a hook in the cell owns the verdict, and it is the live checker's own hook
\ that scans. With the cell EMPTY an ordinary definition is published
\ uncertified, as tier 0 does under `0 set-check`. The owner's scan still fills
\ the tape and reports its verdict. A native build requires that verdict while
\ LOGICAL-RESET has cleared the hook and the target's check-hook.f is not loaded.
\ The hook cell stores a raw execution token; this view states the scan's signature.
CAST: AS-HOOK ( n -- [ ptr u8 n -- n ] )

\ Whether anything is certifying at all. With the hook cell empty nothing is, and
\ ordinary compilation reports the scan's verdict without enforcing it:
\ tier 0 publishes such a definition uncertified with its declaration as its row
\ (habu2.f EM-COMPILE-PUBLISH-HOOKED reads HOOK-CELL), and the two tiers have to
\ agree (DECLARE-HOOKLESS). A build requires a successful scan before recording
\ that unjudged row; ordinary `0 set-check` sessions retain their declared row.
: CERTIFYING? ( -- bool )
   check@ 0 <> ;

70 constant RC-REJECT                 \ the check hook's reject status (check-hook.f CHECK-RC)

\ The scan with the hook cell empty. Ordinary compilation does not enforce its
\ verdict: a body it did not certify compiles against its declaration
\ (DECLARE-HOOKLESS). A build-required scan refuses before that declaration is
\ recorded. KEEP-ARITY refuses over an ordinary verdict only when no
\ declaration could be recorded.
\ The reason is printed now, while the owner still holds the scan that found it:
\ a does> head's scan is replaced by its clause's before KEEP-ARITY runs.
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

public

\ The `does>` split's head: its scan and capture. The caller then advances its
\ own record past the `does>` token and checks the clause (CHECK-DOES-CLAUSE).
: CHECK-DOES-HEAD ( -- )
   CHECKER-OWNER:DOES-BEGIN
   1 M-DOES-FRAME !
   M-DOES @ {: cut:n :}
   M-SRC @ cut 6 - CHECK-PARENT
   TRUSTED? CERTIFYING? 0= or if drop else -1 <> if E-NCOMP-VERDICT throw then then
   CHECKER-OWNER:RECOVERY-USED? M-HEAD-RECOVERY !   \ the clause's scan starts its own
   M-SRC @ cut 6 - 0 NFETCH:CAPTURE ;

\ The `does>` clause, named for a refusal by the unit's name.
: CHECK-DOES-CLAUSE ( ptr u8 n -- n )
   {: na:ptr nu:n :}
   M-DOES @ {: cut:n :}
   M-SRC @ cut +  M-SRC-U @ cut -  M-DOES-SIG @ M-DOES-SIG-U @
   CHECKER-OWNER:DOES-CHECK na nu CLAUSE-CK
   M-SRC @ cut + M-SRC-U @ cut - cut NFETCH:CAPTURE
   CHECKER-OWNER:DOES-IN M-DOES-IN !
   CHECKER-OWNER:DOES-OUT M-DOES-OUT !
   M-DOES-IN @ 0 < M-DOES-OUT @ 0 < or if E-NCOMP-ARITY throw then
   \ WHERE THE CLAUSE'S VALUES END, read here because the checker captured the
   \ clause's rows for this split and the next one replaces them. A row wider
   \ than a cell is placed from these boundaries exactly as a definition's own
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
\ THE CHECKER'S ANSWER AND NOT THE CALLER'S. Every callee's arity already comes
\ from src/compiler/native/dict.f at the point it is used, and this is the same
\ reader asked about the definition being compiled - so the routine's contract
\ and every call to it are derived from one authority instead of agreeing by
\ luck. It asks for CELLS, which is what NELAB:COLON checks the body's value
\ vector against; a term of a family more than one cell wide makes the two
\ counts differ, and dict.f EFF-CELLS is where that choice is stated.
\
\ ASKED WITH THE SOURCE SPELLING kept by KEEP-TAPE-NAME: bare for an
\ author's private definition, qualified when a public definition names a
\ different wordlist. A TRUSTED: effect is registered under that same spelling
\ after the scan, so a family-typed qualified effect has one owner here.
\
\ THE ABSENT ANSWER. SPELL-ARITY answers ARITY-NONE for a name the checker holds
\ no effect for, and it is refused here rather than let through to NELAB:COLON
\ as a -1, which would refuse it as E-NELAB-ARITY and name the wrong thing. With
\ a check hook installed nothing reaches it: E-NCOMP-VERDICT has refused every
\ body the check did not certify. With the cell empty nothing has
\ (CHECK-HOOKLESS), and a body the scan rejects or cannot check compiles
\ against its declaration, which DECLARE-HOOKLESS recorded as its row. Only a
\ definition with no recordable declaration reaches the refusal: no signature,
\ one the checker cannot parse, or one with a scope scheme. The scan has printed
\ why, so the refusal takes the check hook's reject status. After a certified
\ scan nothing reaches it either: a package opened and closed by the source is
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
      M-DOES-FUN @ NPUB:PUBLISH-PENDING-DOES exit
   then
   NPUB:PUBLISH-PENDING ;

private

\ A refusal after the scan owns the checker row this definition recorded: a
\ certified scan's, a TRUSTED: declaration's, and with the hook cell empty its
\ declared row or its multi-error recovery fact. The definition is rolled back,
\ so its row goes too, or the next definition of the name would be a duplicate.
\ Whichever path recorded it, the refusal puts the store back as it stood at
\ STAGE-UNIT, when the body reached the compiler (M-ROWS): the owner truncates
\ it to that end (checker.f CHECKER-RETRACT-ROWS). Until the caller opens the
\ compile window, after RECORD returns (DECLARE-ROW and NFEED:RECORD-WINDOW's
\ observers run before it), only the definition records rows, and from then to
\ publication the window admits only the compiler's own writes, so the cut takes
\ exactly the definition's rows: a callback (a backend pass, an observer) that
\ would record one while the definition compiles is refused E-NCOMP-STATE. The
\ caller owes that window: it opens CHECKER-OWNER:WRITE-WINDOW after VERDICT-CK
\ and before anything it runs can call back (NCOMP WORK). Nothing goes when
\ nothing was recorded, as when its declaration did not parse or had a scope
\ scheme, and the refusal is its own for a catch to receive, TRUSTED: or not.
\ Every row recorded before STAGE-UNIT stays: a twin's in another scope, one
\ CHECK! recorded under its own name, and one a parsing immediate recorded as
\ the body was read. No word outlives the cut pointing at a cut row: while the
\ definition is pending, source a callback evaluates is its body's capture
\ (definers.f COMPILING?), where a definer defines nothing and a `;` is refused
\ (rc 74). Under a hook a scan that did not certify was refused before this, and
\ the checker rolled it back itself, so the cut finds nothing to take. A does>
\ definer rolls back through its own checker frame (DOES-FINISH).
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
