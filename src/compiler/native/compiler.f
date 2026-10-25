\ compiler.f - compile one pending colon definition through the native chain.
\
\ The engine supplies the body captured by its one token reader. The checker's
\ own observer fills the tape while it certifies those bytes, and the compiler
\ commits the pending dictionary record only after native emission validates.
\
\ The NAME is not a parameter: it is read off the recorded tape and checked
\ against the pending record, and a source that published none or several is
\ refused. Neither is the ARITY -
\ dict.f answers what the checker certified this definition takes and leaves, the
\ same reader that answers for every callee, so the routine's contract and the
\ calls to it come from one authority instead of agreeing by luck. Nothing about
\ a callee is a parameter either.
\
\ The engine leaves the definition pending. Only a chain emission that passes
\ every stage publishes it, so a refusal leaves no word behind.
\
\ THE CHAIN, IN ORDER. RECORD opens the source tape (tape.f) that the checker's
\ reader fills (feed.f) and END-RECORDED seals it; the elaborator (elaborate.f)
\ builds the definition's HIR functions from the sealed tape; NBACK:FREEZE folds
\ shared HIR and freezes the module; an open shadow runs its chain from that
\ frozen module; then NBACK:DECLARE, NBACK:SELECT into the target's machine
\ dialect, NBACK:FIXPOINT (allocation, lowering spills until every value fits),
\ NBACK:EMIT into the next code slot, and publication (publish.f).
\
\ NO BACKEND IS NAMED HERE. Every stage that turns an elaborated definition into
\ instructions goes through src/compiler/native/backend.f, at the row the
\ definition's own target contract resolves to, and an architecture with no
\ backend loaded is refused at the declaration before a module is built. The
\ rows are installed by the target's passes module, which LOAD-PASSES below
\ requires when this file loads: src/arch/arm64/passes.f on AArch64 Linux and
\ Darwin, src/arch/x86-64/passes.f on x86-64 Linux.
\
\ A SECOND TARGET RIDES ALONG WHILE A SHADOW IS OPEN (src/compiler/native/
\ shadow.f). After the HIR module is frozen, the same stages run once more for
\ the shadow's binding, in a context nested inside the definition's and bound to
\ it, so every stage resolves that machine's row; its sealed emission is copied
\ into the shadow, and its rows are retired, before that context leaves and
\ before the engine's own selector reads the same frozen module. A refusal on
\ the shadow's side refuses the definition: a cross-build has no use for a word
\ the target cannot have.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/native/fetch-check.f
require src/compiler/ir/symbol.f
require src/compiler/native/checker-owner.f
require src/compiler/native/host.f
require src/compiler/native/abi.f
require src/compiler/native/dict.f
require src/compiler/native/feed.f
require src/compiler/native/elaborate.f
require src/compiler/native/backend.f
require src/compiler/native/publish.f
require src/compiler/native/shadow.f
require src/compiler/native/callback-stub.f
require src/compiler/native/prof.f
require src/compiler/session/lease.f
require src/compiler/session/emission.f

package NCOMP
private

\ The target predicates are read as this file loads: the running engine's, or in
\ a build window the window's own src/os/*/target.f. Each path sits on its own
\ line so tools/manifest-lint.f reads both edges.
: LOAD-PASSES ( -- )
   HB-TARGET-LINUX? if
      s" src/arch/arm64/passes.f" required
      exit
   then
   HB-TARGET-MACOS? if
      s" src/arch/arm64/passes.f" required
      exit
   then
   HB-TARGET-LINUX-X86-64? if
      s" src/arch/x86-64/passes.f" required
      exit
   then
   E-CTGT-ABI throw ;

' LOAD-PASSES
;package
execute

package NCOMP

public
NEWTYPE code-entry

private

CAST: >ENTRY ( n -- code-entry )
CAST: PUBLISHED-XT ( n -- [ n IR-CTX:ctx code-entry n n -- ] )

public
: DECLARE-ENTRY ( ptr code-entry -- )
   0 >ENTRY swap xt! ;

: CLEAR-ENTRY ( ptr code-entry -- )
   0 >ENTRY swap ! ;

private

\ ---- what a recording unit is opened with ------------------------------------
\ The unit's text ceiling is the ENGINE's own body capture, which overflows with
\ an exit rather than a throw. The capture is at least three bytes shorter than
\ its source.
BODYBUF-CAP constant TEXT-CAP

TEXT-CAP CWORK-SPAN TXT u8
\ The definition's name is its spelling in TXT, whole, which stays put until the
\ next definition is recorded: no copy that a longer name could run past.
0 CWORK-VARIABLE NAME-A ptr u8
0 CWORK-VARIABLE NAME-U n

align
1 CWORK-BUFFER M-CTX IR-CTX:ctx
1 CWORK-BUFFER M-BLD IR-BUILD:builder
1 CWORK-BUFFER M-TAPE IR-ARENA:view
1 CWORK-BUFFER M-HM IR-BUILD:module   \ the frozen module every selector reads
1 CWORK-BUFFER M-SH IR-CTX:ctx        \ the shadow's context, nested in CC
0 CWORK-VARIABLE M-LEASE NLEASE:lease
0 CWORK-VARIABLE M-SESSION NSESSION:session
0 CWORK-VARIABLE SH-SESSION NSESSION:session
0 CWORK-VARIABLE M-EMISSION NART:emission

\ Everything the run needs, parked: a quotation cannot read the enclosing word's
\ locals and the whole run is one quotation.
0 CWORK-VARIABLE M-SRC ptr u8
0 CWORK-VARIABLE M-SRC-U n
0 CWORK-VARIABLE M-IN n
0 CWORK-VARIABLE M-OUT n
0 CWORK-VARIABLE PRIOR-ENTRY n
0 CWORK-VARIABLE PRIOR-IN n
0 CWORK-VARIABLE PRIOR-OUT n
0 CWORK-VARIABLE PRIOR-GLUE n
0 CWORK-VARIABLE PRIOR-DEAD bool
0 CWORK-VARIABLE PRIOR-CAST bool
0 CWORK-VARIABLE PRIOR-CALLABLE n
0 CWORK-VARIABLE M-OPEN n                      \ a compilation is running
-1 CWORK-VARIABLE M-CB-SLOT n                   \ -1 outside a callback accessor definition
0 CWORK-VARIABLE M-CB-THUNK n
0 CWORK-VARIABLE M-CB-ENTRY n
0 CWORK-VARIABLE M-RC n                        \ the code the run inside the context reached
0 CWORK-VARIABLE M-PUBLISHED-START n           \ committed parent's index for notification
0 CWORK-VARIABLE M-VERDICT n                   \ the verdict the recorded scan reached
0 CWORK-VARIABLE M-UNJUDGED n                  \ a hook-cell-empty scan's verdict, else -1
0 CWORK-VARIABLE M-REQUIRED bool                  \ this definition's build-required hookless scan
0 CWORK-VARIABLE M-ROWS n                      \ where the checker store ended at STAGE
0 CWORK-VARIABLE M-DOES-FRAME n                \ checker-owned transaction spans a split compilation
0 CWORK-VARIABLE M-DOES n                      \ byte split after `does> `, or zero
0 CWORK-VARIABLE M-DOES-ROW n                  \ the tape row that carries `does>`
0 CWORK-VARIABLE M-DOES-SIG ptr u8
0 CWORK-VARIABLE M-DOES-SIG-U n
0 CWORK-VARIABLE M-DOES-IN n
0 CWORK-VARIABLE M-DOES-OUT n
0 CWORK-VARIABLE M-HEAD-RECOVERY bool          \ a split definition's head bound a recovery row
0 CWORK-VARIABLE M-DOES-GIN n                  \ where the clause's argument row's values end
0 CWORK-VARIABLE M-DOES-GOUT n                 \ where the clause's result row's values end
0 CWORK-VARIABLE M-DOES-FUN n                  \ hidden clause function ordinal
0 CWORK-VARIABLE M-DOES-OFF n                  \ published clause's byte offset, or zero
0 CWORK-VARIABLE DEF-MODULE-BEFORE n
0 CWORK-VARIABLE DEF-GEN-BEFORE n
0 CWORK-VARIABLE DEF-EPOCH-BEFORE n
0 CWORK-VARIABLE DEF-BUILD-BEFORE n
0 CWORK-VARIABLE DEF-MODULE-DELTA n
0 CWORK-VARIABLE DEF-GEN-DELTA n
0 CWORK-VARIABLE DEF-EPOCH-DELTA n
0 CWORK-VARIABLE DEF-BUILD-DELTA n

public
3 constant FIXED-DEFER               \ a defer dispatch body, not an NDICT fixed kind
4 constant FIXED-CAST                \ an engine-published CAST: identity body
private

0 CWORK-VARIABLE M-FIXED n                      \ the pending definer's body kind
0 CWORK-VARIABLE M-FIXED-VAL n                  \ its value or dispatch cell
-1 CWORK-VARIABLE M-FIXED-REC n                 \ published record for a seeded created child, or -1
0 CWORK-VARIABLE M-SHADOW-ONLY bool             \ the engine already published its own body

: CC ( -- IR-CTX:ctx )           0 M-CTX @ ;
: BB ( -- IR-BUILD:builder )     0 M-BLD @ ;
: TAPE ( -- IR-ARENA:view )      0 M-TAPE @ ;
: SH ( -- IR-CTX:ctx )           0 M-SH @ ;
: SS ( -- NSESSION:session ) M-SESSION @ ;
: SHS ( -- NSESSION:session ) SH-SESSION @ ;
: MKEY ( -- IR-ID:ir-module-key ) BB IR-BUILD:MODULE-KEY ;

\ The callable address is the actual committed parent's entry plus this
\ ordinal's sealed emission offset. The callback fills storage reserved while
\ the HIR was borrowed; it must neither allocate nor throw.
: PUBLISHED-ROWS ( n NART:emission -- )
   {: idx:n e:NART:emission :}
   data-base NATIVE-OBS-CELLS:PUBLISHED + @ dup 0= if drop exit then
   PUBLISHED-XT {: q :}
   idx XREF-REC XREF-START >ENTRY {: parent:code-entry :}
   e NART:FUNCTIONS 0 ?do
      idx CC parent i e i NART:FUNCTION-OFFSET@ q execute
   loop ;

: SRC$ ( -- ptr u8 n )
   M-SRC @ M-SRC-U @ ;

: DOES-BYTE@ ( -- n )
   data-base DOESB-CELL + @ ;

: DOES-SIG-FIELD ( -- ptr ptr u8 )
   data-base TCSIG-A-CELL CELL / ptr-field ;

: TRUSTED? ( -- bool )
   data-base TRUSTED-CELL + @ 0<> ;

: TRUST-SIG-A-FIELD ( -- ptr ptr u8 )
   data-base TSIG-A-CELL CELL / ptr-field ;

: TRUST-SIG$ ( -- ptr u8 n )
   TRUST-SIG-A-FIELD @  data-base TSIG-U-CELL + @ ;

: PENDING-NAME$ ( -- ptr u8 n )
   ndict@ XREF-REC XREF-NAME$ ;

\ The pending record is not searchable yet.  Capture the word its bare name
\ denotes while the dictionary and checker still agree about that prior entry.
: READ-PRIOR ( -- )
   PENDING-NAME$ {: a:ptr u:n :}
   a u NDICT:CALL-BINDING {: entry:n kind:n :}
   entry PRIOR-ENTRY !
   entry 0= if exit then
   a u NDICT:SPELL-CALL {: in:n out:n glue:n :}
   in NDICT:ARITY-NONE = if exit then
   glue NDICT:GLUE-UNKNOWN = if exit then
   a u NDICT:SPELL-DEAD? {: dead:bool :}
   in PRIOR-IN !  out PRIOR-OUT !  glue PRIOR-GLUE !  dead PRIOR-DEAD !
   kind DKIND:CAST = PRIOR-CAST !
   1 PRIOR-CALLABLE ! ;

\ A bare name a using refuses is no prior binding: it names no word, and the
\ definition is admitted as at tier 0 (docs/forth.md: shadowing a global from
\ inside a package is legal; the using refusal belongs to a reference). Asked
\ before READ-PRIOR, whose effect query would raise that refusal for the name
\ being defined. A qualified body token remains free to resolve its own
\ unambiguous record after the check.
: KEEP-PRIOR ( -- )
   0 PRIOR-ENTRY !  0 PRIOR-CALLABLE !  false PRIOR-CAST !
   PENDING-NAME$ NDICT:USING-REFUSED? if exit then
   READ-PRIOR ;

\ ---- the module the definition is compiled into ------------------------------
: HIR-MOD ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   b ;

\ Read off the TAPE: the elaborator adds a row per name the body writes that the
\ dialect does not model, and which names those are is not known until it is read.
\ The dialect's own vocabulary is NOT among them: the session registered it once
\ and this definition reads those rows rather than writing them again.
: MODEL-ROWS ( -- n )
   TAPE NTAPE:TOKENS ;

\ The overlay holds only what this definition resolves - its callable and fixed
\ words - and defers every vocabulary spelling to the session's table.
: MODEL ( -- IR-ARENA:arena IR-ARENA:arena )
   \ Each resolved cast needs one identity pick; there cannot be more such
   \ bindings than body tokens. The arena reserves only picks actually used.
   CC BB MODEL-ROWS dup HIR-WORD:NEW-LINKED ;

\ ---- stage N0: the definition the engine compiles ----------------------------
\ Parked rather than left on the stack, because this runs inside the quotation
\ the recovery below catches. The tape's verdict is the scan's; a hook's zero
\ verdict stands over it, so a hook that certifies through the checker and then
\ answers 0 refuses the definition (RECORD), as tier 0 drops it (habu2.f
\ EM-COMPILE-PUBLISH-HOOKED), and the refusal retracts the rows it recorded.
: END-RECORDED ( -- )
   NFEED:END-UNIT {: view:IR-ARENA:view verdict:n :}
   M-VERDICT @ 0<> if verdict M-VERDICT ! then
   view 0 M-TAPE ! ;

\ Row zero's structural span names the exact spelling already copied into TXT.
: TAPE-NAME$ ( -- ptr u8 n )
   TAPE MKEY 0 NTAPE:SPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n u:n :}
   TXT st + u ;

\ The tape's first token is the definition's name. Recover it after sealing even
\ when a rejected second `:` left a tentative spelling in the record slot.
: KEEP-TAPE-NAME ( -- )
   TAPE-NAME$ {: a:ptr u:n :}
   a NAME-A !  u NAME-U ! ;

: NAME$ ( -- ptr u8 n )
   NAME-A @ NAME-U @ ;

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

: CHECK-SOURCE ( -- n )
   SRC$ CHECK-PARENT
   SRC$ 0 NFETCH:CAPTURE ;


\ A refused clause is reported as the check hook reports a refused body: the
\ checker's reason, then the hook's reject status, which RUN names the
\ definition under. NAME$ is set only after the unit seals, so the clause is
\ named from the open unit's first row.
: CLAUSE-CK ( n -- )
   NFEED:UNIT-NAME$ CHECKER-OWNER:DOES-REPORT -1 <> if RC-REJECT throw then ;

\ RECORD opened this unit with NFEED:BEGIN-DOES-UNIT, which refused a cut that
\ is not just past a `does> ` in this source (E-NFEED-CUT).
: CHECK-DOES-SPLIT ( -- n )
   CHECKER-OWNER:DOES-BEGIN
   1 M-DOES-FRAME !
   M-DOES @ {: cut:n :}
   M-SRC @ cut 6 - CHECK-PARENT
   TRUSTED? CERTIFYING? 0= or if drop else -1 <> if E-NCOMP-VERDICT throw then then
   CHECKER-OWNER:RECOVERY-USED? M-HEAD-RECOVERY !   \ the clause's scan starts its own
   M-SRC @ cut 6 - 0 NFETCH:CAPTURE
   NFEED:DOES-CLAUSE M-DOES-ROW !
   M-SRC @ cut +  M-SRC-U @ cut -  M-DOES-SIG @ M-DOES-SIG-U @
   CHECKER-OWNER:DOES-CHECK CLAUSE-CK
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

: CHECK-RECORDED ( -- n )
   M-DOES @ 0<> if CHECK-DOES-SPLIT exit then
   CHECK-SOURCE ;

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

: SCAN ( -- )
   [: CHECK-RECORDED M-VERDICT ! ;] catch {: src-rc:n :}
   [: END-RECORDED ;] catch {: end-rc:n :}
   end-rc 0= if [: KEEP-TAPE-NAME ;] catch else 0 then {: name-rc:n :}
   src-rc 0<> if src-rc throw then
   end-rc 0<> if end-rc throw then
   name-rc 0<> if name-rc throw then
   TRUSTED? if NAME$ TRUST-SIG$ REGISTER-TRUST exit then
   CERTIFYING? 0= if DECLARE-HOOKLESS then ;

\ Read off the SOURCE: a token costs at least two bytes of capture, so n bytes
\ can never produce more than n/2 rows. A tape is a span of the shared mapping.
\ The stream entry's source is a whole file tail, so the count is taken against
\ the unit's text ceiling too: a scan longer than that is E-NFEED-TEXT before a
\ row of it is appended, which makes it the real bound on what one tape can hold.
: TAPE-ROOM ( -- n )
   M-SRC-U @ TEXT-CAP min 2 / 1 max ;

\ A multi-error run whose reader recovers from E-NCOMP-RECOVERY: that reader
\ sets RECOVER-CELL around the compile it calls (layout.f). Under any other
\ reader a refusal is E-NCOMP-VERDICT, which RUN reports.
: RECOVERING? ( -- bool )
   CHECKER-OWNER:MULTI-ERROR?
   data-base NCOMP-DISPATCH:RECOVER-CELL + @ 0<> and ;

: RECORD ( -- n )
   CC BB IR-BUILD:MODULE-KEY TAPE-ROOM NTAPE:NEW {: tp:IR-ARENA:arena :}
   M-DOES @ 0<> if
      CC BB tp TXT TEXT-CAP SRC$ M-DOES @ NFEED:BEGIN-DOES-UNIT
   else
      CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   then
   ndict@ {: before:n :}
   [: SCAN ;] catch {: rc:n :}
   rc 0 <> if NFEED:ABANDON-UNIT rc throw then
   \ Under a recovering reader a definition the checker refused, or any
   \ definition, trusted or not, whose body or does> clause bound a refused
   \ declaration's recovery row, is counted and abandoned unpublished:
   \ E-NCOMP-RECOVERY ends it without a second report.
   TRUSTED? 0= CERTIFYING? and if M-VERDICT @ -1 <> if
      M-DOES @ 0= M-VERDICT @ 0 = M-VERDICT @ 1 = or and if
         RECOVERING? if E-NCOMP-RECOVERY throw then
      then
      E-NCOMP-VERDICT throw
   then then
   RECOVERING? if
      CHECKER-OWNER:RECOVERY-USED? M-HEAD-RECOVERY @ or if E-NCOMP-RECOVERY throw then
   then
   TAPE NFEED:RECORD-WINDOW
   before ;

\ ---- which word the source published -----------------------------------------
\ The compiler owns one pending record, so the dictionary count must not move.
: PUBLISHED-NONE ( n -- ) {: before:n :}
   ndict@ before <> if E-NCOMP-NAME throw then ;

: SOURCE-PUBLICATION-CK ( n -- ) {: before:n :}
   before PUBLISHED-NONE ;

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

\ ---- binding an earlier definition shadowed by the pending record -------------
\ The dictionary key a body token names.
: TOKEN-KEY ( n -- IR-ID:ir-symbol-id )
   {: ix:n :}
   CC BB  TAPE MKEY ix NTAPE:SPELL@  HIR-WORD:KEY-SYM ;

\ Match by the dictionary entry, not by bytes: folding and a qualified spelling
\ can both name the same prior word.  `recurse` has no callable dictionary
\ target and therefore keeps its separate elaborator rule.
: PRIOR-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   PRIOR-ENTRY @ 0= if exit then
   TAPE ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if exit then
   \ Structural operands and locals have no source call decision here.
   ix CHECKER-OWNER:SOURCE-BINDING {: row:ptr size:n :}
   size 0= if exit then
   size CHECKER-OWNER-ABI:BOUND-CELLS cells <> if E-NCOMP-OWNER throw then
   row CHECKER-OWNER-ABI:BOUND-KIND cells + CELL-VIEW @
      CHECKER-OWNER-ABI:BOUND-DICT <> if exit then
   row CHECKER-OWNER-ABI:BOUND-ENTRY cells + CELL-VIEW @
      PRIOR-ENTRY @ <> if exit then
   ix TOKEN-KEY {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:OVERLAY? if exit then
   \ Structural operands can have the same spelling as the definition's bare
   \ tail. Leave an uncallable prior binding unmodeled: NELAB's existing scans
   \ discard operands, while a genuine word use reaches the ordinary refusal.
   PRIOR-CALLABLE @ 0= if exit then
   PRIOR-CAST @ if
      NHOST:CAST TAPE MKEY ix NTAPE:SPAN@ IR-SOURCE:SPAN-START
         NHOST:SOURCE-REFUSE
      CC BB r sy PRIOR-IN @ PRIOR-OUT @ PRIOR-GLUE @
         HIR-WORD:DECLARE-BOUND-CAST exit
   then
   CC BB r sy
   PRIOR-ENTRY @ PRIOR-IN @ PRIOR-OUT @ PRIOR-GLUE @ PRIOR-DEAD @
   HIR-WORD:DECLARE-BOUND-CALLABLE ;

: BIND-PRIOR ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   PRIOR-ENTRY @ 0= if exit then
   TAPE NFEED:PRODUCED-CK
   TAPE NTAPE:TOKENS 1 ?do
      r i PRIOR-STEP
   loop ;

\ ---- the chain ---------------------------------------------------------------
\ Asked of the checker by name, because every OTHER caller in the tree was
\ compiled against that same certificate. A wrong answer cannot publish a wrong
\ routine: either direction is refused by the validator or the memory-order rule.
: NO-RETURN? ( -- bool )
   NAME$ NDICT:SPELL-DEAD? ;

: LEAF? ( -- bool )
   M-FIXED @ NDICT:FIXED-VAL =  M-FIXED @ NDICT:FIXED-ADDR =  or ;

: FIXED-LINKAGE ( -- NBACK:linkage )
   NBACK:L-NONE
   M-FIXED @ NDICT:FIXED-ADDR = if NBACK:L-PATCH NBACK:WITH then ;

\ How control reaches and leaves this definition's routine. The backend composes
\ its own machine contract from this and from what the definition takes and
\ leaves; which registers or frame that means is the backend's answer.
: LINKAGE ( -- NBACK:linkage )
   LEAF? if FIXED-LINKAGE exit then
   NBACK:L-NONE
   NO-RETURN? if NBACK:L-DEAD NBACK:WITH then
   NELAB:CALLED? if NBACK:L-CALLED NBACK:WITH then
   NELAB:TAIL-CALLED? NELAB:TAIL-ENTRY@ NPUB:IN-REGION? and if
      NBACK:L-TAIL NBACK:WITH
   then
   NELAB:CALLS-BACK? if NBACK:L-BACK NBACK:WITH then ;

\ ---- the shadow target's chain -----------------------------------------------
\ A `does>` clause is the companion record's entry in the shadow's routine as it
\ is in the engine's own, so the copy is taken the way publication will take
\ the engine's.
: SHADOW-TAKE ( NART:emission -- )
   M-DOES @ 0<> if M-DOES-FUN @ NSHADOW:TAKE-DOES exit then
   NSHADOW:TAKE ;

\ The engine's own stages in the engine's own order, each the row the shadow's
\ binding resolves to, ending in the emission measured from no slot.
: SHADOW-WORK ( -- )
   SHS M-IN @ M-OUT @ LINKAGE NBACK:DECLARE
   SHS 0 M-HM @ NBACK:SELECT {: m0:IR-BUILD:module :}
   SHS m0 NBACK:FIXPOINT {: ready:IR-BUILD:module :}
   SHS ready NBACK:EMIT-UNPLACED
   SHS NART:COPY SHADOW-TAKE ;

\ Each row gives back what it holds inside its own context, so the context
\ leaves the ordinary way, takes its builders and arenas with it, and leaves
\ NEMIT empty for the engine's own emission.
: SHADOW-CLEAN ( -- )
   SHS NBACK:RELEASE
   SHS NBACK:RETIRE ;

: SHADOW-IN ( NSESSION:session -- )
   dup SH-SESSION ! NSESSION:RESOLVE drop 0 M-SH !
   [: SHADOW-WORK ;] [: SHADOW-CLEAN ;] finally ;

: SHADOW-BODY ( IR-CTX:ctx -- )
   M-LEASE @ NSESSION:NEW [: SHADOW-IN ;] NSESSION:WITH-WORK ;

: SHADOWED ( IR-BUILD:module -- )
   {: hm:IR-BUILD:module :}
   NSHADOW:OPEN? 0= if exit then
   NSHADOW:NATIVE? if exit then
   hm 0 M-HM !
   NSHADOW:BINDING [: SHADOW-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- the one stage, or the two -----------------------------------------------
\ Selection publishes the module that is emitted, and a routine whose values do
\ not all fit its registers is lowered - once per class the allocator seals - and
\ the last lowering publishes it instead. Every stage is the row the definition's
\ own target contract resolves to, so this file names no backend and an
\ architecture with no backend loaded is refused at the declaration. An open
\ shadow's chain runs first, from the same frozen module.
: HOST-CHAIN ( IR-BUILD:module NSESSION:session -- NART:emission )
   {: hm:IR-BUILD:module s:NSESSION:session :}
   s M-IN @ M-OUT @ LINKAGE NBACK:DECLARE
   s hm NBACK:SELECT {: m0:IR-BUILD:module :}
   hm IR-BUILD:RETIRE
   s m0 NBACK:FIXPOINT {: ready:IR-BUILD:module :}
   M-CB-SLOT @ 0 >= if M-CB-SLOT @ M-CB-THUNK @ NCBSTUB:SET then
   s ready NPUB:NEXT-SLOT NBACK:EMIT
   M-CB-SLOT @ 0 >= if NCBSTUB:ENTRY@ M-CB-ENTRY ! then
   s NART:COPY ;

: HOST-CLEAN ( -- )
   NCBSTUB:CLEAR
   SS NBACK:RELEASE
   SS NBACK:RETIRE ;

: HOST-WORK ( IR-BUILD:module NSESSION:session -- NART:emission )
   [: HOST-CHAIN ;] [: HOST-CLEAN ;] finally ;

: FREEZE-CLEAN ( -- )
   NLOOP:BOUND? if NLOOP:RELEASE then ;

: FREEZE-CHAIN ( NSESSION:session -- IR-BUILD:module )
   BB NBACK:FREEZE ;

: FROZEN ( NSESSION:session -- IR-BUILD:module )
   [: FREEZE-CHAIN ;] [: FREEZE-CLEAN ;] finally ;

: EMITTED ( -- NART:emission )
   SS [: FROZEN ;] NSESSION:WITH-WORK {: hm:IR-BUILD:module :}
   hm SHADOWED
   hm SS [: HOST-WORK ;] NSESSION:WITH-WORK {: e:NART:emission :}
   NSHADOW:NATIVE? if e SHADOW-TAKE then
   e ;

: PUBLISH-IT ( NART:emission -- )
   M-DOES @ 0<> if
      dup M-DOES-FUN @ NART:FUNCTION-OFFSET@ M-DOES-OFF !
      M-DOES-FUN @ NPUB:PUBLISH-PENDING-DOES exit
   then
   NPUB:PUBLISH-PENDING ;

: ELABORATE ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   M-DOES @ 0<> if
      CC BB TAPE p r M-IN @ M-OUT @ M-DOES-ROW @
      M-DOES-IN @ M-DOES-OUT @ M-DOES-GIN @ M-DOES-GOUT @
      M-DOES-SIG @ M-DOES-SIG-U @ NELAB:RECORDED-DOES drop
      NELAB:DOES-FUNCTION M-DOES-FUN !
      exit
   then
   CC BB TAPE p r M-IN @ M-OUT @ NELAB:RECORDED-COLON drop ;

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

: HOST-ARITIES ( -- )
   M-IN @ M-OUT @ NHOST:SOURCE-ARITY+
   HOST-ROOT-CONTRACT
   BB IR-BUILD:FUNS 1 ?do
      M-DOES @ 0<> i M-DOES-FUN @ = and if
         M-DOES-IN @ M-DOES-OUT @
      else
         i NELAB:QUOT-ARITY
      then
      NHOST:SOURCE-ARITY+
   loop ;

\ The model is built AFTER the tape, because the table has to be sized from the
\ body and the body is the tape.
: LOWER ( n -- NART:emission )
   {: before:n :}
   MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   before SOURCE-PUBLICATION-CK
   RECORD-NAME-CK
   KEEP-ARITY
   r BIND-PRIOR
   NAME$ NDICT:SPELL-GLUE NELAB:FRAME-GLUE!
   p r ELABORATE
   HOST-ARITIES
   NHOST:SOURCE-FREEZE
   EMITTED dup M-EMISSION ! ;

: SHUT ( -- )
   0 CHECKER-OWNER:WRITE-WINDOW ;

\ THE COMPILE WINDOW opens when RECORD returns. Every row the definition records
\ is recorded by then: the scan and the hook that drives it, a TRUSTED:
\ declaration and a hook-less one all run inside RECORD, and no callback has run
\ yet. What follows until publication's last callback (publish.f UNIT-NOTIFY)
\ is the elaborator, the backend and the observers a caller installed; the
\ compiler scans nothing there and writes nothing to the checker. So the checker
\ refuses every scan and every store write in between with E-NCOMP-STATE
\ (checker.f WRITE-WINDOW): a callback cannot reset the latches publication
\ reads, or record a row the definition's refusal would cut from under a
\ reference to it. publish.f shuts the window before the compiler's own writes
\ begin, and the finally shuts it on every exit.
: WORK ( -- )
   CC HIR-MOD 0 M-BLD !
   RECORD {: before:n :}
   E-NCOMP-STATE CHECKER-OWNER:WRITE-WINDOW
   before [: LOWER PUBLISH-IT ;] [: SHUT ;] finally
   0 M-DOES-FRAME !
   before M-PUBLISHED-START ! ;

\ Fixed definers have no body text or checker scan. A one-row tape gives the
\ HIR function its source and symbol; the definer publishes its effect later.
\ Defer uses the captured qualified name so deadness resolves to its own word.
$20 constant NAME-END

: FIXED-NAME$ ( -- ptr u8 n )
   M-FIXED-REC @ 0 >= if M-FIXED-REC @ XREF-REC XREF-NAME$ exit then
   M-FIXED @ FIXED-DEFER = if
      data-base BODYBUF-OFF + BYTE-VIEW
      data-base BODYLEN-CELL + @ {: b:ptr len:n :}
      0 begin
         dup len < if b over + c@ dup NAME-END <> swap 0<> and else false then
      while 1+ repeat
      {: u:n :}
      b u exit
   then
   PENDING-NAME$ ;

: FIXED-TAPE ( -- )
   FIXED-NAME$ {: a:ptr u:n :}
   u TEXT-CAP > if E-NCOMP-TEXT throw then
   a TXT u BYTE-COPY
   CC BB IR-BUILD:MODULE-KEY 1 NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB TXT u IR-BUILD:ADD-SOURCE {: sid:IR-ID:ir-source-id :}
   CC BB TXT u IR-BUILD:INTERN-SYMBOL {: sym:IR-ID:ir-symbol-id :}
   CC BB tp
   BB sid 0 u IR-BUILD:ADD-SPAN sym NTAPE-MODE:INTERPRETING NTAPE:NAME-TOKEN
   NTAPE:PUSH-INTO drop
   tp NTAPE:SEAL 0 M-TAPE ! ;

: DEFER-WORK ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   KEEP-TAPE-NAME
   0 M-IN ! 0 M-OUT !
   CC BB TAPE p r M-FIXED-VAL @ NELAB:DEFER drop
   EMITTED dup M-EMISSION !
   M-FIXED-VAL @ NPUB:PUBLISH-PENDING-DEFER ;

: FIXED-MODULE ( -- )
   CC HIR-MOD 0 M-BLD !
   FIXED-TAPE
   MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   0 M-IN ! 1 M-OUT !
   CC BB TAPE p r M-FIXED-VAL @ M-FIXED @ NELAB:FIXED drop ;

: FIXED-SHADOW-WORK ( -- )
   M-FIXED @ FIXED-DEFER = if
      CC HIR-MOD 0 M-BLD !
      FIXED-TAPE
      MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
      KEEP-TAPE-NAME
      0 M-IN ! 0 M-OUT !
      CC BB TAPE p r M-FIXED-VAL @ NELAB:DEFER drop
   else
      M-FIXED @ FIXED-CAST = if
         CC HIR-MOD 0 M-BLD !
         FIXED-TAPE
         MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
         KEEP-TAPE-NAME
         1 M-IN ! 1 M-OUT !
         CC BB TAPE p r 1 1 NELAB:COLON drop
      else
         FIXED-MODULE
      then
   then
   SS [: FROZEN ;] NSESSION:WITH-WORK {: hm:IR-BUILD:module :}
   hm SHADOWED
   hm IR-BUILD:RETIRE
   M-FIXED-REC @ NSHADOW:PUBLISH ;

: FIXED-WORK ( -- )
   ndict@ {: before:n :}
   M-FIXED @ FIXED-DEFER = if
      CC HIR-MOD 0 M-BLD !
      FIXED-TAPE
      MODEL DEFER-WORK
   else
      FIXED-MODULE
      EMITTED dup M-EMISSION !
      NPUB:PUBLISH-RAW
   then
   before M-PUBLISHED-START ! ;

\ Asked INSIDE the context so the backend always leaves the ordinary way and
\ gives its arenas back. A shadow emission no publication claimed goes too.
: RETIRE-BODY ( -- )
   NFETCH:RELEASE
   NSHADOW:ABANDON
   NHOST:SOURCE-ABANDON ;

: BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 M-CTX !
   -1 M-PUBLISHED-START !
   c M-LEASE @ NSESSION:NEW M-SESSION !
   [: M-SHADOW-ONLY @ if FIXED-SHADOW-WORK exit then
      M-FIXED @ NDICT:FIXED-NONE <> if FIXED-WORK exit then WORK ;] catch M-RC !
   M-RC @ 0= M-PUBLISHED-START @ 0 >= and if
      [: M-PUBLISHED-START @ M-EMISSION @ PUBLISHED-ROWS ;] catch 0<> if
         s" ncomp: publication callback threw" 76 die
      then
   then
   RETIRE-BODY ;

\ ---- the load's session ------------------------------------------------------
\ WHAT THE SESSION IS. IR-CTX:SESSION-OPEN answers an unscoped context that
\ outlives every definition and dies with the image; a definition is still an
\ ordinary IR-CTX:WITH-CONTEXT, nested inside it, and still owns its own module
\ identities, its own builders and its own arenas. The session exists to hold
\ the things that are the same for every definition of a load.
\
\ WHAT IT HOLDS. One module of its own, holding nothing but an interner into
\ which both dialects intern their whole vocabulary once. Each dialect keeps
\ that pair and starts every module it builds as a copy of it, so a definition's
\ modules are born already holding the dialect's spellings.
\
\ WHAT A DIALECT ASKS OF IT. PROTOTYPE, with the session's context, interner
\ pair and module key, to intern its vocabulary and adopt the pair;
\ PROTOTYPE-CLEAR to give it up. Nothing else: the session hands out no builder,
\ mints no identity a definition uses, and is never the context a definition
\ compiles into.
\
\ WHEN IT OPENS AND CLOSES. The first definition that needs one opens it and it
\ stands until the image is captured, which is where it and everything it holds
\ are given back. Liveness is read off the context itself, so a session that has
\ gone answers as gone and the next definition opens a fresh one.
1 CWORK-BUFFER S-CTX IR-CTX:ctx
0 CWORK-VARIABLE SESSION-OPEN-LEASE n
0 CWORK-VARIABLE SESSION-OPEN-EPOCH n
0 CWORK-VARIABLE SESSION-OPEN-MODULE-BEFORE n
0 CWORK-VARIABLE SESSION-OPEN-MODULE-AFTER n
0 CWORK-VARIABLE SESSION-OPEN-BUILD-BEFORE n
0 CWORK-VARIABLE SESSION-OPEN-BUILD-AFTER n

: SC ( -- IR-CTX:ctx ) 0 S-CTX @ ;

\ The ceilings are the ones the default plan gives any module, so a module that
\ starts as a copy of this one can grow exactly as far as one that does not.
\
\ THE REGISTRATION BUILDER IS GIVEN BACK. HIR-WORD:SESSION-MODEL needs a module
\ to key its 86 rows against and reads nothing from it afterwards - the spellings
\ those rows are keyed by are interned into the PROTOTYPE, which is what a
\ reader holds - so the builder is aborted as soon as registration returns. It
\ is one builder registry slot and seventeen arena slots of the sixty-four, held
\ for the whole load by a module nothing reads: with it standing, a live session
\ left 43 of the 64 arena slots free, and without it 60.
: SESSION-VOCABULARY ( -- )
   SC IR-CTX:NEW-MODULE drop {: k:IR-ID:ir-module-key :}
   SC k IR-SYM:CAP-MAX IR-SYM:BYTE-MAX IR-SYM:NEW
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   SC a r k HIR:PROTOTYPE
   SC a r k M-LEASE @ NBACK:PROTOTYPE
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   SC HIR:NEW-BUILDER {: mb:IR-BUILD:builder :}
   SC a r k mb HIR-WORD:SESSION-MODEL
   mb IR-BUILD:ABORT ;

\ What NCOMP holds that lives in the session context and nowhere else: the HIR
\ prototype, the registered vocabulary, and whatever each loaded backend interned
\ beside them. Each is a flag over storage the session owns, so the flags go out
\ when the session does - through IR-CTX:SESSION-CLOSE, which runs this before it
\ retires the row. Nothing else clears them, so no ordering between a capture's
\ entry points can leave a reader holding a flag over an arena that is gone.
: FORGET-OWNED ( NLEASE:lease -- )
   {: l:NLEASE:lease :}
   l NLEASE:QUIET
   0 SESSION-OPEN-LEASE !
   0 SESSION-OPEN-EPOCH !
   0 SESSION-OPEN-MODULE-BEFORE !
   0 SESSION-OPEN-MODULE-AFTER !
   0 SESSION-OPEN-BUILD-BEFORE !
   0 SESSION-OPEN-BUILD-AFTER !
   HIR-WORD:SESSION-MODEL-CLEAR
   HIR:PROTOTYPE-CLEAR
   l NBACK:FORGET ;

\ A refused close must leave the prototype flags and their context untouched.
\ During a failed session start the compiler's own root is still live; after
\ compilation, no root may be held by another caller.
: SESSION-ADMIT ( -- )
   M-LEASE @ NLEASE:LIVE? if M-LEASE @ NLEASE:QUIET exit then
   NLEASE:IDLE-CK ;

: SESSION-FORGET ( -- )
   M-LEASE @ NLEASE:LIVE? if M-LEASE @ FORGET-OWNED exit then
   [: FORGET-OWNED ;] NLEASE:WITH ;

: INSTALL-FORGET ( -- )
   [: SESSION-ADMIT ;] [: SESSION-FORGET ;] IR-CTX:SESSION-STAND-DOWN! ;
INSTALL-FORGET

\ A vocabulary that fails to build takes the session with it, so the next
\ definition is the first one again rather than the first one with half a
\ prototype.
: SESSION-START ( -- )
   NABI:BINDING IR-CTX:SESSION-OPEN 0 S-CTX !
   NLEASE:HISTORY SESSION-OPEN-LEASE !
   IR-CTX:EPOCH SESSION-OPEN-EPOCH !
   IR-ID:SERIAL-HISTORY SESSION-OPEN-MODULE-BEFORE !
   IR-BUILD:HISTORY SESSION-OPEN-BUILD-BEFORE !
   [: SESSION-VOCABULARY ;] catch {: rc:n :}
   rc 0<> if IR-CTX:SESSION-CLOSE rc throw then
   IR-ID:SERIAL-HISTORY SESSION-OPEN-MODULE-AFTER !
   IR-BUILD:HISTORY SESSION-OPEN-BUILD-AFTER ! ;

: SESSION-READY ( -- )
   SC IR-CTX:LIVE? if exit then
   SESSION-START ;

\ A recursive compiler call would record one definition onto another's tape.
: IN-CONTEXT ( -- )
   SESSION-READY
   IR-ID:SERIAL-HISTORY DEF-MODULE-BEFORE !
   IR-CTX:HISTORY DEF-EPOCH-BEFORE ! DEF-GEN-BEFORE !
   IR-BUILD:HISTORY DEF-BUILD-BEFORE !
   NABI:BINDING [: BODY ;] IR-CTX:WITH-CONTEXT
   IR-ID:SERIAL-HISTORY DEF-MODULE-BEFORE @ - DEF-MODULE-DELTA !
   IR-CTX:HISTORY
      DEF-EPOCH-BEFORE @ - DEF-EPOCH-DELTA !
      DEF-GEN-BEFORE @ - DEF-GEN-DELTA !
   IR-BUILD:HISTORY DEF-BUILD-BEFORE @ - DEF-BUILD-DELTA ! ;

\ A refusal after the scan owns the checker row this definition recorded: a
\ certified scan's, a TRUSTED: declaration's, and with the hook cell empty its
\ declared row or its multi-error recovery fact. The definition is rolled back,
\ so its row goes too, or the next definition of the name would be a duplicate.
\ Whichever path recorded it, the refusal puts the store back as it stood at
\ STAGE, when the body reached the compiler (M-ROWS): the owner truncates it to
\ that end (checker.f CHECKER-RETRACT-ROWS). Until RECORD returns only the
\ definition records rows, and from then to publication the compile window
\ (WORK) admits only the compiler's own writes, so the cut takes exactly the
\ definition's rows: a callback (a backend pass, an observer) that would record
\ one while the definition compiles is refused E-NCOMP-STATE. Nothing goes when
\ nothing was recorded, as when its declaration did not parse or had a scope
\ scheme, and the refusal is its own for a catch to receive, TRUSTED: or not.
\ Every row recorded before STAGE stays: a twin's in another scope, one CHECK!
\ recorded under its own name, and one a parsing immediate recorded as the body
\ was read. No word outlives the cut pointing at a cut row: while the
\ definition is pending, source a callback evaluates is its body's capture
\ (definers.f COMPILING?), where a definer defines nothing and a `;` is refused
\ (rc 74). Under a hook a scan that did not certify was refused before this,
\ and the checker rolled it back itself, so the cut finds nothing to take. A
\ does> definer rolls back through its own checker frame (DOES-FINISH).
: RETRACT ( -- )
   M-DOES @ 0<> if
      M-DOES-FRAME @ 0<> if
         NULL-PTR 0 false CHECKER-OWNER:DOES-FINISH
         0 M-DOES-FRAME !
      then
      exit
   then
   M-ROWS @ CHECKER-OWNER:RETRACT-ROWS ;

: LENGTH-CK ( -- )
   M-SRC-U @ TEXT-CAP > if E-NCOMP-TEXT throw then ;

: IDLE-CK ( -- )
   M-OPEN @ 0<> if E-NCOMP-STATE throw then ;

: ERROR-TEXT ( ptr u8 n -- ) {: a:ptr u:n :}
   2 a u write drop ;

\ The elaborator's refusal names the token it stopped at.
: REPORT-FAILURE ( -- )
   s" ncomp: cannot compile " ERROR-TEXT
   NAME$ ERROR-TEXT
   NELAB:REFUSED$ {: a:ptr u:n :}
   u 0 > if s"  at " ERROR-TEXT a u ERROR-TEXT then
   S\" \n" ERROR-TEXT ;

: REPORT-UNDEFINED-TICK ( -- )
   s" E-UNDEFINED: " ERROR-TEXT
   NELAB:REFUSED$ ERROR-TEXT
   S\" \n" ERROR-TEXT ;

: RUN ( -- )
   LENGTH-CK
   CHECKER-OWNER:RESET-REPORT
   1 M-OPEN !
   0 M-RC !
   [: IN-CONTEXT ;] catch {: entry-rc:n :}
   0 M-OPEN !
   -1 M-CB-SLOT !
   entry-rc 0<> if
      M-DOES-FRAME @ 0<> if NULL-PTR 0 false CHECKER-OWNER:DOES-FINISH then
      entry-rc throw
   then
   M-RC @ {: rc:n :}
   rc E-NCOMP-RECOVERY = if rc throw then
   rc E-NELAB-QUOT = NELAB:SOURCE-UNDEFINED? and if
      REPORT-UNDEFINED-TICK
      RETRACT RC-REJECT throw
   then
   rc 0 <> if
      rc RC-REJECT <> CHECKER-OWNER:JSON-REPORTED? 0= or
      if REPORT-FAILURE then
      RETRACT rc throw
   then ;

: STAGE ( ptr u8 n bool -- )
   {: sa su:n required:bool :}
   IDLE-CK
   required M-REQUIRED !
   0 CHECKER-OWNER:BIND-REGIME!
   NFETCH:RELEASE
   sa M-SRC ! su M-SRC-U !
   KEEP-PRIOR
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
   -1 M-DOES-ROW !
   NDICT:FIXED-NONE M-FIXED !
   -1 M-FIXED-REC !
   false M-SHADOW-ONLY !
   0 NAME-U !
   NELAB:REFUSED-RESET ;

private

: LEASED-MODE ( ptr u8 n bool NLEASE:lease -- )
   M-LEASE !
   [: STAGE RUN ;] [: false M-REQUIRED ! ;] finally ;

: LEASED ( ptr u8 n NLEASE:lease -- )
   false swap LEASED-MODE ;

: LEASED-CHECKED ( ptr u8 n NLEASE:lease -- )
   true swap LEASED-MODE ;

: FIXED-LEASED ( n n NLEASE:lease -- )
   {: val:n kind:n lease:NLEASE:lease :}
   lease M-LEASE !
   TXT 0 false STAGE
   kind M-FIXED ! val M-FIXED-VAL !
   RUN ;

: SHADOW-FIXED-LEASED ( n n n NLEASE:lease -- )
   {: val:n kind:n idx:n lease:NLEASE:lease :}
   lease M-LEASE !
   IDLE-CK
   kind NDICT:FIXED-ADDR <> kind NDICT:FIXED-VAL <> and
   kind FIXED-DEFER <> and kind FIXED-CAST <> and if E-NCOMP-STATE throw then
   idx 0 < idx ndict@ >= or if E-NCOMP-STATE throw then
   NSHADOW:NATIVE? if
      kind FIXED-CAST <> val 0<> or if E-NCOMP-STATE throw then
      idx XREF-REC {: rec:ptr :}
      rec XREF-START rec XREF-CODE-BYTES NSHADOW:TAKE-CAST
      idx NSHADOW:PUBLISH
      exit
   then
   idx M-FIXED-REC !
   val M-FIXED-VAL !  kind M-FIXED !
   0 M-DOES !  -1 M-DOES-FUN !  0 M-DOES-OFF !
   true M-SHADOW-ONLY !  1 M-OPEN !  0 M-RC !
   [: IN-CONTEXT ;] catch {: rc:n :}
   false M-SHADOW-ONLY !  0 M-OPEN !
   rc 0<> if rc throw then
   M-RC @ 0<> if M-RC @ throw then ;

public

\ FILE replay can install definitions without compiling them. Keep the
\ compiler's lazily opened session alive at the same range boundary as a
\ source load; later definitions must not open a second session instead.
: SESSION-LIVE? ( -- bool ) SC IR-CTX:LIVE? ;
: SESSION-SERIAL ( -- n )
   SESSION-LIVE? if SC IR-CTX:SERIAL else 0 then ;
: SESSION-LEASE ( -- n )
   SESSION-LIVE? if SESSION-OPEN-LEASE @ else 0 then ;
: SESSION-EPOCH ( -- n )
   SESSION-LIVE? if SESSION-OPEN-EPOCH @ else 0 then ;
: SESSION-MODULE-BEFORE ( -- n )
   SESSION-LIVE? if SESSION-OPEN-MODULE-BEFORE @ else 0 then ;
: SESSION-MODULE-AFTER ( -- n )
   SESSION-LIVE? if SESSION-OPEN-MODULE-AFTER @ else 0 then ;
: SESSION-BUILD-BEFORE ( -- n )
   SESSION-LIVE? if SESSION-OPEN-BUILD-BEFORE @ else 0 then ;
: SESSION-BUILD-AFTER ( -- n )
   SESSION-LIVE? if SESSION-OPEN-BUILD-AFTER @ else 0 then ;
: SESSION-ENSURE ( -- )
   SESSION-LIVE? if exit then
   [: M-LEASE ! SESSION-READY ;] NLEASE:WITH ;

\ The next definition is an accessor that owns one anonymous C entry after
\ its ordinary return. The entry is published with that definition's emission.
: CALLBACK-STUB! ( n n -- ) {: k:n thunk:n :}
   IDLE-CK
   M-CB-SLOT @ 0 >= if E-NCOMP-STATE throw then
   k M-CB-SLOT !  thunk M-CB-THUNK !  0 M-CB-ENTRY ! ;

: CALLBACK-ENTRY ( -- n )
   M-CB-ENTRY @ dup 0= if E-NCOMP-STATE throw then ;

\ The named parent's effect does not determine the publication callback's effect.
: PUBLISHED! ( [ n IR-CTX:ctx code-entry n n -- ] -- )
   IDLE-CK
   data-base PEND-CELL + @ 0<> if E-NCOMP-STATE throw then
   data-base NATIVE-OBS-CELLS:PUBLISHED + xt! ;

: DEFINITION-CACHEABLE? ( -- bool )
   M-OPEN @ 0=
   DOES-BYTE@ 0= and
   M-CB-SLOT @ 0< and
   NSHADOW:OPEN? 0= and
   data-base NATIVE-OBS-CELLS:PUBLISHED + @ 0= and ;

\ One successful definition's identities start after SESSION-READY and include
\ the ordinary child context's retirement. A warm install skips that child.
: DEFINITION-HISTORY ( -- n n n n )
   DEF-MODULE-DELTA @ DEF-GEN-DELTA @ DEF-EPOCH-DELTA @
   DEF-BUILD-DELTA @ ;

\ Where the last published `does>` definer's clause starts in its body, or 0.
: DEFINITION-DOES-OFFSET ( -- n )
   M-DOES-OFF @ ;

: REPLAY-DEFINITION-LEASED ( NLEASE:lease -- )
   M-LEASE !
   SESSION-READY
   IR-ID:SERIAL-HISTORY DEF-MODULE-DELTA @ +
      {: module:n :}
   IR-CTX:HISTORY
      DEF-EPOCH-DELTA @ + {: epoch:n :}
      DEF-GEN-DELTA @ + {: gen:n :}
   IR-BUILD:HISTORY DEF-BUILD-DELTA @ +
      {: build:n :}
   module IR-ID:SERIAL-CHECK drop
   gen epoch IR-CTX:CHECK-HISTORY
   build IR-BUILD:CHECK-HISTORY
   module IR-ID:REPLAY-SERIAL
   gen epoch IR-CTX:REPLAY-HISTORY
   build IR-BUILD:REPLAY-HISTORY ;

: REPLAY-DEFINITION ( n n n n -- )
   DEF-BUILD-DELTA ! DEF-EPOCH-DELTA ! DEF-GEN-DELTA !
   DEF-MODULE-DELTA !
   [: REPLAY-DEFINITION-LEASED ;] NLEASE:WITH ;

\ The engine has already parsed this definition and built its pending record.
\ Compile the captured body directly.
: COMPILE ( ptr u8 n -- )
   [: LEASED ;] NLEASE:WITH ;

\ A build window can require the live checker's verdict while the target's
\ check hook has not loaded. Ordinary COMPILE still leaves that window
\ explicitly unjudged.
: COMPILE-CHECKED ( ptr u8 n -- )
   [: LEASED-CHECKED ;] NLEASE:WITH ;

: COMPILE-FIXED ( n n -- ) {: val:n kind:n :}
   kind NDICT:FIXED-VAL <> kind NDICT:FIXED-ADDR <> and
   kind FIXED-DEFER <> and if E-NCOMP-STATE throw then
   val kind [: FIXED-LEASED ;] NLEASE:WITH ;

\ LCREATE has already published its ARM body and closed its write window.
\ Compile the same fixed one-row HIR for the open target shadow, then attach
\ that emission to the created record. No second ARM publication occurs.
: SHADOW-FIXED ( n n n -- ) {: val:n kind:n idx:n :}
   NSHADOW:OPEN? 0= if exit then
   val kind idx [: SHADOW-FIXED-LEASED ;] NLEASE:WITH ;

: SHADOW-DOES ( n n -- )
   NSHADOW:PATCH-DOES ;

\ Store a compile entry in NCOMP-DISPATCH:XT-CELL, the image-ABI cell the
\ tier-1 `:` dispatches through. A tool that counts compiles borrows the cell
\ with its wrapper and gives it back with INSTALL.
: DISPATCH! ( [ ptr u8 n -- ] -- )
   data-base NCOMP-DISPATCH:XT-CELL + xt! ;

\ The fixed engine header lies outside a partial compiler capture. Reinstall
\ its dispatch after the captured words have been relocated at fresh boot.
: INSTALL ( -- )
   ['] CALLBACK-STUB! data-base NCOMP-DISPATCH:CALLBACK-STUB-CELL + xt!
   ['] CALLBACK-ENTRY data-base NCOMP-DISPATCH:CALLBACK-ENTRY-CELL + xt!
   ['] COMPILE DISPATCH!
   ['] SHADOW-FIXED data-base NCOMP-DISPATCH:FIXED-SHADOW-CELL + xt!
   ['] SHADOW-DOES data-base NCOMP-DISPATCH:DOES-SHADOW-CELL + xt! ;

\ A whole-image capture closes the session through IMAGE-LIFECYCLE:PREPARE.
\ A chain window captures this entry directly, so close a live session here too.
\ SESSION-FORGET clears its readers before the interner is unmapped.
: CAPTURE-PREPARE ( -- )
   NLEASE:IDLE-CK
   IDLE-CK
   IR-CTX:SESSION-LIVE? if IR-CTX:SESSION-CLOSE then
   NFETCH:RELEASE
   \ A shadow's map lives in mappings no image carries, so the capture ends it.
   NSHADOW:CLOSE
   \ Backends prove their passes idle here; the dynamic registry releases
   \ their mappings with the workspace immediately before DATA copy.
   NBACK:PREPARE
   NHOST:CAPTURE-PREPARE
   CHECKER-OWNER:CAPTURE-PREPARE
   NFEED:CAPTURE-PREPARE
   IR-OP:CAPTURE-PREPARE
   IR-BUILD:CAPTURE-PREPARE
   NPROF:CLOSE ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
