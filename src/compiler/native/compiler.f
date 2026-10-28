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
require src/compiler/native/definition.f
require src/compiler/native/shadow.f
require src/compiler/native/callback-stub.f
require src/compiler/native/prof.f
require src/compiler/session/lease.f

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
using NDEFINITION

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

align
1 CWORK-BUFFER M-CTX IR-CTX:ctx
1 CWORK-BUFFER M-BLD IR-BUILD:builder
1 CWORK-BUFFER M-TAPE IR-ARENA:view
1 CWORK-BUFFER M-HM IR-BUILD:module   \ the frozen module every selector reads
1 CWORK-BUFFER M-SH IR-CTX:ctx        \ the shadow's context, nested in CC
0 CWORK-VARIABLE M-LEASE NLEASE:lease
0 CWORK-VARIABLE M-SESSION NSESSION:session
0 CWORK-VARIABLE SH-SESSION NSESSION:session

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
0 CWORK-VARIABLE M-DOES-ROW n                  \ the tape row that carries `does>`
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
: PUBLISHED-ROWS ( n -- ) {: idx:n :}
   data-base NATIVE-OBS-CELLS:PUBLISHED + @ dup 0= if drop exit then
   PUBLISHED-XT {: q :}
   idx XREF-REC XREF-START >ENTRY {: parent:code-entry :}
   PUB-FUNS @ 0 ?do
      idx CC parent i i PUB-OFF @ q execute
   loop ;

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

\ RECORD opened this unit with NFEED:BEGIN-DOES-UNIT, which refused a cut that
\ is not just past a `does> ` in this source (E-NFEED-CUT). NAME$ is set only
\ after the unit seals (KEEP-TAPE-NAME), so the clause is named from the open
\ unit's first row (NFEED:UNIT-NAME$).
: CHECK-DOES-SPLIT ( -- n )
   CHECK-DOES-HEAD
   NFEED:DOES-CLAUSE M-DOES-ROW !
   NFEED:UNIT-NAME$ CHECK-DOES-CLAUSE ;

: CHECK-RECORDED ( -- n )
   M-DOES @ 0<> if CHECK-DOES-SPLIT exit then
   CHECK-SOURCE ;

: SCAN ( -- )
   [: CHECK-RECORDED M-VERDICT ! ;] catch {: src-rc:n :}
   [: END-RECORDED ;] catch {: end-rc:n :}
   end-rc 0= if [: KEEP-TAPE-NAME ;] catch else 0 then {: name-rc:n :}
   src-rc 0<> if src-rc throw then
   end-rc 0<> if end-rc throw then
   name-rc 0<> if name-rc throw then
   DECLARE-ROW ;

\ Read off the SOURCE: a token costs at least two bytes of capture, so n bytes
\ can never produce more than n/2 rows. A tape is a span of the shared mapping.
\ The stream entry's source is a whole file tail, so the count is taken against
\ the unit's text ceiling too: a scan longer than that is E-NFEED-TEXT before a
\ row of it is appended, which makes it the real bound on what one tape can hold.
: TAPE-ROOM ( -- n )
   SRC$ nip TEXT-CAP min 2 / 1 max ;

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
   VERDICT-CK
   TAPE NFEED:RECORD-WINDOW
   before ;

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
      \ A representation cast (DNAME-REP) lowers as the identity; any other
      \ cast keeps its host call.
      row CHECKER-OWNER-ABI:BOUND-FLAGS cells + CELL-VIEW @ DNAME-REP and 0= if
         NHOST:CAST TAPE MKEY ix NTAPE:SPAN@ IR-SOURCE:SPAN-START
            NHOST:SOURCE-REFUSE
      then
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
\ the engine's, while the emission stands sealed in NEMIT.
: SHADOW-TAKE ( -- )
   M-DOES @ 0<> if M-DOES-FUN @ NSHADOW:TAKE-DOES exit then
   NSHADOW:TAKE ;

\ The engine's own stages in the engine's own order, each the row the shadow's
\ binding resolves to, ending in the emission measured from no slot.
: SHADOW-WORK ( -- )
   SHS M-IN @ M-OUT @ LINKAGE NBACK:DECLARE
   SHS 0 M-HM @ NBACK:SELECT {: m0:IR-BUILD:module :}
   SHS m0 NBACK:FIXPOINT {: ready:IR-BUILD:module :}
   SHS ready NBACK:EMIT-UNPLACED
   SHADOW-TAKE ;

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
\ shadow's chain runs first, from the same frozen module; a native shadow takes
\ the engine's own emission here instead. Publication reads the emission while
\ NEMIT still holds it, before HOST-CLEAN retires it.
: HOST-CHAIN ( IR-BUILD:module NSESSION:session -- )
   {: hm:IR-BUILD:module s:NSESSION:session :}
   s M-IN @ M-OUT @ LINKAGE NBACK:DECLARE
   s hm NBACK:SELECT {: m0:IR-BUILD:module :}
   hm IR-BUILD:RETIRE
   s m0 NBACK:FIXPOINT {: ready:IR-BUILD:module :}
   M-CB-SLOT @ 0 >= if M-CB-SLOT @ M-CB-THUNK @ NCBSTUB:SET then
   s ready NPUB:NEXT-SLOT NBACK:EMIT
   M-CB-SLOT @ 0 >= if NCBSTUB:ENTRY@ M-CB-ENTRY ! then
   NSHADOW:NATIVE? if SHADOW-TAKE then
   PUBLISH-IT ;

: HOST-CLEAN ( -- )
   NCBSTUB:CLEAR
   SS NBACK:RELEASE
   SS NBACK:RETIRE ;

: HOST-WORK ( IR-BUILD:module NSESSION:session -- )
   [: HOST-CHAIN ;] [: HOST-CLEAN ;] finally ;

: FREEZE-CLEAN ( -- )
   NLOOP:BOUND? if NLOOP:RELEASE then ;

: FREEZE-CHAIN ( NSESSION:session -- IR-BUILD:module )
   BB NBACK:FREEZE ;

: FROZEN ( NSESSION:session -- IR-BUILD:module )
   [: FREEZE-CHAIN ;] [: FREEZE-CLEAN ;] finally ;

: EMITTED ( -- )
   SS [: FROZEN ;] NSESSION:WITH-WORK {: hm:IR-BUILD:module :}
   hm SHADOWED
   hm SS [: HOST-WORK ;] NSESSION:WITH-WORK ;

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

\ The model is built AFTER the tape, because the table has to be sized from the
\ body and the body is the tape.
: LOWER ( n -- )
   {: before:n :}
   MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   before SOURCE-PUBLICATION-CK
   RECORD-NAME-CK
   KEEP-ARITY
   r BIND-PRIOR
   NAME$ NDICT:SPELL-GLUE NELAB:FRAME-GLUE!
   p r ELABORATE
   BB IR-BUILD:FUNS [: NELAB:QUOT-ARITY ;] HOST-ARITIES
   NHOST:SOURCE-FREEZE
   PUB-PENDING M-PUB !
   EMITTED ;

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
   before [: LOWER ;] [: SHUT ;] finally
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
   PUB-DEFER M-PUB !
   EMITTED ;

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
      PUB-RAW M-PUB !
      EMITTED
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
      [: M-PUBLISHED-START @ PUBLISHED-ROWS ;] catch 0<> if
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

: LENGTH-CK ( -- )
   SRC$ nip TEXT-CAP > if E-NCOMP-TEXT throw then ;

: IDLE-CK ( -- )
   M-OPEN @ 0<> if E-NCOMP-STATE throw then ;

: RUN ( -- )
   LENGTH-CK
   CHECKER-OWNER:RESET-REPORT
   1 M-OPEN !
   0 M-RC !
   [: IN-CONTEXT ;] catch {: entry-rc:n :}
   0 M-OPEN !
   -1 M-CB-SLOT !
   M-RC @ {: rc:n :}
   entry-rc rc  rc E-NELAB-QUOT = NELAB:SOURCE-UNDEFINED? and
   NELAB:REFUSED$ SETTLE-UNIT ;

: STAGE ( ptr u8 n bool -- )
   IDLE-CK
   STAGE-UNIT
   KEEP-PRIOR
   -1 M-DOES-ROW !
   NDICT:FIXED-NONE M-FIXED !
   -1 M-FIXED-REC !
   false M-SHADOW-ONLY !
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

;using
;package
