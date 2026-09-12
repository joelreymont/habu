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

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/compiler/ir/symbol.f
require src/compiler/native/abi.f
require src/compiler/native/frame.f
require src/compiler/native/dict.f
require src/compiler/native/feed.f
require src/compiler/native/elaborate.f
require src/compiler/native/loop.f
require src/compiler/native/select.f
require src/compiler/native/spill.f
require src/compiler/native/combine.f
require src/compiler/native/emit.f
require src/compiler/native/publish.f
require src/compiler/native/prof.f

package NCOMP

private

\ ---- what a recording unit is opened with ------------------------------------
\ The unit's text ceiling is the ENGINE's own body capture, which overflows with
\ an exit rather than a throw. The capture is at least three bytes shorter than
\ its source.
BODYBUF-CAP constant TEXT-CAP

\ A name past it is refused rather than truncated into one that denotes another word.
64 constant NAME-CAP

create TXT TEXT-CAP allot
create NAME-BUF NAME-CAP allot
PTR-VARIABLE NAME-A
variable NAME-U

here CELL 1- and CELL swap - CELL 1- and allot
1 TYPED-BUFFER M-CTX IR-CTX:ctx
1 TYPED-BUFFER M-BLD IR-BUILD:builder
1 TYPED-BUFFER M-TAPE IR-ARENA:view

\ Everything the run needs, parked: a quotation cannot read the enclosing word's
\ locals and the whole run is one quotation.
PTR-VARIABLE M-SRC
variable M-SRC-U
variable M-IN
variable M-OUT
variable PRIOR-ENTRY
variable PRIOR-IN
variable PRIOR-OUT
variable PRIOR-GLUE
variable PRIOR-DEAD
variable PRIOR-CALLABLE
variable M-OPEN                      \ a compilation is running
variable M-RC                        \ the code the run inside the context reached
variable M-VERDICT                   \ the verdict the recorded scan reached
variable M-SPILLS                    \ padded spill slots that define the cumulative frame
variable M-FUNS                      \ functions sharing the emitted routine contract
variable M-DOES                      \ byte split after `does> `, or zero
variable M-DOES-ROW                  \ the tape row that carries `does>`
PTR-VARIABLE M-DOES-SIG
variable M-DOES-SIG-U
variable M-DOES-IN
variable M-DOES-OUT
variable M-DOES-FUN                  \ hidden clause function ordinal
variable TRUST-VERDICT
PTR-VARIABLE TRUST-SRC-A
variable TRUST-SRC-U

: CC ( -- IR-CTX:ctx )           0 M-CTX @ ;
: BB ( -- IR-BUILD:builder )     0 M-BLD @ ;
: TAPE ( -- IR-ARENA:view )      0 M-TAPE @ ;
: MKEY ( -- IR-ID:ir-module-key ) BB IR-BUILD:MODULE-KEY ;

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
   a u NDICT:CALL-TARGET {: entry:n :}
   entry PRIOR-ENTRY !
   entry 0= if exit then
   a u NDICT:SPELL-CALL {: in:n out:n glue:n neutral:bool :}
   in NDICT:ARITY-NONE = if exit then
   neutral 0= if exit then
   glue NDICT:GLUE-UNKNOWN = if exit then
   a u NDICT:SPELL-DEAD? {: dead:bool :}
   in PRIOR-IN !  out PRIOR-OUT !  glue PRIOR-GLUE !  dead PRIOR-DEAD !
   1 PRIOR-CALLABLE ! ;

\ An ambiguous bare name is no prior binding.  A qualified body token remains
\ free to resolve its own unambiguous record after the check.
: KEEP-PRIOR ( -- )
   0 PRIOR-ENTRY !  0 PRIOR-CALLABLE !
   [: READ-PRIOR ;] catch {: rc:n :}
   rc E-USING-AMBIGUOUS = if exit then
   rc 0<> if rc throw then ;

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
   CC BB MODEL-ROWS 0 HIR-WORD:NEW-LINKED ;

\ ---- stage N0: the definition the engine compiles ----------------------------
\ Parked rather than left on the stack, because this runs inside the quotation
\ the recovery below catches.
: END-RECORDED ( -- )
   NFEED:END-UNIT M-VERDICT !
   0 M-TAPE ! ;

\ Row zero's structural span names the exact spelling already copied into TXT.
: TAPE-NAME$ ( -- ptr u8 n )
   TAPE MKEY 0 NTAPE:SPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n u:n :}
   TXT st + u ;

\ The tape's first token is the definition's name. Recover it after sealing even
\ when a rejected second `:` left a tentative spelling in the record slot.
: KEEP-TAPE-NAME ( -- )
   TAPE-NAME$ {: a:ptr u:n :}
   a NAME-A !  u NAME-U !
   u NAME-CAP > if E-NCOMP-TEXT throw then
   a NAME-BUF u STR-LEN BYTE-COPY-LEN ;

: CHECK-TRUSTED-BODY ( -- )
   TRUST-SRC-A @ TRUST-SRC-U @ CHECK! TRUST-VERDICT ! ;

\ TRUST-DECL is deliberately unavailable to checked code. Keep that authority
\ at this one-line boundary; scanning, recovery, and quiet-state ownership stay
\ checked like the ordinary compiler path.
TRUSTED: REGISTER-TRUST ( ptr u8 n ptr u8 n -- )
   TRUST-DECL ;

TRUSTED: QUIET+ ( n -- )
   DIAG-QUIET +! ;

: CHECK-TRUSTED ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: na:ptr nu:n sa:ptr su:n ba:ptr bu:n :}
   ba TRUST-SRC-A !  bu TRUST-SRC-U !
   1 QUIET+
   [: CHECK-TRUSTED-BODY ;] catch {: rc:n :}
   -1 QUIET+
   rc 0<> if rc throw then
   na nu sa su REGISTER-TRUST
   TRUST-VERDICT @ ;

: CHECK-PARENT ( ptr u8 n -- n ) {: a:ptr u:n :}
   TRUSTED? if PENDING-NAME$ TRUST-SIG$ a u CHECK-TRUSTED exit then
   a u LOWER-CERT-HOOK:HOOK ;

: CHECK-SOURCE ( -- n )
   SRC$ CHECK-PARENT ;

\ CHECK-DOES! is a trusted checker mutation, like the ordinary lower-cert hook.
TRUSTED: CHECK-DOES ( ptr u8 n ptr u8 n -- n )
   CHECK-DOES! ;

: CHECK-DOES-SPLIT ( -- n )
   M-DOES @ {: cut:n :}
   cut 6 < cut M-SRC-U @ > or if E-NCOMP-TEXT throw then
   M-SRC @ cut 6 - CHECK-PARENT
   TRUSTED? if drop else -1 <> if E-NCOMP-VERDICT throw then then
   NFEED:DOES-CLAUSE M-DOES-ROW !
   M-SRC @ cut +  M-SRC-U @ cut -  M-DOES-SIG @ M-DOES-SIG-U @
   CHECK-DOES -1 <> if E-NCOMP-VERDICT throw then
   CHECK-DOES-DIN-CELLS M-DOES-IN !
   CHECK-DOES-DOUT-CELLS M-DOES-OUT !
   M-DOES-IN @ 0 < M-DOES-OUT @ 0 < or if E-NCOMP-ARITY throw then
   CHECK-DOES-WIDE? if E-NELAB-BUNDLE throw then
   -1 ;

: CHECK-RECORDED ( -- n )
   M-DOES @ 0<> if CHECK-DOES-SPLIT exit then
   CHECK-SOURCE ;

: SCAN ( -- )
   [: CHECK-RECORDED M-VERDICT ! ;] catch {: src-rc:n :}
   [: END-RECORDED ;] catch {: end-rc:n :}
   end-rc 0= if [: KEEP-TAPE-NAME ;] catch else 0 then {: name-rc:n :}
   src-rc 0<> if src-rc throw then
   end-rc 0<> if end-rc throw then
   name-rc 0<> if name-rc throw then ;

\ Read off the SOURCE: a token costs at least two bytes of capture, so n bytes
\ can never produce more than n/2 rows. A tape is a span of the shared mapping.
\ The stream entry's source is a whole file tail, so the count is taken against
\ the unit's text ceiling too: a scan longer than that is E-NFEED-TEXT before a
\ row of it is appended, which makes it the real bound on what one tape can hold.
: TAPE-ROOM ( -- n )
   M-SRC-U @ TEXT-CAP min 2 / 1 max ;

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
   TRUSTED? 0= if M-VERDICT @ -1 <> if E-NCOMP-VERDICT throw then then
   before ;

\ Read off the live builder because selection takes its binding before the
\ module freezes.
: TEXT-LEN ( -- n )
   CC BB  TAPE MKEY 0 NTAPE:SPAN@ IR-SOURCE:SPAN-SRC
   IR-BUILD:SOURCE-LEN ;

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

: LATEST-NAME$ ( -- ptr u8 n )
   REC-INDEX XREF-REC XREF-NAME$ ;

: QUALIFIED-RECORD-NAME? ( ptr n ptr u8 n n -- bool )
   {: rec:ptr a:ptr u:n split:n :}
   rec  a split 1+ ZPTR+  u split - 1-  XREF-MATCH? 0= if false exit then
   a split XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? 0= if drop false exit then
   XREF-START  rec XREF-WORDLIST  = ;

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
   REC-INDEX XREF-REC  NAME-BUF NAME-U @  RECORD-NAME? 0= if
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
\ ASKED WITH THE BARE NAME, in the scope the source was compiled in, which is
\ the one form that answers for a private definition as well as a public one.
\ KEEP-TAPE-NAME has copied that name out of the tape, which is what makes it
\ askable while the record's own name span is about to move.
\
\ THE ABSENT ANSWER IS NAMED AND HAS NO REACHING CASE TODAY, which is written
\ down rather than left for a reader to assume either way. SPELL-ARITY answers
\ ARITY-NONE for a name the checker holds no effect for, so the code below is the
\ reader's own contract handled rather than a -1 let through to NELAB:COLON,
\ which would refuse it as E-NELAB-ARITY and name the wrong thing. No shape
\ reaches it: E-NCOMP-VERDICT already refuses anything the engine's check did
\ not certify, and this asks about a record published one step earlier in the
\ scope that published it. A package opened and closed by the source is
\ E-NCOMP-NAME, a `TRUSTED:` body is E-NFEED-STATE,
\ an unsigned body answers its inferred effect. Dot
\ habu-reach-the-absent-360162f5 owns finding one or retiring the code.
: KEEP-ARITY ( -- )
   NAME-BUF NAME-U @ NDICT:SPELL-ARITY {: din:n dout:n :}
   din NDICT:ARITY-NONE = if E-NCOMP-ARITY throw then
   din M-IN !  dout M-OUT ! ;

\ ---- binding an earlier definition shadowed by the pending record -------------
64 constant SPELL-CAP                \ the longest prior spelling this lookup accepts

create SPELL-BUF SPELL-CAP allot

\ Match by the dictionary entry, not by bytes: folding and a qualified spelling
\ can both name the same prior word.  `recurse` has no callable dictionary
\ target and therefore keeps its separate elaborator rule.
: PRIOR-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   PRIOR-ENTRY @ 0= if exit then
   TAPE ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if exit then
   CC BB  TAPE MKEY ix NTAPE:SPELL@  HIR-WORD:KEY-SYM
   {: sy:IR-ID:ir-symbol-id :}
   CC BB sy IR-BUILD:SYMBOL-LEN SPELL-CAP > if exit then
   CC BB sy SPELL-BUF SPELL-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   SPELL-BUF u NDICT:CALL-TARGET PRIOR-ENTRY @ <> if exit then
   r sy HIR-WORD:MODELS? if exit then
   \ Structural operands can have the same spelling as the definition's bare
   \ tail. Leave an uncallable prior binding unmodeled: NELAB's existing scans
   \ discard operands, while a genuine word use reaches the ordinary refusal.
   PRIOR-CALLABLE @ 0= if exit then
   CC BB r sy
   PRIOR-ENTRY @ PRIOR-IN @ PRIOR-OUT @ PRIOR-GLUE @ PRIOR-DEAD @
   HIR-WORD:DECLARE-BOUND-CALLABLE ;

: BIND-PRIOR ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   TAPE NTAPE:TOKENS 1 ?do
      r i PRIOR-STEP
   loop ;

\ ---- the chain ---------------------------------------------------------------
\ Asked of the checker by name, because every OTHER caller in the tree was
\ compiled against that same certificate. A wrong answer cannot publish a wrong
\ routine: either direction is refused by the validator or the memory-order rule.
: NO-RETURN? ( -- bool )
   NAME-BUF NAME-U @ NDICT:SPELL-DEAD? ;

\ All functions share this ABI. No-return and tail-call control describe a
\ single function; quotation siblings must retain their ordinary returns.
: ROUTINE ( -- A64EFF:routine )
   NO-RETURN? M-FUNS @ 1 = and if
      NELAB:CALLED? if
         NABI:SCRATCH M-IN @ M-OUT @ M-SPILLS @ NABI:NORET-FRAMED exit
      then
      NABI:SCRATCH M-IN @ M-OUT @ M-SPILLS @ NABI:NORET-LEAF-FRAMED exit
   then
   NELAB:TAIL-CALLED? NELAB:TAIL-ENTRY@ NPUB:IN-REGION? and
   M-FUNS @ 1 = and if
      NELAB:CALLS-BACK? if
         NABI:SCRATCH M-IN @ M-OUT @ M-SPILLS @ NABI:TAIL-CALLING-FRAMED exit
      then
      NABI:SCRATCH M-IN @ M-OUT @ M-SPILLS @ NABI:TAIL-FRAMED exit
   then
   NELAB:CALLED? if
      NABI:SCRATCH M-IN @ M-OUT @ M-SPILLS @ NABI:CALL-FRAMED exit
   then
   NABI:SCRATCH M-IN @ M-OUT @ M-SPILLS @ NABI:LEAF-FRAMED ;

: A64-BUILDER ( -- IR-BUILD:builder )
   IR-BUILD:PLAN-DEFAULT
   CC A64IR:NEW-BUILDER ;

: HIR-BUILDER ( -- IR-BUILD:builder )
   IR-BUILD:PLAN-DEFAULT
   CC HIR:NEW-BUILDER ;

\ A module with no such loop is handed back UNTOUCHED: rebuilding renumbers
\ values, so a routine that gained nothing could still come out with other bytes.
: CLOSED ( IR-BUILD:module n -- IR-BUILD:module )
   {: m:IR-BUILD:module len:n :}
   m NLOOP:FOLDS {: n:n :}
   n 0= if NLOOP:RELEASE m exit then
   A64SEL:RELEASE
   HIR-BUILDER {: nb:IR-BUILD:builder :}
   CC nb A64SEL:BIND-SOURCE
   CC m nb TXT len NLOOP:REWRITE {: m1:IR-BUILD:module :}
   NLOOP:FOLDED n <> if E-NLOOP-PLAN throw then
   m IR-BUILD:RETIRE
   m1 ;

\ The recorded length is read off the LIVE builder, before the freeze consumes
\ the handle. The lowering pass is bound here because a module's symbols are its own.
: SELECTED ( n -- IR-BUILD:module )
   {: len:n :}
   CC BB NLOOP:BIND-DIALECT
   CC BB A64SEL:BIND-SOURCE
   BB IR-BUILD:FUNS M-FUNS !
   CC BB IR-BUILD:FREEZE {: m0:IR-BUILD:module :}
   m0 len CLOSED {: m:IR-BUILD:module :}
   A64-BUILDER {: ab:IR-BUILD:builder :}
   CC ab A64RA:BIND-DIALECT
   CC ab A64RAV:BIND-DIALECT
   CC ab A64EMIT:BIND-DIALECT
   CC ab A64SPILL:BIND-DIALECT
   CC ab A64COMB:BIND-DIALECT
   CC m ab TXT len ROUTINE A64SEL:SELECT {: selected:IR-BUILD:module :}
   m IR-BUILD:RETIRE
   selected ;

\ A module with no such pair is handed back UNTOUCHED: rebuilding renumbers
\ values and the allocator breaks ties on those numbers.
: COMBINED ( IR-BUILD:module n -- IR-BUILD:module )
   {: m:IR-BUILD:module len:n :}
   NPROF-PHASE:COMBINE NPROF:START
   m A64COMB:REWRITES {: n:n :}
   n 0= if
      A64COMB:RELEASE  NPROF-PHASE:COMBINE NPROF:STOP  m exit
   then
   A64RA:RELEASE
   A64EMIT:RELEASE
   A64SPILL:RELEASE
   A64-BUILDER {: nb:IR-BUILD:builder :}
   CC nb A64RA:BIND-DIALECT
   CC nb A64RAV:BIND-DIALECT
   CC nb A64EMIT:BIND-DIALECT
   CC nb A64SPILL:BIND-DIALECT
   CC m nb TXT len A64COMB:REWRITE {: m1:IR-BUILD:module :}
   A64COMB:REWRITTEN n <> if E-A64COMB-SHAPE throw then
   m IR-BUILD:RETIRE
   NPROF-PHASE:COMBINE NPROF:STOP
   m1 ;

\ Declared for every definition, not only one that calls, so the seam can place
\ it at the slot it really claims.
: EMIT-AT ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   m ROUTINE A64RAV:ACCEPT
   NPUB:NEXT-SLOT A64EMIT:PLACE-AT
   CC m A64EMIT:EMIT ;

\ The reserve is sized from A64RA:FRAME, the same count ROUTINE declares from,
\ so the module and its contract agree by construction.
: LOWERED ( IR-BUILD:module n -- IR-BUILD:module )
   {: m:IR-BUILD:module len:n :}
   A64EMIT:RELEASE
   A64-BUILDER {: nb:IR-BUILD:builder :}
   CC nb A64RA:BIND-DIALECT
   CC nb A64RAV:BIND-DIALECT
   CC nb A64EMIT:BIND-DIALECT
   NPROF-PHASE:SPILL NPROF:START
   CC m nb TXT len A64SPILL:REWRITE
   NPROF-PHASE:SPILL NPROF:STOP ;

\ Turn the allocator's absolute frame high-water back into the ABI's slot count.
\ Alignment holes stay counted, so a later allocation starts after this frame
\ rather than reusing padding as though it were unowned.
: KEEP-FRAME ( A64EFF:routine -- )
   {: r :}
   r A64EFF:TRAITS@  r A64EFF:LINK@  A64FRAME:SPILL-BASE {: base:n :}
   A64RA:FRAME base - {: bytes:n :}
   bytes 0 <  bytes A64IR:SLOT-WIDTH mod 0<> or if E-A64RA-FRAME throw then
   bytes A64IR:SLOT-WIDTH / M-SPILLS ! ;

: NEEDS-LOWERING? ( IR-BUILD:module -- bool )
   {: m:IR-BUILD:module :}
   ROUTINE {: r :}
   CC m r A64RA:ALLOCATE
   A64RA:PLAN-N 0= if false exit then
   A64RA:SPILLS A64RA:REMATS + A64RA:MOVES +
   0= if E-A64SPILL-PLAN throw then
   r KEEP-FRAME
   true ;

\ Each turn consumes a non-empty sealed plan and rewrites all of its decisions.
\ The next allocation either seals an empty plan or contributes another class.
: LOWER-FIXPOINT ( IR-BUILD:module n -- IR-BUILD:module )
   begin
      over NEEDS-LOWERING?
   while
      2dup LOWERED
      rot IR-BUILD:RETIRE
      swap
   repeat
   drop ;

\ ---- the two stages, or the four ---------------------------------------------
\ Frame slots and DECISIONS are different counts: a value re-emitted where it is
\ read takes no slot, so a walk asked through the slot count looks like one that
\ decided nothing. A routine that calls still cannot spill; it is refused.
: EMITTED ( -- )
   TEXT-LEN {: len:n :}
   len SELECTED len COMBINED {: m:IR-BUILD:module :}
   m len LOWER-FIXPOINT {: ready:IR-BUILD:module :}
   A64SPILL:BOUND? if A64SPILL:RELEASE then
   ready EMIT-AT ;

: PUBLISH-IT ( -- )
   M-DOES @ 0<> if M-DOES-FUN @ NPUB:PUBLISH-PENDING-DOES exit then
   NPUB:PUBLISH-PENDING ;

: ELABORATE ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   M-DOES @ 0<> if
      CC BB TAPE p r M-IN @ M-OUT @ M-DOES-ROW @
      M-DOES-IN @ M-DOES-OUT @ NDICT:GLUE-NONE NDICT:GLUE-NONE
      M-DOES-SIG @ M-DOES-SIG-U @ NELAB:DOES drop
      NELAB:DOES-FUNCTION M-DOES-FUN !
      exit
   then
   CC BB TAPE p r M-IN @ M-OUT @ NELAB:COLON drop ;

\ The model is built AFTER the tape, because the table has to be sized from the
\ body and the body is the tape.
: WORK ( -- )
   CC HIR-MOD 0 M-BLD !
   RECORD {: before:n :}
   MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   before SOURCE-PUBLICATION-CK
   RECORD-NAME-CK
   KEEP-ARITY
   r BIND-PRIOR
   NAME-BUF NAME-U @ NDICT:SPELL-GLUE NELAB:FRAME-GLUE!
   p r ELABORATE
   EMITTED
   PUBLISH-IT ;

\ Caught INSIDE the context so it always leaves the ordinary way and gives its
\ arenas back. Each pass is asked about ITSELF, so this cannot get out of step.
: RETURN-BINDINGS ( -- )
   NLOOP:BOUND? if NLOOP:RELEASE then
   A64SEL:BOUND? if A64SEL:RELEASE then
   A64RA:BOUND? if A64RA:RELEASE then
   A64SPILL:BOUND? if A64SPILL:RELEASE then
   A64COMB:BOUND? if A64COMB:RELEASE then
   A64EMIT:BOUND? if A64EMIT:RELEASE then ;

: RETIRE-BODY ( -- )
   M-RC @ 0<> if RETURN-BINDINGS then
   A64EMIT:RETIRE ;

: BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 M-CTX !
   [: WORK ;] catch M-RC !
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
1 TYPED-BUFFER S-CTX IR-CTX:ctx

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
   SC a r k A64IR:PROTOTYPE
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   SC HIR:NEW-BUILDER {: mb:IR-BUILD:builder :}
   SC a r k mb HIR-WORD:SESSION-MODEL
   mb IR-BUILD:ABORT ;

\ What NCOMP holds that lives in the session context and nowhere else: two
\ dialect prototypes and the registered vocabulary. Each is a flag over storage
\ the session owns, so the flags go out when the session does - through
\ IR-CTX:SESSION-CLOSE, which runs this before it retires the row. Nothing else
\ clears them, so no ordering between a capture's entry points can leave a
\ reader holding a flag over an arena that is gone.
: SESSION-FORGET ( -- )
   HIR-WORD:SESSION-MODEL-CLEAR
   HIR:PROTOTYPE-CLEAR
   A64IR:PROTOTYPE-CLEAR ;

: INSTALL-FORGET ( -- )
   [: SESSION-FORGET ;] IR-CTX:SESSION-STAND-DOWN! ;
INSTALL-FORGET

\ A vocabulary that fails to build takes the session with it, so the next
\ definition is the first one again rather than the first one with half a
\ prototype.
: SESSION-START ( -- )
   NABI:BINDING IR-CTX:SESSION-OPEN 0 S-CTX !
   [: SESSION-VOCABULARY ;] catch {: rc:n :}
   rc 0<> if IR-CTX:SESSION-CLOSE rc throw then ;

: SESSION-READY ( -- )
   SC IR-CTX:LIVE? if exit then
   SESSION-START ;

\ A recursive compiler call would record one definition onto another's tape.
: IN-CONTEXT ( -- )
   SESSION-READY
   NABI:BINDING [: BODY ;] IR-CTX:WITH-CONTEXT ;

\ A refusal after a certified, sealed scan owns the checker signature it just
\ recorded. Before then the checker either published no signature or already
\ rolled its own failed scan back.
: RETRACT ( -- )
   TRUSTED? if
      LATEST-NAME$ CHECKER-USIGS-TRUNCATE-FROM-RAW
      exit
   then
   M-VERDICT @ -1 <> if exit then
   NAME-U @ 0= if exit then
   NAME-A @ NAME-U @ CHECKER-USIGS-TRUNCATE-FROM-RAW ;

: LENGTH-CK ( -- )
   M-SRC-U @ TEXT-CAP > if E-NCOMP-TEXT throw then ;

: IDLE-CK ( -- )
   M-OPEN @ 0<> if E-NCOMP-STATE throw then ;

: ERROR-TEXT ( ptr u8 n -- ) {: a:ptr u:n :}
   2 a u write drop ;

: REPORT-FAILURE ( -- )
   s" ncomp: cannot compile " ERROR-TEXT
   NAME-BUF NAME-U @ ERROR-TEXT
   NELAB:REFUSED$ {: a:ptr u:n :}
   u 0 > if s"  at " ERROR-TEXT a u ERROR-TEXT then
   S\" \n" ERROR-TEXT ;

: RUN ( -- )
   LENGTH-CK
   1 M-OPEN !
   0 M-RC !
   [: IN-CONTEXT ;] catch {: entry-rc:n :}
   0 M-OPEN !
   entry-rc 0<> if entry-rc throw then
   M-RC @ {: rc:n :}
   rc 0 <> if REPORT-FAILURE RETRACT rc throw then ;

: STAGE ( ptr u8 n -- )
   {: sa su:n :}
   IDLE-CK
   sa M-SRC ! su M-SRC-U !
   KEEP-PRIOR
   0 M-IN ! 0 M-OUT !
   0 M-VERDICT !
   0 M-SPILLS !
   DOES-BYTE@ M-DOES !
   DOES-SIG-FIELD @ M-DOES-SIG !
   data-base TCSIG-U-CELL + @ M-DOES-SIG-U !
   0 M-DOES-IN ! 0 M-DOES-OUT !
   -1 M-DOES-FUN !
   -1 M-DOES-ROW !
   0 NAME-U ! ;

public

\ The engine has already parsed this definition and built its pending record.
\ Compile the captured body directly.
: COMPILE ( ptr u8 n -- )
   STAGE RUN ;

\ The session is already gone by the time this runs, and it took what NCOMP held
\ in it with it: IMAGE-LIFECYCLE:PREPARE closed the session, and SESSION-FORGET
\ above is the stand-down that close ran, before the interner the dialects were
\ reading was unmapped. So there is nothing session-shaped left to clear here,
\ and no order between the two entry points to get right.
: CAPTURE-PREPARE ( -- )
   IDLE-CK
   NULL-PTR NAME-A !  0 NAME-U !
   NULL-PTR M-SRC !  0 M-SRC-U !
   NULL-PTR M-DOES-SIG !  0 M-DOES-SIG-U !
   NULL-PTR TRUST-SRC-A !  0 TRUST-SRC-U !
   0 PRIOR-ENTRY !  0 PRIOR-IN !  0 PRIOR-OUT !
   0 PRIOR-GLUE !  0 PRIOR-DEAD !  0 PRIOR-CALLABLE !
   A64EMIT:CAPTURE-PREPARE
   A64EMIT:RELEASE-SCRATCH
   A64SPILL:RELEASE-SCRATCH
   A64RAV:RELEASE-SCRATCH
   A64RA:RELEASE-SCRATCH
   A64COMB:RELEASE-SCRATCH
   A64SEL:RELEASE-SCRATCH
   NLOOP:RELEASE-SCRATCH
   NFEED:CAPTURE-PREPARE
   IR-BUILD:CAPTURE-PREPARE
   NELAB:CAPTURE-PREPARE
   NPROF:CLOSE ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
