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
\ NO BACKEND IS NAMED HERE. Every stage that turns an elaborated definition into
\ instructions goes through src/compiler/native/backend.f, at the row the
\ definition's own target contract resolves to, and an architecture with no
\ backend loaded is refused at the declaration before a module is built. The
\ ARM64 rows are installed by src/arch/arm64/passes.f, which this file requires
\ because it is what loads the ARM64 passes today.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/compiler/ir/symbol.f
require src/compiler/native/checker-owner.f
require src/compiler/native/abi.f
require src/compiler/native/dict.f
require src/compiler/native/feed.f
require src/compiler/native/elaborate.f
require src/compiler/native/backend.f
require src/compiler/native/publish.f
require src/compiler/native/prof.f
require src/arch/arm64/passes.f

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
variable PRIOR-CAST
variable PRIOR-CALLABLE
variable PRIOR-TARGET-VALUE
variable PRIOR-TARGET-U
variable M-OPEN                      \ a compilation is running
variable M-RC                        \ the code the run inside the context reached
variable M-VERDICT                   \ the verdict the recorded scan reached
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
   a u NDICT:CALL-BINDING {: entry:n kind:n :}
   entry PRIOR-ENTRY !
   entry 0= if exit then
   a u NDICT:SPELL-CALL {: in:n out:n glue:n neutral:bool :}
   in NDICT:ARITY-NONE = if exit then
   neutral 0= if exit then
   glue NDICT:GLUE-UNKNOWN = if exit then
   a u NDICT:SPELL-DEAD? {: dead:bool :}
   in PRIOR-IN !  out PRIOR-OUT !  glue PRIOR-GLUE !  dead PRIOR-DEAD !
   kind DKIND:CAST = PRIOR-CAST !
   1 PRIOR-CALLABLE ! ;

\ An ambiguous bare name is no prior binding.  A qualified body token remains
\ free to resolve its own unambiguous record after the check.
: KEEP-PRIOR ( -- )
   0 PRIOR-ENTRY !  0 PRIOR-CALLABLE !  0 PRIOR-CAST !
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
   \ Each resolved cast needs one identity pick; there cannot be more such
   \ bindings than body tokens. The arena reserves only picks actually used.
   CC BB MODEL-ROWS dup HIR-WORD:NEW-LINKED ;

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

\ The declared effect is this definition's assertion, so the scan is here only to
\ fill the tape stage N0 reads and its verdict is never enforced (RECORD below
\ enforces one only for a CHECKED definition). The owner suppresses its own
\ render for the scan's duration and restores it on either exit.
: CHECK-TRUSTED-BODY ( -- )
   TRUST-SRC-A @ TRUST-SRC-U @ CHECKER-OWNER:CHECK-UNJUDGED TRUST-VERDICT ! ;

\ TRUST-DECL is deliberately unavailable to checked code. Keep that authority
\ at this one-line boundary; scanning and recovery stay checked like the ordinary
\ compiler path.
TRUSTED: REGISTER-TRUST ( ptr u8 n ptr u8 n -- )
   CHECKER-OWNER:DECLARED-EFFECT ;

: CHECK-TRUSTED ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: na:ptr nu:n sa:ptr su:n ba:ptr bu:n :}
   ba TRUST-SRC-A !  bu TRUST-SRC-U !
   CHECK-TRUSTED-BODY
   na nu sa su REGISTER-TRUST
   TRUST-VERDICT @ ;

\ CERTIFICATION IS THE ENGINE'S HOOK CELL, AND THE SCAN IS THE OWNER'S.
\ Two questions were one call before this: `LOWER-CERT-HOOK:HOOK` by name both
\ scanned the source (the tape is filled from inside a scan) and enforced a
\ verdict, and it did so in the checker instance this file was compiled into.
\ Tier 0 reads the hook CELL per definition, which is why `0 set-check` takes
\ certification off it and installing another hook moves it; tier 1 could not be
\ moved at all.
\
\ So: a hook in the cell owns the verdict, and it is the live checker's own hook
\ that scans. With the cell EMPTY the definition is published uncertified -
\ exactly what tier 0 does under `0 set-check`, and what a window depends on,
\ because LOGICAL-RESET clears the hook and the window's own check-hook.f
\ installs one part-way through its prefix - but the tape still has to be filled,
\ so the owner's scan runs and its verdict is dropped rather than enforced.
TRUSTED: AS-HOOK ( n -- [ ptr u8 n -- n ] ) ;

TRUSTED: CALL-INSTALLED ( ptr u8 n n -- n )
   AS-HOOK execute ;

\ Whether anything is certifying at all. With the hook cell empty nothing is, and
\ the verdict the scan reports is then a fact about the source and not a refusal:
\ tier 0 publishes such a definition uncertified (habu2.f reads HOOK-CELL and
\ skips on zero) and the two tiers have to agree. A window depends on it - its
\ core prefix is compiled between LOGICAL-RESET and its own check-hook.f - and so
\ does every `0 set-check` session.
: CERTIFYING? ( -- bool )
   check@ 0 <> ;

: CHECK-PARENT ( ptr u8 n -- n ) {: a:ptr u:n :}
   TRUSTED? if PENDING-NAME$ TRUST-SIG$ a u CHECK-TRUSTED exit then
   check@ {: hook:n :}
   hook 0= if
      a u CHECKER-OWNER:CHECK-UNJUDGED drop -1 exit then
   a u hook CALL-INSTALLED ;

: CHECK-SOURCE ( -- n )
   SRC$ CHECK-PARENT
   SRC$ 0 NFETCH:CAPTURE ;


: CHECK-DOES-SPLIT ( -- n )
   M-DOES @ {: cut:n :}
   cut 6 < cut M-SRC-U @ > or if E-NCOMP-TEXT throw then
   M-SRC @ cut 6 - CHECK-PARENT
   TRUSTED? CERTIFYING? 0= or if drop else -1 <> if E-NCOMP-VERDICT throw then then
   M-SRC @ cut 6 - 0 NFETCH:CAPTURE
   NFEED:DOES-CLAUSE M-DOES-ROW !
   M-SRC @ cut +  M-SRC-U @ cut -  M-DOES-SIG @ M-DOES-SIG-U @
   CHECKER-OWNER:DOES-CHECK -1 <> if E-NCOMP-VERDICT throw then
   M-SRC @ cut + M-SRC-U @ cut - cut NFETCH:CAPTURE
   CHECKER-OWNER:DOES-IN M-DOES-IN !
   CHECKER-OWNER:DOES-OUT M-DOES-OUT !
   M-DOES-IN @ 0 < M-DOES-OUT @ 0 < or if E-NCOMP-ARITY throw then
   CHECKER-OWNER:DOES-WIDE? if E-NELAB-BUNDLE throw then
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
   TRUSTED? 0= CERTIFYING? and if M-VERDICT @ -1 <> if E-NCOMP-VERDICT throw then then
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
: PRIOR-STEP ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n :}
   PRIOR-ENTRY @ 0= if exit then
   TAPE ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if exit then
   CC BB  TAPE MKEY ix NTAPE:SPELL@  HIR-WORD:KEY-SYM
   {: sy:IR-ID:ir-symbol-id :}
   CC BB sy IR-BUILD:SYMBOL-LEN SPELL-CAP > if exit then
   CC BB sy SPELL-BUF SPELL-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   \ A local can have the same spelling as a public word in more than one
   \ used package. This pass runs before NELAB has built its local table, so
   \ an ambiguous token cannot be identified as a local here. Ambiguity proves
   \ that the token is not a prior binding; preserve that result and leave the
   \ normal elaborator to resolve the local first.
   u PRIOR-TARGET-U !
   [: SPELL-BUF PRIOR-TARGET-U @ NDICT:CALL-TARGET PRIOR-TARGET-VALUE ! ;] catch {: rc:n :}
   rc E-USING-AMBIGUOUS = if exit then
   rc 0<> if rc throw then
   PRIOR-TARGET-VALUE @ PRIOR-ENTRY @ <> if exit then
   r sy HIR-WORD:MODELS? if exit then
   \ Structural operands can have the same spelling as the definition's bare
   \ tail. Leave an uncallable prior binding unmodeled: NELAB's existing scans
   \ discard operands, while a genuine word use reaches the ordinary refusal.
   PRIOR-CALLABLE @ 0= if exit then
   PRIOR-CAST @ if CC BB p r sy HIR-WORD:DECLARE-BOUND-CAST exit then
   CC BB r sy
   PRIOR-ENTRY @ PRIOR-IN @ PRIOR-OUT @ PRIOR-GLUE @ PRIOR-DEAD @
   HIR-WORD:DECLARE-BOUND-CALLABLE ;

: BIND-PRIOR ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   TAPE NTAPE:TOKENS 1 ?do
      p r i PRIOR-STEP
   loop ;

\ ---- the chain ---------------------------------------------------------------
\ Asked of the checker by name, because every OTHER caller in the tree was
\ compiled against that same certificate. A wrong answer cannot publish a wrong
\ routine: either direction is refused by the validator or the memory-order rule.
: NO-RETURN? ( -- bool )
   NAME-BUF NAME-U @ NDICT:SPELL-DEAD? ;

\ How control reaches and leaves this definition's routine. The backend composes
\ its own machine contract from this and from what the definition takes and
\ leaves; which registers or frame that means is the backend's answer.
: LINKAGE ( -- NBACK:linkage )
   NBACK:L-NONE
   NO-RETURN? if NBACK:L-DEAD NBACK:WITH then
   NELAB:CALLED? if NBACK:L-CALLED NBACK:WITH then
   NELAB:TAIL-CALLED? NELAB:TAIL-ENTRY@ NPUB:IN-REGION? and if
      NBACK:L-TAIL NBACK:WITH
   then
   NELAB:CALLS-BACK? if NBACK:L-BACK NBACK:WITH then ;

\ ---- the one stage, or the two -----------------------------------------------
\ Selection publishes the module that is emitted, and a routine whose values do
\ not all fit its registers is lowered - once per class the allocator seals - and
\ the last lowering publishes it instead. Every stage is the row the definition's
\ own target contract resolves to, so this file names no backend and an
\ architecture with no backend loaded is refused at the declaration.
: EMITTED ( -- )
   CC M-IN @ M-OUT @ LINKAGE NBACK:DECLARE
   CC BB NBACK:SELECT {: m0:IR-BUILD:module :}
   CC m0 NBACK:PRUNE {: m:IR-BUILD:module :}
   CC m NBACK:FIXPOINT {: ready:IR-BUILD:module :}
   CC ready NPUB:NEXT-SLOT NBACK:EMIT ;

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
   p r BIND-PRIOR
   NAME-BUF NAME-U @ NDICT:SPELL-GLUE NELAB:FRAME-GLUE!
   p r ELABORATE
   EMITTED
   PUBLISH-IT ;

\ Asked INSIDE the context so the backend always leaves the ordinary way and
\ gives its arenas back.
: RETIRE-BODY ( -- )
   NFETCH:RELEASE
   M-RC @ 0<> if CC NBACK:RELEASE then
   CC NBACK:RETIRE ;

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
   SC a r k NBACK:PROTOTYPE
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
: SESSION-FORGET ( -- )
   HIR-WORD:SESSION-MODEL-CLEAR
   HIR:PROTOTYPE-CLEAR
   NBACK:FORGET ;

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
      LATEST-NAME$ CHECKER-OWNER:USIG-TRUNCATE
      exit
   then
   M-VERDICT @ -1 <> if exit then
   NAME-U @ 0= if exit then
   NAME-A @ NAME-U @ CHECKER-OWNER:USIG-TRUNCATE ;

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
   NFETCH:RELEASE
   sa M-SRC ! su M-SRC-U !
   KEEP-PRIOR
   0 M-IN ! 0 M-OUT !
   0 M-VERDICT !
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

\ The fixed engine header lies outside a partial compiler capture. Reinstall
\ its dispatch after the captured words have been relocated at fresh boot.
: INSTALL ( -- )
   ['] COMPILE data-base NCOMP-DISPATCH:XT-CELL + xt! ;

\ The session is already gone by the time this runs, and it took what NCOMP held
\ in it with it: IMAGE-LIFECYCLE:PREPARE closed the session, and SESSION-FORGET
\ above is the stand-down that close ran, before the interner the dialects were
\ reading was unmapped. So there is nothing session-shaped left to clear here,
\ and no order between the two entry points to get right.
: CAPTURE-PREPARE ( -- )
   IDLE-CK
   NFETCH:RELEASE
   NULL-PTR NAME-A !  0 NAME-U !
   NULL-PTR M-SRC !  0 M-SRC-U !
   NULL-PTR M-DOES-SIG !  0 M-DOES-SIG-U !
   NULL-PTR TRUST-SRC-A !  0 TRUST-SRC-U !
   0 PRIOR-ENTRY !  0 PRIOR-IN !  0 PRIOR-OUT !
   0 PRIOR-GLUE !  0 PRIOR-DEAD !  0 PRIOR-CAST !  0 PRIOR-CALLABLE !
   \ The registry releases buffers immediately before DATA copy, so each loaded
   \ backend gives up its pass reservations here and sizes them again on use.
   NBACK:PREPARE
   CHECKER-OWNER:CAPTURE-PREPARE
   NFEED:CAPTURE-PREPARE
   IR-BUILD:CAPTURE-PREPARE
   NELAB:CAPTURE-PREPARE
   NPROF:CLOSE ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
