\ definition.f - the compiler-neutral step around one pending definition's
\ lowering: staging, the checker scan and verdict, name and arity checks,
\ publication by kind, the pending callback stub, and retraction on refusal.
\ Tier 1 (src/compiler/native/compiler.f, NCOMP) and tier 0 (src/habu/t0.f)
\ both call it. It requires no tier-1 package (tape, feed, elaborator, HIR,
\ backend), so tier 0 can require it on every target.

require src/lib/prelude.f
require src/lib/error.f
require src/lib/string.f
require src/habu/boot-cells.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/workspace.f
require src/core/check-hook.f
require src/compiler/native/checker-owner.f
require src/compiler/native/host.f
require src/compiler/native/dict.f
require src/compiler/native/fetch.f
require src/compiler/native/emission.f
require src/compiler/native/publish.f
require src/compiler/native/callback-stub.f

package NDEFINITION

public

\ Everything the run needs, parked in cells: the whole run is one quotation under catch (compiler.f RUN).
0 CWORK-VARIABLE M-IN n
0 CWORK-VARIABLE M-OUT n
0 CWORK-VARIABLE M-VERDICT n                   \ the verdict the recorded scan reached
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
0 CWORK-VARIABLE M-SCAN ptr u8                 \ the text scanned last
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
: REGISTER-TRUST ( ptr u8 n ptr u8 n -- )
   CHECKER-OWNER:DECLARED-EFFECT ;

70 constant RC-REJECT                 \ CHECK:DEFINITION's reject status (check-hook.f CHECK-RC)

: CHECK-PARENT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a M-SCAN !
   CHECKER-OWNER:RESET-REPORT
   TRUSTED? if -1 CHECKER-OWNER:BIND-REGIME!
      a u CHECKER-OWNER:CHECK-UNJUDGED exit then
   0 CHECKER-OWNER:BIND-REGIME!
   a u CHECK:DEFINITION ;

public

: CHECK-SOURCE ( -- n )
   SRC$ CHECK-PARENT
   SRC$ 0 NFETCH:CAPTURE ;

private

\ A refused clause is reported as CHECK:DEFINITION reports a refused body: the
\ checker's reason, then its reject status, which the run names the
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
   TRUSTED? if drop else -1 <> if E-COMP-VERDICT throw then then
   CHECKER-OWNER:RECOVERY-USED? M-HEAD-RECOVERY !   \ the clause's scan starts its own
   M-SRC @ cut DOES-SPAN - 0 NFETCH:CAPTURE ;

\ The `does>` clause, named for a refusal by the unit's name. The checker's
\ token ordinal first steps past the `does>` keyword between the two bodies.
: CHECK-DOES-CLAUSE ( ptr u8 n -- n )
   {: na:ptr nu:n :}
   CHECKER-OWNER:TAPE-ADVANCE
   M-DOES @ {: cut:n :}
   M-SRC @ cut + M-SCAN !
   M-SRC @ cut +  M-SRC-U @ cut -  M-DOES-SIG @ M-DOES-SIG-U @
   CHECKER-OWNER:DOES-CHECK na nu CLAUSE-CK
   M-SRC @ cut + M-SRC-U @ cut - cut NFETCH:CAPTURE
   CHECKER-OWNER:DOES-IN M-DOES-IN !
   CHECKER-OWNER:DOES-OUT M-DOES-OUT !
   M-DOES-IN @ 0 < M-DOES-OUT @ 0 < or if E-COMP-ARITY throw then
   \ Where the clause's values end, read here because the checker captured
   \ the clause's rows for this split and the next one replaces them. A row
   \ wider than a cell is placed from these boundaries as a definition's own
   \ row is; a row the checker could not separate into cells answers
   \ GLUE-UNKNOWN and the elaborator refuses the definer over it.
   NDICT:DOES-GLUE M-DOES-GOUT ! M-DOES-GIN !
   -1 ;

public

\ A TRUSTED: definition's row, which no scan certified. Run once the scan has
\ named the unit.
: DECLARE-ROW ( -- )
   TRUSTED? if NAME$ TRUST-SIG$ REGISTER-TRUST then ;

private

\ A multi-error run whose reader recovers from E-COMP-RECOVERY: that reader
\ sets RECOVER-CELL around the compile it calls (layout.f). Under any other
\ reader a refusal is E-COMP-VERDICT, which SETTLE-UNIT reports.
: RECOVERING? ( -- bool )
   CHECKER-OWNER:MULTI-ERROR?
   data-base NCOMP-DISPATCH:RECOVER-CELL + @ 0<> and ;

public

\ Under a recovering reader a definition the checker refused, or any
\ definition, trusted or not, whose body or does> clause bound a refused
\ declaration's recovery row, is counted and abandoned unpublished:
\ E-COMP-RECOVERY ends it without a second report. So is a TRUSTED: definition
\ whose declared row the checker refused, reported and counted (SIG-REFUSED?).
: VERDICT-CK ( -- )
   TRUSTED? 0= if M-VERDICT @ -1 <> if
      M-DOES @ 0= M-VERDICT @ 0 = M-VERDICT @ 1 = or and if
         RECOVERING? if E-COMP-RECOVERY throw then
      then
      E-COMP-VERDICT throw
   then then
   RECOVERING? if
      CHECKER-OWNER:RECOVERY-USED? M-HEAD-RECOVERY @ or if E-COMP-RECOVERY throw then
      TRUSTED? if CHECKER-OWNER:SIG-REFUSED? if E-COMP-RECOVERY throw then then
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
   a split CHECKER-RESOLVE:HEAD-ROW
   dup XREF-FOUND? 0= if drop false exit then
   XREF-PKG-PUBLIC  rec XREF-WORDLIST  = ;

: RECORD-NAME? ( ptr n ptr u8 n -- bool )
   {: rec:ptr a:ptr u:n :}
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
\ a scope scheme): CHECK:DEFINITION has refused every uncertified body. After a
\ certified scan nothing reaches it either: a package opened and closed by the source is
\ E-COMP-NAME, an unsigned body answers its inferred effect, and one whose
\ effect is unrecorded (W-EFFECT-NOT-RECORDED) answers CHECKER-OWNER's
\ UNRECORDED-IN and UNRECORDED-OUT cells; no row carries them for callers.
: KEEP-ARITY ( -- )
   NAME$ NDICT:SPELL-ARITY {: din:n dout:n :}
   din NDICT:ARITY-NONE <> if din M-IN !  dout M-OUT !  exit then
   CHECKER-OWNER:UNRECORDED-IN {: uin:n :}
   CHECKER-OWNER:UNRECORDED-OUT {: uout:n :}
   uin 0 < uout 0 < or if E-COMP-ARITY throw then
   uin M-IN !  uout M-OUT ! ;

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

\ Drops a refused definition's rows from M-ROWS on, or its does> frame, so a
\ redefinition is no duplicate (exact cut: checker.f CHECKER-RETRACT-ROWS).
: RETRACT ( -- )
   M-DOES @ 0<> if
      M-DOES-FRAME @ 0<> if
         NULL-PTR 0 false CHECKER-OWNER:DOES-FINISH
         0 M-DOES-FRAME !
      then
      exit
   then
   M-ROWS @ CHECKER-OWNER:RETRACT-ROWS ;

SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL FILE-PATH
SRCLOC:INB-CELL RESERVED-PTR-U8-CELL INPUT-START
SRCLOC:OWNED-CELL RESERVED-PTR-U8-CELL FILE-START
INE-CELL RESERVED-PTR-U8-CELL INPUT-END
PENDTKA-CELL RESERVED-PTR-U8-CELL OPEN-NAME
TKA-CELL RESERVED-PTR-U8-CELL READ-AT

\ The body text's base, through `ptr-field`, the declared door to a DATA cell
\ that holds an address.
: TEXT-BASE ( -- ptr u8 ) data-base BODY-TEXT:BASE-CELL + 0 ptr-field @ ;

\ Cell k of layout.f BODY-SOURCE row i: 0 its text offset, 1 its source offset.
\ The rows follow the text in the block, at base + cap.
: SOURCE-ROW@ ( n n -- n )
   {: i:n k:n :}
   TEXT-BASE data-base BODY-TEXT:CAP-CELL + @ +
   i BODY-SOURCE:ROW * + k cells + CELL-VIEW @ ;

\ The last row at or before text offset off, -1 when none is.
: ROW-BEFORE ( n -- n )
   {: off:n :}
   data-base BODY-TEXT:ROWS-CELL + @
   begin dup 0 > while
      1 - dup 0 SOURCE-ROW@ off <= if exit then
   repeat drop -1 ;

\ The file line of text offset off in buffer [b, e) by row i, 0 when the row
\ has no source or the byte lies past e.
: ROW-LINE ( ptr u8 ptr u8 n n -- n )
   {: b:ptr e:ptr i:n off:n :}
   i 1 SOURCE-ROW@ BODY-SOURCE:NONE = if 0 exit then
   i 1 SOURCE-ROW@ off + i 0 SOURCE-ROW@ - {: src:n :}
   e b - src <= if 0 exit then
   FILE-START @ b src + ERROR-TEXT:LINE ;

\ 0 unless the name's buffer is current and p's run has a row. Rows at or past
\ BODYLEN are stale. A donor engine whose heap does not start at DATA-START has
\ another layout, its BODY-TEXT cells holding other bytes, so it has none.
: BODY-LINE ( ptr u8 -- n )
   {: p:ptr :}
   data-base BOOT:HEAP-START-CELL + @ DATA-START <> if 0 exit then
   p TEXT-BASE - {: off:n :}
   off 0 <  off data-base BODYLEN-CELL + @ >= or if 0 exit then
   INPUT-START @ OPEN-NAME @ INPUT-END @ {: b:ptr nm:ptr e:ptr :}
   nm b - 0 <  e nm - 0 <= or if 0 exit then
   off ROW-BEFORE {: i:n :}
   i 0 < if 0 exit then
   b e i off ROW-LINE ;

\ The file line of byte p, in the buffer being read (a refusal at the closing
\ `;`) or in the body text, 0 when it has none. Lines count from the file's
\ first owned byte, so the buffer must lie in the file (SRCLOC:FILE-BUFFER?).
: LINE-OF ( ptr u8 -- n )
   {: p:ptr :}
   INPUT-START @ INPUT-END @ {: b:ptr e:ptr :}
   p b - 0 >=  e p - 0 > and if FILE-START @ p ERROR-TEXT:LINE exit then
   p BODY-LINE ;

\ `<lead><path>` while a file is open, then `:<line>` of byte p (LINE-OF) when
\ it has one under SRCLOC:FILE-BUFFER?.
: SAY-WHERE ( ptr u8 n ptr u8 -- )
   {: a:ptr u:n p:ptr :}
   data-base SRCLOC:PATHLEN-CELL + @ {: len:n :}
   len 0= if exit then
   a u ERROR-TEXT:TYPE
   FILE-PATH @ len ERROR-TEXT:TYPE
   SRCLOC:FILE-BUFFER? 0= if exit then
   p LINE-OF {: line:n :}
   line 0= if exit then
   s" :" ERROR-TEXT:TYPE
   line ERROR-TEXT:DIGITS$ ERROR-TEXT:TYPE ;

\ The refusal names the definition and the token it stopped at, placed from p
\ by SAY-WHERE.
: REPORT-FAILURE ( ptr u8 n ptr u8 -- )
   {: a:ptr u:n p:ptr :}
   s" ncomp: cannot compile " ERROR-TEXT:TYPE
   NAME$ ERROR-TEXT:TYPE
   u 0 > if
      s"  at " ERROR-TEXT:TYPE a u ERROR-TEXT:TYPE
      s"  in " p SAY-WHERE
   then
   S\" \n" ERROR-TEXT:TYPE ;

\ The JIT's text for an undefined word, located from p as it locates it.
: REPORT-UNDEFINED ( ptr u8 n ptr u8 -- )
   {: a:ptr u:n p:ptr :}
   s" E-UNDEFINED: " ERROR-TEXT:TYPE
   a u ERROR-TEXT:TYPE
   s"  at " p SAY-WHERE
   S\" \n" ERROR-TEXT:TYPE ;

public

\ The `;` that ends the definition: the token the engine read last, whose
\ address in the input places a refusal there on its line.
: CLOSE$ ( -- ptr u8 n ) READ-AT @ data-base TKL-CELL + @ ;

private

\ The checker's refused token in the text scanned last, empty if none; its byte, the
\ closing `;` (CLOSE$) for one past the body's end; and whether it is undefined.
: CHECKER-REFUSED$ ( -- ptr u8 n ptr u8 bool )
   M-SCAN @ NULL-PTR = if NULL-PTR 0 NULL-PTR false exit then
   CHECKER-OWNER:ERROR-TOKEN
   CHECKER-OWNER:REFUSED-SPAN nip {: b:n undefined:bool :}
   M-SCAN @ b + {: p:ptr :}
   SRC$ + p - 0= if CLOSE$ drop else p then  undefined ;

\ A refusal the checker pins its own token for: a reject, a using refusal, a bad
\ stored signature (checker.f SGBAD-PIN) or a shadowed arity (SHADOW-ARITY-CK).
: PINNED? ( n -- bool )
   {: rc:n :}
   rc RC-REJECT =  rc E-USING-SHADOW-GLOBAL = or  rc E-USING-AMBIGUOUS = or
   rc E-BAD-STORED-SIGNATURE = or  rc E-SHADOWED-ARITY = or ;

\ A pinned refusal with no compiler token is the checker's refusal of the
\ definition: name the token it refused instead.
: REPORT-REFUSAL ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n p:ptr rc:n :}
   rc PINNED? 0=  u 0<> or
   if a u p REPORT-FAILURE exit then
   CHECKER-REFUSED$ {: ca:ptr cu:n cp:ptr undefined:bool :}
   ca cu cp REPORT-FAILURE
   undefined if ca cu cp REPORT-UNDEFINED then ;

\ A refusal of a TRUSTED: body, which no scan certified, that names its token:
\ the load ends as a reject.
: MALFORMED? ( n -- bool ) {: rc:n :}
   rc E-T0-CONTROL =  rc E-NELAB-CTRL = or  rc E-UNMODELED-IMMEDIATE = or
   rc E-T0-MATCH = or  rc E-T0-DEFER = or
   TRUSTED? and ;

\ A recovery-bound head's scan marks its record (checker.f RECOVERY-RECORD)
\ unless deferred; a TRUSTED: head's newer TRUST-DECL row has none.
: HEAD-MARKED? ( -- bool )
   M-HEAD-RECOVERY @ TRUSTED? 0= and ;

public

\ A word compiled code cannot reach and a MALFORMED? body take the reject
\ status, as the JIT refuses them; p is the refused token's byte or NULL-PTR.
: SETTLE-UNIT ( n n bool ptr u8 n ptr u8 -- )
   {: entry-rc:n rc:n undefined:bool a:ptr u:n p:ptr :}
   entry-rc 0<> if
      M-DOES-FRAME @ 0<> if NULL-PTR 0 false CHECKER-OWNER:DOES-FINISH then
      entry-rc throw
   then
   rc E-COMP-RECOVERY = if
      \ A HEAD-MARKED? definer keeps its rows, as a withheld colon word does;
      \ every other withheld definer is rolled back.
      M-DOES-FRAME @ 0<> if
         NULL-PTR 0 HEAD-MARKED? CHECKER-OWNER:DOES-FINISH
         HEAD-MARKED? if CHECKER-OWNER:DOES-COMMIT then
         0 M-DOES-FRAME !
      then
      rc throw
   then
   undefined if
      a u p REPORT-FAILURE
      a u p REPORT-UNDEFINED
      RETRACT RC-REJECT throw
   then
   rc MALFORMED? if
      a u p REPORT-FAILURE
      RETRACT RC-REJECT throw
   then
   rc 0 <> if
      rc RC-REJECT <> CHECKER-OWNER:JSON-REPORTED? 0= or
      if a u p rc REPORT-REFUSAL then
      RETRACT rc throw
   then ;

: STAGE-UNIT ( ptr u8 n -- )
   {: sa su:n :}
   0 CHECKER-OWNER:BIND-REGIME!
   NFETCH:RELEASE
   sa M-SRC ! su M-SRC-U !
   NULL-PTR M-SCAN !
   0 M-IN ! 0 M-OUT !
   TRUSTED? NHOST:SOURCE-BEGIN
   0 M-VERDICT !
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

seal-package

;package
