\ checker-owner-abi.f - callable fields in one declaration owner.
\ Loaded before checker.f by each core prefix; also require-able by a compiler
\ being rebuilt against an older retained prefix. The prefix loads the source
\ extension before this file; no engine DATA slots live here.

package CHECKER-OWNER-ABI
public

$0 constant RAW-OFF
$8 constant EFFECT-OFF
$10 constant DEFER-OFF
$18 constant CAST-OFF
$20 constant USING-OFF
$28 constant PACKAGE-OFF
$30 constant PUBLIC-OFF
$38 constant PRIVATE-OFF
$40 constant END-PACKAGE-OFF
$48 constant EXPORT-OFF
$50 constant WIDE-OFF
$60 constant CAPTURE-OFF
$68 constant CHECK-OFF
$70 constant TAPE-INSTALL-OFF
$78 constant TAPE-ARM-OFF
$80 constant TAPE-DISARM-OFF
$88 constant TAPE-ADVANCE-OFF
$90 constant DOES-CHECK-OFF
$98 constant DOES-IN-OFF
$A0 constant DOES-OUT-OFF
$A8 constant DOES-WIDE-OFF
\ $B0 is vacant.
$B8 constant CALL-CELLS-OFF
$C0 constant CALL-GLUE-OFF
$C8 constant CALL-MATCH-OFF
$D0 constant CALL-QUOT-IN-OFF
$D8 constant CALL-QUOT-OUT-OFF
$E0 constant TRUST-DECL-OFF
$E8 constant PARSE-IMM-OFF
$F0 constant EFFECT-QUERY-OFF
$F8 constant EFFECT-DIN-N-OFF
$100 constant EFFECT-DOUT-N-OFF
$108 constant EFFECT-DIN-CELLS-OFF
$110 constant EFFECT-DOUT-CELLS-OFF
$118 constant EFFECT-DIN-SLOT-OFF
$120 constant EFFECT-DOUT-SLOT-OFF
$128 constant EFFECT-DIN-QUOT-OFF
$130 constant EFFECT-DOUT-QUOT-OFF
$138 constant EFFECT-QUOT-UP-OFF
\ The slot has no reader in this tree; checker.f fills it for a master-host build's compiler.
$140 constant EFFECT-RET-NEUTRAL-OFF
$148 constant EFFECT-QUOT-SIMPLE-OFF
$150 constant EFFECT-CATCH-CELLS-OFF
$158 constant EFFECT-EXEC-CELLS-OFF
$160 constant EFFECT-FINALLY-CELLS-OFF
$168 constant EFFECT-MATCH-CELLS-OFF
$170 constant CTL-DEAD-OFF
$178 constant WF-W-AT-OFF
$180 constant REC-MIN-IN-OFF
$188 constant REC-WIDE-PUBLISH-OFF
$190 constant CHECK-UNJUDGED-OFF
$198 constant FAMILY-MATCH-OFF
$1A0 constant FAMILY-CON-OFF
$1A8 constant FAMILY-VARIANT-OFF
$1B0 constant FAMILY-SLOTS-OFF
$1B8 constant FAMILY-VARIANTS-OFF
$1C0 constant FAMILY-NAME-OFF
$1C8 constant VARIANT-TAG-OFF
$1D0 constant VARIANT-PADS-OFF
$1D8 constant VARIANT-PAY-CELLS-OFF
$1E0 constant VARIANT-PAY-TERMS-OFF

$1E8 constant PAYLOAD-ARM-OFF
$1F0 constant PAYLOAD-FREEZE-OFF
$1F8 constant PAYLOAD-LOOKUP-OFF
$200 constant PAYLOAD-SPANS-OFF
$208 constant PAYLOAD-REG-SAVE-OFF
$210 constant PAYLOAD-DISARM-OFF

\ The does> row's own value boundaries: the clause's term counts and the bundle
\ slot of each term. $218 is the frozen certificate, which these rows are
\ appended after.
$220 constant DOES-IN-N-OFF
$228 constant DOES-OUT-N-OFF
$230 constant DOES-IN-SLOT-OFF
$238 constant DOES-OUT-SLOT-OFF
$240 constant VERIFY-START-OFF
$248 constant VERIFY-DONE-OFF
$250 constant FAMILY-CVAR-OFF
$258 constant VERIFY-RECORD-SYM-OFF
$260 constant VERIFY-FIND-SYM-OFF
$268 constant VERIFY-CREATES-SYM-OFF
$270 constant VERIFY-RECORD-CREATED-OFF
$278 constant VERIFY-SOURCE-DOES-OFF
$280 constant NATIVE-DOES-FINISH-OFF
$288 constant NATIVE-DOES-BEGIN-OFF
$290 constant NATIVE-DOES-COMMIT-OFF
$298 constant UNIT-MARK-OFF
$2A0 constant UNIT-EXPORT-OFF
$2A8 constant UNIT-IMPORT-OFF
$2B0 constant TRUSTED-TICK-OFF
$2B8 constant INIT-LAYOUT-OFF
$2C0 constant FIELD-SPAN-OFF
\ The engine's package scope was put back after a throw: the checker re-reads
\ its package mirror from it.
$2C8 constant PKG-RESYNC-OFF
$2D0 constant VERIFY-FILE-OFF
$2D8 constant VERIFY-RENDERS-OFF
\ The `linear:` declarer's registrar: the owner of a DEFLINEAR type mints and
\ erases its token, as `cast:` reaches CAST-OFF.
$2E8 constant LINEAR-OFF
\ The source pre-pass's questions: what the load does with a top-level token,
\ the report of a stretch deferred to the run, what a TRUSTED: body's calls may
\ do, and the report of a definition deferred to the run.
$2F0 constant VERIFY-TOP-OFF
$2F8 constant VERIFY-DEFERRED-OFF
$300 constant VERIFY-REACH-OFF
\ A recording symbol's own identity: its package, which a consumer of the
\ verifier shows with a definition; the tail it was recorded under, the
\ verifier's key for an export's own record; and its visibility.
$308 constant VERIFY-SYM-IDENTITY-OFF
\ The returned span is borrowed from this checker instance. Every field is one
\ cell; EFFECT is an offset plus one in its effect store, not a durable ID.
$310 constant CALL-BINDING-OFF
\ A completed explicitly unjudged scan exposes original resolution and the
\ declared ABI, without granting a checked call or body verdict.
$318 constant UNJUDGED-BINDING-OFF
\ $320, $328 and $330 are vacant.
\ A scan serial from the same owner that supplies the borrowed binding rows.
$338 constant BINDING-WINDOW-OFF
\ The source pre-pass borrows the selected deferred body token's source span.
$340 constant VERIFY-DEFERRED-BODY-OFF
\ Navigation: the verifier arms a named declaration's spelling and location
\ around its registrar, the arm taking ( name len visit start end -- ), and
\ receives the uses a scope's checks bind.
$348 constant VERIFY-DECL-ARM-OFF
$350 constant VERIFY-DECL-DISARM-OFF
$358 constant VERIFY-USES-OFF
\ The verifier scopes its prospective compiler ordering answer to one body.
$360 constant WITH-TICK-ORDER-OFF
\ The source verifier's registrar: TRUST-DECL's registration, answering whether
\ it retained the row, so the verifier reports only the declarations whose
\ rows were kept.
$368 constant VERIFY-DECL-OFF
\ The borrowed C2 transfer fact.
$370 constant C2-STOW-OFF
\ src/lib/error.f names this code E-COMP-BINDING; a retained build host loads
\ this constants-only ABI before it can load the new error word.
-8575 constant BINDING-RC
0 constant BOUND-ORD
1 constant BOUND-KIND
2 constant BOUND-SYM
3 constant BOUND-EFFECT
\ A BOUND-DICT row's record index, where every index from 0 names a record; a
\ row of any other kind holds -1.
4 constant BOUND-RECORD
5 constant BOUND-WID
6 constant BOUND-ENTRY
7 constant BOUND-FLAGS
8 constant BOUND-PEND-IX
9 constant BOUND-PEND-OFF
10 constant BOUND-CTL
\ Every resolved row's return effect is neutral and this tree reads no cell 11,
\ but a retained compiler built before that rule sizes these rows by BOUND-CELLS
\ and refuses a call whose cell 11 holds 0. So the checker still writes 1 there
\ for every resolved effect.
11 constant BOUND-NEUTRAL
12 constant BOUND-DEAD
13 constant BOUND-IN
14 constant BOUND-OUT
15 constant BOUND-GLUE
16 constant BOUND-CELLS
\ The source resolver marked the selected record as the seeded primitive.
$20000 constant BOUND-SEEDED
1 constant BOUND-DICT
2 constant BOUND-INTRINSIC
3 constant BOUND-PENDING
4 constant BOUND-UNRESOLVED
\ A refused definition's rows: the store cut back to the end ROWS-END-OFF read
\ when the definition began.
$378 constant RETRACT-ROWS-OFF
$380 constant ROWS-END-OFF
\ The compile window: from the end of the compiler's scan to publication's last
\ callback the checker refuses every store write with the code set here,
\ 0 when shut.
$388 constant WRITE-WINDOW-OFF
\ The does> clause record a replayed TRUSTED: definer publishes at its `;`: its
\ clause is declared, never checked, so VERIFY-SOURCE-DOES-OFF, which checks one
\ first, is not this.
$390 constant VERIFY-SOURCE-CLAUSE-OFF
\ Completion: the verifier arms the cursor the next body check fires at, and
\ asks which spellings bind at a top-level cursor. The position kind says how
\ each spelling is selected: as a body token binds, as a top-level token binds,
\ or as a body's named operand - a tick or `is` target - binds, which
\ no local answers.
$398 constant VERIFY-CURSOR-OFF
$3A0 constant VERIFY-EACH-VISIBLE-OFF
\ The load's top-level find of a token, asked quietly ( ptr u8 n -- sym eff1 ctl ), eff1 the
\ visible effect record's offset + 1: 0 0 0 if nothing live binds it, 0 0 BINDING-REFUSED if refused.
$3A8 constant VERIFY-TOP-BINDING-OFF
\ The control word's facts its reader tests: the word may read the source after
\ it, it is deferred, its string operand names a word (a `names:` row), a
\ `generates:` row states the words it creates, its record's facts text says
\ `core-op`, and the field holding the id of an engine word, with the ids of
\ `parses:` and `parses-through:`, the declarers of what such a word reads,
\ and of `names:`.
$2000 constant BINDING-PARSES
$10000 constant BINDING-DEFER
$20000 constant BINDING-NAMES
$40000 constant BINDING-GENERATES
\ The top-level walk refused the name, as it does an ambiguous or shadowed one.
$80000 constant BINDING-REFUSED
$10 constant BINDING-CORE-OP
8 constant BINDING-ID-SHIFT
$1F00 constant BINDING-ID-MASK
11 constant BINDING-PARSES-ID
12 constant BINDING-THROUGH-ID
13 constant BINDING-NAMES-ID
0 constant VISIBLE-BODY
1 constant VISIBLE-TOP
2 constant VISIBLE-NAMED
\ Whether a `trust` row is the run's to judge: ( ptr u8 n -- bool ), true when
\ the row names no word in the wordlist its record lands in and a rendering
\ statement read before it marked that wordlist.
$3B0 constant VERIFY-TRUST-OFF
\ The word a symbol is: ( n -- n ), for an export the symbol of the word it
\ exports, through any re-exports; otherwise the symbol itself.
$3B8 constant VERIFY-SYM-SOURCE-OFF
\ A local's final width: ( n -- n ), the cells the local of bind sequence n
\ holds once its body is checked. The tape's K-LOCAL-DECL and K-LOCAL-REF events
\ carry that sequence.
$3C0 constant LOCAL-WIDTH-OFF
\ Prepared checker sections are selected before any FILE range publishes.
$3C8 constant UNIT-PREPARE-BEGIN-OFF  \ ( -- )
$3D0 constant UNIT-PREPARE-OFF        \ ( section size data-floor data-end -- row )
$3D8 constant UNIT-INSTALL-OFF        \ ( row -- )
$3E0 constant UNIT-PREPARE-END-OFF    \ ( -- )
\ Older records end before these fields.
CHECKER-OWNER-EXT:RECORD-CONTRACT-OFF constant RECORD-CONTRACT-OFF      \ ( n -- ptr u8 n bool )
CHECKER-OWNER-EXT:RECORD-PUBLISHED-OFF constant RECORD-PUBLISHED-OFF      \ ( n -- )
CHECKER-OWNER-EXT:GRID-BEFORE-OFF constant GRID-BEFORE-OFF               \ ( ptr n -- n )
CHECKER-OWNER-EXT:GRID-AFTER-OFF constant GRID-AFTER-OFF                 \ ( ptr n n -- )
CHECKER-OWNER-EXT:GRID-REVOKE-OFF constant GRID-REVOKE-OFF               \ ( n -- )
CHECKER-OWNER-EXT:GRID-RETIRE-OFF constant GRID-RETIRE-OFF               \ ( ptr n -- )
CHECKER-OWNER-EXT:RECORD-RESTORE-OFF constant RECORD-RESTORE-OFF         \ ( -- )
\ Borrowed FILE name-resolution rows. The source owner copies the spelling and
\ namespace into each six-cell row before exposing it to the native feed.
$460 constant RESOLUTION-COUNT-OFF       \ ( -- n )
$468 constant RESOLUTION-ROW-OFF         \ ( n -- ptr u8 n )
$470 constant RESOLUTION-TOP-OFF         \ ( ptr u8 n [ ptr u8 n -- ] -- )
$478 constant RESOLUTION-NS-INSTALL-OFF  \ ( [ ptr u8 n -- ] -- )
$480 constant RESOLUTION-NS-DEFAULT-OFF  \ ( -- )
CHECKER-OWNER-EXT:PUBLICATION-USE-OFF constant PUBLICATION-USE-OFF \ ( [ n -- ] -- )
CHECKER-OWNER-EXT:RECORD-CAPTURE-OFF constant RECORD-CAPTURE-OFF \ ( [ n -- n ] [ -- ] -- )
CHECKER-OWNER-EXT:RECORD-SEED-OFF constant RECORD-SEED-OFF \ ( -- )
CHECKER-OWNER-EXT:VERIFY-CTOR-DECL-OFF constant VERIFY-CTOR-DECL-OFF \ ( sym name len visit start end -- )
CHECKER-OWNER-EXT:EXPORT-ACCEPT-OFF constant EXPORT-ACCEPT-OFF \ ( ptr u8 n n -- )
\ Native multi-error recovery and diagnostics: the live checker answers whether
\ this run reports every refusal and whether a refused declaration's
\ recovery row was used, and it owns diagnostic emission even while a baked
\ compiler asks it.
CHECKER-OWNER-EXT:MULTI-ERROR-OFF constant MULTI-ERROR-OFF \ ( -- bool )
CHECKER-OWNER-EXT:RESET-REPORT-OFF constant RESET-REPORT-OFF \ ( -- )
CHECKER-OWNER-EXT:JSON-REPORTED-OFF constant JSON-REPORTED-OFF \ ( -- bool )
CHECKER-OWNER-EXT:RECOVERY-USED-OFF constant RECOVERY-USED-OFF \ ( -- bool )
CHECKER-OWNER-EXT:DOES-UNJUDGED-OFF constant DOES-UNJUDGED-OFF \ ( ptr u8 n ptr u8 n -- n )
CHECKER-OWNER-EXT:NATIVE-DOES-REPORT-OFF constant NATIVE-DOES-REPORT-OFF \ ( n ptr u8 n -- n )
CHECKER-OWNER-EXT:BODY-CLOSE-OFF constant BODY-CLOSE-OFF \ ( -- )
CHECKER-OWNER-EXT:PREFIX-BIND-OFF constant PREFIX-BIND-OFF \ ( ptr u8 n n -- n bool )
CHECKER-OWNER-EXT:UNRECORDED-IN-OFF constant UNRECORDED-IN-OFF \ ( -- n )
CHECKER-OWNER-EXT:UNRECORDED-OUT-OFF constant UNRECORDED-OUT-OFF \ ( -- n )
CHECKER-OWNER-EXT:SIG-COUNTED-OFF constant SIG-COUNTED-OFF \ ( -- bool )
CHECKER-OWNER-EXT:UNJUDGED-CLOSED-OFF constant UNJUDGED-CLOSED-OFF \ ( -- bool )
CHECKER-OWNER-EXT:PIN-SPAN-OFF constant PIN-SPAN-OFF \ ( -- n n bool )
CHECKER-OWNER-EXT:PIN-TOKEN-OFF constant PIN-TOKEN-OFF \ ( -- ptr u8 n )
CHECKER-OWNER-EXT:FAMILY-NICHE-OFF constant FAMILY-NICHE-OFF \ ( n -- bool )
0 constant RES-ORD
1 constant RES-ROLE
2 constant RES-RECORD
3 constant RES-SHAPE
4 constant RES-SPELL-U
5 constant RES-PACKAGE-U
6 constant RES-HEADER-CELLS
0 constant RES-ROLE-GLOBAL
1 constant RES-ROLE-OPEN-PUBLIC
2 constant RES-ROLE-OPEN-PRIVATE
3 constant RES-ROLE-USING-PUBLIC
4 constant RES-ROLE-QUALIFIED-PUBLIC
5 constant RES-ROLE-OPEN-NAMESPACE
6 constant RES-ROLE-USING-NAMESPACE
7 constant RES-ROLE-QUALIFIED-NAMESPACE
8 constant RES-ROLE-PACKAGE-DECISION
9 constant RES-ROLE-USING-DECISION
CHECKER-FETCH-ABI:BYTES constant BYTES

CHECKER-OWNER-EXT:CONTRACT-MAGIC constant RECORD-CONTRACT-MAGIC
CHECKER-OWNER-EXT:MIN-BYTES constant RECORD-CONTRACT-MIN-BYTES
CHECKER-OWNER-EXT:FIXED-NONE constant RECORD-FIXED-NONE
CHECKER-OWNER-EXT:FIXED-VAL constant RECORD-FIXED-VAL
CHECKER-OWNER-EXT:FIXED-ADDR constant RECORD-FIXED-ADDR

\ These cells precede the record; callable offsets and the record pointer stay
\ unchanged. Older records have no descriptor and cannot supply appended fields.
$4842434B4F574E01 constant MAGIC
16 constant HEADER-BYTES

;package
