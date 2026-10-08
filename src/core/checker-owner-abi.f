\ checker-owner-abi.f - append-only callable fields in one declaration owner.
\ Loaded before checker.f by each core prefix; also require-able by a compiler
\ being rebuilt against an older retained prefix. No engine DATA slots live here.

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
\ $48 is vacant: a retained prefix still installs its transfer there, so no
\ later field may take it.
$50 constant SOURCE-ROW-OFF
$58 constant SOURCE-CON-OFF
$60 constant EXPORT-OFF
$68 constant WIDE-OFF
$70 constant RESET-OFF
$78 constant CAPTURE-OFF
$80 constant CHECK-OFF
$88 constant TAPE-INSTALL-OFF
$90 constant TAPE-ARM-OFF
$98 constant TAPE-DISARM-OFF
$A0 constant TAPE-ADVANCE-OFF
$A8 constant DOES-CHECK-OFF
$B0 constant DOES-IN-OFF
$B8 constant DOES-OUT-OFF
$C0 constant DOES-WIDE-OFF
\ $C8 is vacant: a retained prefix still installs its signature truncation
\ there, so no later field may take it.
$D0 constant CALL-CELLS-OFF
$D8 constant CALL-GLUE-OFF
$E0 constant CALL-MATCH-OFF
$E8 constant CALL-QUOT-IN-OFF
$F0 constant CALL-QUOT-OUT-OFF
$F8 constant TRUST-DECL-OFF
$100 constant PARSE-IMM-OFF
$108 constant EFFECT-QUERY-OFF
$110 constant EFFECT-DIN-N-OFF
$118 constant EFFECT-DOUT-N-OFF
$120 constant EFFECT-DIN-CELLS-OFF
$128 constant EFFECT-DOUT-CELLS-OFF
$130 constant EFFECT-DIN-SLOT-OFF
$138 constant EFFECT-DOUT-SLOT-OFF
$140 constant EFFECT-DIN-QUOT-OFF
$148 constant EFFECT-DOUT-QUOT-OFF
$150 constant EFFECT-QUOT-UP-OFF
$158 constant EFFECT-RET-NEUTRAL-OFF
$160 constant EFFECT-QUOT-SIMPLE-OFF
$168 constant EFFECT-CATCH-CELLS-OFF
$170 constant EFFECT-EXEC-CELLS-OFF
$178 constant EFFECT-FINALLY-CELLS-OFF
$180 constant EFFECT-MATCH-CELLS-OFF
$188 constant CTL-DEAD-OFF
$190 constant WF-W-AT-OFF
$198 constant REC-MIN-IN-OFF
$1A0 constant REC-WIDE-PUBLISH-OFF
$1A8 constant CHECK-UNJUDGED-OFF
$1B0 constant FAMILY-MATCH-OFF
$1B8 constant FAMILY-CON-OFF
$1C0 constant FAMILY-VARIANT-OFF
$1C8 constant FAMILY-SLOTS-OFF
$1D0 constant FAMILY-VARIANTS-OFF
$1D8 constant FAMILY-NAME-OFF
$1E0 constant VARIANT-TAG-OFF
$1E8 constant VARIANT-PADS-OFF
$1F0 constant VARIANT-PAY-CELLS-OFF
$1F8 constant VARIANT-PAY-TERMS-OFF

$200 constant PAYLOAD-ARM-OFF
$208 constant PAYLOAD-FREEZE-OFF
$210 constant PAYLOAD-LOOKUP-OFF
$218 constant PAYLOAD-SPANS-OFF
$220 constant PAYLOAD-REG-SAVE-OFF
$228 constant PAYLOAD-DISARM-OFF

\ The does> row's own value boundaries, the shape DIN-SLOT / DOUT-SLOT already
\ give a definition's rows: the clause's term counts and the bundle slot of each
\ term, so the native chain can place a does> row from per-cell facts instead of
\ refusing every clause whose value is wider than a cell. $230 is the frozen
\ certificate (checker-fetch-abi.f), which these rows are appended after.
$238 constant DOES-IN-N-OFF
$240 constant DOES-OUT-N-OFF
$248 constant DOES-IN-SLOT-OFF
$250 constant DOES-OUT-SLOT-OFF
$258 constant VERIFY-START-OFF
$260 constant VERIFY-DONE-OFF
$268 constant FAMILY-CVAR-OFF
$270 constant VERIFY-RECORD-SYM-OFF
$278 constant VERIFY-FIND-SYM-OFF
$280 constant VERIFY-CREATES-SYM-OFF
$288 constant VERIFY-RECORD-CREATED-OFF
$290 constant VERIFY-SOURCE-DOES-OFF
$298 constant NATIVE-DOES-FINISH-OFF
$2A0 constant NATIVE-DOES-BEGIN-OFF
$2A8 constant NATIVE-DOES-COMMIT-OFF
$2B0 constant UNIT-MARK-OFF
$2B8 constant UNIT-EXPORT-OFF
$2C0 constant UNIT-IMPORT-OFF
$2C8 constant TRUSTED-TICK-OFF
$2D0 constant INIT-LAYOUT-OFF
$2D8 constant FIELD-SPAN-OFF
\ The engine's package scope was put back after a throw (src/habu/habu2.f
\ LEVALREC, EM-REPL-RECOVER; src/habu/interpret.f INTERPRET): the checker
\ re-reads its package mirror from the restored scope.
$2E0 constant PKG-RESYNC-OFF
$2E8 constant VERIFY-FILE-OFF
$2F0 constant VERIFY-RENDERS-OFF
\ Renders the diagnostic a quiet scan suppressed, for a caller that will not
\ enforce the verdict but must not drop its reason (src/compiler/native/compiler.f
\ CHECK-HOOKLESS).
$2F8 constant CHECK-REPORT-OFF
\ The `linear:` declarer's registrar (src/core/checker.f CHECKER-LINEAR): the
\ owner of a DEFLINEAR type mints and erases its token, as `cast:` reaches
\ CAST-OFF.
$300 constant LINEAR-OFF
\ The source pre-pass's questions: what the load does with a top-level token,
\ the report of a stretch deferred to the run (src/habu/verify-source.f
\ TOP-TOKEN), what a TRUSTED: body's calls may do (SCAN-TRUSTED-BODY), and the
\ report of a definition deferred to the run (REPORT-DEFERRED).
$308 constant VERIFY-TOP-OFF
$310 constant VERIFY-DEFERRED-OFF
$318 constant VERIFY-REACH-OFF
\ A recording symbol's own identity (src/core/checker.f CHECKER-SYM-IDENTITY):
\ its package, which a consumer of the verifier shows with a definition; the
\ tail it was recorded under, the verifier's key for an export's own record;
\ and its visibility.
$320 constant VERIFY-SYM-IDENTITY-OFF
\ The returned span is borrowed from this checker instance. Every field is one
\ cell; EFFECT is an offset plus one in its effect store, not a durable ID.
$328 constant CALL-BINDING-OFF
\ A completed explicitly unjudged scan exposes original resolution and the
\ declared ABI, without granting a checked call or body verdict.
$330 constant UNJUDGED-BINDING-OFF
\ Concrete one-cell constructors and unchanged stack tails for the native
\ implementation owner's checked call contract. Zero means no concrete term.
$338 constant EFFECT-DIN-CON-OFF
$340 constant EFFECT-DOUT-CON-OFF
$348 constant EFFECT-STACK-STABLE-OFF
\ A scan serial from the same owner that supplies the borrowed binding rows.
$350 constant BINDING-WINDOW-OFF
\ The source pre-pass borrows the selected deferred body token's source span.
$358 constant VERIFY-DEFERRED-BODY-OFF
\ Navigation (src/core/checker.f): the verifier arms a named declaration's
\ spelling and location around its registrar (CHECKER-DECL-AT! ( name len
\ visit start end -- ), CHECKER-DECL-AT-OFF) and
\ receives the uses a scope's checks bind (CHECKER-WITH-USES).
$360 constant VERIFY-DECL-ARM-OFF
$368 constant VERIFY-DECL-DISARM-OFF
$370 constant VERIFY-USES-OFF
\ The verifier scopes its prospective compiler ordering answer to one body.
$378 constant WITH-TICK-ORDER-OFF
\ The source verifier's registrar (src/core/checker.f TRUST-DECL?): TRUST-DECL's
\ registration, answering whether it retained the row, so the verifier reports
\ only the declarations whose rows were kept (src/habu/verify-source.f
\ DECL-SIGNATURE).
$380 constant VERIFY-DECL-OFF
\ $388 is the borrowed C2 transfer fact.
$388 constant C2-STOW-OFF
\ lib/errors.f names this code E-NCOMP-BINDING; a retained build host loads
\ this constants-only ABI before it can load the new error word.
-8575 constant BINDING-RC
0 constant BOUND-ORD
1 constant BOUND-KIND
2 constant BOUND-SYM
3 constant BOUND-EFFECT
4 constant BOUND-RECORD
5 constant BOUND-WID
6 constant BOUND-ENTRY
7 constant BOUND-FLAGS
8 constant BOUND-PEND-IX
9 constant BOUND-PEND-OFF
10 constant BOUND-CTL
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
\ A hook-less definition's declaration, recorded as its row without authority
\ (src/core/checker.f CHECKER-DECLARED-ROW!).
$390 constant DECLARED-ROW-OFF
\ A refused definition's rows: the store cut back to the end ROWS-END-OFF read
\ when the definition began (src/core/checker.f CHECKER-RETRACT-ROWS).
$398 constant RETRACT-ROWS-OFF
$3A0 constant ROWS-END-OFF
\ The compile window: from the end of the compiler's scan to publication's last
\ callback the checker refuses every store write with the code set here, 0
\ when shut (src/core/checker.f CHECKER-WRITE-WINDOW!).
$3A8 constant WRITE-WINDOW-OFF
\ The does> clause record a replayed TRUSTED: definer publishes at its `;`
\ (src/habu/verify-source.f TRUSTED-DEFINITION): its clause is declared, never
\ checked, so VERIFY-SOURCE-DOES-OFF, which checks one first, is not this.
$3B0 constant VERIFY-SOURCE-CLAUSE-OFF
\ Completion (src/core/checker.f): the verifier arms the cursor the next body
\ check fires at (CHECKER-CURSOR!), and asks which spellings bind at a
\ top-level cursor (CHECKER-RESOLVE:EACH-VISIBLE). The position kind says how
\ each spelling is selected: as a body token binds, as a top-level token binds,
\ or as a body's named operand - a tick or `is` target - binds, which no local
\ answers.
$3B8 constant VERIFY-CURSOR-OFF
$3C0 constant VERIFY-EACH-VISIBLE-OFF
\ The word the load's top-level find selects for a token, asked quietly:
\ ( ptr u8 n -- sym eff1 ctl ), its symbol, its visible effect record's offset
\ + 1 and its control word, all 0 when the load refuses the token or nothing
\ live binds it (src/core/checker.f CHECKER-VERIFY-TOP-BINDING).
$3C8 constant VERIFY-TOP-BINDING-OFF
\ The control word's facts its reader tests: the word may read the source after
\ it, it is deferred, its string operand names a word (a `names:` row), a
\ `generates:` row states the words it creates (src/core/checker.f
\ CTL-GENERATES), and the field holding the id of an engine word
\ (CTL-INTRINSIC), with the ids of `parses:` and `parses-through:`, the
\ declarers of what such a word reads, and of `names:`.
$2000 constant BINDING-PARSES
$10000 constant BINDING-DEFER
$20000 constant BINDING-NAMES
$40000 constant BINDING-GENERATES
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
\ statement read before it marked that wordlist (src/core/checker.f
\ CHECKER-VERIFY-TRUST).
$3D0 constant VERIFY-TRUST-OFF
\ The word a symbol is (src/core/checker.f CHECKER-SYM-SOURCE): ( n -- n ), for
\ an export the symbol of the word it exports, through any re-exports;
\ otherwise the symbol itself.
$3D8 constant VERIFY-SYM-SOURCE-OFF
\ A local's final width (src/core/checker.f LOCW-HW@): ( n -- n ), the cells
\ the local of bind sequence n holds once its body is checked. The tape's
\ K-LOCAL-DECL and K-LOCAL-REF events carry that sequence.
$3E0 constant LOCAL-WIDTH-OFF
CHECKER-FETCH-ABI:BYTES constant BYTES

\ These cells precede the record; callable offsets and the record pointer stay
\ unchanged. Older records have no descriptor and cannot supply appended fields.
$4842434B4F574E01 constant MAGIC
16 constant HEADER-BYTES

;package
