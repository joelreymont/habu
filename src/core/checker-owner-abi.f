\ checker-owner-abi.f - callable fields in one declaration owner.
\ Loaded after the source extension and before checker.f; no engine DATA slots
\ live here.

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
$50 constant DOES-CHECK-OFF
$58 constant CALL-CELLS-OFF
$60 constant CALL-GLUE-OFF
$68 constant CALL-MATCH-OFF
$70 constant CALL-QUOT-IN-OFF
$78 constant CALL-QUOT-OUT-OFF
$80 constant TRUST-DECL-OFF
$88 constant REC-MIN-IN-OFF
$90 constant REC-WIDE-PUBLISH-OFF
$98 constant CHECK-UNJUDGED-OFF

$A0 constant PAYLOAD-ARM-OFF
$A8 constant PAYLOAD-FREEZE-OFF
$B0 constant PAYLOAD-LOOKUP-OFF
$B8 constant PAYLOAD-SPANS-OFF
$C0 constant PAYLOAD-REG-SAVE-OFF
$C8 constant PAYLOAD-DISARM-OFF

$D0 constant VERIFY-START-OFF
$D8 constant VERIFY-DONE-OFF
$E0 constant VERIFY-RECORD-SYM-OFF
$E8 constant VERIFY-FIND-SYM-OFF
$F0 constant VERIFY-CREATES-SYM-OFF
$F8 constant VERIFY-RECORD-CREATED-OFF
$100 constant VERIFY-SOURCE-DOES-OFF
$108 constant NATIVE-DOES-FINISH-OFF
$110 constant NATIVE-DOES-BEGIN-OFF
$118 constant NATIVE-DOES-COMMIT-OFF
$120 constant TRUSTED-TICK-OFF
$128 constant INIT-LAYOUT-OFF
$130 constant FIELD-SPAN-OFF
\ The engine's package scope was put back after a throw: the checker re-reads
\ its package mirror from it.
$138 constant PKG-RESYNC-OFF
$140 constant VERIFY-FILE-OFF
$148 constant VERIFY-RENDERS-OFF
\ The `linear:` declarer's registrar: the owner of a DEFLINEAR type mints and
\ erases its token, as `cast:` reaches CAST-OFF.
$150 constant LINEAR-OFF
\ The source pre-pass's questions: what the load does with a top-level token,
\ the report of a stretch deferred to the run, what a TRUSTED: body's calls may
\ do, and the report of a definition deferred to the run.
$158 constant VERIFY-TOP-OFF
$160 constant VERIFY-DEFERRED-OFF
$168 constant VERIFY-REACH-OFF
\ A recording symbol's own identity: its package, which a consumer of the
\ verifier shows with a definition; the tail it was recorded under, the
\ verifier's key for an export's own record; and its visibility.
$170 constant VERIFY-SYM-IDENTITY-OFF
\ The returned span is borrowed from this checker instance. Every field is one
\ cell; EFFECT is an offset plus one in its effect store, not a durable ID.
$178 constant CALL-BINDING-OFF
\ A completed explicitly unjudged scan exposes original resolution and the
\ declared ABI, without granting a checked call or body verdict.
$180 constant UNJUDGED-BINDING-OFF
\ A scan serial from the same owner that supplies the borrowed binding rows.
$188 constant BINDING-WINDOW-OFF
\ The source pre-pass borrows the selected deferred body token's source span.
$190 constant VERIFY-DEFERRED-BODY-OFF
\ Navigation: the verifier arms a named declaration's spelling and location
\ around its registrar, the arm taking ( name len visit start end -- ), and
\ receives the uses a scope's checks bind.
$198 constant VERIFY-DECL-ARM-OFF
$1A0 constant VERIFY-DECL-DISARM-OFF
$1A8 constant VERIFY-USES-OFF
\ The verifier scopes its prospective compiler ordering answer to one body.
$1B0 constant WITH-TICK-ORDER-OFF
\ The source verifier's registrar: TRUST-DECL's registration, answering whether
\ it retained the row, so the verifier reports only the declarations whose
\ rows were kept.
$1B8 constant VERIFY-DECL-OFF
\ The borrowed C2 transfer fact.
$1C0 constant C2-STOW-OFF
\ src/lib/error.f names this code E-COMP-BINDING.
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
11 constant BOUND-DEAD
12 constant BOUND-IN
13 constant BOUND-OUT
14 constant BOUND-GLUE
15 constant BOUND-CELLS
\ The source resolver marked the selected record as the seeded primitive.
$20000 constant BOUND-SEEDED
1 constant BOUND-DICT
2 constant BOUND-INTRINSIC
3 constant BOUND-PENDING
4 constant BOUND-UNRESOLVED
\ A refused definition's rows: the store cut back to the end ROWS-END-OFF read
\ when the definition began.
$1C8 constant RETRACT-ROWS-OFF
$1D0 constant ROWS-END-OFF
\ The compile window: from the end of the compiler's scan to publication's last
\ callback the checker refuses every store write with the code set here,
\ 0 when shut.
$1D8 constant WRITE-WINDOW-OFF
\ The does> clause record a replayed TRUSTED: definer publishes at its `;`: its
\ clause is declared, never checked, so VERIFY-SOURCE-DOES-OFF, which checks one
\ first, is not this.
$1E0 constant VERIFY-SOURCE-CLAUSE-OFF
\ Completion: the verifier arms the cursor the next body check fires at, and
\ asks which spellings bind at a top-level cursor. The position kind says how
\ each spelling is selected: as a body token binds, as a top-level token binds,
\ or as a body's named operand - a tick or `is` target - binds, which
\ no local answers.
$1E8 constant VERIFY-CURSOR-OFF
$1F0 constant VERIFY-EACH-VISIBLE-OFF
\ The load's top-level find of a token, asked quietly ( ptr u8 n -- sym eff1 ctl ), eff1 the
\ visible effect record's offset + 1: 0 0 0 if nothing live binds it, 0 0 BINDING-REFUSED if refused.
$1F8 constant VERIFY-TOP-BINDING-OFF
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
$200 constant VERIFY-TRUST-OFF
\ The word a symbol is: ( n -- n ), for an export the symbol of the word it
\ exports, through any re-exports; otherwise the symbol itself.
$208 constant VERIFY-SYM-SOURCE-OFF
\ A local's final width: ( n -- n ), the cells the local of bind sequence n
\ holds once its body is checked. The tape's K-LOCAL-DECL and K-LOCAL-REF events
\ carry that sequence.
$210 constant LOCAL-WIDTH-OFF
CHECKER-OWNER-EXT:RECORD-PUBLISHED-OFF constant RECORD-PUBLISHED-OFF      \ ( n -- )
CHECKER-OWNER-EXT:GRID-BEFORE-OFF constant GRID-BEFORE-OFF               \ ( ptr n -- n )
CHECKER-OWNER-EXT:GRID-AFTER-OFF constant GRID-AFTER-OFF                 \ ( ptr n n -- )
CHECKER-OWNER-EXT:GRID-REVOKE-OFF constant GRID-REVOKE-OFF               \ ( n -- )
CHECKER-OWNER-EXT:GRID-RETIRE-OFF constant GRID-RETIRE-OFF               \ ( ptr n -- )
CHECKER-OWNER-EXT:PUBLICATION-USE-OFF constant PUBLICATION-USE-OFF \ ( [ n -- ] -- )
CHECKER-OWNER-EXT:VERIFY-CTOR-DECL-OFF constant VERIFY-CTOR-DECL-OFF \ ( sym name len visit start end -- )
CHECKER-OWNER-EXT:EXPORT-ACCEPT-OFF constant EXPORT-ACCEPT-OFF \ ( ptr u8 n n -- )
\ Native diagnostics: the live checker owns diagnostic emission even while a
\ baked compiler asks it.
CHECKER-OWNER-EXT:RESET-REPORT-OFF constant RESET-REPORT-OFF \ ( -- )
CHECKER-OWNER-EXT:JSON-REPORTED-OFF constant JSON-REPORTED-OFF \ ( -- bool )
CHECKER-OWNER-EXT:DOES-UNJUDGED-OFF constant DOES-UNJUDGED-OFF \ ( ptr u8 n ptr u8 n -- n )
CHECKER-OWNER-EXT:NATIVE-DOES-REPORT-OFF constant NATIVE-DOES-REPORT-OFF \ ( n ptr u8 n -- n )
CHECKER-OWNER-EXT:BODY-CLOSE-OFF constant BODY-CLOSE-OFF \ ( -- )
CHECKER-OWNER-EXT:SIG-COUNTED-OFF constant SIG-COUNTED-OFF \ ( -- bool )
CHECKER-OWNER-EXT:UNJUDGED-CLOSED-OFF constant UNJUDGED-CLOSED-OFF \ ( -- bool )
CHECKER-OWNER-EXT:PIN-SPAN-OFF constant PIN-SPAN-OFF \ ( -- n n bool )
CHECKER-OWNER-EXT:PIN-TOKEN-OFF constant PIN-TOKEN-OFF \ ( -- ptr u8 n )
CHECKER-OWNER-EXT:OWNER-BYTES constant BYTES

\ These cells precede the record; callable offsets count from the record
\ pointer, after them.
$4842434B4F574E01 constant MAGIC
16 constant HEADER-BYTES

;package
