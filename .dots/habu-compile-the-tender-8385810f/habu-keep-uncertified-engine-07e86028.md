---
title: Keep uncertified engine helpers internal after native compilation
status: active
priority: 1
issue-type: task
created-at: "2026-09-14T00:49:30.490501+03:00"
---

Owner Cedar. G2 exposed U-TYPE, T-RES, CT-LIVE? and CHECKER-BOUND:CURSORS after native compilation: inferred call shapes were being treated as source authority, and internal-mark recognized only a JIT prologue. The original reproducer is /tmp/cedar-internal-records.f.

The repair uses positive EFFECT-EXTERNAL ($8) provenance in the existing per-symbol flags. Explicit declarations and enforced successful checks grant it; ABI-only scans and failed multi-error rows do not. The checker-owned ABI scope restores enforcement on throws. Export, owner transfer and graph import preserve the bit. Ordinary checking may use a PRIM declaration's own effect when a user row is ABI-only; its trusted-only policy still applies. EFFECT-QUERY remains an ABI query, while CHECKER-RESOLVES? requires a source-visible user effect and still excludes primitive-only rows. Prefix sealing uses DKIND plus the source-authorized minimum, with no instruction decoding. ABI-only rows publish no dictionary minimum.

No owner ABI, graph-record or dictionary-layout growth. The graph mask admits $1000F and rejects unknown bits. Old producer rows without provenance acquire no grant at import; an enforcing target therefore cannot assume early old-host declarations were public. Activation is staged: private producer A records provenance before consumer B enforces it.

Frozen production source:

- 42f05fd7, parent 6ebbd50f: producer publication, scoped scan and native dispatch. This transitional product deliberately retains the old consumer gate.
- 39649b46, parent 42f05fd7: enforcing checker, source query, DKIND seal and focused authority/graph tests.

Actual products under /tmp/cedar-family-stage-abi:

- hb-effect-authority-A, reader-product hosted build, rc 0 in 130.64 s; SHA256 9c8b3906cbb3dc4fc2b171e1ae55074472c297d83854cab6934190539abdb125.
- hb-effect-authority-B, A-hosted build of 39649b46, rc 0 in 129.15 s; SHA256 9cb7464598b9c8a3055830e8ddea905b1a8209a850a87d9018f4103c68ce82ca.
- hb-effect-authority-C, B-hosted repeated build of the same source, rc 0 in 127.29 s; byte-identical to B.

B and C pass the actual tier-1 checker-effect-authority fixture (judged/ABI/explicit rows, primitive pairing, thrown scope, failed check, rollback/regrowth, package shadow/export and real native pre-hook/redefinition publication), the complete original internal-word-gate with an added CHECKER-BOUND cursor negative, and prefix-mark-test. Logs are /tmp/cedar-effect-authority-{unit,internal-gate,prefix-mark}.log and the corresponding -C- names. The complete B aot-payload-graph suite passes, including unknown-bit rejection and exited native producer/reader followed by a fresh consumer that refuses the ABI-only row and executes asserted/checked public calls. The fresh test must trigger lazy intake with CHECK! before EFFECT-QUERY; the ABI query itself does not import rows. The metadata fixture also requires a private bin/hb link to its paired product. Log: /tmp/cedar-effect-authority-graph.log.

Root's independent production review approved all publication paths, CTL ordering, rollback caches, transfer/import flags, primitive pairing and sealing. Root's unchanged checker-scan-index suite passes B (81.828 s). Its type-export run found the former whole-flags-zero expectation now includes EFFECT-EXTERNAL; the focused repair asserts control flags and authority separately and retains raw-zero rollback checks. The repaired suite passes B, rc 0, /tmp/cedar-effect-authority-type-export.log. Root owns combined product composition and the full gate. No compile-floor claim belongs to this correctness repair.

Combined J followup (base `674900c6`): dead-path F5/F11/F12 were whole-flag
zero assertions now seeing `$8`. They now independently assert the intended
dead/throw bits and the checked binding's source authority. The rollback-signature
fixture's ARM/FREEZE/LOOKUP are explicit private payload-owner boundaries;
their membership, rollback and serialized graph assertions are unchanged.
Both repaired suites pass actual J, rc0, in 0.541/0.557 s; logs
`/tmp/cedar-authority-followup-{dead-path,rollback}.log`.

The native window reduction resolved EVENTS' bare SCAN to CHECKER-TAPE:SCAN
(same symbol 3810 as the qualified spelling, flags0, ABI minimum2), distinct
from CHECKER-EFFECT-AUTHORITY:SCAN (symbol1568, minimum1). The tape's PPRIM API
grants observer installation/arming, not its checker-owned event producers.
Only the whitebox EVENTS injector is now trusted. Actual J's tier1 source-owner
window passes all existing callback/detach assertions with `window: 0` and no
stderr, rc0 in31.012 s; `/tmp/cedar-authority-followup-tape-window.log`.

The multi-error cascade is a separate confirmed production regression: failed
declarations retain ABI facts without source authority, so a downstream caller
now adds E-CAP-TRUSTED. Recovery must identify current-run failed rows and taint
dependent analysis without granting executable authority. Root approved that
bounded design for a separate followup; no broad MULTI-ERR visibility bypass.

Recovery repair `5ab65284`, parent `5f0162a5`, uses the existing in-memory
ER.ACTIVE cell: tag2 is a failed declaration or a dependent analysis result.
MULTI-ERR-BEGIN records the effect-store floor; only tag2 rows created in that
run can suppress a cascade. Store rewind moves the floor with the removed
records. Ordinary checks, later runs, and unrelated ABI-only or trusted-only
calls still refuse. Checked publication of a dependent row keeps its external
bit and minimum-input latch clear. Explicit declarations replace tag2 normally,
including inside an ABI scope. Candidate and ABI catch boundaries restore the
enclosing recovery taint; CHECK-RESET clears it for the next definition.

ER.ACTIVE audit: SCAN-USIGS-SYM and CHECKED-ROW distinguish deleted0 from live
rows, so tag2 remains an ABI fact. USX/HIDX cache exact record offsets and
rollback restores their previous heads; interning keys graph nodes after the
record header. EXPORT preserves current-run tag2 without granting authority.
ASIG-GRAPH-COPY replaces the active cell with graph magic, and CK-GRAPH-IMPORT
sets active1. TRANSFER-ROW rebuilds active1 through E-ADD-EFFECT. Both imports
preserve external=false and do not inherit recovery eligibility. The graph
layout and unknown-bit mask are unchanged. Existing FULL-PRODUCE emits an empty
certificate for every MULTI run; the new real publication control pins that
behavior and the published dictionary's zero minimum bits.

Actual J-hosted native product: `/tmp/cedar-family-stage-abi/hb-effect-recovery`,
build rc0 in126.272s from `5ab65284`, SHA256
`83e0b0ea4c4b13f4d6470c4c43372c674a9b0069dc0b10fba7269941e453bb81`.
Tier1 authority and nested tape-observer suites pass (1.102/0.970s); engine
passes (16.342s, MEA1/MEA2 only), lower-cert passes (0.513s), and the complete
all-errors suite passes (0.694s, including cascade-no-phantom). The complete
program-diagnostics suite also passes (32.865s). Logs are
`/tmp/cedar-recovery-{authority,tape,engine,lower-cert,all-errors,program-diagnostics}.log`.
The complete payload-graph suite, including unknown-bit and fresh-process
authority controls, passes (20.277s), as does type-export (0.520s):
`/tmp/cedar-recovery-{graph,type-export}.log`.
Authority controls cover direct/transitive, branch, quotation, tick and inferred
dependencies, stale-run refusal, unrelated ABI/trusted-only refusals,
rollback/regrowth, explicit replacement, thrown ABI scope, nested candidates,
export, actual JIT publication and empty certificates. The compiler still refuses
an actually failed tier1 body; diagnostic replay's forced publication is the
existing tier0 hook path. Root owns combined-product/full-gate acceptance.
