---
title: "Seal a design to its admitted vocabulary"
status: active
priority: 2
issue-type: task
created-at: "2026-09-30T13:40:12.152617+02:00"
---

Problem: Maki checks and builds each AI-authored design in its own process
(maki AGENTS.md) and must confine it to the declarative subset a mechanical
design needs (maki docs/habu-needs.md). Nothing confines a loaded file today:
the demo design.f (`require lib/ffi-abi.f`, `PROCESS-SYMBOLS`, `FUNCTION:`)
printed the pid with rc 0. Every lookup falls through to global wid 0
(habu1.f:4495-4506); `require`, `included`, `evaluate`, `execute`, `catch`,
`!`, `c!`, `set-tier`, `prot-wid-add` are wid-0 records (src/core/include.f:999,
1039; habu1.f:3428-3516); compiler keywords are rows dispatched before any
lookup (interpret rows habu2.f:8572-8598, compile rows 9636-9687, op rows
9740-9787, string rows 8590-8596/9652-9658), so deleting records cannot remove
`begin`, `recurse`, `'`, `[']`, `[:`, `create`, `trusted:`, `does>`, `is`; and
tier 1 captures a body (habu2.f:7908-7940), runs immediates during capture
(7869-7906) and resolves the rest through NDICT (dict.f:56-95) and the HIR-WORD
dialect rows (hir-word.f:1290-1371, `begin` compiles a loop: probe loop1).

Design: one seal, one admission predicate, two readers. STATE (layout.f, two
claims in data-claims.f): POLICY-NDICT-CELL ($2820; 0 = unsealed, else NDICT at
the seal) and POLICY-BITS-OFF ($2C00, a PROT-shaped $400-byte WID bitmap,
PROT-WID-MAX bound), both in the $2800..$3000 band layout.f:517-524 and
1245-1251 document as free after SRCLOC ($2800-$2810) and EXIT-HOOK-CELL
($2818). Written only by two prims that refuse once sealed: `policy-admit
( ptr u8 n -- )` resolves the package name through WLFIND:LENTRY with wid
DICT-WL:NAMESPACE (habu1.f:3249-3322; x11 = record[0] = the public wid) and
sets that wid's bit (PROT-BITS-AT, habu1.f:3144, bound check as BPROTWIDADD
3163-3178); `policy-seal ( -- )` stores NDICT. lib/policy.f, package POLICY:
`ALLOW ( ptr u8 n -- )` and `SEAL ( -- )` over those prims; nothing else.
PREDICATE. A record is admitted iff unsealed, or its index >= POLICY-NDICT
(the design's own definitions, in whatever wordlist `package` put them), or the
bit of its wid (record[40]) is set. A keyword is admitted iff unsealed or its
baked spelling lies in the KWDATA design span [LKWIF, LKWDESIGNEND): `if`
`else` `then` `{:` `:}` `s"` `package` `public` `private` `;package` `using`
`;using`. Everything else is NOT FOUND: `hb: not in vocabulary: <token> at
<path>:<line>` through LCOMPILEDIE (habu2.f:10132, the one located tail),
exit ENGINE-ERROR:POLICY (105) at top level, a catchable throw of 105 inside an
`included`/`evaluate` frame, nothing later executed. SITES (tier 0, habu2.f):
LPOLICYREC (x5 = record) at the `found` labels of EM-INTERPRET-FIND (8609),
EM-COMPILE-CALL (9803) and after CAPTURE-IMMEDIATE's LFIND when x13 <> 0
(7871-7872), the only loops that consume a SOURCE token's LFIND hit (LFINDUSED
rejoins at `found`); LKWCMP's match exit (2040-2053: sealed and x0 >=
LKWDESIGNEND -> LPOLICY), which every row inherits (20 callers in habu2.f and
jit.f, including CAPTURE-STRING, CAPTURE-DOES, `;match`/`of`, `:}` and the
definition-name guard LDEFKWGUARD); LUNDEF's head (10037: sealed -> LPOLICY),
so a sealed miss is the same located refusal. `:` (7976-8047), `;`
(9624, 7908), `\`/`(` (EM-COMMENT 7943-7960), numbers (LNUM before find) and
local references (EM-COMPILE-LOCAL) are matched before any row or lookup and
stay. TIER 1 reads the same cells in checked Habu: NDICT:WL-CANDIDATE
(dict.f:56-59) answers XREF-NULL for a refused record (covers used-public
tails, which capture never resolves, and every NCOMP resolution);
HIR-WORD:LOOKUP (hir-word.f:959-969) answers "no row" for a modeled symbol
other than CONTROL open-if/mid-else/close-if and OPEN-/CLOSE-LOCALS, so
`begin`, `recurse`, `[:`, `[']`, `exit`, `>r`, `match`, `case` fall to the
callable path, find nothing and NCOMP refuses the definition
(E-HIR-UNMODELED -> `ncomp: cannot compile <DEF> at <token>`, compiler.f:598,
rc 70, fatal before any code runs: probe u1b). No write gate: a definition
lands where `package` puts it and is admitted by index; reopening a dependency
exposes only gated records and can only add words made of admitted words;
system packages keep C-PACKAGE-SEAL-GUARD (8171). No globals, no re-exported
core words, no definers, no ticks, no quotations, no arithmetic or stack rows:
Maki's packages provide typed length/angle operations and predicates.
TERMINATION is structural: no admitted keyword has a back edge (`if/else/then`
patch forward placeholders only); a body can call only records that exist when
it compiles, and the pending record is outside [0,NDICT) at tier 0 (LFIND
invariant habu1.f:4409-4415, publish at `;` EM-COMPILE-PUBLISH 9605) and unbound at
tier 1 (compiler.f:377-379: `recurse` is the only self-call and it is off), so
`: A ... A ;` is undefined (probes self, self1) and forward references need
`defer`/`is`, which are off; hence the design's call graph is a DAG ordered by
dictionary index and every run is bounded by induction on that index, with no
fuel, timeout or analysis pass. The obligation this leaves Maki: every word in
an admitted public wordlist terminates on every input (a throw counts), and an
admitted parsing word consumes a bounded number of tokens. The checker is
unchanged and still certifies every admitted body at `;` (probe chk0).

Decided by the Fable design (2026-09-30), from Joel's requirement that a Maki design is a limited declarative vocabulary that provably terminates, with the words a design may not use removed from what it can find: (1) the sealed built-ins are exactly `:` `;` `\` `(` numbers, locals
and the twelve spellings above; `if/else/then` are kept for configurations (a
SolidWorks user suppresses features; flags come only from Maki predicates),
`s"` for names the source states, `package`/`public`/`private`/`;package` for
the design's own qualified identities, `using`/`;using` because forth.md
requires consumers to import a package they call twice, `{: :}` because stack
shuffles are off. (2) admission by index-or-wid-bit for records and by baked
spelling span for keywords; both readers of the seal (engine and NCOMP) use the
same two cells. (3) the diagnostic is one located line and rc 105; a sealed
undefined token shares it. (4) `using`, `package` (new or reopened) stay legal
and harmless; no write gate. (5) the definition-name guard follows the seal:
`: begin` is refused as `not in vocabulary: begin`. (6) load form: the
harness's own word admits, seals and `included`s the design path from the
process's script arguments; no policy-file token is read after the seal.
(7) x86-64: kernel-x64.f registers `policy-admit`/`policy-seal` as REFUSE rows
until the tier-1 path there is ported (krait), as the callbacks dot does.
Not under C2; C2 is not a prerequisite.

Acceptance: lib/policy-test.f spawns `bin/hb --load test/policy/allow.f --
test/policy/<case>.f` and the same with test/policy/allow-tier1.f (first line
`1 set-tier`, then `require lib/policy.f`, `require test/policy/dep.f`,
`require lib/ffi-abi.f`, and `: RUN ( -- ) s" PDEP" POLICY:ALLOW POLICY:SEAL
0 SCRIPT-ARGV$ included ; RUN`) through PROC-CMD (RESET, ARG+, RUN-OUTCOME,
ERR$, OUT$; lib/process-command.f:443-482) with T-OUTCOME-EXITED=
(lib/test/outcome.f:9), writing `<tier> <case> rc=<n> <first stderr line>` to
build/policy-run.txt. dep.f: package PDEP with private HIDDEN, public ANSWER
( -- n ) 42, ZERO, BIG? ( n -- bool ), SHOW ( n -- ), NAME-LEN ( ptr u8 n --
n ), BOOM ( -- ) -30001 throw; foreign.f: package PFOREIGN public LEAK.
Cases and expected first stderr line (rc 105 unless stated): ok.f — a design
package with a private helper called by a public word, locals, `s" front"
PDEP:NAME-LEN`, `PDEP:ANSWER PDEP:BIG? if ... else ... then`, a `using PDEP …
;using` block and a top-level call printing 42 and 5: no stderr, rc 0, at both
tiers; bypass.f (the demo design.f verbatim) — `hb: not in vocabulary: require
at …/bypass.f:2`, no "design called C" output; using-ffi.f (`using FFI` then
bare `PROCESS-SYMBOLS`) — `… PROCESS-SYMBOLS at …:2` (the `using` itself is
legal); trusted.f — `… trusted: at …:1`; begin.f (`: SPIN ( -- ) begin again
;`) — `… begin at …:1` at tier 0; at tier 1 rc 70 and stderr containing
`ncomp: cannot compile SPIN`, no later line; recurse.f — `… recurse`;
self.f (`: R ( -- ) R ;`) — `… R at …:1`; require.f — `… require`; included.f
(`s" lib/task.f" included`) — `… included`; capture.f (`: BODY ( -- ) require
lib/task.f ;`) — `… require at …:1` at BOTH tiers (tier 1 refuses at
CAPTURE-IMMEDIATE); tick.f (`' PDEP:ANSWER`) — `… '`; btick.f — `… [']`;
quote.f (`: Q ( -- ) [: ;] ;`) — `… [:`; execute.f — `… execute`; store.f
(`: S ( n ptr a -- ) ! ;`) — `… !`; evaluate.f — `… evaluate`; string.f
(`c" x"`, then `." x"`) — `… c"`; arith.f (`: T ( n -- n ) 1 + ;`) — `… +`;
private.f (`PDEP:HIDDEN`) — `… PDEP:HIDDEN`; reopen.f (`package PDEP HIDDEN
;package`) — `… HIDDEN at …:2`; foreign.f-case (`PFOREIGN:LEAK`) — `…
PFOREIGN:LEAK`; policy.f-case (`s" PFOREIGN" POLICY:ALLOW`) — `… POLICY:ALLOW`;
seal.f-case (`POLICY:SEAL`) — `… POLICY:SEAL`; kwname.f (`: begin ( -- ) ;`) —
`… begin`; checked.f (`: BAD ( -- n ) ;`) — the checker's own `habu: in bad:
… expected: n` and rc 70 (the seal adds nothing); throw.f (`PDEP:BOOM`) —
uncaught throw, rc 67 as today. Two harness-level cases through
test/policy/allow-sealed-admit.f (RUN seals then ALLOWs) — `hb: policy:
sealed`, rc 105 — and allow-no-package.f (`s" NOPE" POLICY:ALLOW`) — `hb:
policy: no package NOPE`, rc 105. Unsealed smoke: `bin/hb --load lib/policy.f
test/policy/dep.f test/policy/ok.f` prints 42 and 5, rc 0; `bin/hb --load
test/run.f` unchanged.

Files: lib/policy.f, lib/policy-test.f, test/policy/ (dep.f, foreign.f,
allow.f, allow-tier1.f, allow-sealed-admit.f, allow-no-package.f, one file
per case), docs/policy.md, docs/forth.md Packages paragraph (one bullet),
test/gate-stdlib-cases.f (`SUITE policy lib/policy-test.f ;SUITE`),
src/compiler/native/dict.f (NDICT:SEALED?, the record predicate in
WL-CANDIDATE), src/habu/xref.f (`XREF-INDEX ( ptr n -- n )`, the inverse of
XREF-REC-ADDR), src/compiler/native/hir-word.f (LOOKUP). Engine:
src/habu/layout.f + data-claims.f (two claims), src/core/engine-error.f +
engine-error-effects.f (`105 constant POLICY`, its TRUST row), src/habu/prims.f
(two EPRIM rows), src/habu/habu1.f (two prim bodies), src/habu/habu2.f
(KWDATA span + LKWDESIGNEND, LKWCMP, LPOLICYREC + three calls, LPOLICY, LUNDEF
head), src/habu/kernel-x64.f (two REFUSE rows).

Verify: `bin/hb --load lib/policy-test.f` (both tiers, artifact
build/policy-run.txt); `bin/hb --load test/run.f`; the fixpoint refresh
(`tools/build-fixpoint-refresh.f -- install`, three identical generations)
because layout cells, KWDATA order and prim rows change; the unsealed smoke;
`bin/hb --load test/policy/allow.f -- <the demo design.f>` refused with rc 105.

Depends: none. C2 is not a prerequisite.

Ownership: lib/policy*.f, test/policy/, docs/policy.md, the forth.md bullet,
gate-stdlib-cases.f, dict.f, xref.f, hir-word.f: this dot's worker, with the
native-compiler owner reviewing dict.f/hir-word.f. layout.f, data-claims.f,
habu1.f, habu2.f, prims.f, engine-error*.f: alder reviews and integrates.
kernel-x64.f REFUSE rows: krait. Claim: agent=zephyr.
