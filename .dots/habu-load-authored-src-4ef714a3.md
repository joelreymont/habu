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
`!`, `c!`, `set-tier`, `prot-wid-add`, `PROCESS-SYMBOLS`, `FUNCTION:` are
wid-0 records (src/core/include.f:999, 1039; habu1.f:3428-3516;
lib/ffi-abi.f:1090-1100); compiler keywords are rows dispatched before any
lookup (interpret rows habu2.f:8572-8598, compile rows 9636-9687, op rows
9740-9787, string rows 8590-8596/9652-9658), so deleting records cannot remove
`begin`, `recurse`, `'`, `[']`, `[:`, `create`, `trusted:`, `does>`, `is`; and
tier 1 captures a body (habu2.f:7908-7940), runs immediates during capture
(7869-7906) and resolves the rest through NDICT (dict.f:56-95) and the HIR-WORD
dialect rows (hir-word.f:1290-1371; `begin`, `recurse`, `[:`, `[']` all
compile at tier 1: probes).

Design: one seal, one admission predicate, read where source tokens are read.
STATE (layout.f, claims in data-claims.f): POLICY-NDICT-CELL ($2820; 0 =
unsealed, else NDICT at the seal) and POLICY-BITS-OFF ($2C00, a PROT-shaped
$400-byte WID bitmap, PROT-WID-MAX bound), in the $2800..$3000 band
(layout.f:517-524, 1245-1251) between EXIT-HOOK-CELL ($2818) and LOCNAMES
($3000, layout.f:361); EXIT-HOOK-CELL gets its missing DATA-CLAIMS row so
CLAIMS-ASSERT proves the placement rather than a sweep. Written only by two
prims that refuse once sealed: `policy-admit ( ptr u8 n -- )` resolves the
package name through WLFIND:LENTRY with wid DICT-WL:NAMESPACE
(habu1.f:3249-3322; x11 = record[0] = the public wid), refuses a wid at or
above PROT-WID-MAX as BPROTWIDADD does (3163-3178) and sets the bit
(PROT-BITS-AT, 3144); `policy-seal ( -- )` stores NDICT. lib/policy.f, package
POLICY: `ALLOW ( ptr u8 n -- )` and `SEAL ( -- )`; nothing else. PREDICATE. A
record is admitted iff unsealed, or ALL of: (i) it is in scope — its index >=
POLICY-NDICT (the design's own definitions, in whatever wordlist `package` put
them), or its wid (record[40]) is below PROT-WID-MAX unsigned AND that bit is
set, the bound tested before the band is addressed as LPROTWIDQ does
(habu1.f:4052) because PROT-BITS-AT, requires it (3135-3143) and wids reach
WID:MAX ($FFFFFFFE, layout.f:235); and (ii) it is callable from checked
source — its flags carry no DNAME-INT (the rule of the tier-0 compile-call
guard, habu2.f:9792-9795, whose TRUSTED-CELL escape cannot arm under the seal
because `trusted:` is off), its wid is not OWNER-API-PRI-WID (layout.f:203)
and its start cell [0] is not zero — exactly what NDICT:VISIBLE-RECORD?
(dict.f:47-55) and CALL-BINDING (274-287) require, so the predicate is the
intersection of the design's scope with the native compiler's visibility. A
keyword is admitted iff unsealed or its baked spelling lies in the KWDATA
design span [LKWIF, LKWDESIGNEND): `if` `else` `then` `{:` `:}` `s"` `package`
`public` `private` `;package` `using` `;using`. Everything else is NOT FOUND:
`hb: not in vocabulary: <token> at <path>:<line>` through LCOMPILEDIE
(habu2.f:10132), exit ENGINE-ERROR:POLICY (105) at top level, a catchable
throw of 105 inside an `included`/`evaluate` frame, nothing later executed.
SITES, all in the engine's token loops, where a SOURCE token is resolved and
nowhere else: LPOLICYREC (x5 = record) at the `found` labels of
EM-INTERPRET-FIND (habu2.f:8609) and EM-COMPILE-CALL (9803), ahead of the
legacy internal guards there (LINTERNAL 8611, C-COMPILE-CALL-GUARD 9805),
which LFINDUSED rejoins; in CAPTURE-IMMEDIATE (7871) on an LFIND hit, and
under the seal on a miss after a LFINDUSED probe (8355-8425: x5 = record,
x13 = found; the probe only gates, then restores the miss path, so
used-public immediates still do not run at capture); LKWCMP's match exit
(2040-2053: sealed and x0 >= LKWDESIGNEND -> LPOLICY), which every row
inherits (20 callers in habu2.f and jit.f: control, loop, meta, string, op,
does>, `;match`/`of`, `:}`, the definition-name guard, C-UNIT-DISPATCH);
LUNDEF's head (10037: sealed -> LPOLICY), so a sealed miss is the same located
line. `:` (7976-8047), `;` (9624, 7908), `\`/`(` (EM-COMMENT 7943-7960),
numbers (LNUM before find) and tier-0 local references are matched before any
row or lookup and stay. COMPILER-OWNED RESOLUTION NEVER PASSES THE GATE:
dict.f and xref.f are unchanged, so NDICT:CALL-TARGET for the trap routine
`die` (select.f:2623, trap.f:135-136, generated for a call to a
certified-dead word, elaborate.f:3186 and 3100-3104), for `type`, `evaluate`,
`xt!`, `execute`, `catch` (elaborate.f:3157-3279), `throw`
(select-x64.f:1232) and the prior binding (compiler.f:394) resolve as today.
THE RECORD CAPTURE ADMITS IS THE RECORD NCOMP CALLS: at tier 1 every body
token passes the capture loop before it reaches the tape (EM-COMPILE
7892-7920); the engine walks the legs private, public, global, then the used
publics (LFIND habu1.f:4495-4506, LFINDUSED), and NDICT walks the same legs in
the same order (dict.f:4-7; BARE-REC 89-95, USED-REC 64-77, QUALIFIED-REC
98-103 through the namespace record as LFIND's FIND-NMATCH); each wordlist
holds at most one record per folded name (C-REJECT-DUP-DEF habu2.f:3471,
forth.md:224-231); the engine takes the first leg that holds the name while
NDICT takes the first leg that holds a VISIBLE record of the name. Under the
seal capture dies unless the engine's record passes LPOLICYREC, and LPOLICYREC
admits only visible records (ii), so the legs before it hold no record of the
name at all and NDICT stops at the same record; on the used leg LFINDUSED
answers one record or dies USING-AMBIGUOUS, and USED-REC's visible candidates
are a subset of that one. An immediate hit is run at capture, never bound
(CALL-BINDING refuses DNAME-IMM). A record published later in the same body by
an admitted immediate has index >= POLICY-NDICT and is admitted by the same
predicate. Hence NCOMP never binds a tape name to a record the seal did not
admit, and no NDICT caller is gated or special-cased. TIER-1 BODIES: the capture loop (CAPTURE-IMMEDIATE) applies tier 0's predicate
at tier 1's read site. Under the seal a body token is admitted iff it names a
local declared in the body (checked before LFIND; a matched `{:` group is
recorded at capture by tier 0's store, C-LBRACE-STORE-ONE), LFIND or the
LFINDUSED probe hits a record LPOLICYREC admits, LNUM accepts it, or it spells
one of the span's body keywords; anything else is LPOLICY at the token. So
`begin`, `recurse`, `[:`, `[']`, `[char]`, `exit`, `>r`, `do`, `case`, `[`,
`postpone`, a self-call and an undefined name are 105 at the token at both
tiers, and hir-word.f is unchanged. Every new branch sits behind a
POLICY-NDICT-CELL load, so unsealed capture is unchanged. Top level is the same interpreter at both tiers. No write gate: a
definition lands where `package` puts it and is admitted by index; reopening a
dependency exposes only gated records and can add only words made of admitted
words; system packages keep C-PACKAGE-SEAL-GUARD (8171). No globals, no
re-exported core words, no definers, no ticks, no quotations, no arithmetic or
stack rows: Maki's packages provide typed length/angle operations and
predicates. TERMINATION is structural: no admitted keyword has a back edge
(`if/else/then` patch forward placeholders only, LBCHAIN 2053-2064); a body
can call only records that exist when it compiles, and the pending record is
outside [0,NDICT) at tier 0 (LFIND invariant habu1.f:4409-4415, publish at `;`
EM-COMPILE-PUBLISH 9605) and unbound at tier 1 (compiler.f:377-379), so `: A
... A ;` is undefined (probes self, self1) and forward references need
`defer`/`is`, which are off; the call graph is a DAG ordered by dictionary
index and every run is bounded by induction on that index, with no fuel,
timeout or analysis pass. MAKI'S OBLIGATION, which the seal assumes and
docs/policy.md states: every word in an admitted public wordlist terminates on
every input (a throw counts), an admitted parsing word consumes a bounded
number of tokens, and no admitted word stores to a caller-supplied address or
widens the policy (calls `policy-admit`, `prot-wid-add`, `set-current`,
`evaluate`, `included` on caller data): the admitted API is the design's
whole reach. The checker is unchanged and still certifies every admitted body
at `;` (probe chk0).

Decided by the Fable design (2026-09-30), from Joel's requirement that a Maki design is a limited declarative vocabulary that provably terminates, with the words a design may not use removed from what it can find: (1) the sealed built-ins are exactly `:` `;` `\` `(` numbers, locals
and the twelve spellings above; `if/else/then` for configurations (flags come
only from Maki predicates), `s"` for names the source states, the package
keywords for the design's own qualified identities, `using`/`;using` because
forth.md:296-299 requires consumers to import a package they call twice,
`{: :}` because stack shuffles are off. (2) admission by index-or-wid-bit for
records with the unsigned bound first, intersected with the native compiler's
visibility (no DNAME-INT, not OWNER-API-PRI-WID, nonzero start), and by baked
spelling span for keywords; the engine reads the seal for every source token
at both tiers and the tier-1 dialect table reads it for modeled symbols; the
native compiler's own name resolution is never gated. (3) one located
diagnostic and rc 105 for every engine refusal, at both tiers; a sealed
undefined token shares it. (4) `using`, `package`
(new or reopened) stay legal and harmless; no write gate. (5) the
definition-name guard follows the seal: under it a definition name that any
row the emitter dispatches would match is refused at the name, before the tier
fork (C-QUALIFY-DEF), at both tiers: a row outside the span is `not in
vocabulary: <name>` 105 (`: begin`, `: dup`, `: create`, `: tuck`), one inside
keeps E-RESERVED-DEFINITION 70 (`: if`, `: package`). The walk covers the union
of the interpret, compile, string, op and pass-2 width rows and is generated
from their registrars, so a new row is covered by construction; `policy-admit`
runs the same walk over the public wordlist it admits and refuses a package
that publishes such a name: `hb: policy: package <name> publishes keyword
<word>`, 105. (6) locals are matched before any lookup at both tiers, so
`{: type:n :} type` is legal at both; at capture a local declared anywhere in
the body is admitted, and block scope stays NCOMP's own check. (7)
load form: the harness's own word admits, seals and `included`s the design
path from the script arguments; no policy-file token is read after the seal.
(8) x86-64: kernel-x64.f registers the two prims as REFUSE rows (krait). (9)
EXIT-HOOK-CELL's claim row belongs here because this dot's Verify relies on
CLAIMS-ASSERT for the watermark cell's neighbours. (10) the visibility part of
the predicate lives inside LPOLICYREC rather than as a second guard at
capture: one predicate is what the capture-equals-NCOMP argument quantifies
over, it is seal-conditional by construction so unsealed tier-1 capture is
byte-identical, and it covers the two visibility legs the compile-call guard
does not (OWNER-API-PRI-WID, zero start); an internal record under the seal
is therefore `not in vocabulary` at every site, ahead of E-INTERNAL and
E-UNDEFINED. Not under C2; C2 is not a prerequisite.

Acceptance: lib/policy-test.f spawns `bin/hb --load test/policy/allow.f --
test/policy/<case>.f` and the same with test/policy/allow-tier1.f (first line
`1 set-tier`; both then `require lib/policy.f`, `require test/policy/dep.f`,
`require test/policy/foreign.f`, `require lib/ffi-abi.f`, and `: RUN ( -- )
s" PDEP" POLICY:ALLOW POLICY:SEAL 0 SCRIPT-ARGV$ included ; RUN`) through
PROC-CMD (RESET, ARG+, RUN-OUTCOME, ERR$, OUT$; lib/process-command.f:443-482)
with T-OUTCOME-EXITED= (lib/test/outcome.f:9), writing `<tier> <case> rc=<n>
<first stderr line>` to build/policy-run.txt. dep.f: package PDEP with private
HIDDEN, public ANSWER ( -- n ) 42, ZERO, BIG? ( n -- bool ), SHOW ( n -- ),
NAME-LEN ( ptr u8 n -- n ), BOOM ( -- ) -30001 throw, and STEP ( -- n ) 1
marked internal right after its definition by a private `TRUSTED: MARK-LAST
( -- ) ndict@ 1- int-mark ;` (the shape of test/compiler/native-internal-call.f
:29-31; its checked effect survives, :36); foreign.f: package PFOREIGN public
LEAK ( -- n ) and STEP ( -- n ) 2. "105 at L" below means stderr line 1 is
`hb: not in vocabulary: <token> at <abs path>:L`, rc 105, no stdout; a case
with one outcome behaves the same at both tiers. Admitted: ok.f (a design
package with a private helper called by a public word, locals, `s" front"
PDEP:NAME-LEN PDEP:SHOW`, `PDEP:ANSWER PDEP:BIG? if PDEP:ANSWER else PDEP:ZERO
then PDEP:SHOW`, a `using PDEP … ;using` block, top-level calls) prints 42 and
5, rc 0; dead.f (`: FAIL ( -- ) PDEP:BOOM ;` then `FAIL`) compiles at both
tiers, tier 1 generating the trap through NDICT:CALL-TARGET, and exits 67 with
`uncaught throw code -30001`; checked.f (`: BAD ( -- n ) ;`) is the checker's
`habu: in bad: at 'BAD' expected: n`, rc 70; throw.f (`PDEP:BOOM`) exits 67.
Top level, both tiers: bypass.f (the demo design.f verbatim) `require` 105 at
2, no "design called C" output; using-ffi.f (`using FFI` then `NOW`) `NOW` 105
at 2 (the `using` is legal; NOW is FFI:NOW, lib/ffi-abi.f:345-349, found by
LFINDUSED); trusted.f `trusted:` 105 at 1; tick.f (`' PDEP:ANSWER`) `'`;
evaluate.f (`s" 1" evaluate`) `evaluate`; included.f (`s" lib/task.f"
included`) `included`; require.f `require`; string.f (`c" x"`) `c"`;
private.f `PDEP:HIDDEN` (a miss, via LUNDEF); reopen.f (`package PDEP` /
`HIDDEN`) `HIDDEN` 105 at 2 (found in PDEP's private wid); foreign.f-case
`PFOREIGN:LEAK`; allow.f-case (`s" PFOREIGN" POLICY:ALLOW`) `POLICY:ALLOW`;
seal.f-case `POLICY:SEAL`; kwname.f (`: begin ( -- ) ;`) `begin`. In a body,
both tiers 105 at the token's line: capture.f (`: BODY ( -- ) require lib/task.f
;`) `require` (tier 0 at EM-COMPILE-CALL, tier 1 at CAPTURE-IMMEDIATE);
execute.f (`: E ( [ -- ] -- ) execute ;`) `execute`; store.f (`: S ( n ptr a --
) ! ;`) `!`; arith.f (`: T ( n -- n ) 1 + ;`) `+` (tier 0 by the op row, tier 1
as the wid-0 record at capture); used.f (`using PFOREIGN` / `: U ( -- n ) LEAK
;`) `LEAK` at 2 (tier 0 via usedtry, tier 1 via the sealed LFINDUSED probe);
dotq.f (`: D ( -- ) ." x" ;`) `."` (tier 1 via CAPTURE-STRING); internal.f
(`package PDEP` / `public` / `using PFOREIGN` / `: I ( -- n ) STEP ;` /
`;using` / `;package` / `PDEP:I PDEP:SHOW`) `STEP` 105 at 4 and no `2` on
stdout at either tier — the engine's first leg is PDEP's public STEP, which is
internal, so LPOLICYREC refuses it at EM-COMPILE-CALL (tier 0) and at capture
(tier 1); without clause (ii) tier 1 would capture the token, NDICT would skip
the internal record and bind PFOREIGN:STEP, and the run would print 2.
Refused in a body at both tiers, 105 at the token on line 2: begin.f (`: SPIN ( -- ) begin
again ;`) `begin`, recurse.f `recurse`, quote.f `[:`, btick.f `[']`,
char-literal.f `[char]`, self.f (`: R ( -- ) R ;`) `R`, exit.f `exit`,
bracket.f `[`, postpone.f `postpone`, rstack.f `>r`, do.f `do`. local-name.f
(`: LT ( n -- n ) {: type:n :} type ;` / `42 LT PDEP:SHOW`) prints 42, rc 0,
at both tiers. Definition names, both tiers: own-keyword.f `dup` 105 at 4;
defname-interpret.f (`: create ( -- ) ;`) `create` 105 at 2; defname-p2.f
(`: tuck ( -- n ) 5 ;`) `tuck` 105 at 2; defname-span.f (`: package ( -- ) ;`)
rc 70 with first line `hb: compile keyword cannot be a definition name:
package`. Harness-level: allow-sealed-admit.f (RUN seals
then ALLOWs) `hb: policy: sealed`, rc 105; allow-no-package.f (`s" NOPE"
POLICY:ALLOW`) `hb: policy: no package NOPE`, rc 105; allow-seal-twice.f
(ALLOW, SEAL, SEAL) `hb: policy: sealed`, rc 105; allow-keyword-package.f
(admits a package `PKW` whose public words include `DUP`) `hb: policy: package
PKW publishes keyword DUP`, rc 105, and the same for a package publishing
`TUCK`. Unsealed smoke: `bin/hb
--load lib/policy.f test/policy/dep.f test/policy/foreign.f test/policy/ok.f`
prints 42 and 5, rc 0; `bin/hb --load test/run.f` unchanged,
test/compiler/native-dead-path.f and native-internal-call.f included.

Files: lib/policy.f, lib/policy-test.f, test/policy/ (dep.f, foreign.f,
allow.f, allow-tier1.f, allow-sealed-admit.f, allow-no-package.f, one file
per case), docs/policy.md, docs/forth.md Packages paragraph (one bullet),
test/gate-stdlib-cases.f (`SUITE policy lib/policy-test.f ;SUITE`),
docs/stdlib.md (Layout, storage class, `## Sealed vocabulary`). Engine:
src/habu/layout.f + data-claims.f (three claim rows: watermark, bitmap, EXIT-HOOK-CELL),
src/core/engine-error.f + engine-error-effects.f (`105 constant POLICY`, its
TRUST row), src/habu/prims.f (two EPRIM rows), src/habu/habu1.f (two prim
bodies, the admit-time keyword walk), src/habu/habu2.f (KWDATA span +
LKWDESIGNEND, LKWCMP, LPOLICYREC + its calls, the capture predicate and `{:`
recorder in CAPTURE-IMMEDIATE, the generated row walk and its registrar
prologues, LPOLICY, LUNDEF head), src/habu/kernel-x64.f (two REFUSE rows).

Verify: `bin/hb --load lib/policy-test.f` (both tiers, artifact
build/policy-run.txt); `bin/hb --load test/run.f`; the fixpoint refresh
(`tools/build-fixpoint-refresh.f -- install`, three identical generations)
because layout cells, KWDATA order and prim rows change; the unsealed smoke;
`bin/hb --load test/policy/allow.f -- <the demo design.f>` refused with rc 105.

Depends: none. C2 is not a prerequisite.

Ownership: lib/policy*.f, test/policy/, docs/policy.md, the forth.md bullet,
docs/stdlib.md, gate-stdlib-cases.f: this dot's worker. layout.f, data-claims.f, habu1.f, habu2.f, prims.f,
engine-error*.f: alder reviews and integrates, and that engine lands before
lib/policy.f is written. kernel-x64.f REFUSE rows: krait. Claim: agent=zephyr workspace=.jj-ws/policy-seal.
