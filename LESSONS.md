# Lessons

What worked, what didn't, and why. Read at session start; update after findings,
mistakes, or insights. Lessons only — no API reference or code snippets (→ `docs/`).

## Certified dominates: the whole toolchain self-checks (2026-06-11)

The checker now models locals ({: :} pops bound to type vars), control flow
(branch states unified at then/repeat/loop joins, occurs check kills cyclic
rows from mismatched arms), records every certified word's rendered sig (and
every variable/create/constant via the engine's c-defhook), and srclist loads
util/checker/render FIRST so the hook covers asm/icode/mnem too. Toolchain
verdicts went 1 certified / 235 unchecked → **554 / 109 / 0 rejected**.

Bugs found on the way, all of one species — A ROUTINE'S SCRATCH REG WAS A
CALLER'S LIVE REG:
- Lbcap kept token len in x6; c-lbrace keeps its locals block-start in x6
  across the per-name capture call → locals pop-loop miscounted (x17 now).
- Lbcap composes via x15; c-constant had already popped the VALUE into x15 →
  every constant baked garbage (pop moved after the capture). A garbage
  `MAXTV constant` then made TVT's allot wreck DP — the SIGBUS appeared two
  creates later at an innocent c!. Debug recipe that worked: lldb -k on the
  crash, file-offset the pc, dict-walk to name the prim, then ask "who CALLS
  this prim with those registers".
- render's SEEN was 64 cells but is indexed by typevar PAY (up to MAXTV).
When BL-ifying inline engine code, list the caller's live registers FIRST.

Also: checker buffers must be ENGINE-sized with fail-closed guards. SPA's 64
pushes silently wrapped SPN (the variable sat right after the buffer) — big
bodies reused live spine records, masked because homogeneous int stacks look
alike. MK-PUSH/U-PUSH/USIG-ADD/PT2+/RSBUF all die 76 on overflow now.

## Bootstrap fixpoint needs an extra generation (2026-06-11)

`tools/build.sh` reported FIXPOINT BROKEN after the reg-shuffle commit. Hours of
"which word miscompiles?" suspicion — but no compiler bug existed. bin/hb is
engine + *embedded* builder source; a rebuild executes the OLD embedded builder,
so gen1 = old engine + new text. Only gen1's run of the new text emits the new
engine. Any commit that changes the emitted engine therefore needs gen2 vs gen3,
not gen1 vs gen2 — build.sh now iterates until gen_n == gen_{n+1} (bound 4).
Classic compiler bootstrapping; the one-rebuild check only works when the change
doesn't touch the emitted engine.

Diagnosis pattern that worked: dict-walk both images (entries are 48 bytes:
start/end/len/name[16]/wid), compare per-word offsets — "ALL prims shifted by
-4, dict by -0x1A8" reads as "different engine version", not "one bad word".

Trap inside the trap: the gforth signer (`bootstrap/cg/sign.fs`) sets SIG-ID to
the OUTPUT BASENAME; the self-hosted signer always signs "hb". A reference
binary built as `/tmp/hb-ref` differs from a correct image in signature alone
(ident "hb-ref" vs "hb"). For byte-comparisons, name the gforth reference so its
basename is `hb`, or compare only __TEXT.

## Tools that pay rent: probe.sh and parity-lint (2026-06-11)

Two frictions recurred enough to deserve checked-in tools. tools/probe.sh builds a
one-off habu binary from a program string and runs it (optionally asserting output) —
replaces the ad-hoc bash function retyped a dozen times per debugging session.
tools/parity-lint.py token-diffs every paired emitter definition between the bootstrap
builder and the habu-side port (case-folded, label-normalized, with an allowlist for
the legitimately-different idiom words) — the recurring bug class here is silent
builder divergence (instruction order, template constants), and the goldens only say
"bytes differ" while the lint names the word and the first divergent token. It runs in
test/run.sh before the suite. Rule of thumb earned twice now (profiler, lint): when
the same diagnostic dance happens three times, stop and build the tool.

## The gate was abort-blind — exit codes, not grep pipelines (2026-06-11)

For some stretch of commits the combined gate LIED: a gforth exception while loading
one test file (a stale path in t-sh-balance after the Mach-O convergence) aborted the
whole suite mid-run, and the `| grep -c INCORRECT` check counted zero — abort looks
identical to all-green. Three things were hiding behind it: the balance file-order
bug, a long-broken test line (`5 3 D-FLD` — two args to a three-arg word), and the
SCODE scratch buffer sitting within ~100 bytes of overflow (the habu rename's +1-byte
strings pushed it over; now guarded + sized to the __TEXT budget). Fixes: test/all.fs
already exits nonzero via #ERRORS and gforth exits nonzero on aborts — USE THE EXIT
CODE (test/run.sh), never a grep pipeline that swallows it; selfhost-all.fs got the
same #ERRORS exit line. Lesson: a gate must fail CLOSED — verify the checker itself
fails when the suite dies early, not just when assertions fire.

## The engine became a register-allocating JIT — incrementally, gate-green (2026-06-11)

vs.fs's allocator model now lives INSIDE the standalone's `:` compiler, landed as five
gate-green increments (A: VS state + spill helpers, inert; B: literal folding +
spill-at-every-consumer; C: con-entry shuffles — a dropped literal emits nothing;
D: register binops/comparisons + locals loaded straight into registers; E: adversarial
sweep t-sh-vsjit). Tags: con (no code yet) / reg (x9..x15); after Lvspill the VS is
simply EMPTY — no vs.fs "mem" tag; spilled values are ordinary [x19] cells, so every
existing consumer (calls, keywords, locals, `;`, the check hook) stays correct by
spilling first. 30M iterations of a locals-word call: 0.165s. Bugs the increments
caught: an UNALLOCATED label variable (Lvtop1c) read as label 0 = the ANCHOR, silently
re-basing RBASE and corrupting every dict address — allocate every label before any
LBL,; Lvspill's loop counter in x5 clobbered by its own callee (keep loop state in the
frame across BLs); Lvlitpush materializing constants through x9 — a POOLED register
that may hold a LIVE later entry (spill constants via x16/IP0, never pooled); and the
movz/movk templates carry x9 in their rd bits — mask before re-targeting. Probe
discipline: behavior probes must be stack-BALANCED or they report phantom leaks (two
"miscompiles" were my own unbalanced test programs — .s settles it immediately).

## Build the tool: the profiler found in one run what bisection circled (2026-06-11)

The dogfooded self-compile hung (90 s, 100% CPU). File-bisection localized it to
habu2.f bodies; then a SAMPLING PROFILER built into the engine (prof.fs both sides,
~100 lines: SIGALRM + setitimer 1 ms, handler reads pc from the ucontext like crash.fs,
maps pc -> dict word by addr/clen, counts; at a sample limit dumps "name count" +
exit(99) — a hang diagnoser by construction) named the culprit immediately:
R-RES 1071, RV@ 729, PAY 476, ISROW 456 — the checker's typevar resolver. Root cause:
TVT/RVT pools were 64 cells; engine-sized bodies allocate hundreds of fresh typevars;
RV! past the pool corrupted adjacent memory into cyclic var chains that the guard-less
R-RES followed forever. Fix: pools 2048 + FRESH dies loudly at exhaustion (exit 76).
90 s hang -> 0.033 s. The checked self-compile now gates stage2: the compiler
type-checks its own source while compiling itself, full fixpoint test 0.33 s.
Lesson: when a bisect narrows to "somewhere in here", stop bisecting and build the
measuring tool — it answers "where exactly" and stays in the toolbox.

## The loop closes WITHOUT gforth: stage3 == stage2, signed (2026-06-11)

t-sh-stage2 now signs stage2 (sign2.fs — the full-builder port of the ad-hoc signing
post-pass, fixed identifier "stage2" on both sides so bytes match) and RUNS it: stage2
compiles the same source and emits stage3; stage3 == stage2 byte-identically, and
Apple's codesign validates the signature that the standalone-compiled compiler emitted.
gforth is out of the loop — the toolchain sustains itself. Guards added the same round:
c-lbrace refuses {: inside IF/BEGIN/DO and locals named i/I at compile time (exit 75) —
the two footguns that kept biting are now errors, not LESSONS entries. Flow behavior
(DO/LOOP/I incl. zero-trip, ELSE both arms, WHILE/REPEAT) and the stdin-mode engine
(golden + a piped-program behavioral test) are covered. Leaf prims (everything but
catch/execute) dropped their x30 frames — 2x cheaper when called, fully inlineable.

## THE FIXPOINT: the standalone compiles its own compiler (2026-06-11)

t-sh-stage2 closes the self-hosting loop: stage1 (gforth-built standalone whose baked
program is the complete ported compiler — asm/icode/mnem/util/walk/rt/crash/macho/
engine/engine2 + the stage2 driver) READS that same source from a file (new `read`
prim), compiles it with the ported EMIT-FORTH, wraps it with the ported BUILD-MACHO,
and the result is BYTE-IDENTICAL to the gforth-built engine image. Compiler fixpoint,
from source — not image copying. Enablers: real calls + selective inlining (deep helper
chains compile), 8KB body capture (engine words are long), $hex literals, the `read`
prim (added to BOTH bootstrap/cg/forth.fs and the habu1.f port in lockstep — the goldens
enforce parity), __TEXT grown to $20000 and DATA-SIZE to $200000 (baked source + build
buffers; macho-min/rebuild/sign NCSLOT updated together). Last port bug: a {: :} locals
group INSIDE an IF branch (emit-source) — the documented frame-corruption footgun;
probes that hoisted it passed, pinpointing it. The engine-port goldens (t-sh-engine,
t-sh-habu2: 2893 words identical) plus t-sh-stage2 now gate every engine change on
both sides staying in lockstep.

## The engine port: golden byte-parity is the only test that matters (2026-06-11)

Porting src/cg (gforth-hosted) emitters into the subset (selfhost/): the working
method is a GOLDEN test per file — gforth emits via its real assembler, the standalone
emits via the ported one, compare word-for-word (or byte-for-byte files for Mach-O).
This caught, in one day: the selfhost D26 branch mask was 24-bit not 26 (every backward
B/BL mis-encoded — never seen because vs/walk only emit backward CBZ/BCOND); a
zero-looping m-zeros; and `28 constant CP` in habu1.f shadowing icode's CP variable
(compile-time binding protects earlier callers, but later code reading `CP @` loads
from address 28 — SIGSEGV that the in-binary crash handler diagnosed). Port rules that
keep transcription mechanical: a mnemonic layer (mnem.fs) so lines read identically to
src/cg; ALL `NEWLBL {: x :}` pairs merged into ONE locals group at the word top (label
ids don't affect output bytes, only LBL,/branch pairing); `?do` -> BEGIN/WHILE with
zero-trip guards; `>r`/`move`/`2constant`/`abort"` -> variables/byte-loops/die. LIT64's
minimal movz/movn+movk selection is ported logic-exact and golden-tested over 15 probe
values — stage2 byte-identity hinges on it. Done so far, all byte-identical: encoders,
LIT64, rt (print/atoi), crash handler, FULL Mach-O builder, engine part 1 (53 prims +
tok/find/num/prot/flush/cemit + seed dict, 1611 words). Remaining: keyword JIT +
emit-main + EMIT-FORTH, then the stage2 byte-compare.

## Adversarial review pays: three real bugs in "green" code (2026-06-11)

All tests green, then an adversarial pass found: (1) interpret-mode undefined words
were still silently SKIPPED (only compile mode errored) — making it error immediately
caught a live bug: disasm-demo.fs comments began `\\` (2-char token, not the 1-char
comment word), compiling to nothing for weeks. (2) c-call emitted BL for cross-region
calls — JIT words live in a kernel-placed mmap, prims in __TEXT; BL's +-128MB imm26
silently truncates if they land far apart. Tests passed only because this machine maps
them close. Fixed: absolute `movz/movk x16 + blr x16` (x16 = IP0, the ABI call
scratch). (3) NUMBER? probed byte[idx] for '$' before the idx<len bounds check — a
bare `-` as the last token of the baked source (end of __TEXT) reads past the mapping.
Lesson: "gate is green" tests the inputs you wrote, not the assumptions you baked in.
Attack address-space assumptions, mode asymmetries (interpret vs compile), and
boundary reads explicitly. Remaining holes found are dotted (frame collision, IF-depth
balance, BODYBUF truncation, catch/throw-across-frames test gap).

## Process: jj new immediately after every push (2026-06-11)

Three times in one session: pushed master, kept editing, and the working-copy commit
(@) mixed new work into the already-pushed change — forcing restore/copy dances to
split. In jj, @ IS a commit; after `jj git push`, run `jj new` before touching any
file. Same discipline after `jj describe` of a finished change.

## Standalone parses $hex now (2026-06-11)

The standalone's `NUMBER?` (`emit-num` in forth.fs) was decimal-only, so the codegen
source was written with bare decimal magic numbers. It now parses `$hex` too (optional
leading `-`, case-insensitive): `$FF`=255, `-$2A`=-42, `$deadBEEF`. Decimal still works
and the dispatch order is unchanged (NUMBER? before FIND), so `0=`/`1+`/`$foo` still fall
through to FIND. Source may now use hex where it reads better; no need to pre-convert to
decimal. t-sh-hex covers it. Output is still decimal-only (`.`).

## Real calls break the inlining wall — the engine fix (2026-06-11)

The leaf-only inlining model (below) was THE blocker for self-hosting: the standalone's
`:` INLINED every colon body, so any reused helper or a register-exhaustion spill path
exploded the compiled code and SIGSEGV'd at load. Proven with the Dig protocol: adding a
spill fallback to `R-ALLOC` (called from ~8 sites) crashed vs-demo; removing just the
*call* restored it — pure code-size, not logic. The earlier "BL if large" attempt failed
because a BL'd word that itself BLs clobbers LR (x30). The real fix is a uniform frame:
**every** word saves/restores x30 — prims via FPRIM (`sub sp,#16; str x30,[sp]` … `ldr
x30,[sp]; add sp,#16; ret`), user `:` words emit the same prologue at the name and
epilogue before RET (Lcemit raw words $D10043FF/$F90003FE … $F94003FE/$910043FF). Then
the compiler emits ONE `BL (target-CP)>>2` per call (`c-call`) instead of inlining
(`c-copy`, now dead and removed). Arbitrary call nesting is safe because each frame
restores LR before returning. The data stack is in memory (x19), so nothing else needs
saving; leaf defining-words (CREATE/CONSTANT) need no prologue. Result: register SPILL
(>7 live values → spill deepest reg to [x19,#k*8], reload via V-FORCE) now compiles and
runs — `dup×8 + ×8` of input 5 exits 45 (t-sh-spill). All 16 standalone tests stay
green. This unblocks deep stacks, full codegen, and the compiler fixpoint (the compiler's
own source is far too large to inline). The "leaf-only wall" note below is now historical.

## Self-signing replaces external codesign (2026-06-11)

habu now ad-hoc-signs its own Mach-O — no external `codesign`. SHA-256 in Forth
(sha256.fs, FIPS-180 vectors), then a POST-PASS (sign.fs) that rewrites the finished
unsigned binary: insert LC_CODE_SIGNATURE into header slack, grow __LINKEDIT, append
a CSMAGIC_EMBEDDED_SIGNATURE SuperBlob with one version-0x20400 CodeDirectory
(flags=adhoc, execSegFlags=MAIN_BINARY, SHA-256 of each 4 KiB page). Apple's own
`codesign -v` validates it. Key design call: **sign as a post-pass, not in
BUILD-MACHO** — keeps the canonical unsigned artifact byte-identical to the
standalone's emitter, so the drift guard and self-rebuild fixpoint stay meaningful.
Gotchas: ad-hoc signatures are deterministic (no timestamp), so the fixpoint can
hold WITH signatures once the standalone ports the post-pass. SHA-256 footguns —
gforth case-folds `S0`/`s0` (use BSIG0/SSIG0); a local named `i` shadows the loop
index inside `?DO`; declaring `{: :}` locals inside a loop corrupts the return
stack (factor the loop body into its own word); `?DO` is `( limit start -- )`.

## Standalone codegen gets CONTROL FLOW — and the leaf-only inlining wall (2026-06-11)

The register-allocating codegen now compiles IF/THEN: pop the flag to a reg, SPILL the
whole VS to canonical memory slots ([x19,#k*8]) so both branch paths agree, `cbz` past
the body, re-spill at the merge. Compiles abs() on a runtime input (`negate dup 0< if
negate then`) — verified both signs (t-sh-if). The HARD part wasn't the algorithm, it
was the standalone's **leaf-only stencil-inlining model**: `:` INLINES every colon word
(memcpy of body-minus-RET), so a deep helper chain (V-FORCE->V-REG->V-SPILL-ALL + a
register-exhaustion spill path) explodes the compiled body and crashes. Tried adding a
"BL if large" path to the engine (forth.fs) — but a BL'd word that itself BLs clobbers
LR (x30), and inlined words can't carry LR save/restore; the two models are
incompatible without a frame-based redesign. The working fix: keep the helpers SMALL —
drop the register-exhaustion spill (shallow bodies don't need it), keep only the
canonical V-SPILL-ALL at boundaries. So IF/THEN works for stacks <=7 deep; deeper
register spilling still needs the engine's call mechanism reworked. BEGIN/UNTIL loops
followed the same pattern (spill at the loop top + the back-edge so the layout is
invariant) — a countdown that exits 42 verifies it. The codegen now handles arithmetic,
stack ops, comparisons, IF/THEN, and loops — real programs. Still NOT in the codegen:
locals, create/variables, memory ops, strings, FP — which the compiler's OWN source
uses heavily, so the standalone can't yet compile its compiler (the fixpoint gap).

## Standalone register allocator — the real codegen win (2026-06-11)

The peephole (16->15) was barely worth it; the VS ALLOCATOR is the real win. Ported
habu's register-allocating model to the standalone (src/arch/arm64/vs.f): the data stack
lives in REGISTERS (x9..x15), tracked by an abstract value stack. `swap`/`over` become
free relabels, arithmetic is reg->reg, literals load straight into a reg — ZERO ldr/str
until the pool spills. `5 dup *` compiles to 7 instructions (movz/movk/mov/mul/mov + exit)
vs 16 for the memory model, with no memory traffic at all. Emitters dispatch by xt from
a table (`['] G-DUP … XTS !` then `execute`) so the dispatcher stays tiny despite the
standalone inlining colon words. Verified correct across arithmetic/dup/swap/over/nip.
Then added CONSTANT FOLDING: VS entries are tagged reg-or-constant; a binop with two
constant operands folds at compile time (no code). `5 dup *` now folds to `movz x9,#25`
— 5 instructions, the multiply gone. `2 3 4 + *` folds entirely to 14. So: 16 (memory)
-> 7 (registers) -> 5 (folding). The standalone is now a genuinely OPTIMIZING native
compiler. Still straight-line only (control flow + FP + spill are the remaining work).

## Standalone peephole optimizer + the `i`-local footgun (2026-06-11)

Ported the first optimizer pass to the standalone (src/arch/arm64/opt.f): store-to-load
FORWARDING drops a `ldr Rd,[x19]` right after a `str Rd,[x19]` (Rd still holds it) —
sound for the branchless arithmetic bodies walk.fs emits. `5 dup *` goes 16 -> 15
instructions and still computes 25. RECURRING FOOTGUN that cost time again here: a
local named **`i`** (or `j`) resolves to the DO-LOOP index word **I**, not the local —
so `WGET {: i :} … i …` ignored its argument and every access hit the same slot.
Always name index locals `ix`/`n`, never `i`/`j`. (Same class bit me in sha-round.)
Debugged it by DOGFOODING the standalone disassembler (`CODE CP @ DISASM`) to see the
str/ldr pair, and `.s` to catch WSET/WGET returning the same value — the tools paid off.

## Checked toolchain + standalone disassembler (2026-06-11)

The debugging tools are built and dogfooded: a Forth disassembler (bootstrap/cg/disasm.fs)
whose decode math (disasm-core.fs) and the encoders (asm-checked.fs) are CHECKED typed
Forth — habu certifies them (CHECK-CODE=0). Caveat: habu type vars are SINGLE letters, so
sigs must read `( a b c -- d )` (multi-char like `( rd rn rm -- w )` -> E-UNCHECKED), and
sign-extend must be branchless (`(f^sign)-sign`; an IF version didn't certify). A STEP
single-stepper (stepper.fs) traces a snippet token-by-token with the stack. And the
standalone now disassembles its OWN generated code (src/arch/arm64/disasm.f, data-table
driven) — self-hosted debugging, zero gforth/python. Two more standalone footguns hit:
locals declared INSIDE a BEGIN/WHILE loop corrupt the frame (use variables for row
fields); and a test bug — `,` stores 8-byte CELLS, but ARM64 instructions are 4-byte
u32s, so a `create P n , n ,` buffer is double-spaced (build code with `c,` LE bytes).

## Standalone errors on undefined words; token compiler; Forth disassembler (2026-06-11)

The standalone now COMPILES Forth source bodies to native code (src/arch/arm64/walk.f:
tokenize -> per-op instruction templates in a DATA table + literals + encoders +
assembler -> self-signed binary; `7 dup *` -> exit 49). And it finally ERRORS on an
undefined word in a `:` body (writes the name to stderr, exit 70) — the old silent
no-op hid real bugs (`0<`, `STR=`, and the walk dispatcher emitting every template
because undefined `STR=` made the match always true). Found that last one instantly
with the EXISTING Forth disassembler (bootstrap/cg/disasm.fs, `DISASM addr nwords`) — use it,
not python/otool. Making undefined-word an error required: implementing the prims the
checker knows but the engine lacked (`1+`/`1-`/`0<`), and fixing a test that relied on
a tolerated forward reference (define the callee first — the standalone has no forward
refs). Lesson reinforced: in a Forth with silent undefined-word skipping, a typo or a
missing `require`d helper compiles to nothing — make it loud.

## Standalone GENERATES native code — encoders + assembler + a runnable binary (2026-06-11)

Major codegen-port milestone: the standalone now has runnable ARM64 encoders
(src/arch/arm64/asm.f, verified byte-for-byte vs habu in t-sh-asm) and a single-pass
assembler with labels + branch backpatching (src/arch/arm64/icode.f). It assembles a loop
(exit 5+4+3+2+1=15) and emits a self-signed Mach-O that runs with ZERO external tools
(t-sh-cg). Standalone gotchas hit and fixed: `{: x -- }` (a `--` inside a locals decl)
isn't parsed — drop the `--`; `0<` is NOT a standalone prim (only `0=`/`<`/`>`) and an
UNDEFINED word compiles to a silent no-op (so `dup 0<` became `dup IF` — the standalone
should error on undefined words); a SECOND `{: :}` locals group mis-reads its slot
(BR-EMIT, EMITW, PATCH — use a variable, not a 2nd local). BIGGEST time-sink: an AMFI
**signature cache** — once a path (/tmp/cg-out) is written with an invalid signature,
the kernel SIGKILLs even a later VALID binary at that same path/inode. codesign -v
passes, page hashes match, but it won't run. Fix: write to a fresh path (or rm+rewrite).
This is why a binary that ran after external re-sign still died self-signed at the old
path. Tooling: lldb works on habu binaries now (revealed load-time vs runtime kills);
python+otool disassemble.

## Standalone render — and a second-locals-group bug (2026-06-11)

Added RENDER to the standalone (src/core/render.f): walks the checker's inferred
residual stack (DCUR) and prints types as canonical letters a,b,c (bottom-to-top),
int=n, flag=f — the 'render' half of the native sigparse/checker (test/t-sh-render).
No recursion (the JIT would inline-explode) and no emit/+! (absent) — a collect-loop
+ a 1-byte type buffer. Finding it exposed a latent bug: `VAR-OF`'s second locals
group (`{: c :} c 97 - {: i :}`) mis-read its slot, collapsing EVERY type var to one.
Harmless to pass/fail verdicts (vars unify with anything) so the prim tests still
passed — but rendering exposed `swap` as "a a" not "a b". Fix: compute the slot on
the stack, no 2nd group. Lesson: a second `{: :}` group whose value is computed
between groups is unreliable in the standalone; prefer one group or stack/vars.

## Standalone checker — generic sigparse + data-table prims (2026-06-11)

Replaced the standalone checker's 4-token hardcode (dup/+/*/0=) with a generic
signature parser (PARSE-SIG: " a b -- b a " → a step effect; single letter = type
var, `n`=int, `f`=flag, shared row var) and a 25-entry prim table, plus numeric
literals as `( -- n )`. Verdicts match Forth semantics (test/t-sh-prims.fs). Two
standalone limits shaped the design: (1) declaring `{: :}` locals inside an IF/loop
corrupts the locals frame (read locals at word top, flat IF chain) — same class as
the gforth gotcha; (2) **the standalone INLINES colon-word bodies**, so a big
dispatch word with many `PARSE-SIG` calls overflows (crashes at ~3). Fix: a DATA
table (`create … c,`, scanned by a small FIND-SIG loop) so DO-TOK calls PARSE-SIG
just twice. Top-level `s"` doesn't work either (compile-mode only) — wrap test
drivers in a word.

## Standalone self-signs — and a dictionary-search bug it exposed (2026-06-11)

The standalone now SELF-SIGNS a Mach-O with zero gforth and zero external codesign:
selfhost/sign.f is the ad-hoc CodeDirectory post-pass (SHA-256 page hashes from
sha256.fs) over the unsigned image macho-min.fs builds. Apple's `codesign -v`
validates the result and it runs (test/t-sh-sign.fs builds+signs exit(42) entirely
inside the standalone). Getting there exposed a real standalone bug: **its dictionary
search returned the OLDEST definition of a redefined word, not the newest** —
`: M32 …(mask) : M32 …(writer)` left BUILD calling the mask. Forth requires LIFO
(newest shadows). emit-find / search-wl (forth.fs) scanned oldest→newest and broke on
the first match; fix is to keep scanning and take the LAST match. (Standalone binds
calls EARLY — at compile time — so already-compiled words keep their original target;
only later lookups see the new shadowing. That's why sha256's own `M32` still meant
mask while BUILD, compiled later, correctly picked the writer.)

## Standalone SHA-256 + the do-while DO gotcha (2026-06-11)

Ported SHA-256 into the standalone's own Forth (src/core/sha256.f) — the first step
to the standalone self-signing without gforth or codesign. Matches FIPS-180 vectors
(abc, 56-char, 64/100-byte) run natively (test/t-sh-sha.fs). Standalone porting
gotchas vs gforth: **the number parser is decimal-only** (no `$` hex — decimalize
all constants); **no `move`/`fill`/`emit`** (write byte-loop helpers; output via `.`,
which appends `\n`); only plain **`DO`, not `?DO`** — and the standalone's `DO` is
**do-while**: `0 0 DO … LOOP` runs the body ONCE, not zero times. So every loop that
can have zero trips (here the full-block loop when nb=0, and BMOVE when n=0) MUST be
guarded `n 0 > if … then`. ROTR/sigma/round all matched first try; the only bug was
an unguarded `nb 0 DO` hashing a garbage block for short messages. Nested `DO` across
a word call preserves the outer `I` correctly (the data-region loop-frame stack).

## RUN-EXE stack leak + register-loop throw-fault (2026-06-11)

Two bugs the self-signer work surfaced (both fixed). (1) RUN-EXE left `addr u`
(the filename) under the exit code — `2dup EMIT-EXE 2dup {: pa pu :}` never drops
the original; t-cg-word's 85 cases all showed WRONG NUMBER OF RESULTS. Fix:
`2dup {: pa pu :} EMIT-EXE`. (2) The register-resident-loop speculative-rollback
path **faulted** (gforth -9) on `E-RLOOP throw` — gforth 0.7.9 faults unwinding a
throw across emit-rloop's `{: :}` locals. This path had NEVER been exercised by a
passing test. Fix: flag, don't throw — RL-ACTIVE marks the body emit, any empty-VS
memory access trips RL-FAIL (regstack.fs), emit-rloop bails and CHECK-LOOP-CG rolls
back. The flag is also **sound** where the old VSP-delta check wasn't (a body that
underflows but nets back to the same depth is now caught). And: register residency
is unsound under an enclosing loop (the optimizer can hoist the carry's init out of
the OUTER back-edge → miscompile, e.g. nested `?DO` summed to 15 not 5), so any loop
with LOOP-DEPTH>0 takes the proven memory path. Lesson: a speculative path guarded
by `catch` is dead weight until a test actually drives it through the throw.

## Self-host milestone 4 — execution tokens + catch/throw (2026-06-10)

- Standalone now has `'`/`[']`/`EXECUTE` and `catch`/`throw`. `[']` bakes the
  found word's code address as a literal push (via the existing `c-lit` emitter,
  x11=addr); `EXECUTE` saves x30 around `BLR` (the enclosing word still needs its
  link). `catch`/`throw` chain 48-byte handler frames through a data-region cell
  `[x20+8]` (HND): the frame saves prev-HND, data-sp(x19), machine-sp, link, and a
  **resume address from an ADR inside the catch stencil** — PC-relative, so it
  survives the memcpy that inlines the stencil into a word; `throw` restores all
  four and `br`s to it. Verified: throw returns the code, no-throw returns 0, the
  data stack is restored to the catch point, nesting + sequencing work.
- **`Lkwcmp` must fold only A–Z**, not `|0x20` unconditionally, or symbol
  keywords (`'`, `[']`, `s"`) never match (`'['|0x20` = `'{'`). Keywords are
  stored literally; only uppercase-letter source bytes are lowered.
- **No forward references** in the single-pass standalone: `['] FOO` before FOO is
  defined silently emits nothing (FIND fails, the guard skips `c-lit`), leaving a
  garbage xt → `catch`/`EXECUTE` jumps to it and faults. Define before use.
- **Locals `{: a b :}`** (completes dot 4): a data-region header holds the
  per-word locals table (count, frame size, name records). `{:` records the names,
  carves an `sp` frame, and pops the declared values into slots (slot 0 = first
  name; pop top→highest slot). A body token is matched against the table
  (`Lloc-find`) BEFORE FIND → resolves to `ldr x9,[sp,#slot*8]`. `;` tears down the
  frame before RET. Locals coexist with catch: the locals frame sits below catch's
  handler frames, so `throw` unwinds to the catch and the locals stay readable.

## Self-host 7 — type-checking a word body runs natively (2026-06-10)

- Capstone: the checker's full pipeline — build effects, compose them
  (`APPLY` = unify the word's `din` against the current stack, thread `dout`
  forward), infer — runs natively. Demonstrated by type-checking `DUP *`: the
  composition forces the duplicated value to `i64` and yields `i64`, computed
  entirely by standalone-emitted machine code. With the unifier + composition
  native, the checker's *algorithmic core* is self-hosted. What remains for a full
  dot-6/7 port is the surface plumbing (sigparse string→effect, the prim table,
  render, the body tokenizer) — large but mechanical relative to the engine.

## Self-host 7 — full unification engine runs natively (2026-06-10)

- The checker's central operation — unifying two four-row stack effects — now runs
  as native code emitted by the standalone: type unification (con/var/ptr), ROW
  unification (row-var/push), structural recursion through push/rest, binding of
  both type-vars and row-vars, all via a worklist (no by-name recursion). Verified:
  `unify (i64 on row0) (var1 on row2)` binds `var1←i64` and unifies `row0~row2`;
  `unify con(3) con(4)` fails. The unifier is row-polymorphic Hindley–Milner over
  stacks — the hard part of the type checker — and it's proven runnable natively.
- Pattern for porting recursive checker algorithms to the single-pass standalone:
  replace by-name recursion (unavailable — a word isn't in the dict until `;`)
  with an explicit worklist of pending pairs; dispatch by tag (row tags vs type
  tags are disjoint). Read-only locals (no `to`) → resolve inline, don't reassign.

## Self-host 7 — occurs-check (the "core risk") runs natively (2026-06-10)

- The PLAN flags occurs/resolve mutual recursion through structure as the core
  risk. Done iteratively (a worklist instead of recursion, which the single-pass
  standalone can't do by-name): `OCCURS` resolves each term and pushes a `ptr`'s
  inner onto the worklist. Verified natively: `var0 ∈ ptr(ptr(var0))` → true,
  `var0 ∈ ptr(var1)`/`ptr(con)` → false. Adds `T-PTR` + a CREATE'd arena for ptr
  inners. This is the hardest checker algorithm, proven runnable on the standalone.
- **Standalone limitation: one `{: :}` block per word.** `c-lbrace` resets the
  locals table on each `{:`, so a second block in the same word wipes the first
  (bit `MK-PTR` when it used `{: inner :} … {: idx :}`). Use a single block, or no
  locals. (A real fix would accumulate across blocks + grow the frame.)

## Self-host 6/7 — checker core runs natively (2026-06-10)

- `CONSTANT` added (defining word: pop the value, emit a `c-lit` push body). With
  constants + the existing `CREATE`/`@`/`!`/`cells` + `BEGIN/WHILE/REPEAT` +
  `IF/ELSE/THEN`, the checker's **algorithmic core** — type-term encode/decode,
  occurs-style checks, **unification, and binding-chain resolution** — compiles
  and runs as native code emitted by the standalone (verified end to end:
  2-link var→var→con resolves, unify binds and a re-resolve sees it, con≠con
  fails). The heart of dots 6/7, proven runnable on the standalone itself.
- **Buffer sizes are real limits as the standalone grows:** `SCODE` (assembled-
  code scratch) was 8 KB and the standalone hit 8504 B → `EMITW` wrote past it →
  gforth `-9` (invalid memory address) inside ASSEMBLE, not a clean throw. Bumped
  `SCODE`→96 KB, `MPAGE` (__TEXT)→64 KB, `MSIZE` (Mach-O buf)→128 KB. Watch these
  when porting more code natively.

## Self-host milestone 5 — wordlists (2026-06-10)

- Each dict record carries a wordlist id (DREC 40→48, wid at +40; seed prims = 0 =
  FORTH). A data-region header cell holds CURRENT (new defs take it) and a WIDNEXT
  counter. `WORDLIST` hands out fresh ids; `get-current`/`set-current` pick the
  target; `search-wl ( a u wid -- addr|0 )` is FIND restricted to one wid
  (case-folded). The interpreter's FIND stays wid-agnostic (so the standalone can
  always call its own words); wordlist scoping is opt-in via `search-wl` — exactly
  what the codegen dispatch port needs (CG-VS vs FORTH).
- `S"` is compile-only — testing `search-wl` at the top level silently fed it a
  garbage string (S" skipped as unknown in interpret mode). Exercise it inside `:`.

## Crash diagnostics — in-binary register dump (2026-06-10)

- habu-built binaries (NATIVE-EVAL exes + the standalone) install an in-binary
  signal handler (`crash.fs`) for SIGILL/TRAP/BUS/SEGV that dumps the faulting
  registers (sig, x0..x28, fp, lr, sp, pc) as hex to stderr and `exit(134)`. habu
  itself dumps codegen state (`#IC`/`VSP`/`CARRY-N`/`LOOP-DEPTH`/token pos) on a
  codegen throw. No external debugger — **lldb can't launch our minimal Mach-O in
  this sandbox** (its batch `-o run` hangs even on a normal C binary; debugger
  task-port acquisition is denied), so self-contained is the only robust path.
- **macOS arm64 signal ABI without libc:** `sigaction(#46)` with a
  `struct __sigaction { sa_handler, sa_tramp, sa_mask, sa_flags }`. Set BOTH
  sa_handler and sa_tramp to the handler — the kernel enters sa_tramp directly
  with x2=sig, x4=ucontext (no libc `_sigtramp` needed). `mcontext = [ucontext+48]`;
  `__ss.__x[0]` at `mcontext+16`, then x0..x28, fp(+248), lr(+256), sp(+264),
  pc(+272). `SA_SIGINFO`=0x40.
- **Reg-31 footgun (cost two debug cycles):** in the shifted-register ALU forms
  (`ADD/MOV/ORR rd,rn,rm`) register 31 is **XZR**, but in immediate/load-store
  forms (`ADDI`, `LDR/STR [rn,#off]`) it is **SP**. `ADD x15, sp, x11` silently
  used XZR → garbage address → handler faulted → re-entered → hung. To get SP into
  a GP reg use `ADDI rd, sp, #0`, never `MOV`/`ADD`-register.

## Self-host is the real frontier — decomposed, not faked (2026-06-10)

- The standalone `bootstrap/cg/forth.fs` is a 300-line stencil-JIT native Forth with
  only `+ - * dup drop swap .` + int literals. "Standalone IS habu" (run the full
  checker + ICode codegen natively, stage2==stage3, drop gforth) is a multi-week
  bootstrap, not one increment. Decomposed into 10 ordered sub-dots (core words →
  memory → strings → locals/catch → wordlists → port arena/types → port checker →
  port codegen → in-process code-allocator → stage2==stage3). Don't fake a fixpoint.
- **Adding standalone primitives is cheap because they ARE habu ICode.** Each prim
  is a niladic word emitting icode.fs mnemonics on the x19 data stack (`b+` =
  `B g-pop A g-pop A A B ADD, A g-push`); FPRIM registers start/RET/end labels for
  stencil inlining. Milestone 1 added the comparison/logic/shift/`/`/`mod`/shuffle
  set this way — golden-tested through the standalone's own REPL (NF/NF=). Control
  flow (IF/THEN/BEGIN) is the harder remaining piece: the stencil JIT must patch
  relative branches across inlined copies.
- **Milestone 1b (control flow) landed.** The standalone's `:` compiler now emits
  IF/ELSE/THEN + BEGIN/UNTIL/AGAIN/WHILE/REPEAT by keeping a control-flow stack at
  a reserved region offset (CFSTK-OFF) and patching each branch's relative offset:
  forward branches (IF/WHILE) emit a CBZ placeholder and record its address;
  THEN/REPEAT compute `(CP-addr)/4` and OR it into the recorded word. One `Lpat`
  routine auto-detects CBZ (imm19, bit31 set) vs B (imm26) so it patches both.
  Backward branches (UNTIL/AGAIN/REPEAT) emit the offset directly (target known).
  Keyword dispatch is a tiny `Lkwcmp` (case-folded) over embedded lowercase
  keyword bytes. Verified through the standalone REPL: ABS, SGN (nested IF/ELSE),
  counted BEGIN/UNTIL, and BEGIN/WHILE/REPEAT sums all compute correctly.
- **Milestone 2 (memory + data space) landed.** `@ ! c@ c! cells here allot , c,`
  as stencils, plus CREATE/VARIABLE as interpret-mode defining words (reuse the
  `:` slot pattern + `c-lit` to emit a push-address body). **Key gotcha:** the
  data space CANNOT live in the code mmap — `;` mprotects the whole region to RX,
  so a later `!` to data bus-errors (writing an executable page). Data needs a
  SEPARATE always-RW mmap. x20 (RBASE) is dead after the startup seed-dict copy,
  so it's repurposed as the data base; `[x20]` holds DP. Verified: variables,
  arrays via `create…cells allot`, and read-modify-write all compute correctly.
- **Milestone 3 (strings) landed.** `S" …"` (compile mode) parses the string from
  the input cursor (not the whitespace tokenizer), embeds the bytes in the code
  image, emits a `B` over them, and pushes the bytes' ABSOLUTE address (known at
  compile time — no PC-relative ADR needed) + length via `c-lit`. `TYPE` is the
  `write(2)` syscall (#4). Gotcha that cost a debug cycle: `Lpat` clobbers x10, so
  keeping `len` there made the length push garbage — stash live values in
  registers the callee (Lpat/c-lit) doesn't touch, and patch the B BEFORE the
  literals are emitted (its target is the push site) while preserving len in x15.

## Register-resident DO..LOOP — habu ties clang -O3 (2026-06-10)

- **The spill was NEVER the loop bottleneck — the missing back-edge register
  liveness was.** The memory-path loop spilled the carry every iteration, but
  that store/load was hidden under the xorshift latency chain; making the carry
  register-resident alone changed nothing. habu stayed at 1.94× clang until the
  carry actually stayed in a register *across the back-edge*. Then it tied clang
  -O3 (0.20s/1e8 iters, exit 221). Measure the real critical path, don't assume.
- **Mechanism:** pre-scan the body; if every token is VS-safe (a CG-VS prim, `I`,
  or a literal) and there's no nested loop, pin the carry into fixed register
  homes at loop entry (`carry-snap`), walk the body register-resident, and at the
  back-edge parallel-move the carry-out regs back into those homes (`carry-recon`,
  cycle-safe via a T0 scratch — handles SWAP/ROT carries). Anything else (IF,
  `.`, `>R`, nested DO) makes the body non-straight-line → speculative
  `cg-snapshot`/`cg-rollback` cleanly reverts to the proven memory path.
- **A linear peephole optimizer is UNSOUND across a loop back-edge.** COPY-PROP +
  `REG-DEAD-AFTER?` treated the loop-top LABEL as "register dead after here," so
  it killed `mov x28,limit` while the back-edge `cmp x27,x28` still read x28 →
  loop ran ~0 times, wrong result, silently (correct-looking on tiny inputs).
  Fix: PIN the loop-carried regs (x27/x28 + carry homes); pinned regs are never
  proven dead and never have a copy coalesced into them. The same boundary-as-
  dead assumption was already latent-unsound for forward fall-through.
- **Correct results do NOT prove the fast path ran.** Both paths are correct;
  only disassembly (`otool -tv`) or an op-count probe shows whether the carry is
  register-resident. A stack-underflow bug in the `?DO`/`DO` guard (`2over` on a
  3-item stack) silently sent every loop to the memory path while all tests
  passed. Verify the generated code, not just the answer.

## Floating point — floats on the data stack (2026-06-10)

- **One model decision unlocked everything: an f64 is ONE data-stack cell holding
  the IEEE-754 bits** — no separate FP stack (unlike hosted gforth). DUP/DROP/
  SWAP/@/!/literals all "just work" on floats for free; only the arithmetic prims
  (F+ F- F* F/ FNEGATE FABS FSQRT, the compares, S>F/F>S) are new. They move the
  cell bits X→D (`FMOVXD`), compute in the D-register file, move back D→X
  (`FMOVDX`) into a pool register. D0/D1 are scratch FP regs.
- **Float literals MUST be classified BEFORE `NUMBER?`.** `s>number?` parses
  "2.0" as a *double* (n=2), so `NUMBER? nip nip` is truthy and would steal it as
  i64. Gate on an FP marker ('.', 'e', 'E') + `>float` success, ahead of the
  integer clause, in BOTH the checker (`CHECK-FLOAT`) and codegen
  (`EMIT-FLOAT`). Reinterpret bits via a scratch `f!` then `@`.
- **FP-register residency, not a second stack, is the perf answer.** habu is
  statically typed, so the checker already separates `f64` from `i64` — the job a
  classic Forth FP stack does at runtime. Keep the unified data stack; add a third
  VS tag `V-FREG` (value lives in a D-register) so chained ops (`F+ F* F-`) stay
  in the D-file. `v-popd` FMOVs a non-resident operand in; results push V-FREG;
  `v-popr`/`v-spill` FMOV out only at a GP consumer or control-flow boundary.
  Measured: `2.0 3.0 F+ 4.0 F* F>S` emits 0 FMOVDX (down from 2) — only the
  unavoidable literal loads remain. The shuffle/spill words must each free/copy
  the right pool (`d-free`/`FMOVDD` for V-FREG) or floats leak D-regs.
- **FP condition codes differ from integer.** After `FCMP`: F< → `MI` (N set),
  F> → `GT`, F= → `EQ`; F0< → `MI`, F0= → `EQ`. (Integer `<` uses `LT`.)
- **Adding a TC-* type touches FOUR renderers or CHART throws 1495
  (E-BADTYPE):** sigparse `CON-CODE`, render `CODE$`, diag `TY-NAME`, plus the
  config code itself. Miss `render.fs CODE$` and even `PRIM` fails at chart time
  (it renders the canonical sig). No gforth-FP oracle exists for our model, so
  test via `F>S`→int exit codes.

## Environment

- Homebrew ships Gforth **0.7.3** only; "0.7.9" is the unreleased dev branch and
  needs a source build from git.
- **habu targets 0.7.9**, built from `git://git.savannah.gnu.org/gforth.git`
  (version `0.7.9_20260513`), installed at `~/.local/bin/gforth`. 0.7.3 also
  present at `/opt/homebrew/bin/gforth` — ensure `~/.local/bin` precedes it on PATH.

## Building Gforth 0.7.9 on macOS ARM

- `configure` **rejects Apple clang** ("long unfixed bug, use GCC"). Must build
  with real GCC: `./configure CC=gcc-15` (brew `gcc`). `/usr/bin/gcc` is clang — fails.
- Need GNU autotools + texinfo + GNU libtool from brew: `autoconf automake libtool texinfo`.
  Run `glibtoolize` (not Apple `libtool`); `./autogen.sh` picks it up via `LIBTOOLIZE=glibtoolize`.
- texinfo + m4 are keg-only: prepend `/opt/homebrew/opt/texinfo/bin` and
  `/opt/homebrew/opt/m4/bin` to PATH for `makeinfo`.
- Full recipe: `./autogen.sh && ./configure --prefix=$HOME/.local CC=gcc-15 && make && make install`.
  `install-info` prints harmless "excess command line argument" warnings.

## Gforth 0.7.9 gotchas (verified against the binary)

- **`echo '…' | gforth -e bye` swallows stdout.** Run via a `.fs` file
  (`gforth /tmp/x.fs -e bye`) whenever output matters.
- **`gforth … -e bye` exits 0 even when a `T{}T` assertion fails.** Do NOT use
  the exit code as a pass/fail signal — maintain a failure counter and
  `(bye)` with an explicit code (see `PLAN.md` Test strategy).
- **`IF`/`ELSE`/`;` are compile-only** — all conditional/diagnostic logic must
  live inside `:` definitions, never at the top level.
- **Hayes tester ships with gforth** at `share/gforth/<ver>/test/{tester,ttester}.fs`
  but is **not** on the `require` path — vendor it by copying into the repo.
- **`parse-name` returns a transient `(c-addr u)`** invalidated by the next
  `s"`/`."`/`refill` — `move` the bytes out immediately before parsing again.
- **`s>number?` returns a double** `( c-addr u -- d flag )` — narrow with `d>s`.
- Verified working: locals `{: a b :}`, `RECURSE` (incl. through `EVALUATE`),
  overriding `:` via saved xt + reentrancy flag, `outfile-execute` output
  capture, `wordlist`/`set-current`/`create ,`/`search-wordlist`/`>body`.

## Process

- **One concern per file.** Don't bundle unrelated responsibilities in a single
  file (e.g. `sig.fs` was about to hold parser + renderer + DB + primitive
  table). It hurts readability AND blocks parallelism — split at responsibility
  seams (`render`/`sigparse`/`db`/`prims`) and those build concurrently.
  Co-locate only things that change together or share one hook. **Why:** the
  user flagged the over-stuffed file; finer files = more parallel agents + easier
  review. **How to apply:** when a file's description lists "X + Y + Z" of
  distinct kinds, that's a smell — make X, Y, Z separate files with explicit deps.

## Debugging — never diagnose at the gforth interpret level (BLOCKING)

The single biggest time-sink in the Stage-2 build was **broken diagnostics**, not
broken codegen. Compile-only constructs read GARBAGE when typed at the REPL /
`gforth -e`, silently, with no error:
- `[']` is compile-only → use `'` interpretively (or wrap in a `:` def).
- `i`/`j` and `{: … :}` locals only work inside a colon definition.
- `>r … r@ … rdrop` straddling a `do … loop` returns the **loop index** from
  `r@`, not your saved value (the loop owns the return stack).

Each of these made a probe "prove" a codegen bug that did not exist (mis-bound
label offsets, missing prim bodies). **Rules:** (1) wrap any multi-step
diagnostic in a `:` definition and call it; (2) prefer the dogfooded inspectors
in **`bootstrap/cg/inspect.fs`** (`ICDUMP`, `ICSCAN`, `?LBL`, `ICAT`) and the
**`test/nf.fs`** harness (`NFX` = build+run+show, `NF-RUN`/`NF=`) over hand-rolled
one-liners; (3) for ground truth on an emitted binary, use the EXTERNAL oracle
`otool -tV <file>` — it has no habu-tooling bugs. **Don't fight habu's tooling;
improve it** — when a one-liner is awkward, add a tested word to `inspect.fs`.

The type checker would NOT have caught these: it checks the stack *effect* of habu
source, not the *value* semantics of a return-stack/`do-loop` interleaving, and
throwaway `gforth -e` snippets are never run through it at all. The fix is
discipline + reliable tooling, not a new checker feature.

## Case-insensitivity (gforth, and habu itself)

- **gforth is case-insensitive**, so a `{: decl :}` local collides with a
  `variable DECL` — `decl DECL !` stored at the local's value (a type term) →
  "Invalid memory address". Never name a local the same as a global ignoring case.
- **habu should be case-insensitive too** (it checks Forth). Word/type lookups go
  through `search-wordlist`/`find-name` (already CI). Keyword/type-name matching
  must use a CI compare, not `compare`. The ONE case-meaningful element is the
  single-letter signature var: lowercase = type var, uppercase = row var — and
  that's unambiguous because type names are ≥2 chars, so length disambiguates.

## Test hygiene (shared gforth image)

All tests run in ONE gforth image, so global state leaks between test files —
per-file tests can pass while the combined `all.fs` suite fails. Rules:
- **Never `is` a real seam defer in a test** (e.g. `is OCCURS-TYPE`) — it stays
  re-pointed and breaks every later test. Use a throwaway `defer` to exercise the
  mechanism. (This corrupted `unify` across the suite.)
- **Tests that build raw-id terms and then resolve must `TV-CLEAR RV-CLEAR`
  first** — `TV-RESET`/`RV-RESET` only zero the NEXT counter, not the bind arrays;
  a prior file's bindings on ids 0,1,… leak in. Production is safe (the checker
  only touches `TV-ALLOC`-cleared ids). (This broke `t-render` after `t-unify`.)
- **Don't chart names that collide with the primitive table** (re-`create` warns
  and the warning fails an empty-output check).
- **Always run the integrated suite**, not just per-file — it's the only thing
  that surfaces cross-file leakage.

## Implementation findings (build of the checker)

- **Nested parens break `( … )` stack comments** — the inner `)` closes the
  comment early (`( (R0,i64) )` leaves a stray `)`). Never put parens inside a
  comment; write `( R0 i64 )`.
- **zsh doesn't word-split unquoted `$VAR`** — `gforth $SRC …` passes the whole
  string as one filename. List the `.fs` files explicitly (or `${=SRC}`).
- **Tag-sentinel collision (design):** any tagged cell stored in a slot where
  `0` means "unbound" must encode to nonzero. Row tag `S-ROW=0` made `MK-ROW 0`
  (row var id 0) equal `UNBOUND` → fixed by `S-ROW=1 S-PUSH=2`. Type vars were
  safe only because `T-VAR=1`. Keep tags that can carry payload 0 nonzero.
- **Clean error-code tests:** wrap the failing sequence in a `( -- )` named word
  and use `' WORD catch` → a clean `( code )`. `catch` restores the data stack to
  its depth when `catch` ran; operands built *inside* the word vanish on throw,
  so no stray operands are left (unlike building them before `catch`).
- **Mutual recursion:** `defer` the seam (in `forward.fs`) + `is` it later for
  cross-file; `RECURSE` for a word recursing on itself; a file-local `defer`+`is`
  for two mutually-recursive words in the same file (used for occurs).
- **Scheme persistence:** a scheme is a canonical signature **string** (DB holds
  strings); `INST`=re-parse (fresh vars by name per call → polymorphism for free),
  `GENERALIZE`=render. Sidesteps copying terms out of the per-check arena.

## habu REPL + TUI (Forth, dogfoods the checker, 2026-06-10)

Two front-ends, both Forth (no C/Zig — the Zig `~/Work/pz` is reference only):
- **`src/repl.fs`** (`habu-repl.fs`) — line REPL. Enter a checked def → `✓ NAME (
  effect )` or the habu diagnostic; non-def lines `EVALUATE`. Reads stdin via
  `stdin read-line` (NOT `refill`, which reads the `-e` eval source and hits EOF).
- **`src/tui.fs`** (`habu-tui.fs`) — full-screen TUI with **as-you-type** feedback:
  `RUN-TUI` raw-modes the terminal (`stty raw -echo`, restore `stty sane`),
  single-line live editor (`habu> <buf>   <status>`, horizontal positioning only —
  robust everywhere), and on each keystroke shows the inferred effect / diagnostic
  via **`CHECK-DRY`** (checks WITHOUT charting — refactored `CHECK-DEF` into a
  non-charting `CHECK-CORE`; else every keystroke pollutes the effect DB).

- A new `CHECK-CODE` var in `colon.fs` lets the REPL read the last `:`-outcome
  (-2 none / 0 ok / err code) so success shows the effect; failures self-report.
- Define REPL/TUI infra with **`CHECKING-ON? off`** then re-enable at file end —
  `( -- )` parses as a valid sig, so otherwise the helper words chart themselves
  as `R -- R` and clutter `WORDS`.
- The raw key loop needs a real tty; gate it behind `TTY?` (`test -t 0` via
  `system`/`$?`) and fall back to a message on a pipe. Test the parser +
  `CHECK-DRY` (no tty needed); the key loop is thin glue exercised by hand.

## Register allocation — store-forwarding, and the register-reuse wall (2026-06-10)

`opt.fs` gained two block-local passes (`STORE-FWD`, `X19-CANCEL`): forward
`STR→LDR` to register MOVs, kill overwritten/unobserved stores (DSE), then cancel
the orphaned inverse `ADDI/SUBI x19` pairs. Boundaries (label/call/branch/generic
memory op) flush tracking; a slot's forward-register is dropped whenever that
register is redefined (the correctness lynchpin — backstopped by the differential
native-exe suite). Real wins on multi-value shuffles: `OVER +` 13→9, `SWAP OVER -`
18→14, `DUP ROT * +` 22→18 (~20-30%).

The dup-heavy hot loop (xorshift) didn't improve under store-forwarding — the
**register-reuse wall**: `DUP 13 LSHIFT XOR` loaded `h`→x9, stored the dup, then
`LSLI x9` reused x9, so the dup'd `h` survived only in memory; forwarding the
reload was (correctly) impossible. Root cause: every `templ.fs` primitive
hardcodes `T0/T1/T2`, so distinct live values collide.

**Fixed — abstract register stack (`bootstrap/cg/regstack.fs`).** A compile-time value
stack whose entries are POOL registers (`x13-x15,x20-x24`) or CONSTANTS; pure
arithmetic/shuffle primitives operate on it with NO memory traffic (DUP copies to
a fresh register, so the copy survives later ops). `walk.fs` SPILLS the whole VS
to memory before anything that isn't a VS primitive (control flow, calls, return-
stack ops, `>R`, unsupported words) — so those keep the proven memory path
unchanged; correctness is isolation-by-spill + the checker's branch balance. The
VS folds constants and selects immediate shifts itself (subsumes the old `CTS`).
Result: `DUP 13 LSHIFT XOR DUP 7 RSHIFT XOR DUP 17 LSHIFT XOR` **25→13** ops (and
pre-opt == post-opt — the allocator emits near-optimal code directly); `DUP DUP *
*` 15→8, `DUP *` 8→6. Full differential suite green (control flow, recursion, CLI).

- **Wordlist-collision footgun:** the VS comparison helper was first named `g-cmp`
  — but `templ.fs` already defines `g-cmp` in the FORTH wordlist, and a CG-VS word
  referencing `g-cmp` resolves it via the SEARCH order (which has templ's, not
  CG-VS's), silently calling the old memory version. Renamed to `vcmp`/`vcmp0`.
  When a new wordlist's words call helpers, give the helpers names that aren't
  shadowed in the search order.
- **Invariant:** no two VS entries name the same register (DUP/OVER emit a copy),
  so reusing a popped operand register as an op's result is always safe.
- **Shifted-operand fusion (B3, done).** With values register-resident, an
  in-place immediate shift feeding an ALU op (`LSLI rd,rd,#k ; EOR rx,ry,rd`, rd
  dead after) fuses to `EOR rx,ry,rd,LSL #k` — one instruction, matching LLVM.
  Mechanism: `SHIFT,` packs (type,amount) into the ALU record's `IC-D`; the `RRR`
  encoder ORs in bits [23:22] (type) + [15:10] (amount); `d=0` = `LSL #0` so
  unshifted ops are unaffected. Done as an opt.fs peephole (`OPT-SHIFT-FUSE`,
  reusing the store-forwarding liveness/boundary machinery) rather than threading
  a shifted-register kind through the value stack — far less surface. Result:
  xorshift chain **13→10**.
- **MOV-coalescing / copy propagation (done).** `OPT-COPY-PROP` turns the DUP-copy
  of an in-place self-op (`MOV r2,r ; EOR r,r,r2,LSL#k`) into `EOR r,r,r,LSL#k` —
  propagate the copy into the reader, kill the MOV when the copy is dead after.
  **Footgun:** the reader of the copy is often *also* the instruction that
  redefines the copy's source (`EOR r,r,r2` writes `r`=source) — that's a
  read-before-write, so check the reader case BEFORE the "source redefined → stop"
  guard, or it never fires. Result: **xorshift 10→7** (`eor r,r,r,lsl#k` ×3 +
  load/store framing = LLVM hand-asm parity; full pipeline **54→7**, ~7.7×). The
  last lever is loop-invariant allocation across the back-edge.

## AOT locals + the u8/u32 "typed payoff" reality (2026-06-10)

- **Locals (`bootstrap/cg/cglocals.fs`) — done.** `{: a b :}` lowers to a per-word
  FRAME (`LOCSZ` bytes carved BELOW the data stack in `g-prologue`, addressed
  `[sp,#slot*8]`). At the opener: spill the VS, pop the named inputs into slots;
  a name use LDRs its slot onto the VS. Because locals are in memory (not the VS),
  they **survive spills** — so they read correctly across IF/ELSE and loops (the
  key correctness property; tested). Hooked into `EMIT-TOKEN` before the
  number/prim classification via `CHECK-LOCAL-CG`; reuses the checker's
  `BRACE-OPEN?`/`NAME-PART`/`CI=`. Inputs-only (v0), matching the checker.
  **Layout gotcha:** locals at `[sp,…)` would alias the data stack (`Xds=sp`) —
  `g-prologue` now sets `Xds = sp+LOCSZ` so the frame sits below the data stack.
- **u8/u32 width is correct by construction — no typed selector needed.** The
  plan assumed fixnum tags/boxing and a typed-stencil bank; habu never had tags
  (values are full 64-bit cells, like gforth), and the register allocator makes
  registers IR fields (no frozen stencils to specialise). gforth's model is
  "full-cell arithmetic, truncate only at a byte store" — habu matches it exactly:
  `c@`/`c!` use `LDRB`/`STRB` (zero-extend / low-byte). So the "typed payoff"
  (unboxing) is free, and width truncation happens at the right point already. No
  `UXTB`/`UXTW` in registers required for correctness.

## Speed gate CLOSED — the bar is LLVM, not gforth (2026-06-10)

gforth performance is not the competition; **LLVM (`clang -O3`) is the bar** a
native backend must rival. `bench/dispatch.{fs,s,c}` — a dispatch-bound byte-mix
(load + add + 3× `dup <<n xor`, ~10 cheap ops/byte, 983 M iters); the serial
xorshift chain is latency-bound so NEITHER side can vectorize — a fair
head-to-head. Measured on this M-series box:

| build | ns/iter | note |
| ----- | ------- | ---- |
| **clang -O3 (LLVM — the bar)** | **2.252** | optimized C |
| hand ARM64 (what habu targets) | 2.256 | LLVM **parity** (same exit 203) |
| clang -O0 (unoptimized C) | 7.21 | — |
| gforth-fast (its own native codegen) | 2.27 | parity too |
| gforth threaded (habu-checked runs on) | 23.87 | 10.6× slower — irrelevant baseline |

**Verdict: habu's native target matches LLVM -O3** on this loop. Caveat: the hand
baseline fuses `dup <<n xor` into one shifted-EOR; habu's current backend emits
separate shift+xor through the stack, so habu's *real* output sits above 2.25 —
the shifted-operand-fusion peephole (dot) closes that last gap to LLVM. Always
measure against LLVM `-O3`, never gforth.

**Constant folding (`bootstrap/cg/walk.fs`).** Literal arithmetic folds at compile time
via a compile-time value stack: numbers are deferred (not emitted); a foldable op
over pending constants folds them; any other token first flushes them as g-lits
(so runtime values never mix with deferred ones). `3 4 + 5 *` → one `LIT 35`.
Only ops whose gforth semantics match the emitted ARM64 are folded (NOT `/`,`MOD`,
`2/` — division rounding / shift signedness differ). A **constant shift amount**
over a runtime value (one pending const at `LSHIFT`/`RSHIFT`) emits an *immediate*
shift (`lsl/lsr #k`, a UBFM alias — the disassembler shows raw `.word`) instead
of materialising `k` + a register shift. Full shifted-operand *fusion* (fold the
shift into the next `EOR`/`ADD`/… — the last gap to LLVM on the dispatch loop)
needs ICode ALU records to carry a shift field — a bigger IR change (dot B3).
**Footgun:** `s>number?`
leaves its double on the stack *even on failure* — the non-number path must
`2drop` it or a garbage "token" reaches the encoder (E-NO-ENC).

**Dogfooding the codegen (the honest limit).** The codegen proper (`EMIT-TOKEN`,
the value stack) is metaprogramming — `s>number?`, `search-wordlist`, `execute`,
raw stacks — so it cannot be written as fully *checked* habu. But the bug class
that actually bit the build IS catchable: chart `s>number?` as `( R str -- R i64
bool )` (the double modeled as one i64) and the checker's "both IF arms must
leave equal stacks" rule rejects the exact mistake — a branch that consumes the
flag but forgets the value — with `E-BRANCH` (`test/t-dogfood.fs` proves
`S>NUMBER? IF DROP THEN` is refused, `S>NUMBER? IF THEN` accepted). So: the
codegen stays trusted, but the *failure mode* is now expressible and demonstrably
caught — which is the real value the dot asked for.

## Codegen Phase 0.3 — speed gate (superseded by the dispatch bench above)

Serial scalar LCG (`x = x*A + C`, 1e9 iters; `bench/inner-loop.{fs,s}` +
`bench/run.sh`). **Native baseline must be hand-written ARM64** (`inner-loop.s` —
the exact `mul; add` habu emits), NOT clang `-O2` C: clang unrolled/scheduled to
0.97 ns/iter, flattering native; the faithful naive loop is **1.26 ns/iter** and
**unroll-8 is identical (1.26)** → the loop is **latency-bound** (mul→add serial
chain; unrolling buys nothing).

| Build | ns/iter | native advantage |
| ----- | ------- | ---------------- |
| native (hand ARM64, real floor) | 1.26 | — |
| gforth-fast | 2.08 | **1.66×** |
| gforth threaded (= habu today) | 5.98 | **4.76×** |

**Two lessons.** (1) **Don't measure the native ceiling with C** — clang's
optimizer ≠ what habu emits; use hand-asm. (2) **The LCG is the WRONG gate
benchmark**: it's latency-bound (2 serial ops, `mul` latency irreducible and
shared by both engines), so native's real win — eliminating per-op threading
dispatch — is a small fraction → only 1.66× over gforth-fast. The plan specified a
**dispatch-bound** loop ("decoder/VM step, arith + `@`/`c@` + a branch") where
gforth pays NEXT per cheap op and native collapses them to register ops → expect
3–10×. **Gate is PENDING a dispatch-bound re-bench.** Decisive even now: **4.76×
over the threaded engine habu actually uses**; the 2× bar is only contested vs
gforth-fast (an engine habu doesn't use) on an unfavorable loop.

**asm gotcha:** in clang's integrated ARM assembler **`;` is a comment**, not a
statement separator — `mul …; add …` silently drops the `add`. Put one
instruction per line. (Caught a fake "0.16 ns/iter, 8× speedup" = dead muls.)

### Dispatch-bound bench — gforth-fast IS a native engine (2026-06-10)

`bench/dispatch.{fs,s}`: xorshift byte-mix over 64KB × 15000 (~10 cheap ops/byte),
ns/byte (all three agree, low byte 203):

| Build | ns/byte |
| ----- | ------- |
| native (hand ARM64) | 2.28 |
| gforth-fast | 2.21 (**parity with native**) |
| gforth threaded (plain) | 23.69 (10.4× slower) |

**gforth-fast uses dynamic superinstructions / native-code copying — it compiles
to native (~0.2ns/op << one NEXT).** So hand-asm is at PARITY, not ahead. Hard
consequences:
- **habu-checked words are gforth colon words → running habu under `gforth-fast`
  already gives native speed for free, no backend.** A backend that only matches
  gforth-fast adds nothing on speed.
- The "unboxing" win doesn't apply vs gforth: gforth cells are raw/untagged
  (nothing to unbox — that was a *habu* fixnum-tag cost, not gforth's).
- Even register-kept accumulator (asm) = parity; gforth-fast's stack ops are
  L1-cheap.

**Reframe the speed gate:** "≥2× over gforth-fast" was wrong — gforth-fast is a
native engine, not a threaded strawman. The backend's justification is the
committed **self-host / standalone (gforth dropped)** goal, which needs native
codegen regardless of speed; parity-with-gforth-fast + 10× over threaded is a
fine target for a standalone. Beating gforth-fast is a later *bonus* (cross-word
regalloc, type-specialized width — look modest here), not a gate. Don't bank the
project on outrunning gforth-fast.

## Codegen working end-to-end (2026-06-10)

habu generates ARM64 machine code on the Mac, in Forth, no C. Pipeline:
Forth body → `cg/templ.fs` (tokenize, stack-op generators over Xds=x19) →
`cg/icode.fs` IR → `cg/opt.fs` peephole → `cg/asm.fs` encoders → `cg/macho.fs`
(dynamic Mach-O in a buffer) → `cg/exec.fs` (write + `codesign -f -s -` +
`system`-run). Proven: `test/t-cg-exe.fs` (exit/add/mul/loop/stdout),
`test/t-cg-word.fs` (`DUP *`→square, `3 + 2 *`, `DUP OVER + +`, …).

Mach-O / exec findings:
- gforth `$?` is the raw **wait status**; exit code = `8 rshift $FF and`.
- `codesign -f -s -` adds the ad-hoc `LC_CODE_SIGNATURE` into **header slack** —
  leave the entry at file offset 0x1000 (cmds end ~440) so there's room; it also
  extends `__LINKEDIT` (segment at 0x4000, file padded to one page).
- libSystem LC_LOAD_DYLIB: ts=2, current=1356.0.0 (`$054C0000`), compat=1.0.0
  (`$00010000`); dylinker `/usr/lib/dyld`; `__text` flags `$80000400`; header
  flags `$00200085` (incl. PIE). Static binaries are SIGKILLed — dynamic only.
- Data-stack model: reserve on the machine stack (`sub sp,sp,#256; add x19,sp,#0`),
  push=`str reg,[x19]; add x19,x19,#8`, pop=`sub x19,x19,#8; ldr reg,[x19]`.
  `mov xN,sp` must be `add xN,sp,#0` (reg 31 = SP only as ldr/str/add base; it is
  XZR for `mov`/logical).

**Remaining for fully-standalone (gforth dropped):** wire `CODEGEN-HOOK` so live
`:` definitions auto-compile (read `CAP$`); broaden the op set (control flow,
combinators, locals); then Part F — a native Forth runtime (interpreter +
dictionary + `evaluate`) so the artifact self-compiles without gforth (the
stage2≡stage3 fixpoint). Part F is the genuine long pole.

## habu is a working native AOT compiler (2026-06-10)

habu (hosted on gforth) compiles checked Forth to **standalone ARM64 macOS CLI
executables** — no gforth at runtime, no C, no LLVM. `bootstrap/cg/`:
`icode` (IR+mnemonics) → `opt` (peephole) → `asm` (encoders) → `templ` (prim/
control generators) → `walk` (tokenize body) → `link` (subroutine ABI, deps,
multi-word + MAIN) → `rt` (`.`/atoi runtimes) → `macho`/`exec` (emit+sign+run).
Wired to the checker via `CODEGEN-HOOK` (`forward.fs`/`colon.fs:80`), gated by
`CODEGEN-ON?`. Front door: `s" /tmp/sq" HABU-EXE SQUARE` → `./sq 12` prints 144.

Supported subset (tested, `test/t-cg-*.fs`): DUP DROP SWAP OVER NIP, + - * / MOD
1+ 1- NEGATE, AND OR XOR, < > = <= >= <> 0= 0< 0>, IF/ELSE/THEN,
BEGIN/UNTIL/AGAIN/WHILE/REPEAT, ?DO/DO/LOOP/I, EXIT, RECURSE, word→word calls,
`.` (signed-decimal print). Verified standalone: `rfact 7`=5040, `sumto 100`=5050.

Gotchas hit:
- **Non-leaf detection must count every BL-emitting token** (`.`, RECURSE, a
  callee), else x30 isn't saved and the word's `RET` jumps to itself → infinite
  loop. (Disasm caught it.)
- **`mov xN, sp` is `add xN, sp, #0`** — reg 31 is SP only as a ldr/str/add base;
  it's XZR for mov/logical.
- **Load cg under `CHECKING-ON? off`** — its `( idx -- u32 )` comments parse as
  habu sigs and locals confuse the override; cg is infra, not checked habu.
- **`EMIT-PRIM` must throw silently** (no diagnostic print) — the codegen hook
  validates by catching it, so printing leaks during normal skips.
- **Subroutine ABI:** Xds (x19) is a global threaded through calls (push/pop
  mutate it, never restored); non-leaf words save/restore x30; args/results live
  on the Xds data stack; CLI entry gets x0=argc, x1=argv (save argv in x22).

**Still gforth-hosted:** the COMPILER runs on gforth. Fully standalone (compiler
self-hosts, gforth dropped) needs Part F — a native Forth runtime (interpreter +
dictionary + evaluate). That remains the long pole.

## Part F — standalone native Forth interpreter (2026-06-10)

`bootstrap/cg/forth.fs` emits a **standalone native Forth** (no gforth, no C): a Mach-O
with a dictionary + subroutine-threaded primitives + an outer interpreter that
parses an embedded source line, number-pushes, FINDs, and EXECUTEs. Proven
(`test/t-cg-forth.fs`): `2 3 + .`→5, `10 20 + 5 * .`→150, `8 3 swap - .`→-5.

Design choices that worked:
- **PC-relative throughout** (PIE-safe): non-PIE binaries still get ASLR'd on this
  macOS (load base ≠ VMBASE — measured), so bake nothing absolute. Code base
  `RBASE = ADR(anchor)` at startup; dict stores **code byte-offsets**, EXECUTE =
  `RBASE + offset` → BLR. Data (strings, dict) embedded via new ICode pseudo-ops
  `BYTES,`/`DCQ,`/`DLBL,` (DLBL = a cell holding a label's byte offset), reached
  by `ADR`.
- **Subroutine-threaded**: primitives are native routines ending in `RET`;
  EXECUTE is `BLR`. No IP/NEXT/DOCOL engine needed.
- Bug that cost time: a routine that **bakes a count must run after the thing it
  counts** — `emit-find` baked `#PRIMS` but ran before `emit-prims` → baked 0 →
  FIND looped zero records → silent no-op. Emit prims first.

Registers: x19=DSP, x20=RBASE, x21/x22=input ptr/end, x23/x24=tok addr/len.

**Stage 2 (runtime `:`/`;`) — BUILT & working.** The emitted Forth JITs new words
into an `mmap`'d region by INLINING stencils: each token's machine code (a
primitive's body, or a prior word's, both minus the trailing `RET`) is copied
into the new word, so compiled words are fully flattened/leaf — no calls, no
`x30` save needed, and BL-range to `__TEXT` never matters. Literals compile to a
`movz/movk` + push stencil. `5 SQ .`→25, 4-level nesting (`C=B B`,`B=A A`,
`A=DUP *`: 2→65536) all pass (`test/t-cg-forth.fs`).

- **JIT memory on Apple Silicon: W^X, pure syscalls.** Plain `RWX` mmap (`prot=7`)
  faults on execute (verified: exit 0/garbage). `MAP_JIT` needs the jit
  entitlement + a libSystem call (`pthread_jit_write_protect_np`) — avoided. The
  working recipe (proven by a probe → exit 25): `mmap` **RW** (`prot=3`,
  `MAP_ANON|MAP_PRIVATE=0x1002`) → write code → `mprotect` **RX** (`prot=5`) →
  flush → execute. No entitlement, no C, no `pthread_*`.
- **Cache flush is mandatory & ordered:** per 64-byte line `DC CVAU` (clean
  dcache to PoU — new `DCCVAU,` op, `0xD50B7B20|Rt`), then `DSB ISH`, then
  `IC IVAU` per line, `DSB ISH`, `ISB`. Do it AFTER the `mprotect` RX.
- **W^X re-toggle bug:** the runtime dict lives in the same region as the code.
  After `;` makes the region RX, the NEXT `:` must `mprotect` **RW _before_**
  writing the new dict slot — else the slot store hits read-only memory. (Cost a
  while: a single def worked, the second produced empty output.)
- **Case-insensitivity bites the emitted FIND.** habu source is UPPER-CASE
  (`DUP`); `emit-prims` registers lower-case (`dup`); a raw byte `FIND` matched
  `+ - *` (no letters) but not `DUP/DROP/SWAP`. Stage-1 tests only ever used
  lower-case source, so it hid for ages. Fix: fold `A–Z→a–z` on BOTH bytes in
  FIND's compare (branchless: `sub #'A'; cmp #26; cset cc; lsl #5; orr`). The
  native Forth must be case-insensitive like gforth + habu itself.

**Stage 3 (read program from STDIN) — built.** `FORTH-REPL-EXE` emits a Forth
with no baked source: it `mmap`s a 1 MB RW buffer and loops `read(fd=0,…)` until
EOF, then runs the same outer interpreter over it. `echo ': SQ DUP * ; 5 SQ .' |
./forth` → 25; multi-line works.
- **Tokenizer must treat newline as whitespace.** Stage-1/2 split tokens on
  **space (32) only**; baked single-line tests never contained `\n`, so it hid.
  Piped/multi-line stdin has `\n` (and `echo` appends one) → tokens glued, FIND
  failed, empty output. Fix: any byte **≤ 32** is a delimiter (`CMP #32` →
  `C-HI` starts a token, `C-LS` ends it). Probe the syscall in isolation first
  (`read` round-trips fine) to localize the bug to the tokenizer, not `read`.

## Type checker — capability vs. coverage (2026-06-10)

When dogfooding the codegen (`bootstrap/cg/`) as typed habu, the question arose whether
the checker was lagging. **It wasn't the engine — it was prim-DB coverage.**

- The checker ENGINE is capable: `src/control.fs` already models `IF/ELSE/THEN`,
  `BEGIN/UNTIL/AGAIN/WHILE/REPEAT`, `DO/?DO/LOOP/+LOOP`, `I`/`J` (depth-tracked),
  `EXIT`, `RECURSE`; `src/prims.fs` charts the return stack (`>R/R>/R@/2>R/…`) and
  combinators (`DIP/KEEP/BI/TIMES/EACH/MAP/FOLD`). A `?DO … LOOP` word checks
  cleanly; the earlier review claim that counted loops were unmodeled was wrong —
  verify by test, not by assumption.
- What lagged: the codegen-supported words `MIN MAX 0< 0> 0<> U< U> WITHIN 2DUP
  2DROP 2SWAP /MOD` were **not charted**, so any program using them escaped to
  `E-UNCHECKED` (compiled native, unverified). Fixed by extending `src/prims.fs`.
  Lesson: the checker's reach = the prim DB; keep it in step with what the
  codegen accepts, or checkable code silently slips to unchecked.
- Genuinely untypeable (stay out of checked code): `?DUP` (variadic 0/2 outputs),
  `PICK`/`ROLL`/`DEPTH` (runtime depth). Not worth special-casing yet.
- Dogfooding payoff is real: the session's bugs (encoder `drop` arity, CF-stack
  order) are exactly stack-effect errors the checker rejects. Charting more prims
  widens how much of our own code habu can verify. Effect syntax → `docs/effects.md`.
- Smell noted: `EFFECT-OF` returns `( a u -- ea eu )` when found but a single `0`
  when absent — asymmetric stack effect; callers must `dup 0= if drop …`.

## Self-host 6/7 — native type-checker over source bodies (2026-06-11)

- The standalone now TYPE-CHECKS real source bodies natively: a tokenizer walks
  the body string, each word maps to a fresh-instantiated effect (builder words
  using a FRESH var counter), effects compose via the native unifier (threading
  the stack state in a variable since standalone locals are read-only), and type
  errors are detected and reported. Verified natively: `dup +` ok→i64,
  `dup 0= +` REJECTED (`+` can't consume a bool), `dup dup + +` ok. This is the
  checker — tokenize → lookup → unify-compose → detect — running on the self-hosted
  Forth. Remaining for full dots 6/7: string `sigparse`, the complete prim table,
  and `CHECKED:` wired into the standalone's `:`.
- Recurring trap: `S"` is COMPILE-ONLY. Any S"-driven test at the top level feeds
  the callee garbage (S" is skipped as unknown in interpret mode). Wrap in a `:`.

## Self-host 6/7/9 — checked compilation wired in (2026-06-11)

- The standalone is now a SELF-HOSTING CHECKED Forth: a `set-check` primitive
  installs a hook (a word `( body-addr body-len -- ok )`); the `:` compiler
  captures the body tokens (space-joined) into a data-region buffer, and at `;`
  runs the hook and PUBLISHES the word only if it returns nonzero. Wiring the
  native type-checker (CHECK) as the hook: `: SQ dup * ;` checks ok and is
  published; `: BAD dup 0= + ;` is rejected (not published, `dup 0= +` is a type
  error); `7 SQ` → 49. The standalone checks AND compiles natively — dot 9's
  codegen-hook in spirit, with the stencil JIT as the in-process code allocator.
- Order at `;`: flush region to RX BEFORE calling the hook (so the hook, a
  compiled word, is callable), and publish (NDICT++) AFTER, gated on the verdict.
  Save x30 around the hook BLR (the compiler loop needs its link).

## Self-host 8 — ARM64 encoders run natively (2026-06-11)

- The codegen's instruction encoders are pure bit-math, which the standalone does
  natively. Ported add/sub/mul/orr/movz (`$base | rm<<16 | rn<<5 | rd`, locals for
  operands) and cross-checked against gforth's `asm.fs`: byte-identical machine
  words. So the standalone can encode ARM64 itself — the heart of dot 8. With the
  native checker (6/7), the stencil JIT as in-process allocator (9), and native
  encoders (8), every algorithmic piece of habu is proven runnable on the standalone.
- Watch hand-entered hex→decimal opcode constants: `0x9B007C00` is `2600500224`,
  not the `2600988672` I first typed — a wrong base silently produces wrong (but
  plausible) machine words. Always cross-check the encoder against the oracle.

## Self-host 8 — full codegen pipeline runs natively (2026-06-11)

- The whole codegen pipeline now runs natively on the standalone: an ICode record
  buffer (4-cell records in a CREATE'd array), a peephole optimizer (kills a
  self-move `MOV x5,x5` by marking the record DEAD), and ARM64 encoding — output
  byte-identical to habu's `asm.fs`. `[MOV x5,x5; ADD x1,x2,x3; MOV x7,x8]` →
  optimize → encode yields exactly the ADD and the live MOV. ICode + asm + a
  peephole, self-hosted — dot 8's substance. The remaining mechanical work is
  porting the *rest* of the encoders/rules (same patterns) and the VS register
  allocator.
- More wrong-opcode-constant bugs: `ENC-MOV` base must be `0xAA0003E0` (Rn=xzr=31),
  not `0xAA0001E0` (Rn=15); and `ICREC` needs the `*` (`4 cells * IC +`) or every
  record aliases offset 32. Always cross-check encoders against the gforth oracle.

## Self-host 10 — foundation laid; true self-rebuild is the remaining frontier (2026-06-11)

- habu's Mach-O EMISSION is byte-deterministic: the same source builds to identical
  bytes every time (verified by `cmp`; the signed-binary diff is only codesign's
  non-deterministic signature, external to habu). Reproducible build = the fixpoint
  prerequisite.
- Status of dot 10 (stage2==stage3, drop gforth): every ALGORITHMIC component of
  habu is now proven to run natively on the standalone — the checker (unify/resolve/
  occurs/compose + checked compilation), the codegen (ICode buffer + peephole +
  ARM64 encoders, byte-identical to asm.fs), and the in-process JIT. What's NOT
  done is the literal self-rebuild: the standalone emitting its OWN complete binary.
  That needs `forth.fs` (the ~700-line build compiler) + macho.fs re-expressed in
  standalone-Forth (a from-scratch reimplementation in the constrained dialect),
  not an incremental port. The components are proven; assembling them into a
  self-emitting whole is the genuine remaining engineering project.

## Self-host 10 — the standalone emits runnable native executables (2026-06-11)

- BREAKTHROUGH toward "drop gforth": the standalone now produces a runnable native
  Mach-O executable ITSELF. Added file-I/O syscalls (open/write/close) and ported
  macho.fs to standalone source: it builds the header + 6 load commands, encodes
  the program (exit(42): movz x0,#42; movz x16,#1; svc), and writes the file. After
  the same external ad-hoc `codesign` habu already shells out to, the OS runs it and
  it exits 42. NO gforth in the emission path — the standalone is a self-contained
  native code emitter (checker + codegen + ICode + encoders + Mach-O + file I/O).
- What this does NOT yet do: emit the WHOLE standalone (the literal stage2==stage3
  self-rebuild). That needs forth.fs's complete emit logic (every emit-* routine +
  all mnemonics + the dict/source embedding) re-expressed in standalone-Forth — a
  large port. But the standalone now has every PRIMITIVE capability it requires;
  the remaining work is assembling them into the full self-emit, not new machinery.

## Self-host 10 — THE FIXPOINT: the standalone rebuilds itself (2026-06-11)

- ACHIEVED: stage2 == stage3, byte-identical. gforth builds the standalone
  (stage2); stage2, run, re-emits its OWN binary (stage3) byte-for-byte; a third
  iteration (stage4) matches too — a stable fixpoint. gforth is dropped from the
  rebuild loop (only macOS's `codesign` stays, which habu already shells out to).
- How: `REBUILD` (in the embedded source, selfhost/rebuild.f) deterministically
  rebuilds the Mach-O (header + 6 load commands — a deterministic linker) and
  copies its compiled code from its OWN loaded image. A `rbase` primitive returns
  the saved __TEXT load base (x20 at startup, stored before x20 is repurposed as
  the data base); CODELEN is read from the live `__text` section-size field at
  `[load_base+216]`; the code is copied `[rbase, +CODELEN)`. Header rebuilt fresh
  (so it's the UNSIGNED form, matching gforth's emission — codesign only adds an LC
  later), code reused — a deterministic re-link, byte-identical.
- Two bugs that hid the win: (1) `emit-source` used `ADDI` for the source length
  (imm12 max 4095) → builds with a >4 KB embedded source threw "immediate out of
  range"; use `LIT64`. (2) the one-`{:`-block-per-word limit again: REBUILD's
  `{: rb :} … {: clen :} … {: fd :}` wiped earlier locals → it copied zeros. Single
  block (`{: rb clen :}`) + keep `fd` on the stack. Cross-checking the emitted
  Mach-O against habu's `asm.fs` byte-for-byte (after fixing the `$80000400`
  section-flag constant) was what made the fixpoint reachable.

## Self-host hardening — sound checker + drift guard + gate (2026-06-11)

- Checker soundness: the native checked-compile hook used to silently PASS bodies
  with control flow (it ignored IF/loops and composed the linear remainder). Fixed:
  any token that isn't a known prim (control flow, literal, unknown word) sets an
  UNCHECKABLE flag; CHECK returns 1 (uncheckable, published but NOT certified)
  distinct from -1 (well-typed) and 0 (type error). It no longer claims to have
  checked what it can't. The checker lives in src/core/checker.f now (not inline).
- Drift guard: the standalone's hand-transcribed encoders + Mach-O builder are
  cross-checked byte-identical to habu's asm.fs/macho.fs (both emit exit(42), cmp).
  test/selfhost-all.fs is the gate: sound checker + drift + the self-rebuild fixpoint.

## Standalone — comments + DO/LOOP/I (2026-06-11)

- `\` (to end-of-line) and `( )` comments now skipped in the main loop, both modes.
- DO/?DO/LOOP/I: index/limit can't use x27/x28 (the compiler's NDICT/CP), so they
  live in a data-region frame stack ([x20+LOOP-STK-OFF], depth at [x20+LOOPSP-CELL]).
  j-do pushes a frame + records loop-top; j-loop bumps the index, compares, b.lt back,
  pops the frame on exit; j-i pushes the index. Nesting works (frame stack). Encodings
  computed offline and emitted as fixed words — verified with sum/factorial/nested.

## vsjit — loop-resident registers (2026-06-12)

- BEGIN loops now keep their stack values in registers across iterations:
  j-begin forces every VS entry into a register (Lvsnap: con->movz emitted once,
  BEFORE the loop top) and pushes (k, packed-regs) on a snapshot stack
  ($358/$360); the back edges (until/again/repeat) reconcile (Lvrecon): if the
  VS is exactly the snapshot, NOTHING is emitted — zero-cost back edge; else
  spill-all + k pops into the snapshot registers. The branch tests x17 (never
  a VS register), so a reconcile reload can't clobber the flag. 30M-iteration
  `0 begin 1+ dup 30000000 >= until` went 0.25s -> 0.016s (body: 8 instrs,
  zero memory traffic). Anything that spills mid-loop (calls, if/then, do)
  still works — the back edge reloads the snapshot from memory.
- WHILE keeps the spill-everything exit convention (cfb-entry): the exit path
  and body2 both see memory state; repeat reloads the snapshot for the back
  edge and then RESETS the compile-time VS to empty, because the live exit
  path arrives from WHILE's spilled state, not from the back edge's.
- Cache-flush stride bug (SIGILL, nondeterministic): a DC CVAU/IC IVAU loop
  stepping 64 from an UNALIGNED start skips the final cache line whenever
  start+len crosses fewer strides than lines. Lflush only worked before
  because region+DICT-SIZE was line-aligned; flushing [word-start, CP) needs
  `start & ~63` first. Symptom: SIGILL at pc near CP, executing stale zeros;
  appears/disappears with unrelated code-size changes.
- Latent-overlap lesson: the dict-full guard said 1300 but CFSTK-OFF/DREC =
  $F000/48 = 1280 — slots 1280..1299 overlay the CF stack. Survived for
  months because the only writer (`:`'s CFSP reset) repaired cell 0 and
  nothing READ the clobbered slots; became a SEGV the day Lflush started
  reading slot.addr (DC CVAU on 0). A guard limit must come from the layout
  constants, not a round number.
- Image growth (engine + embedded source) needed MPAGE $20000 -> $40000:
  size-check via PASS1 (computes length without writing) BEFORE PASS2 touches
  SCODE — the old post-write guard segfaulted before it could fire. Growing
  the image breaks tools/build.sh by design (the OLD embedded builder refuses
  the bigger emit, exit 73) — capacity changes go through tools/bootstrap.sh.

## clobber-lint — a register lint is useless without contract modeling (2026-06-12)

- The raw lint (writes-before-BL poisoned by callee clobbers, read-after flags)
  produced 72 findings, ALL false. Making it useful needed four models:
  RETURNS (Lcfpop->x9, Lkwcmp->x0, Lcemit->CP... reads after the BL are the
  contract, and the value can still be poisoned by a LATER call), PRESERVES
  (frame-saved args like Lvpushc's x11; regs written only on no-return exit
  paths like Lbcap's x0-x2/x16), routine regions ending at RET-before-next-
  `Lx @ LBL,` (not end-of-word — packed helpers inflated each other's sets),
  and callee = ops[-2] of `Lx @ BL,` (ops[0] misattributed callees after
  builder-word token junk). x30/x31/x28 are never tracked: LR is frame-saved
  around BLs, 31 encodes SP and XZR, and every emitter advances CP by design.
- Triage payoff beyond the lint: it caught the stale "clobbers x9..x15"
  comment on Lhex (it also clobbers x0-x2/x16 — its write syscall). Clobber
  comments must list syscall registers, that's exactly what a caller needs.

## Process: anything over ~15s is a hang (2026-06-12)

- Engine builds run in well under a second (build.sh 0.36s, bootstrap.sh ~2s);
  a single gforth test file is seconds. Cap commands at 10-20s — a generous
  timeout just delays discovering the code is hanging or the invocation is
  wrong (the "slow stage2 build" was a 0.06s path error).
