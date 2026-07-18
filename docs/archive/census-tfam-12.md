# TFAM-12 Stack-Primitive / Lowering Census

Dot: `habu-tfam-12-layout-057181a9` — "Make all checked stack operations
layout-aware" (PLAN.md item 12, lines 834-946).

Every site a layout bundle (multi-physical-cell logical value) can be split by a
one-cell operation. Grouped by the six task categories. All paths absolute-file
+ line. "role" = what the site does today and why item 12 must touch it.

---

## Category 1 — Checker primitive-effect axioms (`src/core/checker.f`)

### 1a. The axiom table proper (`PRIM: … PRIM;`), single source of truth

The prim table is built by `PTABLE-START` (3190) … `PTABLE-END` (3406) into the
`PES` array (`3061`), looked up by `PRIM-FIRST-SYM` (`3113`) / cached by
`PRIM-FIRST-IDX` (`3105`). This is the ONE place primitive stack effects live.

| word | file:line | effect | role |
|---|---|---|---|
| `dup` | checker.f:3192 | `PE-A → PE-A PE-A` | copy TOS; row-poly `a` — copies exactly one logical cell today |
| `drop` | checker.f:3193 | `PE-A →` | pop one cell |
| `swap` | checker.f:3194 | `a b → b a` | 2 one-cell |
| `over` | checker.f:3195 | `a b → a b a` | copy 2nd |
| `nip` | checker.f:3196 | `a b → b` | drop 2nd |
| `tuck` | checker.f:3197 | `a b → b a b` | copy TOS under 2nd |
| `rot` | checker.f:3198 | `a b c → b c a` | 3 one-cell |
| `-rot` | checker.f:3199 | `a b c → c a b` | 3 one-cell |
| `2dup` | checker.f:3200 | `a b → a b a b` | copy pair |
| `2drop` | checker.f:3201 | `a b →` | pop pair |
| `2swap` | checker.f:3202-3203 | `a b c d → c d a b` | 4 one-cell |
| `2over` | checker.f:3204-3205 | `a b c d → a b c d a b` | copy 2nd pair |
| **`?dup`** | **ABSENT** | **no axiom anywhere** | **FINDING: `?dup` has NO checker primitive effect (rg `?dup` over `src/core/` = 0 hits). It exists only as an emitter prim (BQDUP). Item 12's "`?dup` must reject layout ADTs / declare truthiness policy" requires ADDING a `?dup` axiom, not modifying one.** |
| `constant` | checker.f:3404 | `PE-A → ` out (`PE-A PE-OUT`) | runtime constant word pushes one cell (distinct from the definer, §5) |
| `.` `.s` `depth` | checker.f:3269-3271 | `.`=`n→`, `.s`=`→`, `depth`=`→n` | introspection (see §5) |
| float ops `f+`..`f.` | checker.f:3372-3386 | `PE-R`… | scalar; unchanged but in the axiom census the plan says to update |

### 1b. `PE-*` representation (how `a` / `PE-N` / `PE-PTR-*` are built)

| element | file:line | role |
|---|---|---|
| `PE-A/PE-B/PE-C/PE-D` | checker.f:3165-3168 | `$61..$64 VAR-OF` → row-poly type vars `a b c d` (the polymorphic side) |
| `PE-N/PE-F/PE-R/PE-U8` | checker.f:3169-3172 | `MK-CON` concrete cons (n / bool / float / u8) |
| `PE-PTR` / `PE-PTR-A…U8/PTR-PTR-B` | checker.f:3173-3179 | `MK-PTR` wrappers |
| `PE-IN` / `PE-OUT` | checker.f:3159-3163 | push a type onto the effect's DIN / DOUT row |
| `PRIM:` / `PE-OPEN` | checker.f:3146-3147, 3137 | open a prim; `NEW` fresh var pool |
| `PRIM;` / `PE-CLOSE` | checker.f:3156, 3149-3154 | `E-BUILD-EFFECT` → arena effect record, `PRIM-ADD` |
| `PRIM-ADD` / `PES` / `PE-REC` | checker.f:3083-3088, 3061, 3055 | the table storage |
| `CHECKER-STEP` | checker.f:1030-1038 | applies one effect: `UNIFY-IN` DCUR vs din, sets DCUR=dout; the per-token workhorse. Layout widths must be threaded here. |
| `STEP-TYPE-IN/OUT`, `STEP-N-IN`, `STEP-BOOL-IN`, `STEP-NN-IN`, `STEP-FETCH/STORE` | checker.f:2059-2101 | structured single-type steps used by control/loop consumers |
| `U-TYPE` var-bind | checker.f:835-857 (`over ISVAR` at **851**) | where a polymorphic var side binds during unification — hidden fields must NOT bind to `a` here |
| `UNIFY` / `UNIFY-IN/EXACT/COERCE` | checker.f:859-878 | worklist unifier |
| `FIELD-PAIR?` / `FIELD-COERCE?` | checker.f:847-848 | existing value-record field coercion path the plan says must reject hidden fields |

### 1c. Return-stack transfers + higher-order (special-cased, NOT in PRIM table)

Dispatched by `RS-TOK?` (checker.f:**1165-1176**), tried in `DO-TOK1` before the
normal prim lookup. Each builds rows directly with `MK-PUSH`/`MK-VAR`.

| word | file:line | role |
|---|---|---|
| `>r` → `RS->R` | checker.f:1043-1048 | 1 cell data→return |
| `r>` → `RSR>` | checker.f:1050-1055 | 1 cell return→data |
| `r@` → `RSR@` | checker.f:1057-1062 | peek 1 cell |
| `2>r` → `RS2->R` | checker.f:1064-1070 | 2 cells data→return |
| `2r>` → `RS2R>` | checker.f:1072-1078 | 2 cells return→data |
| `2r@` → `RS2R@` | checker.f:1080-1085 | peek 2 cells |
| `execute` → `RSEXEC` | checker.f:1103-1133 | applies a quot effect / binds a var xt — higher-order; must not split bundles |
| `catch` → `RSCATCH` | checker.f:1137-1161 | stack-preserving quot + throw edge |

### 1d. Locals bind / ref

| element | file:line | role |
|---|---|---|
| `LOC-ADD` | checker.f:4155-4170 | register a local name, `FRESH MK-VAR` per slot |
| `LOC-BIND` | checker.f:4172-4178 | at `:}` — one `MK-PUSH` per local, then `CHECKER-STEP` pops them (one cell each) |
| `LOC-TOK` / `LOC-BEGIN` | checker.f:4180-4190 | `{:` mode entry |
| `LOC-REF?` | checker.f:4192-4203 | push a local's var back (one cell) |

---

## Category 2 — Native lowering (`src/habu/jit.f`, `habu2.f`, `habu1.f`, `regalloc.f`, `rt.f`)

### 2a. Optimized JIT (register-resident virtual stack)

The VS models each entry as ONE cell: a tag byte (`VTAG-OFF`) + value cell
(`VVAL-OFF`), `VSP-CELL` = logical depth. The reg pool is one byte per slot
(`regalloc.f`). All "min depth" guards count cells, not bundles.

| dispatcher | file:line | keywords wired (habu2.f) | role |
|---|---|---|---|
| `VSHUF-ENTRY` | jit.f:773-782 | `dup drop swap over nip` via `XDUP/XDROP/XSWAP/XOVER/XNIP` (jit.f:785-798), dispatch **habu2.f:3204-3208** | relabels VS entries / register moves; 1-cell min-depth guard (`6 min CMPI`) |
| `VOP-ENTRY` | jit.f:335-349 | `* and or xor`, dispatch habu2.f:3197-3200 | binary op fold/emit on one-cell operands |
| `VOPI-ENTRY` | jit.f:354-372 | `+ -`, dispatch habu2.f:3195-3196 | VOP + small immediate |
| `VCMP-ENTRY` | jit.f:406-421 | `= <> < > <= >=`, dispatch habu2.f:3212-3217 | one-cell compare → flag |
| `VUN-ENTRY` | jit.f:805-820 | `1+ 1- 0= 0< negate invert`, dispatch habu2.f:3221-3226 | one-cell unary |
| `FOP-ENTRY` | jit.f:593-603 | `f+ f- f* f/`, dispatch habu2.f:3230-3233 | float binop |
| VS relabel prims | jit.f:427-522 | `LVDROP` 431, `LVSWAPX` 456, `LVNIPX` 471, `LVCOPY` (dup/over) | the actual "no code, just relabel" bundle-splitters |
| `LVSPILL` / `LVPUSHR` / `LVSNAP` / `LVRECON` | jit.f:23, 317, 661, 759 | spill-all + loop-carried snapshot/reconcile (one cell/slot) |
| dispatch tables | habu2.f:3194-3241 | `EM-COMPILE-ARITH/SHUFFLE/COMPARE/UNARY/FLOAT-OPS` + `EM-COMPILE-OPS` 3236 | KEEP?-gated wiring |
| **regalloc** | regalloc.f:1-91 (whole file) | `VRPACK` pool, `VRTAB`/`VRITAB`, `VRALL`, `VRFREE-CELL` | THE allocator; VS entry = 1 reg/cell. Bundle-aware VS needs width per entry here. |

### 2b. Fallback spilled call

| site | file:line | role |
|---|---|---|
| `EM-COMPILE-CALL` | habu2.f:3244-3257 | `LVSPILL` then `BLR` to the word — all non-JIT stack ops (`rot -rot tuck 2dup 2drop 2swap 2over ?dup 2>r 2r> 2r@`) reach the raw prim through here |

### 2c. Raw primitive bodies (one physical cell each; `G-POP`/`G-PUSH` = ±8 bytes, `rt.f:5-9`)

| word | file:line | role |
|---|---|---|
| `BDUP` `BDROP` `BSWAP` | habu1.f:115,118,121 | 1-cell copy/pop/swap |
| `BNIP` `BOVER` `BTUCK` `BROT` `BMROT` | habu1.f:1151,1154,1157,1160,1163 | raw cell shuffles |
| `B2DUP` `B2DROP` `B2SWAP` `B2OVER` | habu1.f:1166,1169,1172,1175 | pair shuffles (fixed 2-cell) |
| `BQDUP` | habu1.f:1178-1180 | **tests raw TOS cell for truth** (`A done CBZ,`) — the exact unsound `?dup` niche the plan flags (tag 0 is valid) |
| registration `EMIT-STACK-PRIMS` | habu1.f:1695-1700 | registers all above + `?dup` + `2>r/2r>/2r@` |

### 2d. Return-stack native

| site | file:line | role |
|---|---|---|
| `J-TOR` / `J-RPOP` / `J-RFROM` / `J-RFETCH` | habu2.f:1261-1280 | JIT `>r r> r@`, one cell (`sub x19,#8; ldr x9`), dispatch habu2.f:3156-3158 |
| `RSTK-PUSH` / `RSTK-POP` | habu1.f:1226-1239 | one-cell return-stack move |
| `B2TOR` / `B2RFROM` / `B2RFETCH` | habu1.f:1241-1248 | raw `2>r 2r> 2r@` (registered 1700). **Note: no JIT special-case for these — only the raw prim.** |

### 2e. Locals native

| site | file:line | role |
|---|---|---|
| `C-LBRACE-CARVE-FRAME` / `C-LBRACE` | habu2.f:2117-2137 | pop one cell per local (`sub x19,#8; ldr; str [sp,#off]`) |
| `C-LOCAL-REF` | habu2.f:2340-2360 | push one cell per local ref (`LVPUSHR` or spilled push) |

---

## Category 3 — Gforth bootstrap mirror (`bootstrap/cg/forth.fs`, `bootstrap/cg/jit.fs`)

Byte-for-byte mirror of the native codegen. Same one-cell model.

### 3a. Raw prim bodies (forth.fs)

| word | file:line |
|---|---|
| `BNIP` `BOVER` `BTUCK` `BROT` `BMROT` | forth.fs:404,406,408,410,412 |
| `B2DUP` `B2DROP` `B2SWAP` `B2OVER` | forth.fs:414,416,418,420-430 |
| `BQDUP` (raw TOS truth) | forth.fs:432 |
| `EMIT-STACK-PRIMS` registration | forth.fs:641-646 |

### 3b. **2>r / 2r> / 2r@ parity gap (plan-flagged) — CONFIRMED MISSING**

`EMIT-STACK-PRIMS` (forth.fs:641-646) registers `dup drop swap nip over tuck rot
-rot 2dup 2drop 2swap 2over ?dup` — but **no `2>r` / `2r>` / `2r@`**. There is
NO `B2TOR`/`B2RFROM`/`B2RFETCH` in forth.fs and NO JIT case in jit.fs
(rg over both files: only `>r r> r@` via `J-TOR/J-RFROM/J-RFETCH` forth.fs:1821-1839,
KWDATA forth.fs:1641, dispatch forth.fs:3154-3156; the one `2>r`/`2r>` hit at
forth.fs:3485 is the Gforth HOST meta-compiler, not a habu-target prim).
Native HAS them (raw prims, habu1.f:1700). → Gforth scalar inventory must add
`2>r/2r>/2r@` or fail-close before layout tests, per item 12 acceptance.

### 3c. Gforth JIT (jit.fs)

| dispatcher | file:line | role |
|---|---|---|
| `VOP-ENTRY` / `VOPI-ENTRY` | jit.fs:339-353 / 358-376 | binary op |
| `VCMP-ENTRY` | jit.fs:399-414 | compare |
| `FOP-ENTRY` | jit.fs:588-596 | float |
| `VSHUF-ENTRY` + `XDUP/XOVER/XDROP/XSWAP/XNIP` | jit.fs:767-785 | shuffles (1-cell min-depth) |
| `VUN-ENTRY` | jit.fs:792 | unary |
| `EMIT-SHUFKW` (kw data dup/drop/swap/over/nip) | jit.fs:131-134 | |

### 3d. Gforth compiler dispatch + control + locals + return-stack + interpret

| site | file:line | role |
|---|---|---|
| `EMIT-COMPILE-SHUFFLE-OPS` | forth.fs:3212-3217 | dup/drop/swap/over/nip → VSHUF |
| `EMIT-COMPILE-ARITH/COMPARE/UNARY/FLOAT-OPS` | forth.fs:3204-3239 | VOP/VCMP/VUN/FOP wiring |
| `EMIT-COMPILE-CALL` (spilled fallback) | forth.fs:3248-3261 | |
| `EMIT-COMPILE-LOCAL` | forth.fs:3173-3192 | local ref push (1 cell) |
| `C-LBRACE-CARVE-FRAME` | forth.fs:2287-2303 | local bind pop (1 cell) |
| `J-TOR/J-RPOP/J-RFROM/J-RFETCH` | forth.fs:1821-1840 | 1-cell return-stack; dispatch 3154-3156 |
| control lowering `J-IF/J-WHILE/J-UNTIL/J-OF/J-CASE/J-DO…` | forth.fs (dispatch `EMIT-COMPILE-CONTROL-KEYWORDS` ~3118-3132) | mirror of native §4 |

---

## Category 4 — Control / loop scalar consumers

### 4a. Checker (`src/core/checker.f`), `CF-*`, dispatch `CF-TOK?` (4560-4578)

| word | file:line | consumes |
|---|---|---|
| `if` → `CF-IF` | checker.f:4366 | `STEP-BOOL-IN` (1 bool cell) |
| `while` → `CF-WHILE` | checker.f:4463 | `STEP-BOOL-IN` |
| `until` → `CF-UNTIL` | checker.f:4452 | `STEP-BOOL-IN` |
| `of` → `CF-OF` | checker.f:4384-4391 | `STEP-N-IN` ×2 (selector + case value) |
| `case` / `endcase` → `CF-CASE`/`CF-ENDCASE` | checker.f:4368,4403 | `STEP-N-IN` |
| `do` / `?do` → `CF-DO` | checker.f:4476 | `STEP-NN-IN` (limit+start) |
| `+loop` → `CF-+LOOP` | checker.f:4489 | `STEP-N-IN` (increment) |
| helpers | checker.f:2076,2085,2088 | `STEP-N-IN` / `STEP-BOOL-IN` / `STEP-NN-IN` |

### 4b. Native (`habu2.f`)

| site | file:line | pops |
|---|---|---|
| `J-IF` | habu2.f:949 | `C-POPFLAG` (habu2.f:940) — 1 cell |
| `J-WHILE` | habu2.f:1001 | `C-POPFLAG` |
| `J-UNTIL` | habu2.f:999 | pop flag → x17 |
| `J-OF` | habu2.f:964-971 | `C-POP-X16` (habu2.f:942) — 1 cell |
| `J-CASE`/`J-ENDCASE` | habu2.f:961,976 | |
| `J-DO`/`J-?DO` | habu2.f:1036,1039 | `J-FRAME` pops limit/start |
| `J-LOOP`/`J-+LOOP` | habu2.f:1058,… | |
| dispatch | habu2.f:3118-3164 | `EM-COMPILE-CONTROL/LOOP-KEYWORDS` |

### 4c. Gforth — mirror of 4b (`forth.fs` `J-*`, dispatch `EMIT-COMPILE-CONTROL-KEYWORDS` ~forth.fs:3118-3132, `EMIT-COMPILE-LOOP-KEYWORDS` 3150-3164).

---

## Category 5 — Interpret / top-level stack handling + frame state

### 5a. Checker

| site | file:line | role |
|---|---|---|
| `constant` definer → `STEP-N-IN` | checker.f:4048-4052 (`DEFINER-TOK`) | consumes the value at def time (distinct from runtime `constant` prim, §1a/3404) |
| `depth` / `.s` / `.` | checker.f:3269-3271 | introspection axioms |
| `run-in-stack` | checker.f:3266 | `PE-N PE-PTR-U8 PE-N →` fresh-stack frame |
| `execute` / `catch` | checker.f:1103 / 1137 | frame-crossing (also §1c) |

### 5b. Native (`habu1.f`, `habu2.f`)

| site | file:line | role |
|---|---|---|
| `EM-INTERPRET-NUMBER` | habu2.f:3016-3018 | pushes literal `G-PUSH` on the live data stack (raw cell) |
| `EM-INTERPRET-FIND` | habu2.f:3021-3024 | `BLR` — executes word interactively on raw stack (where a public constructor would leave a bundle) |
| `C-CONSTANT` | habu2.f:1615-1632 | pops value `15 G-POP` (habu2.f:1622), bakes literal |
| `C-CREATE`/`C-VARIABLE`/`C-DEFHOOK` | habu2.f:1607,1612,1418 | defining-word hook (pushes body ptr/len, `G-PUSH`/`G-POP`) |
| `B.S` / `BDEPTH` | habu1.f:1055-1064 / 1066-1070 | **walk / count RAW physical cells** (`(S0-XDS)/8`) — exposes hidden-field cell count today |
| `B-EVAL` | habu1.f:1009-1027 | saves/restores INP/INE/CP/NDICT/**XDS**/DP — no logical metadata |
| `BRUNSTACK` (run-in-stack) | habu1.f:1570-1575 | swaps XDS to a fresh buffer; raw only |
| `BCATCH` / `BTHROW` | habu1.f:1577-1593 / 1604-1632 | save/restore XDS (x19) only |
| `EM-EVAL-THROW-RECOVER` | habu2.f:3296-3324 (restores XDS/DP 3307-3313) | eval-frame rollback — raw XDS, no bundle tags |
| `EM-EVAL-UNDEF-ROLLBACK` | habu2.f:3275-3286 | ditto |

### 5c. Gforth mirror

| site | file:line |
|---|---|
| `B.S` / `BDEPTH` | forth.fs:306-322 |
| `B-EVAL` | forth.fs:266-283 |
| `BCATCH` / `BTHROW` | forth.fs:550-578 |
| `C-CONSTANT` (`15 G-POP` 2194) | forth.fs:2189-2200 |
| `EMIT-EVAL-CLEAN-EXIT` / `EMIT-EVAL-UNDEF-ROLLBACK` / `EMIT-REPL-RECOVER` | forth.fs:3314-3320 / 3280-3290 / 3292-3301 |

All frame words save/restore the raw data-stack pointer (XDS/x19) and DP only —
there is NO parallel logical-metadata (width/tag) stack to save. Item 12's
"nested evaluate/catch/run-in-stack must save+restore logical metadata OR reject
bundles before frame entry" has no existing storage to extend.

---

## Category 6 — Compiler data path (checker ↔ emitter ordering)

**The body is emitted to machine code per-token BEFORE the checker runs; the
checker runs ONCE, at `;`, on the recorded token-text buffer.**

| stage | file:line | fact |
|---|---|---|
| per-token emit dispatch | habu2.f:`EM-COMPILE` 3395, `EM-COMPILE-OPS` 3236, `EM-COMPILE-CALL` 3244 | each body token is compiled to ARM immediately as it is read (locals→keywords→literal→ops→call). VSHUF/VOP width decisions are made HERE. |
| body text recorded | `BODYBUF-OFF` / `BODYLEN-CELL` (habu2.f:3083-3084, 525) | token text accumulated for the post-hoc check |
| checker HOOK at `;` | habu2.f:`EM-COMPILE-SEMI` 3106-3115 → `EM-COMPILE-PUBLISH` 3095 → `EM-COMPILE-PUBLISH-HOOKED` 3080-3086 | pushes BODYBUF/BODYLEN, `9 BLR` the HOOK (= checker `CHECK`), reads verdict; **on reject rolls CP back** (3088-3090). This is strictly AFTER all emission. |
| interpret defining words | `C-DEFHOOK` habu2.f:1418-1425 (create/variable/constant) | hook runs after the defining word already emitted its body |
| the HOOK = checker `CHECK` | checker.f:`CHECK` 4969-4979, `CHECK-SCAN` 4928-4958, `DO-TOK1` 4854-4879 | scans the SAME token buffer; the declared `( in -- out )` sig is parsed at scan start (4932-4949) but still inside this one post-emit pass |
| Gforth mirror | forth.fs:`EMIT-COMPILE` 3263-3270, `EMIT-COMPILE-SEMI`, `EMIT-COMPILE-PUBLISH-HOOKED` | same emit-then-check-at-`;` order |
| trial/candidate check | checker.f:`CHECK-CANDIDATE!` 5100, `CHECK-CANDIDATE-START/DONE` 5067-5099 | trial-checks recorded token text; used for multi-error/candidate flows, still post-emit, still text-based |

**Checker-before-emission does NOT exist for compiled bodies.** The emitters
choose one-cell `VSHUF`/`VOP`/`VCMP` widths purely from the runtime virtual
stack (`VSP-CELL` depth + per-slot tag byte in the regalloc pool), never from
checker logical widths. Item 12 must add a path: either (a) run the checker on
the body token stream first and hand per-token width/refinement facts to the
emitter, or (b) resolve widths inline per token before `VSHUF` emits. Option (a)
maps cleanly onto the existing BODYBUF token buffer that the HOOK already
consumes — it would need to run at colon-open / per-token instead of at `;`.
