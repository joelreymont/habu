# Census — Switchover Migration Inventory (post-TFAM ADT adoption)

Read-only scout census, 2026-07-04, branch `maki-type-families` (~head ea89bbc1).
Orchestrator correction: the scout's "items 5–16 unbuilt" framing came from
stale census §0 headers — items 5 (ordered events) and 6 (TYPEFAMILY/SUMTYPE
declaration grammar, `src/core/sumtype.f`) ARE landed; items 7–16 remain
queued. The site inventory below is unaffected (sentinel conventions predate
the ADTs). Wave dots: `habu-switchover-wave-*` (minted 2026-07-04).

The "switchover" is the post-item-8..16 migration of habu itself onto the
landed ADT types (option/result over sentinels, block enums, products). This
census is a target list, not a ready-to-run migration.

## 1. Sentinel conventions in checked public APIs

Habu's honest-error convention is **`throw` a named code** (`docs/forth.md`
§ Errors, `lib/errors.f` ranged `E-*`); out-of-band flag returns are
discouraged. The shipped stdlib pervasively uses **value+flag** and
**sentinel** returns because the checker cannot yet express
`option<T>`/`result<T,E>`. These are the switchover targets.

Category sizes (lib+tools+src/core+src/habu, defs only):
- `-- <one> bool` single-value+flag → **65** sites (option<scalar>)
- `-- <vals> bool` multi-value+flag → **25** sites (option<tuple>/result)
- `-- ... rc` return-code returns → **34** sites (result<T,errno>)
- `-1`-as-missing index/pos finders → ~15 lib sites (option<idx>)
- pure `-- bool` predicates → **770** sites — **NOT candidates** (honest
  booleans: STR=, FILE?, LEAP-YEAR?, …); the switchover must not touch these.

### 1a. Union / id-or-false / -1-index / rc-plus-value returns

**`-- id true | false` (FIND idiom).** Only in `src/core/type-family.f`
(predates its own ADT): `type-family.f:248 TFAM-FIND-IN`, `:263
TFAM-FIND-PUBLIC`, `:280 TFAM-RESOLVE`, `:370 SUMV-FIND`, `:432 PF-FIND`,
`:489 LAY-FIND`, `:696 TFAM-QUAL-RESOLVE`, `:704 TFAM-SIG-RESOLVE`; xt cell
`checker.f:369 TFAM-RESOLVE-XT`. Target: `option<id>` (or
`result<id,E-TFAM-AMBIG>` for the resolvers distinguishing ambiguity).
Difficulty: needs-MATCH. Blast radius: internal, ~1–3 callers each. These
self-host the registry the ADTs live in — highest bootstrap sensitivity,
migrate LAST (Wave E).

**`-1`-means-missing index returns (`option<idx>`).**
- `lib/string.f:81 FIND-SUB ( ptr u8 n ptr u8 n -- n )` returns -1 (`:83,:87`);
  `:92 INDEX-OF` (`:96`). Radius: FIND-SUB **27**, INDEX-OF **21** hits.
- `lib/array.f:180 A-FIND-INDEX`, `:187 A-FIND-INDEXI` (`:184,:191`). Radius 9.
- `lib/object.f:332 FIND-TAG`, `lib/hashmap.f:16 HM-PROBE`,
  `lib/map.f:152 MAP-INDEX` / `:156 MAP-PROBE`.
- `src/habu/aot-capture.f:95 ACAP-POOL-FIND` ("entry off, or -1 if absent").
- Manifest-documented: `lib/std.manifest:333 FS-TRY-STAT-MODE … or -1`,
  `:334 FS-TRY-LSTAT-MODE`.
Target: `option<idx>`. Difficulty: needs-MATCH (callers do `-1 =`/`0<` tests,
e.g. `CONTAINS? FIND-SUB 0 < 0=` `lib/string.f:89-90`). Blast radius HIGH.

**rc-plus-value pairs (`result<T,errno>`).** `rc` nominal role (`roles.f:65
>RC`), negative = error. Dense cluster = process stack:
- `lib/process.f:96 PROC-WAIT-RC` (radius **17**), `:104 PROC-RUN-RC`
  (radius **11**), `:107 PROC-RUN-IO-RC`, `:90 PROC-STATUS>RC`,
  `:86 PROC-OUTCOME>RC ( kind code -- rc )`.
- Value+rc capture words: `lib/process.f:460 RUN-CAPTURE ( … -- len len rc )`,
  `:438 PROC-CAPTURE-RC@`; mirrors in `process-env.f:281,:303`,
  `process-cwd.f:51,:64`, `process-argv.f:99,:119` — literally
  `result<(outlen,errlen),errno>`.
- Native emitter mirrors keep the sentinel at the raw boundary:
  `src/habu/habu1.f:439 BRUNRC`, `:493 BPIPE`, `:515 BDUP2`, `:547 BFCNTL`,
  `:565 BPOLL`, `:600 BKILL`, `:611 BSETPGID`, `:622 BWAITRC` — trusted
  emitters stay rc-sentinel; only checked wrappers migrate.
Target: `result<T,errno>` over `E-PROC-*` (`lib/errors.f:43-51`). Difficulty:
needs-MATCH + item-12 layout (multi-cell `len len rc`). Blast radius HIGH.

### 1b. Flag-plus-value stacks → option<x>

Exhaustive for lib/ + tools/ (production/public):

Multi-value `-- <vals> bool` (25 total):
- `lib/string.f:180 SPLIT-NEXT ( … -- ptr u8 n n bool )` → option<slice>
- `lib/object-index.f:116 LOAD ( ptr u8 n -- ptr u8 n bool )` → option<record>
- `lib/object.f:274 NEXT-LINE` → option<line> (radius 8)
- `lib/regex.f:454 RX-FIND-FROM`, `:464 RX-FIND ( … -- off len bool )` →
  option<(off,len)> (radius 8)
- `lib/process-env.f:180 PROC-ENV-DEFAULT$?` → option<str>
- `lib/float.f:51 FL-STRIP-SIGN`
- tools: `json-file.f:96,101,107,120,128` (JSONLF-* row parsers),
  `json.f:914,921 JSONL-PARSE-ROW ( -- i64 i64 i64 bool )`,
  `check-all-errors-core.f:348,613,624`, `imgdump.f:263 HEX-BODY`,
  `imagedisasm.f:84`, `gate-json-assert-core.f:86 GJA-SUGGEST-ROW`,
  `check-test-lib.f:573`.

Single-value `-- <one> bool` (65 total; biggest option<scalar> class):
- Parsers → option<n>: `lib/string.f:214 STR-PARSE-POS`, `:222 STR-PARSE-NEG`,
  `:230 STR>NUMBER?` (radius **16**); `lib/date.f:125 DATE-N`, `:134
  PARSE-YMD`; `tools/date.f:126,135`; `tools/imgdump.f:268,280,292`,
  `tools/imagedisasm.f:70,96`, `tools/trusted-inventory.f:689 PARSE-COUNT`,
  `tools/gate-json-assert-core.f:124 GJA-U?`,
  `tools/stdlib-manifest-test.f:786`.
- Floats → option<r>: `lib/float.f:41 FL-DIGITS>F`, `:63 FL-SIG`,
  `:87 STR>FLOAT`.
- Lookups → option<val>: `lib/map.f:206 MAP-GET ( … -- n bool )` (radius
  **7**), `lib/ptx/ir.f:112 PTXIR-FIND-RAW`, `:118 PTXIR-FIND`,
  `lib/regex.f:144 RX-META-TOKEN`, `:425 RX-PREFIX-LEN`.
- Path search → option<len>: `lib/process-env.f:347 PROC-TRY-PATH-SEG`,
  `:353 FIND-EXECUTABLE-IN-PATH`, `:370 FIND-EXECUTABLE`.

Difficulty: mostly mechanical→needs-MATCH; needs item-12 layout-aware stack
ops for multi-cell payload+tag bundles; construction item-8, elimination
item-9. Per-word radius low-moderate (7–27); aggregate ~90 sites = the
switchover's bulk.

Boundary note: several `-- … bool` words are honest predicates carrying no
payload (e.g. `lib/sort.f:32 HS-STEP`). Only words where the pre-bool values
are conditionally valid on the flag are candidates; the lists above are
hand-filtered to those.

### 1c. Throw-as-signal (result union is the honest type)

Most `catch` sites are legitimate recovery boundaries (top-level runners
`stage2.f:65`, `build.f:64`, `maker.f:51`, `verify-source.f:488,503`;
unit-test catch) — those stay `catch`. Genuine throw-as-signal:
- `src/core/type-family.f:708-711 TFAM-SIG-RESOLVE`: `['] TFAM-RESOLVE catch`
  branching on `E-TFAM-AMBIG` — three-way outcome (found/ambiguous/absent)
  encoded as throw-code + flag. Honest type: option/result union.
  Difficulty: needs-MATCH; Wave E (self-hosting).
- `src/core/sumtype.f:67-68`, `src/core/type-schema.f:164`: catch+`rc 0=`
  boolean conversion — result-shaped.
- `lib/process.f:86 PROC-OUTCOME>RC` folds a (kind,code) sum into a single rc
  sentinel (`128 code +` for signals) — erases a sum type; honest type is the
  outcome sum (§2/§3); callers re-deriving kind from rc are adjacent.

## 2. Legacy numeric enums (item-14 inventory)

`ENUM+`/`ENUM4+` (`src/core/enums.f:6,9`): inventoried in
`docs/census-tfam-14.md` §1 — zero production consumers, only
`test/gate-dictionary-lib.f:888-916 GD-ENUMS` + docs. Stride-4/non-zero-start
families have no block-ENUM analog (block tags are 0,1,2…) — those stay
`ENUM+` (census-14 §3c).

Hand-rolled `N constant TAG` clusters (NEW inventory, the real item-14
migration surface, dispatched by `=`/`case`):

| Cluster | Site | Members | Persisted? | Dispatch |
|---|---|---|---|---|
| Process outcome kind | `lib/process.f:23-25` `PROC-OUTCOME-EXIT/SIGNAL/TIMEOUT` (0/1/2) | 3 | in-process (folded to rc) | `PROC-OUTCOME>RC :87` `=`; `PROC-STATUS>OUTCOME :78` returns (kind code) |
| Map slot state | `lib/map.f:15-17` `MAP-EMPTY 0 / MAP-DELETED -1 / MAP-OCCUPIED 1` | 3 | in-process | `MAP-EMPTY? :39` etc. via `=` |
| FFI arg kind | `lib/ffi.f:13-16` `FDEF-N/PTR/NOM/VOID` (0-3) | 4 | in-process | per-arg marshalling |
| Checker type-term tag | `src/core/checker.f:1` `T-CON..T-PARAM` (0-7) | 8 | **persisted** (AOT image type graph) | core unification |
| VREC node tag | `checker.f:1249` `VR-CON..VR-PARAM` (0-7) | 8 | **persisted** | `VREC-COPY`/`VREC-INST` |
| Schema node tag | `SC-CON..SC-LAYOUT`, `type-schema.f` | 7 | **persisted** | schema walk |
| Family kind / layout policy | `type-family.f:15-20` `TK-*` (5), `:24-29` `TL-*` (5) | 5+5 | **persisted** (TFAM records) | `TFAM-KIND@` etc. |
| Poll/signal/fcntl constants | `lib/process.f:9-20` | — | ABI constants, NOT enums — leave |
| json parse status | `tools/json.f:104-105` `JSON-PARSE-OK/THROW` | 2 | in-process | `=` |

Schema-compat risk: `T-*`/`VR-*`/`SC-*`/`TK-*`/`TL-*` are serialized into the
AOT snapshot (`type-family.f:594-602`, census-16 §1e). Migrating those changes
the on-image encoding — bootstrap-fixpoint hazard; they are the checker's
self-representation and migrate LAST or stay raw as an explicit self-hosting
boundary. In-process clusters (process outcome, map state, FFI kind, json
status) are the safe item-14 targets. Process outcome is a genuine sum
(`exited<n> | signaled<n> | timeout`) — see §5 Wave C.

## 3. Value-record sites (item-15) + PTX IR

`VALUE-RECORD … END-VALUE-RECORD`: authoritative inventory in
`docs/census-tfam-15.md` §1i. Confirmed, plus post-census additions:
- Production (only one): `lib/ptx/ir.f:18 ptxir-node` (5-cell); physical store
  is the separate `PTXIR-REC`/`PTXIR-NODES` array (`:20-28`) — migration types
  the on-stack bundle only.
- Engine test records: `test/engine-suite.f` point/rect/box/hdl (census-15
  §1i) plus TFAM-6 fixtures `tfam6r-vr`, `scq-vr`, `tfam6r-rb`, `tfam6r-vrp`,
  `test/type-decl-suite.f tdvrec`, `cae-vr`/`cae-cv` in tool tests — gate
  fixtures, not migration targets.
- `PROC-STATUS>OUTCOME (-- kind code)` (`lib/process.f:78`) is an un-recorded
  by-value sum a `SUMTYPE outcome` models directly.

PTX IR clusters: mapped in `docs/census-tfam-16.md` §2d (`ptxir-node` +
separate array; GPU tile/acc/gridctx families are TK-CELL width-1, NOT layout
families — PRODUCT/policy never touches the kernel hot path). By-value
destructure blockers `PTXIR-NODE-DROP` (5 raw drops, `ir.f:79-80`),
`PTXIR-NODE-DUP-RAW` (`:82-88`) must become one layout dup/drop under item 12.

Open decision (census-15 §R8, PLAN.md ~:918-920): VALUE-RECORD → PRODUCT sugar
vs typed compat layer; straight migration flips ~7 fixtures.

## 4. Trusted boundaries that shrink once ADTs land

Correction to the premise: the formally-tagged `discharge-candidate` rows in
TRUSTED.md (checker self-typing `*-RC>PTR`/`USIGS-CELL-AT`, combinators) are
NOT sum/option-dischargeable — they are rc>ptr self-casts and combinator
boundaries owned by other capability dots.

The ADT-dischargeable rows are the null/unset-sentinel rows (currently
`builder-emit`/`stdlib-boundary`/`prim-axiom`), which exist only because the
checker has no option/null-literal:
- `TRUSTED.md:82 BP-NULL -- ptr u8` (null code-pointer sentinel) → option<ptr>
- `TRUSTED.md:325 TASK-NULL -- ptr a` → option<ptr> (`lib/task.f`)
- `TRUSTED.md:135-136 c-defer-find-unset / c-defer-cell` (unset sentinel xt)
  → option<xt> / typed defer (`src/habu/habu2.f`; DEFER-UNSET is option<xt>)
- `TRUSTED.md:403 NULL$ -- ptr u8 n` (absent env values) → option<str>
  (`src/os/env-base.f`; also `:401 ENV-FALSE`)
- `TRUSTED.md:89 BFR-USIG-END-PTR` reset sentinel — weaker candidate.

Classification: ~6–8 rows discharge once option/sum land; the overwhelming
majority (engine/asm builder-emit rows, raw-syscall rows, prim-axiom nominal
casts, FFI boundaries) are genuinely unexpressible and stay. The ADT campaign
itself adds zero trust rows; discharges are separate follow-on dots.

## 5. Dependency-ordered migration waves

Item 8 publishes constructors, item 9 adds MATCH, item 12 makes stack ops
layout-aware — the hard prerequisite for any multi-cell option/result.
Nothing migrates on item 8 alone.

- **Wave A (after 8+9+12)** — option<scalar>: the 65 single-value+flag
  parser/lookup words + the ~15 `-1`-index finders. ~80 sites across
  lib/string,date,float,map,process-env + tools/{imgdump,imagedisasm,date,
  json,trusted-inventory}. FIND-SUB/INDEX-OF have the widest caller edits.
- **Wave B (multi-cell proven)** — option<tuple>/result: 25 multi-value+flag
  words (SPLIT-NEXT, NEXT-LINE, RX-FIND*, LOAD, JSONL rows) + the 34-site
  process rc family → result<T,errno>; raw emitters stay sentinel at the
  boundary.
- **Wave C (needs 14+9)** — sums/enums: PROC-STATUS>OUTCOME →
  `SUMTYPE outcome (exited<n>|signaled<n>|timeout)`; MAP slot state, FFI arg
  kind, json status → block ENUMs. No persisted-value risk.
- **Wave D (item 15)** — `ptxir-node` → PRODUCT (or compat), after the
  census-15 §R8 decision + PTXIR-NODE-DROP/DUP-RAW rewrite under item 12.
- **Wave E (last, bootstrap-sensitive)** — type-family.f resolvers →
  option<id>/result; TFAM-SIG-RESOLVE throw-as-signal; persisted checker tags
  only if proven fixpoint-safe (else explicit self-hosting boundary); trust
  discharges per §4, one dot each.

Aggregate: ~150 checked-API sites across ~35 files, dominated by lib/string.f,
lib/process*.f, lib/{array,map,object*,regex,float,date}.f, and
tools/{json*,imgdump,imagedisasm,date,check-all-errors-core}.f.

## 6. Not swept (explicit gaps)

- maki/ sample-based only: overwhelmingly honest predicates; only
  `maki/eval-repair.f:36 ER-GREEN?` and score encodings are sentinel-ish; not
  read line-by-line.
- test/ trees not swept for sentinels (not public APIs), except VALUE-RECORD
  fixtures.
- bootstrap/ Gforth mirror not swept (mirrors, not defines, the surface).
- Blast-radius numbers are `rg -w` hit counts (defs+callers+tests), an upper
  bound; caller-branch semantics not verified per site.
- The 770 pure `-- bool` predicates asserted non-candidates as a class; a
  handful may hide payloads — spot-check during migration.
- Enum dispatch style determined by reading predicate words, not a full
  control-flow audit; none use case/of on these tags today.
