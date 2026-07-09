# Parametric-Type Parsing Census — dot habu-tfam-4-replace-97a246e3

Scope: PLAN.md item 4 (`PLAN.md:378-417`). Replace `PARAM-CTOR?` whitelist,
repair nested `family<...>` parsing, thread a resolved `family-id` through every
term-rebuild path, add growable arg storage, add quotation-payload schemas
(`SC-QUOT`). Everything below is `file:line` + role. Terms are 3-bit tagged:
`TAG = x and 7`, `PAY = x >> 3` (`checker.f:116,118`). Tags:
`T-CON 0 / T-VAR 1 / T-PTR 2 / T-QUOT 5 / T-ATOM 6 / T-PARAM 7` (`checker.f:1,3`).

---

## 1. Current param machinery

### Storage arrays (parallel SoA, index = PAY of the T-PARAM term)
- `checker.f:307` `PARAM-MAX-ARGS = 4` — the hard arity cap the plan removes.
- `checker.f:308-312` boot arrays `PARAMA-BOOT`(name ptr), `PARAMU-BOOT`(name len),
  `PARAMC-BOOT`(argc), `PARAMARGS-BOOT`(args, `MAXPARAM-INIT * 4` cells).
  Grown via `PARAM-ENSURE` (`checker.f:325-333`) — doubles all four; args row
  stride is hard-coded `PARAM-MAX-ARGS *` (`checker.f:331-332`).
- `checker.f:313` `PARAM-SCR` — the single global 4-cell parse scratch buffer
  (root of the nested-parse bug).
- `checker.f:314-318` counters/pointers: `PARAMN` (next free param index),
  `PARAM-SCR-N` (scratch depth), `PARAM-I` (shared loop cursor — also reused by
  `PARAM-PAIR-ARGS`, `TY-OCC?`), plus arena pointers `PARAMA/U/C/ARGS-P`,
  `PARAM-CAP`.

### T-PARAM term representation (fields)
- `checker.f:341-345` accessors: `PARAM>NAME-A/-U` (spelling ptr/len — the ONLY
  identity today), `PARAM>ARGC`.
- `checker.f:346-350` `PARAM-ARG-IDX` / `PARAM>ARG` / `PARAM-ARG-OR-DUMMY`
  (arg i as a full type term; dummy = `1 MK-CON` past argc). Arg stride uses
  `PARAM-MAX-ARGS` — 4-slot cap embedded here too.
- `checker.f:368` term encoding: `PARAMN @ 3 lshift T-PARAM or` (index in payload).
- NOTE: there is **no** `family-id` field today; identity = raw name bytes.
  The new `family-id` + retained spelling/qualifier text is a NEW column that
  must be added to this SoA and to every node copy (VN/EN slots, see §2).

### Scratch push/build
- `checker.f:351-356` `PARAM-SCR-RESET` (zeroes `PARAM-SCR-N`), `PARAM-SCR-FULL?`
  (`>= PARAM-MAX-ARGS` — the reject that caps arity at 4), `PARAM-SCR+`
  (append one arg term to scratch, bump `PARAM-SCR-N`).
- `checker.f:357-369` `MK-PARAM ( a u -- t )` — reads `PARAM-SCR-N` args out of
  `PARAM-SCR`, writes name/argc/args into the SoA, allocates the term.
  **`MK-PARAM` does NOT reset `PARAM-SCR-N`** — it leaves the scratch depth dirty
  for the caller, which is exactly what lets a nested build corrupt the parent.

### Whitelist gate
- `checker.f:1743-1758` `PARAM-CTOR?` — the 15-entry hard-coded whitelist
  (`ptr span matrix gridctx fanctx idxctx uniqidxctx coopctx rowctx tile acc
  mmctx mmacc uniform rowidx`). This is what the plan replaces with
  package-aware TFAM lookup. Also referenced by `TYPE-RESERVED?`
  (`checker.f:1772`) as a reserved-name source.
- `checker.f:1759-1776` neighbor gates: `TYPE-VAR-TOK?`, `TYPE-BAD-CHAR?`
  (rejects raw `< > ,`), `TYPE-RESERVED?` (union incl. `field`, VREC, CT, atom,
  var, PARAM-CTOR?). Bare zero-arity families must resolve to a family-id HERE,
  before nominal/builtin fallthrough (plan `PLAN.md:383-386`).

### Compare / unify (identity = folded source spelling, the aliasing bug)
- `checker.f:790-791` `PARAM-NAME-OK?` — compares two params by `CORE-STR=` on
  name bytes. This is the "folded source spelling" the plan replaces with
  `family-id` compare so two packages can share a lowercase tail without aliasing.
- `checker.f:793-800` `PARAM-PAIR-ARGS` — unify entry: argc must match, names must
  match (`PARAM-NAME-OK?`), then pairs args 0..argc-1 via `PAIR`. Uses shared
  `PARAM-I`.
- `checker.f:849-850` `U-TYPE` dispatch: both `T-PARAM` -> `PARAM-PAIR-ARGS`.
- `checker.f:826-831` `TY-OCC?` occurs-check descends param args 0..3 via
  `PARAM-ARG-OR-DUMMY` (hard-unrolled to 4 — another arity-cap site).

### The parse path for `family<...>` (checker.f:1864-1886, entry SIG-TYPE)
- `checker.f:1839-1851` `NEXT-SIG-TOK` + `SIG-DELIM-CHAR?` — tokenizer treats
  `< > ,` (60/62/44) as single-char delimiter tokens, enabling spaceless
  `span<space-global,f32,extent-n>`.
- `checker.f:1864-1886` `SIG-TYPE`:
  - line 1865: `PARAM-CTOR?` gate.
  - line 1866-1867: consume `<`, then `PARAM-SCR-RESET`.  <-- **BUG ROOT**
  - line 1868-1879 loop: `NEXT-SIG-TOK`; `>` -> `MK-PARAM` done; `DELIM?` or
    `PARAM-SCR-FULL?` -> `SGBAD-SYNTAX!` + `MK-PARAM`; else
    `RECURSE PARAM-SCR+` (parse one arg, append to shared scratch); then expect
    `,` (continue) or `>` (`MK-PARAM` done) else syntax error.
  - line 1880-1882: zero-arity bare family (no `<`) -> `PK!` push-back, falls to
    nominal/builtin lookup — this is where a bare `color` currently CANNOT resolve
    to a family id (plan requires it to).
  - line 1884-1886: separate `ptr` handling (`ptr<inner>` via `MK-PTR`), and the
    final `TOK-TYPE` fallthrough.

### Exactly how nested `foo<bar<n>,n>` breaks today
Single global `PARAM-SCR` + `PARAM-SCR-N`, reset at the START of every param
parse (`checker.f:1867`), and `MK-PARAM` never clears `PARAM-SCR-N`
(`checker.f:357-369`). So a nested param both (a) wipes args the parent already
pushed and (b) leaks its own leftover scratch into the parent.

Trace `foo<bar<n>,n>` (assume foo,bar admitted):
1. `SIG-TYPE(foo)`: `<`, `PARAM-SCR-RESET` -> `N=0`.
2. tok `bar` -> `RECURSE SIG-TYPE(bar)`:
   - `<`, `PARAM-SCR-RESET` -> `N=0` (harmless here — parent had 0 yet).
   - tok `n` -> `RECURSE` yields `con 1` -> `PARAM-SCR+` -> `SCR[0]=n, N=1`.
   - `>` -> `MK-PARAM(bar)` builds `bar<n>` reading N=1 but **leaves N=1**.
   - returns `bar<n>` term, `PARAM-SCR-N` still `1`, `SCR[0]=n`.
3. back in foo loop: `PARAM-SCR+` appends `bar<n>` at index `N=1`
   -> `SCR[0]=n(stray inner), SCR[1]=bar<n>, N=2`.
4. tok `,`; tok `n` -> `RECURSE` con -> `PARAM-SCR+` -> `SCR[2]=n, N=3`.
5. `>` -> `MK-PARAM(foo)` reads `N=3` -> **foo parsed with argc=3
   `[n, bar<n>, n]` instead of argc=2 `[bar<n>, n]`.** The inner arg leaks in.

Worse variant `foo<span<..>, bar<n>>`: at step where `bar` is the SECOND arg,
its inner `PARAM-SCR-RESET` (`checker.f:1867`) **wipes the parent's already-pushed
first arg** (`span<..>`), silently corrupting it. Reproducer sketch for the
negative regression: any signature whose param has a parametric argument that is
NOT in first position, e.g. `( span<field-tile<n>,f32,n> -- )` or the plan's
`foo<bar<n>,n>`, mis-parses argc/args. Fix = recursive-safe per-level growable
arg lists (save/restore or a stack of arg vectors) instead of one global scratch,
and remove the 4-cap in `PARAM-MAX-ARGS`/`PARAM-SCR-FULL?`/node slots.

---

## 2. Replay / copy paths that rebuild T-PARAM from stored form

All four are hard-unrolled to 4 args (D/E/F/G) and rebuild identity from the
NAME STRING only; each is a site where the resolved `family-id` must be threaded
and where growable-N replaces the 4-slot unroll. Node storage (VN/EN) has 8
slots A..H (`checker.f:1360-1377`); T-PARAM uses A/B=name off/len, C=argc,
D/E/F/G=args0-3 — so **node storage caps arity at 4 too**, not just `PARAM-SCR`.

- `checker.f:1461 VREC-COPY` / body `checker.f:1508-1517` — copies a live term
  into a persisted VREC schema node (`VR-PARAM`). Stores name via
  `VREC-COPY-STR`, argc via `VN.C!`, args 0-3 via `VN.D!/E!/F!/G!` after
  `RECURSE`. Carries: name bytes + argc + arg subtrees. MISSING: family-id column
  in the node; SC-QUOT payload rows.
- `checker.f:1567 VREC-INST` / body `checker.f:1584-1591` — instantiates a VREC
  schema node back into a live term. `PARAM-SCR-RESET` + up to 4 `PARAM-SCR+`
  + `MK-PARAM` using `VREC-I-STR` (name). This is a replay that REUSES the same
  global `PARAM-SCR` — so instantiating a nested-param VREC field hits the SAME
  corruption class as the parser. Must thread family-id into `MK-PARAM`.
- `checker.f:2713 E-COPY` / body `checker.f:2760-2768` — copies a live term into
  the persisted effect-record (`EN-PARAM`, slots via `EN.C/.D/.E/.F/.G !`). Same
  4-arg unroll, name-only identity. Threads family-id here for effect-row persist.
- `checker.f:2918 E-INST` / body `checker.f:2937-2943` — instantiates a stored
  effect node; `PARAM-SCR-RESET` + `PARAM-SCR+`*4 + `MK-PARAM` via `E-I-STR`.
  Same shared-scratch replay hazard as VREC-INST.

Additional rebuild/consumer sites the plan text implies ("any replay path that
copies/rebuilds T-PARAM"):
- `checker.f:1003-1005 LIN-TYPE-COUNT` — walks T-PARAM (only `field` inner today).
- `checker.f:826-831 TY-OCC?` — walks args 0-3 (occurs check).
- `render.f:155-162 QREND` — see §4.

---

## 3. Registered cell families (whitelist) + lib/ptx use counts

Full whitelist (`checker.f:1743-1758`), with `family<` occurrence count and file
count in `lib/ptx/`. All 15 are actually used, so registration during
core/prelude load must cover every one (preverify + runtime child identical,
plan Risk `PLAN.md:413-414`):

| family      | uses | files |
|-------------|------|-------|
| ptr         | 23   | 5     |
| span        | 122  | 17    |
| matrix      | 34   | 6     |
| gridctx     | 17   | 4     |
| fanctx      | 3    | 1     |
| idxctx      | 6    | 2     |
| uniqidxctx  | 4    | 2     |
| coopctx     | 4    | 2     |
| rowctx      | 11   | 3     |
| tile        | 98   | 13    |
| acc         | 12   | 3     |
| mmctx       | 5    | 1     |
| mmacc       | 5    | 1     |
| uniform     | 19   | 7     |
| rowidx      | 3    | 1     |

Notes:
- `ptr` is dual-purpose: whitelisted AND special-cased in `SIG-TYPE`
  (`checker.f:1884-1885`) and `LOCAL-TYPE` (`checker.f:1795-1796`) as `MK-PTR`,
  not a generic family. Registration must not double-handle it.
- `field` is a T-PARAM family too but is NOT in the whitelist — it is built
  internally by VREC (`checker.f:2009 s" field" MK-PARAM`) and gated separately
  in `TYPE-RESERVED?` (`checker.f:1770`). See §5.
- False positives when grepping `[a-z]+<`: `tid< f< r< rd< p<` are comment text /
  PTX `.reg %f<64>;` register-count strings, NOT type families. Only the 15 above
  (plus `field`) are real param constructors.

---

## 4. Rendering / diagnostics

- `render.f:102-103 PARAM-START` — prints `name` bytes + `<` from
  `PARAM>NAME-A/-U`. This is the spelling-based render; family-id rendering plugs
  in here (render canonical family name / qualified spelling from id, keep raw
  spelling for diagnostics per `PLAN.md:392-393`).
- `render.f:155-162` `QREND` T-PARAM arm — `PARAM-START`, then loops
  `0..PARAM>ARGC`, emits `,` between args, `RECURSE` each `PARAM>ARG`, closes `>`.
  This loop is already argc-driven (not 4-unrolled), so it survives growable
  storage; it just needs SC-QUOT arg rendering (quot arms already exist at
  `render.f:141-153`).
- Diagnostic sources that name families: `SGBAD-SYNTAX!` in `SIG-TYPE`
  (`checker.f:1872,1873,1877`), and `TYPE-RESERVED?`/`CT-ADD-*` die messages
  (`checker.f:1778,1782`). Family-specific diagnostics (unknown family, wrong
  arity, bad delimiter — `PLAN.md:402-404`) must replace the generic
  "bad or duplicate signature type" / `SGBAD-SYNTAX!` here.

---

## 5. VREC interaction (value-record expansion)

- `roles.f:172-188 VALUE-RECORD` — top-level definer: parses name + field tokens
  to `END-VALUE-RECORD`, calls `CHECKER-DEFRECORD` (`checker.f:2050`).
- `checker.f:2003-2009 VREC-FIELD-WRAP` — wraps each field as a `field` T-PARAM:
  `PARAM-SCR-RESET` + `MK-ATOM(rec) MK-ATOM(field) <type>` pushed + `s" field"
  MK-PARAM`. So a `field<rec,name,inner>` param is a 3-arg T-PARAM whose 3rd arg
  is the field's declared type — which may itself be a `family<...>` param.
  => nested-param corruption reaches VREC whenever a field type is parametric
  (`SIG-TYPE` at `checker.f:2045` runs inside `VREC-PARSE-FIELDS`).
- `checker.f:490-524` `FIELD-PARAM?` / `FIELD-REC/NAME/INNER` / `FIELD-ID-SAME?`
  / `FIELD-PAIR?` / `FIELD-COERCE?` — the `field` family's special unify. Keys off
  `PARAM>ARGC 3 =` and `PARAM>NAME = "field"` (`checker.f:492-493`). If T-PARAM
  gains a `family-id` field, `field` must get a reserved id and these checks must
  compare id (not name string / not just argc==3), or they silently break.
- `checker.f:1508-1516 VREC-COPY` T-PARAM arm and `checker.f:1584-1591 VREC-INST`
  T-PARAM arm (see §2) persist/instantiate `field<...>` and any nested family
  param; both must carry family-id and must NOT reuse the global scratch unsafely.
- `checker.f:1938-1944` PSTACK VREC hook: a bare VREC name expands to its fields
  (`VREC-PUSH-FIELDS`, `checker.f:1595-1603`), each field re-instantiated via
  `VREC-INST`. `checker.f:2015-2029` `VREC-FIELD-NAME=`/`-DUP?` read
  `VN.TAG@ VR-PARAM` and `VN.E@` (arg slot) by position — position-based access
  that a family-id column addition must keep consistent.
- What breaks if T-PARAM gains a family-id field: (a) SoA add a column in
  `checker.f:308-333` and refetch in `MK-PARAM`; (b) VN/EN nodes need a slot for
  id (H is free for non-quot, or add a column) — `checker.f:1508,2760`;
  (c) `FIELD-PARAM?` name/argc check -> id check (`checker.f:490-493`);
  (d) `field` must be registered with a reserved family-id at core load so
  `VREC-FIELD-WRAP` (`checker.f:2009`) can stamp it.

---

## 6. Quotation payloads (SC-QUOT) — does any current param carry one?

**No. `SC-QUOT` is entirely new; there is zero implementation today.**
- `rg SC-QUOT` over `src/ lib/ tools/` = 0 hits (only PLAN.md). The `E?C-QUOTE`
  hits in lint tools are `SKIP-ESC-QUOTE`, unrelated.
- Param args cannot be quotations under the current grammar: `SIG-TYPE`'s param
  loop (`checker.f:1874`) `RECURSE`s into `SIG-TYPE`, which yields only
  `T-PARAM / T-PTR / T-CON / T-ATOM / T-VAR` (`checker.f:1864-1886`,
  `TOK-TYPE` `checker.f:1785-1793`). Quotations are parsed ONLY by `PSTACK`'s
  `[ ... ]` arm (`checker.f:1910-1936 MK-QUOT`), never reachable from a param arg.
- Quotation/effect-row terms DO exist and DO persist — just not as param payloads:
  - `checker.f:247-259 MK-QUOT` + `Q>DIN/DOUT/RIN/ROUT` (four effect rows,
    32-byte stride, extra fields `QX!/Q>XHAS...`).
  - persistence/replay of quot rows already exists in VREC and effect nodes:
    `VR-QUOT` copy `checker.f:1490-1500`, `VREC-INST` `checker.f:1575-1582`;
    `EN-QUOT` copy `checker.f:2742-2752`, `E-INST` `checker.f:2927-2934`; render
    `render.f:141-153`. These are the templates the new `SC-QUOT` param-arg
    payload should mirror — a param arg node that itself holds four effect rows +
    nested family ids, threaded through parse/persist/inst/copy/render.

Implication: SC-QUOT is net-new schema (parse grammar for a quot-typed param arg,
a node kind, and all five persist/inst/copy/render/diagnose arms), not a
modification of an existing payload.
