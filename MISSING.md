# MISSING — Habu ergonomics: what porting Odin surfaced, and how to fix it properly

Status: design proposal with partial implementation. Foundation C, Foundation
A2, and the floating-point cleanup are landed; A1 and B remain active checker
work. Author: FFI/port agent. Audience: the agent working on Habu core
(checker/compiler/stdlib).

## Why this document exists

The open language/stdlib quirks under `habu-habu-quirk-fixes-5a0d1f1e` were all
discovered the same way: porting Odin's pure-data layer from Zig to checked Habu
kept hitting friction the checker or compiler imposed on otherwise-correct code.
They are currently filed as a flat list of separate fixes. Treated that way, each
fix adds one more special case to `src/core/checker.f`.

The claim of this document: **the list is not a set of independent papercuts. It
is three missing foundations plus one stdlib cleanup.** Build the three
foundations and an entire *category* of friction disappears instead of one
instance of it. One of the three is also an opportunity to make Habu's type
system genuinely better than Zig's for this domain, not merely catch up.

A mapping from the descriptive names used below back to the existing dot IDs is at
the end so the work stays traceable.

---

## Reframe: the open items are three foundations + one cleanup

| Foundation | Open items it resolves | Nature of the change |
|---|---|---|
| **A. A data-driven integer type algebra** | narrow→wide integer widening landed; declarable nominal integer types remain | type *representation* in the checker |
| **B. A lexical local-variable environment** | clear error when a local shadows a builtin; block-scoped locals; locals allowed to shadow ordinary words | name *resolution* + slot codegen |
| **C. Structured type-syntax diagnostics** | contract gate landed over checker JSONL fixtures | error *production* in checker/render |
| **(cleanup) Floating-point prelude words** | `f<=` `f>=` `fdup` `fover` landed | stdlib + signature extractor |

Each foundation is described in full below, with the concrete code touchpoints I
know from working in this tree. Every change here is a checker/compiler change, so
every one is bound by the same hard constraints (stated once, at the end):
byte-for-byte fixpoint self-rebuild, full native gate green, and a *negative*
fixture proving the new rule actually fires.

---

## Foundation A — a data-driven integer type algebra (the prize)

### The current state

Nominal integer roles are **hardcoded constants** in `src/core/checker.f`
(`CC-IDX`, `CC-LEN`, `CC-COUNT`, `CC-OFF`, `CC-FD`, `CC-RC`, `CC-PID`, `CC-MS`,
`CC-NS`, … numbered roughly 12–29). The structural integer family is a flat set,
`INT-FAM?` = { `n`, `i64`, `u8`, `u32`, `cell`, `char`, `addr` }, and `CON-OK?`
is the compatibility gate that, among other things, makes generic `n` compatible
with members of that family.

Two distinct problems live in this one place.

### Problem A1 — nominal integer types are not user-declarable

Because the roles are an enum baked into the engine, application code cannot
introduce its own nominal integer. The Odin port wants exactly this: a camera
serial, a frame index, an exposure in microseconds, a GMSL channel — all are
integers that should be **distinct** so the checker stops you mixing them, yet
today you either edit the engine or fall back to bare `n` and lose the safety.

**The right fix:** move the role enum out of the engine into a runtime-extensible
**tag table**, and add a declaration form to the language. Sketch:

```
nominal frame-idx : n        \ declares a new nominal integer over base n
```

Declaring a nominal:
- registers a tag in the table (no engine edit, no recompile of the checker),
- generates its explicit converter pair (`FRAME-IDX>N` / `N>FRAME-IDX`), matching
  the existing pattern (`PID>N`, `FD>N`, `>RC`),
- makes `CON-OK?` consult the table rather than a fixed switch.

**Non-negotiable invariant (do not weaken):** a user-declared nominal gets the
*same* strict treatment the built-in roles get today — it is distinct from `n`
and from every other nominal, and the only way across the boundary is the
explicit converter. We deliberately do **not** make `n` satisfy a nominal, and we
do **not** auto-collapse a nominal to `n`. The whole value of nominal roles is
that the conversion is visible. (This was specifically called out as a hack to
avoid: do not make `int` satisfy `pid`/`fd`/`rc`; convert explicitly or refuse.)

**Why this is the prize:** Zig has no cheap nominal *integer* — to get the same
guarantee you wrap the int in a struct and pay an ergonomic tax everywhere. A tag
table gives the Odin domain compile-checked distinct integers at zero runtime
cost. This is a place Habu can be *better* than Zig, not just even with it.

### Problem A2 — narrow→wide integer widening is implemented

Previously, passing or storing a `u8` where a wider integer (`u32`, `cell`, `n`,
`i64`) was expected forced an explicit conversion even though widening a smaller
unsigned integer into a larger one is always lossless. The old flat `INT-FAM?`
set modeled "all in the family interconvert" with no notion of *direction*, so it
could neither allow the safe direction implicitly nor reject the unsafe one.

**Implemented fix:** replace the flat family with a **width lattice**. Give each
structural integer a width (`u8`=8, `u16`=16, `u32`=32, `cell`/`n`/`i64`=64) and a
signedness. Then:
- **Widening** (smaller → larger, compatible signedness) is implicit and sound.
- **Narrowing** (larger → smaller) requires an explicit, named truncation
  (`>U8`, …) with a documented wrap/range contract.
- **Sign changes** are explicit.

This is exactly Zig's split between implicit coercion (widening) and `@truncate`
(narrowing) — borrow it directly. `CON-OK?` becomes a partial-order check instead
of a set-membership check.

### How A1 and A2 compose

Keep the two axes **orthogonal**: a type is `(structural width) × (nominal tag)`.
The width lattice governs the structural axis only. Nominal tags never widen into
each other regardless of base width — a `frame-idx` is not a `camera-serial` even
if both sit over `n`. This separation keeps both rules simple and keeps the
distinctness invariant intact.

**How to prove it on the port (measure, don't assume):** count the explicit
integer conversions (`>N` / `N>` and friends) in `odin/*.f` before and after A is
in. A correct design collapses a large fraction of that conversion noise. If the
count barely moves, the design missed.

---

## Foundation B — a lexical local-variable environment

### The current state

Locals are introduced with `{: name :}` and today behave as a **flat,
function-scoped namespace** with no defined precedence against the dictionary.
Three separately-filed problems are all symptoms of that single gap.

- **A local that shadows a builtin word** does not produce a clear, located
  error; the failure is cryptic.
- **There are no block-scoped locals** — a local lives for the whole definition
  body, so you cannot introduce a temporary inside a control region and have it
  end with the region, and you cannot reuse the name or its slot.
- **Locals cannot cleanly shadow ordinary words in the body** — natural names
  like `i`, `j`, `k`, `code`, `dup` are either reserved or collide, forcing
  awkward renames (this is why the port carries names like `IX`/`JX`).

### The right fix: one model, scope frames

Introduce a real lexically-scoped local environment:

- **Scope frames.** Entering a block (a quotation, a control region such as
  `begin`/`if`, or an explicit scope) pushes a frame; leaving it pops the frame.
- **Deterministic resolution order:** innermost frame → enclosing frames →
  dictionary. With that order, every one of the three symptoms becomes
  well-defined:
  - shadowing an *ordinary* word: the local wins *within its scope*, and the
    checker can note it — natural names like `i`/`code`/`dup` become usable as
    locals without colliding with the words outside the scope;
  - shadowing a *reserved/structural* word (the ones that drive control flow):
    a hard, located error, because you can never actually mean that;
  - block scope: simply "frames have lifetimes."
- **Stack-disciplined slot allocation.** Slots are freed when their frame pops, so
  nested blocks reuse slots. The compiler tracks a scope depth; `LLOC-FIND`,
  `LBCAP`, and `C-LOCAL-REF` gain a scope-depth dimension. The checker tracks the
  in-scope local set at each program point — which it already needs in order to
  emit the shadowing diagnostics and to type local references correctly.

This is the highest-blast-radius change in the document: it touches codegen, so it
is the one most likely to disturb the byte-for-byte fixpoint. Prototype it on a
`BF-BUILD-ALL` temp engine and keep it well away from the shared `bin/hb` until it
rebuilds to fixpoint and the gate is green. Do it last.

Required fixtures: positive (a block-scoped local used mid-control and after an
`exit`; `i`/`j`/`k`/`code`/`dup` used as locals) **and** negative (a local
shadowing a reserved control word rejected with a located message).

---

## Foundation C — make good diagnostics a contract, not a one-off

### The current state

The bare-pointer signature fix (already implemented as the template here) makes a
`ptr` with no element type report **at the offending token**, with an error code
(`E-BARE-PTR-SIGNATURE`), a repair class, and a concrete suggestion ("give `ptr`
an element type, e.g. `ptr u8` or `ptr a`"), wired through `render.f`’s `DCODE` /
`REPAIR-CLASS` / `SUGGEST-TEXT` / `DIAG-PROSE` and proven by a gate-diagnostics
fixture. The unknown-signature-token error landed earlier with the same shape.
Both replaced a previously cryptic failure (e.g. an error reported "at then", or a
token plus a bare exit code).

### The improvement: generalize the shape into an enforced contract

Status: the contract gate is implemented for checker JSONL fixtures. The parser
promotion below remains useful for future syntax work.

Two moves:

1. **Promote the signature parser.** Turn `E-BAD-SIGNATURE` from a catch-all into
   a proper recursive-descent signature parser that carries a **source span per
   token**. Every future type-syntax feature — including Foundation A's `nominal …`
   declaration syntax — then inherits located, repairable errors for free instead
   of needing its own bespoke message plumbing.
2. **Gate the contract.** Add a gate assertion that **no error code may ship
   without a span, a repair class, and a suggestion fixture.** That is what stops
   the next error from regressing to "generic message at the wrong location." The
   bare-pointer and unknown-token fixtures already establish the pattern; make it
   mandatory for all codes.

---

## Cleanup — floating-point prelude words the extractor rejects

Done: `f<=`, `f>=`, `fdup`, and `fover` are back in `lib/prelude.f`, public
signature extraction emits `F<=`/`F>=`, the four words have manifest rows and
`lib/prelude-test.f` coverage.

---

## The meta-fix: stop discovering these reactively

The reason this whole list exists is that there was no *signal* until porting
hurt. Two cheap mechanisms turn reactive pain into a measured, ranked backlog:

1. **A checker-rejection corpus.** Log every checker rejection of plausible
   user code (anonymized: error code + source span, no payload). Mine it each
   cycle — the highest-frequency rejections *are* the next round of ergonomics
   work, ranked by real pain instead of by guesswork. This is "measure, don't
   assume" applied to language ergonomics.
2. **No implicit shadowing, anywhere — one rule, not three lints.** Silent
   word redefinition (last-definition-wins), local-vs-word shadowing, and package
   re-publish are the *same* principle violated in three places: redefinition must
   be *explicit intent* (an audited `TRUST`/override path), never silent. Core has
   already started this for word redefinition (the reserved-name lint and the
   redef guard); generalize the same rule to locals (Foundation B) and to package
   publishing so there is one coherent rule across the system.

---

## Sequencing and the hard constraints

Every item is a checker/compiler/stdlib change and is therefore bound by:

- **Byte-for-byte fixpoint self-rebuild.** Prototype via `BF-BUILD-ALL` into a
  temp `hb-new`; never disturb the shared `bin/hb` until the change rebuilds the
  engine byte-identically and the change is proven.
- **Full native gate green** (`test/run.f`) on an *uncontended* host — full-gate
  budget timing is meaningless while another worktree is running a gate; check for
  competing `bin/hb … test/run.f` processes and host load before claiming a budget
  pass or regression. Run the gate with stdin from `/dev/null` (or `printf '' |`):
  the gate spawns nested `hb` children that enter a stdin REPL after running a
  script file, and an open inherited stdin makes them block until the deadline —
  which surfaces as a misleading `E-PROC-TIMEOUT` on the process-env slice, not a
  real failure.
- **A negative fixture per fix** that was previously cryptic and now reports
  precisely — the fix must be shown to actually fire.

Suggested order, cheapest/lowest-risk first so each de-risks the next:

1. **Foundation C** — diagnostic contract. Landed.
2. **Width lattice (Foundation A2)** — landed as a self-contained `CON-OK?`
   relation change, no codegen impact.
3. **Nominal tag table (Foundation A1)** — needs the declaration syntax (rides on
   C) and converter generation; medium.
4. **Floating-point prelude cleanup** — landed.
5. **Foundation B (locals environment)** — largest, codegen + fixpoint risk;
   temp-engine prototype, do last and most carefully.

---

## Mapping back to the dot IDs (traceability)

Parent: `habu-habu-quirk-fixes-5a0d1f1e`.

| Descriptive name in this doc | Dot ID | State |
|---|---|---|
| Floating-point prelude words `f<=`/`f>=`/`fdup`/`fover` | `habu-a-followup-prelude-c92f07f3` | done |
| Unknown-signature-token error at offending site | `habu-b1-unknown-signature-0ffd951c` | done |
| Clear error when a local shadows a builtin | `habu-b2-local-shadows-ae2492da` | open |
| Bare-pointer signature error at offending site (the template) | `habu-b3-bare-ptr-4939e141` | implemented, pending gate-green merge |
| Block-scoped locals (mid-control + after-exit) | `habu-c1-block-scoped-fa472987` | open |
| Declarable/extensible nominal integer types | `habu-c2-extensible-nominal-25afdeae` | open |
| Narrow→wide integer widening (`u8`/`u16`/`u32` → `n`/`i64`) | `habu-checker-int-lattice-ed8f99ab` | implemented, pending gate-green merge |
| Locals may shadow ordinary words (`i`/`j`/`k`/`code`/`dup`) | `habu-c4-locals-shadow-3c7310cb` | open |
| Fixpoint + gate-green constraint for all engine fixes | `habu-constraint-fixpoint-gate-81094225` | open |

Foundation A = the nominal-types dot + the widening dot. Foundation B = the two
locals-shadowing dots + the block-scope dot. Foundation C = the bare-pointer dot
(template, done) generalized, with the unknown-token dot as prior art.
