# Extent nominal substrate — decision record

Decision for dot `habu-choose-extent-nominal-f61dac3e`. Which substrate does
`EXTENT:` / candidate-B `idx<#extent>` (`docs/golden-syntax.md` §B) mint on?

## Decision

**Extents are TFAM families** (`src/core/type-family.f`), not CT-ROLE roles and
not `extent-*` atoms:

- a **flat extent** `#M` = a package-scoped **arity-0 TFAM cell family**
  (a nominal scalar, lowercase tail);
- the **index nominal** `idx<#M>` = an **arity-1 TFAM family** applied to the
  extent (candidate B / `habu-extent-typed-tensor-bde435dc` builds this);
- the **product** `#B*#T` (BTC-7) = an **arity-2 TFAM family** `xprod<#B,#T>`
  (or the `*` infix desugared to it), whose **ordered** argument list already
  makes the free (outer) factor distinct from the in-block (inner) factor.

The deciding reason is **BTC-7** (`docs/batch-sequence-design.md` §5,
`habu-extent-role-product-8e364885`), now a hard downstream requirement: a folded
row index must be typed `idx<#B*#T>`, factored into free `idx<#B>` × in-block
`idx<#T>`, with a contraction rule that accepts inner extents (`#T`,`#k`) and
**rejects** the free `#B`. A product-of-extents is a *structured* type argument.
Only TFAM already carries structured, ordered, nesting, identity-unified type
arguments — so the product's **type representation and its identity unification**
come for **free**: a `NEWTYPE <tail> 2` whose ordered args already reject a
swapped or mismatched factor and nest inside `idx<…>` (probes below), with no new
registry or unification kernel. What stays **net-new BTC-7 work** is the *rules
on top* of that representation: the `*` infix desugar, the factorization/split,
the inverse join, and the free-vs-inner contraction rule (the `XP-SPLIT` probe
confirms a split is a real re-typing, not identity unification). TFAM's win is
that this new work rides an **existing** parametric representation instead of
inventing one — it does not make factorization or the contraction rule free.
Atoms are opaque names (even the product *representation* needs a whole new
structured-atom grammar); CT roles are flat cons with no argument slots (a
product is not expressible without reinventing parametric families in the CT
registry).

All three substrates were probed and **all type `idx<#M>` with two extents
distinct** (`test/extent-substrate-probe.f`; probes below). The choice is decided
by the *product/factorization* axis and the secondary axes (typo protection,
package-scoping, persistence, SPEC read-back), on all of which TFAM wins or ties.

## Evidence (probed on `bin/hb`, 941c2877)

Family argument slots are **substrate-agnostic and kind-unenforced**: `span`,
`matrix`, `uniqidxctx`, and a stand-in `idx` family each accept an atom, a
CT-role, a TFAM nominal, or even a bare `n` in an extent slot — all distinct, no
cross-substrate aliasing (`matrix<…,extent-r,extent-c>` ≠ `matrix<…,role,role2>`).
So "one declaration feeds both the golden and the device plan"
(`docs/golden-syntax.md:70-71`) holds for **any** substrate; the device families
are not tied to atoms. The `extent-*` atom is consumed **only** by identity
unification (`ATOM-OK?`, `checker.f:1012`); there is no downstream
`extent-`-prefix pass — `extent-relative bounds` is enforced by family-identity
unification, not by reading atom names (0 `extent-*` usages in `src/core`
outside the parser prefix at `checker.f:2478`; in `maki`, 1 production sig
(`fusion.f:71`) plus 2 live checked eval fixtures — `maki/eval/repair-mech-test.f:85,87`
and `maki/eval/device-fault-test.f:28,32` (`span<…,extent-n>`); the rest is docs
prose).

Product structure (the decider) rides TFAM parametric unification **today**:

| probe | verdict |
|---|---|
| `idx<xprod<xm,xn>> -- idx<xprod<xm,xn>>` | ACCEPT |
| `idx<xprod<xm,xn>> -- idx<xprod<xn,xm>>` (ordered factors) | REJECT |
| `idx<xprod<xm,xn>> -- idx<xm>` (split is a real re-typing) | REJECT |
| `idx<xm> -- idx<xn>` (flip extents) | REJECT |
| `idx<xm> -- n` / `n -- idx<xm>` | REJECT |
| undeclared `idx<xundeclared>` (typo) | REJECT (TFAM/role) · **ACCEPT (atom)** |

The atom row is the ergonomic hazard: any `extent-<anything>` is a silently-valid
distinct atom — **no typo protection**. Roles and TFAM reject undeclared names.

TFAM name tails are **lowercase** (`E-TFAM-CASE`, code 7101) — consistent with
every existing type name (`n`, `idx`, `span`, `extent-r`) and distinct from the
UPPER-CASE rule for *executable words*. `EXTENT: #M` therefore desugars `#M` to a
legal lowercase family tail (EXTENT: owns the mangling).

## Criterion matrix (pulls stated, not averaged)

| criterion | atom | CT-role (DEFTYPE / A1) | **TFAM family (chosen)** |
|---|---|---|---|
| (a) flat `idx<#M>` fit | ✓ incumbent phantom tag | ✓ | ✓ |
| (b) **BTC-7 product/factorization** | ✗ new structured-atom grammar even for the *representation* | ✗✗ flat cons, no arg slots | **◑ product *representation* + ordered/nesting identity-unification free (`NEWTYPE <tail> 2`); the split/join + free-vs-inner contraction rule + `*` desugar are net-new BTC-7 work, but ride existing parametric unification** |
| (c) package-scoping (A1b cost) | ✗ global prefix, none | ✗ needs CON-OF restructure (A1b) | **✓ already package-scoped (TFAM-DECL)** |
| (d) converter / crossing | n/a (phantom, no value) | ✓ auto `>NAME`/`NAME>N`, strict-vs-n | none auto — but extents are phantom args, not cast values (index↔n is `idx`'s job, not the extent's) |
| (e) persistence (snapshot/AOT/fixpoint) | none needed (lexical, reset per-check) — but **no registry to reflect** | ✓ CT-SNAPSHOT-PERSIST + RBF rollback | **✓ TFAM-SNAPSHOT-PERSIST + TFAM-ROLLBACK (same path span/matrix ride)** |
| (f) SPEC read-back (golden/dataflow/PROMOTE) | opaque name string; product = fragile string-parse | flat name; product **not representable** | **✓ structured args; product exposes both factors → batched-contraction dataflow read directly** |
| typo protection | ✗ undeclared silently valid | ✓ | ✓ |

### Where the criteria pull apart

- **(a) + (d) pull toward atoms/roles.** Atoms are the incumbent phantom tag for
  the device families and semantically "pure phantom" (no runtime cell); CT-roles
  give auto-converters and the strict-vs-`n` story A1 locked. **But** (a) is a
  capability tie (all substrates work, device families accept all), the atom
  incumbency is one code site plus docs, and (d)'s converter story is about
  *value* nominals (pid/fd) — an extent `#M` is a *phantom* type argument with no
  value to cross, so the index↔`n` crossing belongs to the `idx` family, not the
  extent. These pulls are real but light.
- **(b), (c), (e)-lite, (f) pull decisively toward TFAM**, and (b) is the
  dominant, hard-required axis. TFAM wins.

**The one condition that would flip the decision:** if BTC-7's
product/factorization were dropped or deferred indefinitely, atoms would become
the front-runner (incumbent, zero-overhead phantom tag) — the only remaining
gap being typo protection. BTC-7 is a *hard* prerequisite of BTC-2/BTC-5
(`docs/batch-sequence-design.md` §5), so this is **not** a tie; the decision is
TFAM. Recorded here so the pivot is explicit.

## What each rejected substrate would have cost

- **CT-ROLE (A1 / DEFTYPE):** the product former is structurally impossible on
  flat cons — roles have no argument slots. Delivering BTC-7 on roles means adding
  arg-carrying roles = reinventing parametric families inside the CT registry
  (new record columns for arg lists, new con-arg unification, new nesting). Plus
  package-scoping needs the CON-OF/CT-FIND resolution restructure (A1b). Highest
  cost, structurally wrong for a phantom *parameter*. (CT roles remain correct for
  flat *value* nominals — pid/fd/rc — where A1's contract stands.)
- **Atom (`extent-*`):** flat case is free and incumbent, but BTC-7 needs a new
  structured-atom node (product with ordered children) + product unification +
  `*` grammar in the atom path — all net-new, on a representation with no
  registry to persist/reflect and no typo protection. Package-scoping absent.
- **TFAM (chosen):** flat extent = `NEWTYPE <tail> 0`; product =
  `NEWTYPE <tail> 2`; both already unify, nest, persist, roll back, and
  package-scope. BTC-7's *new* work shrinks to the `*` infix desugar + the
  factorization/join rule + the free/inner contraction rule — no new type
  representation or unification kernel.

## Probe fixture

`test/extent-substrate-probe.f` — checked, standalone (not a routed gate case;
BTC-2/BTC-5/BTC-7 own the permanent regressions). Run:

    bin/hb < test/extent-substrate-probe.f      # exit 0 + "ok"

It demonstrates the chosen substrate typing `idx<#M>` with two extents distinct,
plus the BTC-7 product structure (ordered factors, split-is-re-typing, mismatch
reject) riding existing TFAM parametric unification.

## Proposed re-scope drafts (orchestrator mints/edits the dots)

### `habu-foundation-a1b-pkg-6692f4e3` — de-prioritize (off the extent path)

> **Re-scope:** CT-role package scoping is **no longer on the extent/BTC critical
> path.** The extent substrate decision (`docs/extent-substrate.md`) puts
> `EXTENT:`/`idx<#M>` on TFAM families, which are **already package-scoped**
> (`TFAM-DECL`, package-scoped resolution via `SIG-FAM?`/`TFAM-RESOLVE`). CT roles
> (`DEFTYPE`, A1) stay global-flat, which is correct for syscall-style **value**
> nominals (pid/fd/rc). Implement the package-scoped CT-role resolution (the
> `CON-OF`/`CT-FIND`/`TYPE-RESERVED?` restructure) **only** if a concrete consumer
> needs a package-private *value* nominal that a TFAM arity-0 nominal scalar
> cannot serve; otherwise close as **obviated** by the TFAM extent decision. Keep
> the CON-OF-restructure design notes for that contingency.

### `habu-extent-typed-tensor-bde435dc` — substrate is TFAM

> **Substrate (from `docs/extent-substrate.md`): TFAM families.**
> - `EXTENT: #M` mints a package-scoped **arity-0 TFAM cell family**; desugar
>   `#M` to a legal **lowercase** family tail (`E-TFAM-CASE` — type tails are
>   lowercase; EXTENT: owns the `#`→tail mangling and collision avoidance).
> - `idx` is a **arity-1 TFAM family** (the width-1 index-value nominal); the
>   `TENSOR:`/`ITENSOR:` accessor sigs carry `idx<extent-family>`. Two extents
>   distinct + flipping = checker reject rides TFAM parametric identity
>   unification (proven, `test/extent-substrate-probe.f`).
> - The index↔`n` crossing (offsets/arithmetic) is the **`idx` family's**
>   responsibility (accessor-mediated or an explicit `idx` converter), NOT the
>   extent's — extents are phantom type arguments with no runtime value.
> - Typo protection is free: an undeclared extent tail is an unknown-signature
>   reject (atoms would have accepted it silently).
> - **Product `#B*#T` is BTC-7's remit, not this dot's** — but do not preclude it:
>   use TFAM so BTC-7's arity-2 product family rides existing parametric
>   unification (ordered args already distinguish free vs inner factor).
> - **Legacy `extent-*` atoms:** device families accept TFAM extent args
>   transparently (proven), so a SPEC-declared TFAM extent flows into both the
>   golden `idx<xm>` and the device `span<…,xm>` — "one declaration feeds both".
>   Migrate the live atom sigs — `maki/fusion.f:71`, `maki/eval/repair-mech-test.f:85,87`,
>   `maki/eval/device-fault-test.f:28,32` — and the docs opportunistically; atoms
>   and TFAM extents may coexist (both unify by identity), but a single substrate
>   is preferred — track the migration.

## Open questions for the orchestrator

1. **Legacy `extent-*` migration vs coexistence.** Adopt TFAM extents for new
   `EXTENT:` surface and migrate the live atom sigs (`maki/fusion.f:71`,
   `maki/eval/repair-mech-test.f:85,87`, `maki/eval/device-fault-test.f:28,32`) +
   `docs/ptx.md` examples, or allow atoms (device) + TFAM (golden) to coexist
   during transition? (Capability is unaffected either way; this is a consistency
   call.) Recommend: migrate opportunistically, single substrate as the target.
2. **`#`→tail mangling scheme.** `TDECL-RESERVED?` (`src/core/sumtype.f:152-160`)
   fails a declaration closed when the tail is a single letter (`u 1 =`), an
   atom-prefix token (`ATOM-TOK?`: `extent-`/`space-`/`mask-`/`block-`/`geom-`/
   `parity-`/`align-`), a builtin or CT-role name (`CON-OF`), or a control/keyword
   token (`TYPE-NAME:CONTROL?`/`TDECL-KEYWORD?`). So **both** `#M`→`m` (single-letter)
   **and** `#M`→`extent-m` (atom prefix) are rejected at declaration. The mangling
   scheme must avoid single-letter tails, the atom prefixes, existing
   CT-role/builtin names, and control/keyword tokens — e.g. a multi-letter,
   non-atom-prefix tail such as `extm`/`extb` (the probe fixture's `xm`/`xn`/`xk`
   satisfy the guard). Fix the scheme in `habu-extent-typed-tensor-bde435dc`.
3. **A1b closure.** If no package-private *value*-nominal consumer exists, close
   `habu-foundation-a1b-pkg-6692f4e3` as obviated (see re-scope). Confirm.
