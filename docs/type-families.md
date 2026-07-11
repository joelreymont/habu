# Habu Type Families and Algebraic Data Types

**Proposed repository path:** `docs/type-families.md`  
**Status:** design proposal  
**Primary goal:** implement generic, efficient, checked algebraic data types in Habu without turning `Result` into a one-off special case.

---

## 1. Naming

Yes, the umbrella mechanism should be called **type families**.

In Habu, a **type family** is a named, parameterized checker-level type constructor. A family may be purely compile-time, or it may also define a concrete runtime layout and generated operations.

Type-family names are system vocabulary. They are lowercase everywhere they are
declared or used in signatures (`result<...>`, not `RESULT<...>` or
`Result<...>`). The parser must reject uppercase or mixed-case family tokens
instead of folding them to lowercase. Qualified references split the package
qualifier from the family tail: `PKG:result<n>` is valid, `PKG:Result<n>` is
not. Family declarations also reject reserved signature tokens such as
`a`..`z`, builtins (`n`, `f`, `r`), pointer/layout tokens (`ptr`, `field`), atom
prefixes, and existing type names.
Family and sum-variant tails share one collision policy within the global or
active package scope: whichever is declared second rejects with
`E-TDECL-NAME`. Package-local variant tails do not reserve unrelated packages.

Use the generic term internally and the specific terms externally:

| Public concept | Internal kind | Example |
|---|---:|---|
| Ordinary parametric type | `cell-family` | `span<space-global,t,e>` |
| Product type / by-value record | `product-family` | `pair<a,b>` |
| Sum type / tagged union | `sum-family` | `result<a,b>` |
| Enum | `enum-family` | `color` |
| Capability / proof / evidence token | `evidence-family` | `aligned<ptr,t,align-16>` |

Recommended public defining words:

```forth
TYPEFAMILY span 3
PRODUCT pair 2 ... ;PRODUCT
SUMTYPE result 2 ... ;SUMTYPE
ENUM color ... ;ENUM
```

Recommended internal registry prefix:

```forth
TFAM
```

`TFAM`, `TYPE`, and `MATCH` are system implementation packages. They must be
sealed against user reopening because package-private words are intentionally
visible across reopened package blocks. Registry mutators, rollback hooks, and
constructor publish hooks are not ordinary public `TFAM:*` APIs; user source
cannot call or `undefine` them. Parser/checker code reaches them only through a
sealed system/friend capability. Core/native/Gforth/bootstrap sources may create
or reopen sealed implementation packages only through an explicit source-origin
friend path; user source with the same spelling rejects before mutation. Raw
wordlist and dictionary mutation paths such
as `set-current`, `get-current`, `search-wl`, `parse-name`, `'`, `[']`,
`execute`, `postpone`, `compile,`, `wordlist`, `XREF-START`, `XREF-LEN`, `XREF`,
`LATEST`, raw record readers,
checker package-scope mutators, direct legacy checker registry mutators,
`immediate`, `DNAME-IMM`, lifecycle truncation/forget words, low-level XREF
retirement, `cp@`, `rbase`, `dbase@`, `data-base`, `patch32`, `!`, `c!`, `+!`,
atomics, `here`/`allot`/`,`/`c,` must not publish into, lookup through, read or
execute/postpone/compile from, or delete from sealed system wordlists or
generated constructor metadata wordlists. Public generated constructor packages
are closed but callable: published words such as `RESULT:OK` may be looked up,
executed, postponed, and compiled as public APIs, but users cannot add tails,
delete entries, expose mutable handles, or reach private metadata through that
package. Protected wordlists and checker/dictionary memory ranges carry
provenance: stores, atomics, arena writes, every primitive/syscall effect with a
writable pointer (`read`, `readlink`, `stat64`, `lstat64`, `getdirentries64`,
`poll`, `ioctl`, future writer syscalls), `mmap` remapping, and `ffi-call*`
pointer arguments cannot target protected regions without the sealed friend
capability. AOT image restore must persist protected WIDs, restore WIDs without
u8 truncation, and advance `WIDN` past every restored protected WID before user
wordlist allocation resumes. Protection is case-insensitive and applies to the
native, `habu1`, and Gforth bootstrap mirrors.

So: **call the document “Type Families and Algebraic Data Types.”**

---

## 2. Design position

A real Rust-like `result<t,e>` is not just a parametric type expression.

It requires four separate but coordinated layers:

```text
logical type      result<a,b>
runtime layout    payload slots + tag
constructors      RESULT:OK, RESULT:ERR
elimination       checked MATCH with payload refinement
```

The implementation should be generic enough that `result`, `option`, enums, products, GPU evidence tokens, and future layout-bearing types all use the same infrastructure.

The guiding rule:

> Every ADT family defines a logical type, a physical layout, generated introduction forms, and a checked elimination form.

For `result`:

```text
logical:   result<a,b>
physical:  @result.slot0<a,b> @result.tag<a,b>
intro:     RESULT:OK, RESULT:ERR
elim:      MATCH result ... ;MATCH
```

The `@result...` names are diagnostic renderings. Internal hidden physical
field terms carry the resolved `family-id`, so a same-tail `result` defined in a
different package cannot unify with this one.

Private family constructors are checker-owned tokens keyed by package, family,
and variant id. They are not bare dictionary words and do not export constructor
packages. Public families may publish constructor packages such as `RESULT:OK`;
after the constructor token protocol is installed, private families construct
only through that checker-owned protocol while the owning package is open.

For `option`:

```text
logical:   option<a>
physical:  @option.slot0<a> @option.tag<a>
intro:     OPTION:NONE, OPTION:SOME
elim:      MATCH option ... ;MATCH
```

For an enum:

```text
logical:   color
physical:  @color.tag
intro:     COLOR:RED, COLOR:GREEN, COLOR:BLUE
elim:      MATCH color ... ;MATCH
```

---

## 3. Why Habu needs this

Habu already has most of the substrate:

- checked stack-effect signatures;
- row-polymorphic stack checking;
- concrete nominal types;
- linear types;
- value-record expansion;
- parametric `T-PARAM` type terms;
- structural unification of parametric types;
- renderer support for parametric types;
- PTX/GPU types that already rely on parametric phantom/evidence types.

The current weak point is that parametric constructor names are hard-coded. Types like `span`, `tile`, `gridctx`, and `uniform` are accepted because the parser has a whitelist.

That should become a registry.

The second weak point is that Habu has by-value record expansion, but not a generic layout-family abstraction that can support products, sums, enums, and future packed/niche/boxed policies.

This design fixes both.

---

## 4. Runtime layout principle

Habu stack values are cells. Therefore a logical type that occupies multiple runtime cells must not be modeled as a single checker cell.

Do **not** represent this as one opaque `T-PARAM` cell:

```forth
result<ptr u8,n>
```

unless the runtime value is actually one cell.

For the default unboxed layout, represent it physically as:

```text
payload-slot-0 payload-slot-1 ... payload-slot-(M-1) tag
```

where `M` is the maximum payload width, in stack cells, across all variants.

Examples:

```forth
SUMTYPE result 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
```

Physical layout:

```text
slot0 tag
```

```forth
SUMTYPE option 1
  VARIANT none   ;VARIANT
  VARIANT some a ;VARIANT
;SUMTYPE
```

Physical layout:

```text
slot0 tag
```

For `none`, `slot0` is padding.

```forth
ENUM color
  red
  green
  blue
;ENUM
```

Physical layout:

```text
tag
```

This is efficient:

- no heap;
- no GC;
- no fat pointer;
- no hidden runtime metadata;
- direct tag dispatch;
- values remain stack/register friendly;
- layout is predictable for future JIT/AOT/GPU lowering.

---

## 5. Canonical stack layout

Canonical sum layout:

```text
slot0 slot1 ... slotM-1 tag
```

The tag is top-of-stack.

Reasons:

1. Tag inspection is cheap.
2. Branch dispatch can read/drop the tag first.
3. Payload slots remain in declared order.
4. Padding sits above the real payload and is easy to drop.

For a variant with payload width `p` and family max payload width `M`:

```text
constructor output:
  payload-cell-0 ... payload-cell-(p-1) padding... tag
```

For branch entry:

```text
before:
  slot0 ... slotM-1 tag

after dropping tag and padding:
  payload-cell-0 ... payload-cell-(p-1)
```

---

## 6. Type-family registry

Add a persistent checker registry for type families.

Internal record concept:

```text
TFAM record:
  package-id          wordlist/package id
  visibility          public | private
  tail-name           canonical lowercase family name
  source-name         original string for diagnostics
  arity               n
  parameter-kinds     cell | layout | type | evidence
  kind                cell | product | sum | enum | evidence
  flags               bitset
  layout-policy       stack-cell-tag | packed-tag | niche | boxed | custom
  variant-start       index into SUMV
  variant-count       n
  field-start         index into product-field table
  field-count         n
  max-payload-cells   n
  tag-width           stack-cell by default
```

Minimum v1 fields:

```text
tail-name
package-id
visibility
arity
kind
variant-start
variant-count
max-payload-cells
layout-policy
```

Required operations:

```forth
TFAM-ADD-INTERNAL      ( name-addr name-len arity kind -- family-id )
TFAM-FIND-INTERNAL     ( name-addr name-len -- family-id true | false )
TFAM-ARITY@            ( family-id -- n )
TFAM-KIND@             ( family-id -- kind )
TFAM-LAYOUT?           ( family-id -- bool )
TFAM-SUM?              ( family-id -- bool )
TFAM-ENUM?             ( family-id -- bool )
TFAM-PRODUCT?          ( family-id -- bool )
```

Mutating operations are friend-only implementation words, not user-callable
public API. Read-only query words may be exported only if they cannot mutate
registry state or bypass visibility. The implementation must resolve names
through the active package scope. Declarations and signature references accept
only lowercase family tokens. Parsed `T-PARAM` terms store the resolved
`family-id`; the source name is diagnostics only. Unification compares
`family-id`, not only spelling, so two packages may define the same lowercase
family tail without aliasing.

Replace the hard-coded parametric constructor whitelist with package-aware
internal lookup.

---

## 7. Variant registry

Add a registry for sum/enum variants.

Internal record concept:

```text
SUMV record:
  family-id
  variant-name
  tag-value
  payload-schema-start
  payload-schema-count
  payload-cell-count
  generated-constructor-symbol
  constructor-package-id
```

For:

```forth
SUMTYPE result 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
```

The registry contains:

```text
family result:
  arity = 2
  kind = sum
  max-payload-cells = 1
  variant-count = 2

variant ok:
  tag = 0
  payload = paramref 0

variant err:
  tag = 1
  payload = paramref 1
```

For:

```forth
ENUM color
  red
  green
  blue
;ENUM
```

The registry contains:

```text
family color:
  arity = 0
  kind = enum
  max-payload-cells = 0
  variant-count = 3

red:
  tag = 0
  payload = empty

green:
  tag = 1
  payload = empty

blue:
  tag = 2
  payload = empty
```

---

## 8. Type schema nodes

Family definitions must not store live checker type variables directly. They should store persistent schemas that are instantiated later.

Schema node kinds:

```text
SC-CON        concrete built-in type code
SC-PARAMREF   positional family parameter reference
SC-PTR        child schema
SC-ATOM       atom name
SC-PARAM      family-id + child schemas
SC-QUOT       quotation effect schema
SC-LAYOUT     layout family-id + child schemas
```

For `result<a,b>`:

```text
ok payload schema:
  SC-PARAMREF 0

err payload schema:
  SC-PARAMREF 1
```

For:

```forth
SUMTYPE parse-result 1
  VARIANT ok  a ;VARIANT
  VARIANT err ptr u8 n ;VARIANT
;SUMTYPE
```

The `err` payload schema is:

```text
SC-PTR(SC-CON u8)
SC-CON n
```

Instantiation:

```text
parse-result<token>

paramref 0 => token

ok payload => token
err payload => ptr u8 n
```

---

## 9. Public defining words

### 9.1 `TYPEFAMILY`

For ordinary one-cell parametric types:

```forth
TYPEFAMILY span 3
TYPEFAMILY matrix 4
TYPEFAMILY gridctx 3
TYPEFAMILY tile 3
TYPEFAMILY uniform 1
```

This replaces hard-coded parser knowledge.

### 9.2 `SUMTYPE`

Syntax:

```forth
SUMTYPE result 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
```

Parameter names are positional:

```text
arity 1 => a
arity 2 => a b
arity 3 => a b c
```

Arity is stored in growable schema lists. Do not bake `PARAM-MAX-ARGS` into the
type-family design; the old four-argument checker storage is an implementation
limit to remove.

### 9.3 `ENUM`

Syntax:

```forth
ENUM color
  red
  green
  blue
;ENUM
```

Equivalent to a zero-payload sum:

```forth
SUMTYPE color 0
  VARIANT red   ;VARIANT
  VARIANT green ;VARIANT
  VARIANT blue  ;VARIANT
;SUMTYPE
```

### 9.3.1 `DERIVE eq` (derive S1+S2)

A PUBLIC arity-0 enum, sum, or product may opt into derived typed equality
with a `DERIVE eq` header clause (after the optional `POLICY`, before the
variants/fields):

```forth
ENUM color DERIVE eq  red green blue ;ENUM
SUMTYPE shape 0 DERIVE eq
  VARIANT dot ;VARIANT
  VARIANT seg n n ;VARIANT
;SUMTYPE
PRODUCT probe 0 DERIVE eq
  FIELD col color        \ enum-typed field: color must also derive eq
  FIELD amt n
;PRODUCT
```

This generates ORDINARY CHECKED words into the family's reserved constructor
package — no pending window, no trust rows, no engine lowering. The bodies are
plain checked `MATCH`/`UNMAKE`/call text the checker certifies exactly like
user code, so equality is semantic and layout-policy agnostic. Derived eq
CONSUMES both operands (ordinary non-linear values; callers keep copies):

```forth
: COLOR:TAG ( color -- n )          \ discriminant: declaration-order tag
   match color red of 0 endof green of 1 endof blue of 2 endof ;match ;
: COLOR:EQ ( color color -- bool )  \ payload-free: tag equality (O(V))
   COLOR:TAG swap COLOR:TAG = ;
: SHAPE:EQ ( shape shape -- bool )  \ payload sum: diagonal double MATCH
   match shape
     dot of match shape dot of 0 0= endof seg of drop drop 1 0= endof ;match endof
     seg of {: q0:n q1:n :}
       match shape dot of 1 0= endof
         seg of {: p0:n p1:n :} p0 q0 = p1 q1 = and endof ;match endof
   ;match ;
: PROBE:EQ ( probe probe -- bool )  \ product: UNMAKE both, field-wise compare
   PROBE:UNMAKE {: q1:n :} COLOR:TAG {: q0:n :}
   PROBE:UNMAKE {: p1:n :} COLOR:TAG {: p0:n :}
   p0 q0 = p1 q1 = and ;
```

`TAG` exposes only the declaration-order tag — public metadata any checked
`MATCH` could already observe, never a hidden field; sums and enums get it,
products do not (no discriminant). Integer payloads (any `CT-INT` scalar)
compare with `=` after the widening local bind; an enum-typed product field
routes through ITS family's `PKG:TAG` (that family must also `DERIVE eq`,
else `E-TDECL-DERIVE`). The scalar `=`/`0=` wall on layout values is
untouched: `( color color -- bool ) =` still rejects (pinned by TD12-ZEQ and
the derive suite). The generated tails `eq`/`tag` are generator-owned: a
DERIVE-marked family rejects a variant spelled `eq` or `tag`
(`E-TDECL-NAME`), the words are extend/undefine-protected exactly like
constructors (`TFAM-DERIVED-AT?` feeds the item-8 predicates; products
recognize `eq` only), and all ride the ctor package's closed-but-callable WID
protection and registry rollback (the request lives in the family row's
`TF.DERIVE` bitmask, so a rolled-back declaration forgets it with the row).

Underivable payload roles reject at the DECLARATION with `E-TDECL-DERIVE`:
pointer payloads (identity-eq would need a checked pointer-equality surface,
which does not exist — `( ptr u8 ptr u8 -- bool ) =` rejects); non-integer or
linear scalars (comparing a linear value consumes it; deferred to TFAM-11);
parametric families (`derive requires a concrete (arity 0) family` — open
payload types have no comparator). A private family rejects (`derive requires
a public family` — there is no package to hold the words).

The clause is an order-free feature list: the FIRST token after `DERIVE` must
be a known feature (`eq`, `hash`; unknown rejects `E-TDECL-DERIVE`), and the
list continues greedily while tokens stay known features (repeats are
idempotent), so `DERIVE eq hash` and `DERIVE hash eq` are equivalent and the
first non-feature token safely starts the variants. `DERIVE order` stays
deferred.

### 9.3.2 `DERIVE hash` (derive S3, semantic reference)

`DERIVE hash` generates `PKG:HASH ( fam -- n )` from the SAME checked
generator family: FNV-1a folded over whole cells (`h = BASIS`, then per cell
`h = (h xor v) * PRIME`; 64-bit offset basis `$cbf29ce484222325`, prime
`$100000001b3`, named `DRV-FNV-BASIS`/`DRV-FNV-PRIME` in sumtype.f and
rendered into the generated text as hex literals). Sums/enums fold the
variant tag then each bound payload scalar per MATCH arm; products `UNMAKE`
and fold the fields, enum-typed fields through their family's `PKG:TAG` —
exactly the cells derived eq compares, so **equal values hash equal by
construction**. `TAG` rides ANY derive feature on sum/enum kinds; a
hash-only family gets `HASH` + `TAG` and no `EQ`; products get `EQ`/`HASH`
per feature and never `TAG`. The payload-role gate is shared with eq
(CT-INT scalars only; pointer/linear/parametric reject; product enum fields
require the field family to also derive).

Hash VALUES are an IN-MEMORY contract only: this checked generator is the
semantic reference, and a later flat-cell `EM-ADT-HASH` engine fast path
(gated on the zeroed padding of `stack-cell-tag`, with this generator as its
differential oracle) may change the produced values. NOTHING may persist
derived hashes across engine versions — consumers (e.g. the SKEY replay
table) must key in-memory tables only and keep their durable formats on
stable renders (SK-KEY$ stays the on-disk contract).

### 9.4 `PRODUCT`

Syntax:

```forth
PRODUCT pair 2
  FIELD fst a
  FIELD snd b
;PRODUCT
```

Landed (item 15). A product is a single-shape record family (`TK-PRODUCT`):
each `FIELD name type` registers one product-field row keyed by
`(family-id, field tail)` plus one field schema, in declaration slot order
(slot 0 deepest). There is no tag and no variants:

```text
WIDTH(product<...>) = sum of field widths
```

Field names are their own tail namespace (single letters such as `x` are
legal, lowercase canon enforced, duplicates reject). Field types use the
variant payload grammar: positional letter params within arity, concrete cell
types, and `ptr T` — plus, since layout-kinded fields S1 (dot
habu-checker-capability-layout-4e7f1f03), an S1-tier LAYOUT FAMILY reference
(sum/enum kind, arity 0, width 1 — the enum tier). Such a field's schema is a
family application (SC-APP) carrying the resolved family-id; `PF.SLOT` is the
field's cumulative CELL OFFSET and the product's width is the field-width sum
(both identity with the old index/count values while every field is one cell);
`MAKE`/`UNMAKE` consume/produce the field typed as its family, so a swapped
same-width enum field is a checker reject. Wider, parametric,
product-kinded (incl. self-referential), and linear layout fields keep the
`E-TDECL-PAYLOAD` reject (the S2 tier), as do SUM variant payloads (S3).
Pinned in `test/type-decl-suite.f` (TDPREC/tdpbad*).

A PUBLIC product publishes exactly two generated words in its derived
constructor package, with fixed generator-owned tails (the analogue of a
sum's per-variant constructors):

```forth
PAIR:MAKE   ( fst snd -- pair<a,b> )
PAIR:UNMAKE ( pair<a,b> -- fst snd )
```

Both bodies are empty and certify under the k=0 pending-constructor window: a
product bundle is exactly its field cells in slot order, so construction and
destructure are physical no-ops and the declared signatures are checker-owned
metadata truth. Parametric products publish both words — MAKE's open result
expands and UNMAKE's open input absorbs the caller's hidden run through the
logical/hidden row coercion (§19) — and linear instantiations stay
fail-closed at the signature/argument-bind layers. Field accessors are
ordinary checked user compositions over `UNMAKE` (destructure, then
`drop`/`nip` the raw field cells), which keeps single-field access fully
checked and makes the linear discipline automatic; no per-field words are
generated.

`MATCH` and `construct` remain kind-gated to sum/enum families: a product is
eliminated only by `UNMAKE` and constructed only by `MAKE`, so a PRIVATE
product currently has no construction surface (fail-closed by design until a
product form is specified).

Verdict (item 15, decided by evidence): `VALUE-RECORD` is NOT subsumed. It
remains a typed, tested compatibility layer over its own registry (VREC) with
touchable `field<...>` cells — its fixtures rely on ordinary
`drop`/`nip`/`over over` destructuring and the field-to-inner output
coercion, which the hidden-field model deliberately forbids. New by-value
records should use `PRODUCT`; `VALUE-RECORD` stays for the existing engine
fixtures and PTX IR until those migrate.

---

## 10. Hidden physical field types

Logical layout-bearing types expand to hidden physical fields.

For:

```forth
result<ptr u8,n>
```

Checker row expansion:

```text
@result.slot0<ptr u8,n> @result.tag<ptr u8,n>
```

For:

```forth
option<n>
```

Checker row expansion:

```text
@option.slot0<n> @option.tag<n>
```

For:

```forth
color
```

Checker row expansion:

```text
@color.tag
```

Zero-arity family tokens such as `color` still resolve through internal TFAM lookup and
store `family-id`; they do not fall through to nominal-name lookup by spelling.

Hidden names must be rejected by the public signature parser. They are generated internally only.

Rule:

```text
public parser rejects type-family names beginning with @
```

This prevents users from forging physical implementation details in checked signatures.

Hidden fields are a checker-owned kind. They can only be consumed by
checker-owned layout operations, constructors, and `MATCH` lowering; ordinary
polymorphic or concrete primitives must not bind them as plain scalar, pointer,
or cell values.

---

## 11. Logical row expansion

Today stack parsing eventually does something equivalent to:

```forth
SIG-TYPE MK-PUSH
```

Change this to:

```forth
SIG-TYPE PUSH-LOGICAL
```

Pseudo-Forth:

```forth
: PUSH-LOGICAL ( row type -- row )
  dup LAYOUT-TYPE? IF
    LAYOUT-PUSH-FIELDS
  ELSE
    swap MK-PUSH
  THEN ;
```

For ordinary one-cell types, behavior is unchanged.

For sum/product/enum families, `LAYOUT-PUSH-FIELDS` expands to hidden physical fields.

**Implementation status (PLAN item 7, reject-only).** `PUSH-LOGICAL` is installed
as the signature-parse seam (replacing `SIG-TYPE MK-PUSH`), but the
`LAYOUT-PUSH-FIELDS` expansion branch is deliberately **not enabled yet**: a
sum/enum/product family in a signature stays ONE logical `T-PARAM` cell carrying
its resolved `family-id` plus the slot/tag metadata (`TFAM-SLOTS@`, SUMV
paycells) that items 8/9/12/16 will consume. Physical expansion waits for item
12's width-aware generic stack operations, which alone can preserve whole
bundles across `dup`/`drop`/`swap`/… (§17). Until then item 7 makes the logical
cell **fail closed** rather than one-cell-touchable: a layout cell may flow
through (identity — the same `family-id` unifies), but any ordinary one-cell
primitive that would bind or consume it (`dup`/`drop`/`swap`/`over`/`nip`/`>r`,
or capture into a local) is rejected in the checker's unifier
(`LAYOUT-PARAM?`/`LAYOUT-EITHER?` in `U-TYPE`). Cell families are unaffected, and
the public parser keeps rejecting hidden `@family.slotN`/`@family.tag` names, so
no hidden field is ever expanded, rendered, or exposed at this stage.

---

## 12. Constructors

Constructors are generated introduction forms.

For `result`:

```forth
RESULT:OK  ( a -- result<a,b> )
RESULT:ERR ( b -- result<a,b> )
```

For `option`:

```forth
OPTION:NONE ( -- option<a> )
OPTION:SOME ( a -- option<a> )
```

For `color`:

```forth
COLOR:RED   ( -- color )
COLOR:GREEN ( -- color )
COLOR:BLUE  ( -- color )
```

Constructor packages are keyed by the defining `family-id`, not by a bare
uppercase tail. A top-level `SUMTYPE result` may publish `RESULT:OK` and
`RESULT:ERR`. A `result` family declared inside another package must publish into
a distinct constructor package derived from stable source identity by one
pinned algorithm shared byte-identically by native `habu2`, `habu1`, and the
Gforth bootstrap mirror: uppercase each canonical package path segment and the
family tail, escape every literal `-` inside every joined segment — the family
tail included — as `--`, and join segments plus tail with single `-`
separators (escaping only the package segments is not injective: package `a`
with family `b-c` would collide with a top-level family `a-b-c`); if the
escaped spelling exceeds the engine dictionary name-length limit, the spelling
is `T` plus the first 16 lowercase hex digits of SHA-256 over the
length-prefixed unescaped segment list, then `-` plus the raw uppercase family
tail (unescaped: the fixed-width hash region already delimits it, and hash
spellings are always longer than the escaped-form limit, so the two forms can
never collide). It must not
collide with the top-level `RESULT` package. The spelling must be a legal
one-colon package call, injective, and stable across unrelated earlier
package/family declarations; it must not use allocation-order numeric
package/family ids or raw hyphen concatenation. Hyphenated names such as package
`A-B` with family `c` and package `A` with family `b-c` must not derive the same
constructor package (`A--B-C` vs `A-B--C`).
Generated constructor package names are reserved and
non-reopenable by ordinary `package`; an existing ordinary package with that
spelling, or an existing qualified definition-created wordlist with that
spelling, makes the family declaration fail. Private family constructors are not
exported as external constructor packages and are not bare words. They use the
checker-owned source form:

```forth
construct family variant
```

The parser consumes the family and variant tokens, resolves them while the
owning package is open, and records `(owning-package-id, family-id, variant-id)`
for checker effects and native/Gforth lowering. `construct` is introduced with
the constructor/MATCH token protocol after private constructor metadata exists;
it is a reserved parser token and participates in preverify/all-errors replay
like the public ADT definers.

Checker semantics (item 9): the two operand tokens are captured before locals
reference, control dispatch, and dictionary lookup, so they can never resolve
as locals or words. The ownership predicate is package identity — the family
must live in the ACTIVE checker package (top level owns the global empty
package), public or private; cross-package construction never resolves, even
for public families (those are constructed through their generated words).
Qualified `PKG:family` operand tokens do not resolve either: the form is
owner-only by design. Only sum and enum kinds construct. Operand tokens fold
like every body token, so `construct ZRES OK` and `construct zres ok` agree.
The applied effect is the generated-constructor effect built inline from SUMV
metadata (payloads consumed, layout bundle produced, one fresh type var per
family parameter), through the same unification and linear-conservation step
as a word call — linear payloads are consumed into the minted bundle with
exact accounting, identical to the generated constructor words. Until item 10
lands lowering, a certified construct body fails closed at engine compile
(`E-UNDEFINED: construct`, exit 70) on every real load path.

Constructor runtime rule:

```text
payload cells are already on stack
push M-p zero padding cells
push tag
```

Where:

```text
M = max payload cell count for the family
p = payload cell count for this variant
```

### Example: `RESULT:OK`

Input:

```text
a
```

Output:

```text
a 0
```

### Example: `RESULT:ERR`

Input:

```text
b
```

Output:

```text
b 1
```

### Example: `OPTION:NONE`

Input:

```text
<empty>
```

Output:

```text
0 0
```

The first `0` is padding. The second `0` is the tag.

### Example: `OPTION:SOME`

Input:

```text
a
```

Output:

```text
a 1
```

The generated constructor body is checked code. The generator must not emit
`TRUST`, `TRUSTED:`, `set-check`, or require a `TRUSTED.md` manifest row,
including inside generated strings later passed to `evaluate`.

Do not expose public unchecked converters from `n` to enum/sum tags.

---

## 13. Checked `MATCH`

The main user-facing eliminator should be a compiler/checker control form, not a quotation combinator.

Preferred syntax:

```forth
: RESULT>CODE ( result<ptr u8,n> -- n )
  MATCH result
    ok OF
      drop 0
    ENDOF
    err OF
      \ err payload n is already on stack
    ENDOF
  ;MATCH ;
```

Inside the `ok` branch, the checker knows the payload is `ptr u8`.

Inside the `err` branch, the checker knows the payload is `n`.

The user should not manually compare tags, manually unwrap payload slots, or write quotation plumbing.

This is the compiler’s job.

---

## 14. `MATCH` checker semantics

At:

```forth
MATCH result
```

checker steps:

```text
1. Resolve family name.
2. Require family kind = sum or enum.
3. Inspect top logical stack value.
4. Verify physical top cells match the hidden layout for that family.
5. Recover family arguments from hidden field types.
6. Pop hidden physical fields from DCUR.
7. Push CF-MATCH frame.
```

The match frame stores:

```text
family-id
family arguments
base data row
base return row
accumulated output data row
accumulated output return row
seen variant bitset
payload slot count
normal/live branch state
```

At:

```forth
ok OF
```

checker steps:

```text
1. Require top control frame = CF-MATCH.
2. Resolve variant `ok` in the current family.
3. Reject duplicate variant.
4. Mark variant as seen.
5. Instantiate variant payload schema using family arguments.
6. Set DCUR = base row + instantiated payload.
7. Set RCUR = base return row.
8. Mark branch as live.
```

At:

```forth
ENDOF
```

checker steps:

```text
1. If branch has normal continuation, accumulate DCUR/RCUR into match output.
2. Mark current path dead until next variant or ;MATCH.
3. Emit jump-to-join metadata for compiler.
```

At:

```forth
;MATCH
```

checker steps:

```text
1. Verify all variants have been handled.
2. Reject missing variants.
3. Reject duplicate variants.
4. Unify all live normal branch outputs.
5. Treat invalid-tag fallback as no-normal-return.
6. Set DCUR/RCUR to joined output.
7. Pop CF-MATCH frame.
```

V1 has no default branch syntax. Every variant must be named explicitly. A
default branch would need its own reserved token, replay support, diagnostics,
and runtime lowering proof, so it is a later extension.

V1 scrutinee rule (item 9 slice 3): `MATCH` requires the scrutinee's
width-expanded hidden-field bundle on top of the data row. An open-arg
parametric value (one conservative logical cell — its args still unresolved
vars, so possibly linear) rejects; it becomes matchable when whole-bundle
MATCH consumption lands with the TFAM 11 tail. A `MATCH` inside a `[: ;]`
quotation also rejects: quotation rows are open and inferred forward, so no
scrutinee bundle exists to verify — a future declared-effect quotation form
would lift this. `MATCH` family resolution is signature scope (own package
first, else the unique public family, qualified `PKG:tail` accepted):
eliminability follows nameability, unlike `construct`'s owner-only rule.
Frame headroom is fail-closed: a match that cannot reserve its two control
frames hard-rejects with pinned diagnostics — never a silent uncheckable.

Branch outputs still need to unify because the code after `;MATCH` has one continuation. This is not a user-facing contortion; it is the same kind of coherence Rust requires for `match` expressions and Habu already requires for structured control flow.

---

## 15. Runtime tag checking

Generated `MATCH` must check the runtime tag.

Even if checked Habu code cannot construct invalid tags, trusted code or FFI can corrupt invariants.

Runtime lowering for:

```forth
MATCH result
  ok OF ... ENDOF
  err OF ... ENDOF
;MATCH
```

should be equivalent to:

```text
peek tag

if tag == 0:
  drop tag
  expose slot0 as ok payload
  run ok branch
  jump join

if tag == 1:
  drop tag
  expose slot0 as err payload
  run err branch
  jump join

else:
  drop tag
  drop slot0
  die "bad result tag"

join:
```

For `OPTION:NONE`, where payload width is zero but family max payload width is one:

```text
if tag == 0:
  drop tag
  drop padding slot0
  run none branch
```

For `OPTION:SOME`:

```text
if tag == 1:
  drop tag
  expose slot0 as some payload
  run some branch
```

Invalid-tag fallback has no normal continuation.

---

## 16. Compiler lowering

### Constructors

For a generated constructor:

```text
payload width p
max payload width M
tag k
```

emit:

```text
payload already present
push zero padding M-p times
push tag k
```

### Match

For small sums:

```text
peek tag
cmp tag, 0
branch-eq L_variant_0
cmp tag, 1
branch-eq L_variant_1
...
jump L_bad_tag
```

For dense enums or large dense sums:

```text
bounds-check tag
jump-table tag
```

v1 can use compare/branch chain everywhere.

### Branch prologue

For each variant:

```text
drop tag
drop padding cells
leave real payload cells on stack
```

### Branch epilogue

For each normal branch:

```text
jump L_join
```

### Invalid tag path

```text
drop tag
drop all payload slots
die "bad <family> tag"
```

**LANDED (native, item 10 slice 3a).** The compiler (`src/habu/habu2.f`) lowers
`MATCH family v OF … ENDOF … ;MATCH` exactly as above: `J-MATCH` (via `CF-ENTRY`,
which spills the width-expanded bundle to the physical stack) arms the token
machine and pushes a `J-CASE`-shape CF sentinel; `EM-ADT-MATCH-FAM/VAR/OF`
consume the family/variant/`of` operands (resolving through the checker-friend
`tfl-match-fam?`/`tfl-cvar?` bridges) and emit the peek-tag compare/branch chain
+ per-variant prologue (drop tag + `M-p` pads, expose the `p` payload cells);
`ENDOF` reuses the CASE `J-ENDOF` codegen and re-arms the token machine only for a
match branch (a CMBK branch-kind bitstack, the compiler analogue of the checker's
`CF-ENDOF-DISPATCH`); `EM-MATCH-SEMI` emits the invalid-tag die then patches every
`ENDOF` jump to the join with a `J-ENDCASE`-style loop. The die is emitted
write+exit only (`C-DIE-BAD-TAG`): because `exit_group` terminates the process, the
`drop tag / drop all payload slots` above are unobservable before the exit and are
elided, keeping the compiled word minimal — the diagnostic and the `E-BAD-TAG`
(85) exit are the observable contract (see §24). **The `bootstrap/cg/forth.fs`
stage0 mirror landed in slice 3b** (byte-identical fixpoint from the
Gforth-recovered engine), and slice 5 landed AOT/object persistence of matched
definitions: `tools/hb-build.f --preseed-entry NAME --preseed-seed HEX` builds a
selected non-`MAIN` entry that seeds a forged value-stack bundle before calling the
matched helper, so the persisted-then-restored artifact reaches the same
`E-BAD-TAG` die; the entry/seed/mode axis is folded into the artifact key, the
object source-index key, and the object bytes (a new `entry` schema row) so a
preseeded run can never restore a stale normal-`MAIN` artifact.

---

## 17. Layout-aware generic stack operations

This is essential to finish the job.

If a logical value occupies multiple cells, ordinary stack words must operate on logical values, not raw physical cells, in checked code.

Given:

```forth
result<a,b>
```

physically:

```text
slot0 tag
```

Then:

```forth
dup
```

must lower to:

```text
slot0 tag -> slot0 tag slot0 tag
```

not:

```text
slot0 tag -> slot0 tag tag
```

Likewise:

```forth
drop
```

must drop both physical cells.

```forth
swap
```

must swap logical bundles, not just one cell.

### Recommended final semantics

Checked generic stack words operate on logical values. The compiler lowers them to the required physical cell operations.

### Implementation staging

Do not introduce result-specific staging helpers. If an intermediate stage needs
explicit helpers, they must be generated uniformly for any layout family, remain
internal/test-only, and disappear behind generic layout-aware stack operations
before the public ADT surface is enabled. The user experience must not leak
layout width.

`?dup` is not a generic layout operation. Raw `?dup` branches on the top cell,
but tag 0 is a valid variant for common sums. It must reject layout values unless
a family policy defines a checked truthiness/niche representation.

Top-level value-consuming definers and stack introspection need the same rule.
`constant` must reject layout values or store the whole logical value; it cannot
pop only the tag cell. `depth` and `.s` must report logical stack shape or reject
rows containing hidden fields; they must not expose raw hidden physical cells.

**Implementation status (PLAN item 12, slices 1–2).** Checked generic stack ops
now treat a layout value as one whole logical bundle. The transport set —
`dup drop swap over nip rot -rot tuck 2dup 2drop 2swap 2over`, the return-stack
transfers `>r r> r@ 2>r 2r> 2r@`, and `{: :}` locals capture — may bind a
layout `T-PARAM` cell to its fresh effect var (`LAYOUT-XPORT` mode set per
token in `DO-TOK1`/`LOC-BIND`, consumed by `LAYOUT-BLOCK?` in `U-TYPE`). Every
other touch keeps item 7's fail-closed reject: `?dup` (branches on the tag
cell; sticky `QDUPBAD` verdict), scalar control predicates, arithmetic/compare
/unary, memory ops, `execute`/`catch`/defer quotation operands, `constant`'s
value pop, `throw`, and hidden `@family.*` names in public signatures. A
stack-preserving quotation still passes a layout value through untouched
(whole-row absorption), and `evaluate` stays rejected in checked bodies.
Acceptance is sound at this stage because a layout value is still ONE physical
cell: `LAYOUT-PUSH-FIELDS` expansion is not enabled and no constructors are
published, so a wider-than-one-cell layout value is not constructible; runtime
behavior is unchanged. Transport mode keys on the folded token name, and user
signatures resolve before prims — sound because the name then denotes a checked
definition (its effect is verified against its body, which can only move the
one-cell value) or an audited `TRUSTED` boundary. A layout family whose args
contain a linear con — or an arg still unresolved, which may later bind linear —
must not transport at all (copies would duplicate and drops would lose the
hidden payload resource; locals capture launders the count): the transport bind
rejects until TFAM 11 teaches the linear discipline whole-bundle counting.
Identity flow of such a value stays legal.

**Width facts for emitters (slice 2).** The checker computes logical widths per
§18 — `TFAM-WIDTH@ ( id -- n )` in the registry (sum: slots+tag, enum: tag,
product: field cells, cell/evidence: 1) and `T-WIDTH ( type -- n )` on resolved
type terms — and records, per checked definition, a width-fact table: one row
per LAYOUT operand of each transport op and locals capture, holding (body token
index, operand position 0=top, family-id, registry logical width). Absence of a
row means every operand is one cell. Query surface (checker-modeled, callable
from checked code): `WF-N@`, `WF-TOKIX@`, `WF-POS@`, `WF-FAM@`, `WF-WIDTH@`.
The table is per-`CHECK` scratch: valid from a definition's verdict until the
next `CHECK`, never persisted, never rolled back.

**Emission-ordering requirement (slice 3+).** The native compiler emits code
token-by-token during parse and runs the checker hook at publish time
(`C-DEFHOOK`, `EM-COMPILE-PUBLISH-HOOKED`) — after the body is compiled. Width-
aware lowering needs each op's operand widths BEFORE emitting it, so the colon
pipeline for layout-touching bodies must become check-first: capture body, run
`CHECK` (fills the width facts), then emit consuming the facts by body token
index. The Gforth bootstrap mirror follows the same contract. Emitters must not
lower from these facts until `LAYOUT-PUSH-FIELDS` expansion lands in the same
slice — the facts carry the registry logical width, which only then equals the
physical width.

**Storable layouts S1 (dot habu-checker-capability-typed-a480c423).** `!`/`@`
through a `ptr family<..>` address move the pointee's whole logical value for
the width-1 (enum) tier: the ADDRESS type carries the family identity — a bare
`ptr a` layout store, an `n` into an enum slot, a mismatched family, and an
enum fetched back as `n` all keep the item-12 reject — and a var may bind a
width-1 non-linear layout pointee under a ptr spine (`CUR-STRICT` context), so
a checked accessor certifies `( -- ptr color ) VAR-NAME` against the
variable's `-- ptr a` row. The compiled one-cell `!`/`@` are already the exact
lowering at W = 1, so the change is checker-only. W > 1 store/fetch waits for
the S2 width-aware engine legs; possibly-linear (incl. open-arg) pointees stay
fail-closed until TFAM 11 whole-bundle counting. Pinned in
`test/type-decl-suite.f` (TDS1-*).

---

### 17.1 Typed locals for family types (slice 1)

A `{: x:fam :}` annotation accepts a bare arity-0 family tail, resolved with
signature scope. An enum-tier layout (W=1 sum/enum, incl. a single-field
product) asserts the family's one-cell hidden term: the `:}` bind unifies the
captured bundle's tag term against it (wrong family = standard `E-MISMATCH`
with family fields; a scalar operand or a scalar-annotated bundle rejects the
same way), and a read restores the exact bound term — family id intact, so
`MATCH`/derived words work on local reads. An arity-0 CELL family asserts its
nominal scalar exactly as a signature would. Parametric spellings
(`x:fam<..>`), arity>0 tails, and W>1 layout annotations stay fail-closed as
named unknown-annotation rejects (their slices are tracked on the typed-locals
dot); bare (unannotated) locals keep the item-12 wide-bundle behavior
unchanged, and linear layouts still never expand into locals.

## 18. Width and parameter kinds

Add width metadata.

Width function:

```text
WIDTH(T-CON)              = 1
WIDTH(T-VAR:cell)         = 1
WIDTH(ptr τ)              = 1
WIDTH(cell-family<...>)   = 1
WIDTH(product<...>)       = sum field widths
WIDTH(sum<...>)           = max variant payload widths + tag width
WIDTH(enum)               = tag width
WIDTH(boxed<...>)         = 1
```

v1 should keep type parameters cell-kinded:

```forth
SUMTYPE result 2
```

means:

```text
a: cell
b: cell
```

This rejects:

```forth
option<result<n,n>>
```

because `result<n,n>` is a layout value, not a cell type.

Later syntax:

```forth
SUMTYPE option 1
  PARAM a layout
  VARIANT none   ;VARIANT
  VARIANT some a ;VARIANT
;SUMTYPE
```

could allow layout-polymorphic parameters.

Store parameter kinds now even if v1 exposes only cell parameters.

---

## 19. Linear/resource interaction

Sums must respect Habu’s linear discipline.

If a variant may contain a linear payload, then the sum itself must be treated as linear.

Examples:

```forth
option<own>
result<ptr u8,own>
```

Rules:

1. Constructing `OPTION:SOME` consumes the linear payload and produces a linear option.
2. Matching a linear option consumes the option.
3. The `some` branch receives the linear payload exactly once.
4. The `none` branch receives no payload.
5. Dropping a linear sum is rejected unless an explicit destructor/match consumes payloads correctly.

Required helpers:

```forth
LAYOUT-LINEAR?       ( type -- bool )
LAYOUT-LINEAR-COUNT  ( row -- n )
```

`MATCH` must be a checker control form partly because it is the only sound way to refine linear payloads per variant.

---

## 20. Renderer compaction

Internal rows contain hidden physical fields.

Without compaction, diagnostics would show:

```text
@result.slot0<ptr u8,n> @result.tag<ptr u8,n>
```

User-facing diagnostics should show:

```text
result<ptr u8,n>
```

Add row compaction:

```text
scan row
if consecutive cells match a registered layout family pattern:
  render logical family<args>
  skip physical cells
else:
  render ordinary type
```

This is rendering only. Checker correctness should not depend on renderer compaction.

---

## 21. Scope, rollback, and snapshots

The new registries participate in checker rollback and image snapshotting.

### 21.1 Transactional rollback-frame stack (PLAN item 3)

Rollback is a depth-safe **frame stack**, not single save slots. `CHECKER-SCOPE-START`
and `CHECK-CANDIDATE-START` push a frame; `CHECKER-SCOPE-DONE` and
`CHECK-CANDIDATE-DONE` pop it. Both a *rejected* scoped load and a *successful*
candidate probe roll back, so a failed family declaration cannot poison later
checks, and nested candidates/scopes (all-errors replay, preverify,
`CHK-RUN-STATIC-LINTS` inside `CHK-RUN-SCOPED`) never overwrite a parent frame.

Each frame saves every mutable high-water mark. `checker.f` owns the core frame
(`RBF-REC`, `RBF-PUSH`/`RBF-POP`):

```text
UEND  NORET-END  SYM-N  SYM-STR-U  CTN  CT-STR-U  LIN-NDECL
VREC-N  VREC-FIELD-N  VREC-NODE-N  VREC-STR-U  CHK-CAND  VSIG
CHECKER-PACKAGE-MODE  CHECKER-PACKAGE-U  (+ package-name bytes)  DFER-END
```

The TFAM/SUMV/SCHEMA registries hang parallel frames off the
`REG-EXT-RB-SAVE-XT` / `REG-EXT-RB-RESTORE-XT` hooks that `type-family.f` and
`type-schema.f` install, pushed/popped in lockstep with the core frame:

```text
TFAM-N  TF-STR-U  TF-PK-N  SUMV-N  PF-N  LAY-N        (TFAM-ROLLBACK-SAVE/RESTORE)
SCH-N   SCH-ROOT-N                                    (SCHEMA-ROLLBACK-SAVE/RESTORE)
```

Entry-retirement rules:

- **TFAM/SUMV/PF/LAY** use linear scans keyed on `(package, tail)` — no separate
  hash index — so restoring the counter *is* entry retirement: `*-FIND` only scans
  `[0,N)` and re-adding under the same name interns fresh at the restored pool end.
- **SYM** carries a hash index (`HIDX`). `RBF-POP` calls `HIDX-SYMS-RETIRE` to pop
  the retired bucket rows *before* rewinding `SYM-N`, so a retired signature cannot
  be found post-rollback and a reused id gets zeroed cache cells.
- **DFER-END** rewinds and `DFER-TERM` re-terminates the scan. The deferred-target
  cache (`HIDX-DFR`) is kept honest by `HIDX-DFR-SYNC`/`HIDX-DFR-DEP+`: a cached
  answer records the `DFER-END` it depends on and is flushed (epoch bump) when a
  rollback rewinds below it, mirroring the existing `HIDX-EFF`/`HIDX-CTL` sync.

### 21.2 Snapshot persistence

Persistence helpers bake each grown store into fresh image DATA:

```forth
TFAM-SNAPSHOT-PERSIST
SCHEMA-SNAPSHOT-PERSIST
```

They run through the checker's `REG-EXT-PERSIST` hook, on the same snapshot
preparation path that persists concrete types, value records, symbols, user
signatures, and no-return metadata. That hook also drops the transient rollback
frame arenas back to their baked boot stores (`RBF-SNAP-RESET`,
`TFAM-RBF-SNAP-RESET`, `SCHEMA-RBF-SNAP-RESET`) — frames are process-local and
always at depth 0 at snapshot time, like the `HIDX` mapping.

---

## 22. Runtime layout policies

### 22.0 The `POLICY` header clause

A family selects its physical representation with an optional `POLICY <name>`
clause on its declaration header — after the arity on a `SUMTYPE`/`PRODUCT`, and
after the name on an `ENUM`, before the first `VARIANT`/`FIELD`:

```forth
SUMTYPE option 1 POLICY stack-cell-tag
  VARIANT none   ;VARIANT
  VARIANT some a ;VARIANT
;SUMTYPE
```

`POLICY` is a reserved token: it may not name a family, variant, or field.

Policy is bound **per family**, chosen once at declaration; it is never a
per-use-site decision. A missing clause defaults to `stack-cell-tag` (§22.1).

v1 grammar surface (item 16 foundation):

- `stack-cell-tag` — accepted; the universal default, the only policy v1 lowers.
- `packed-tag`, `niche-null`, `boxed` — recognised policy names, but the grammar
  rejects them today with `layout policy not yet supported` (§24). They ship as
  separate checked extensions, each with constructor/match/stack-op/invalid-tag
  tests, before being exposed publicly — a physical-layout policy must not be
  selectable before its lowering support exists (PLAN item 16 risk).
- any other token (including `custom`, a v1 non-goal even though the `LAY-*`
  registry range admits `TL-CUSTOM`) rejects with `unknown layout policy` (§24).
- a bare `POLICY` with no following name rejects with `missing layout policy
  name`.

Every reject is transactional: the family row and any layout state roll back.

### 22.1 Default: `stack-cell-tag`

Universal v1 representation:

```text
M payload cells + 1 tag cell
```

This is the default for all sums/enums.

### 22.2 Packed memory layout

Keep stack representation as cells, but allow a memory ABI descriptor:

```text
tag-byte-width = u8 | u16 | u32 | cell
payload-offsets
alignment
size
```

This matters later for arrays of ADTs, GPU buffers, and ABI-stable structs.

**Descriptor computation (`PACKED-DESC`, `src/core/type-family.f`).** Packed
keeps the *stack* representation as cells — the stack width `W` is identical to
`stack-cell-tag` (§4/§18), so constructors, `MATCH`, and layout-aware stack ops
lower exactly as the default; packed adds *only* the memory descriptor. Because
v1 payloads are cell-kinded (§4: `slot0 .. slot(M-1) tag`, `M` = `TFAM-SLOTS`
cells), the only field packed narrows is the **tag**:

```text
tag-byte-width = PACKED-NARROW(variant-count)
   K ≤ 256      → 1 (u8)     \ tags 0..K-1
   K ≤ 65536    → 2 (u16)
   K ≤ 2^32     → 4 (u32)
   else         → 8 (cell)
   enum/sum only; products carry no tag → 0
```

Payloads stay `CELL`-wide and `CELL`-aligned, so per-field byte offsets are
implicit (`slot i` at byte `i * CELL`) and need no offset table — `size`,
`alignment`, and `tag-byte-width` fully specify the v1 ABI. The tag is placed
**after** the payload, matching the stack order. The descriptor is:

```text
payload-bytes = M * CELL
alignment     = CELL when M > 0, else tag-byte-width (byte for the empty case)
size          = align_up(payload-bytes + tag-byte-width, alignment)   \ array stride
```

Examples: `ENUM` of 3 → `size 1, align 1, tagw 1`; `SUMTYPE` of 2 variants,
`M=1` → `size 16, align 8, tagw 1`; `PRODUCT` of two cell fields → `size 16,
align 8, tagw 0`. These land in the `LAY-*` registry (`LAY.SIZE/ALIGN/TAGW`) —
this is the ABI the maki store/fetch capability reads; habu defines it, the
capability marshals against it. A mixed narrow-width payload tier (an explicit
`payload-offsets` table) is a later refinement. The descriptor is pure
compile-time metadata: no heap, no per-value runtime cost.

Staging: `PACKED-DESC` computes the descriptor for any family regardless of its
declared policy; the grammar still **rejects** `POLICY packed-tag` until a later
sub-slice flips the accept and wires `PACKED-DESC` → `LAY-ADD` at declaration.

### 22.3 Niche optimization

Later:

```forth
SUMTYPE option 1 POLICY niche-null
  VARIANT none   ;VARIANT
  VARIANT some nonnull-ptr<a> ;VARIANT
;SUMTYPE
```

Representation:

```text
one cell
```

`none` is null. `some` is non-null.

Do not make this implicit for arbitrary pointers. Require a non-null type or capability.

### 22.4 Boxed layout

Later:

```forth
SUMTYPE tree 1 POLICY boxed
  VARIANT leaf a ;VARIANT
  VARIANT node ptr tree<a> ptr tree<a> ;VARIANT
;SUMTYPE
```

Stack representation:

```text
ptr tree-box<a>
```

Use this for recursive or large ADTs.

Do not start with boxed layout.

---

## 23. Syntax examples

### Result

```forth
SUMTYPE result 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE

: OK-PTR ( ptr u8 -- result<ptr u8,n> )
  RESULT:OK ;

: ERR-N ( n -- result<ptr u8,n> )
  RESULT:ERR ;

: RESULT>BOOL ( result<ptr u8,n> -- bool )
  MATCH result
    ok OF
      drop true
    ENDOF
    err OF
      drop false
    ENDOF
  ;MATCH ;
```

### Option

```forth
SUMTYPE option 1
  VARIANT none   ;VARIANT
  VARIANT some a ;VARIANT
;SUMTYPE

: FIND ( n -- option<ptr u8> )
  dup 0 = IF
    drop OPTION:NONE
  ELSE
    LOOKUP OPTION:SOME
  THEN ;

: USE-OPTION ( option<ptr u8> -- n )
  MATCH option
    none OF
      0
    ENDOF
    some OF
      \ ptr u8
      drop 1
    ENDOF
  ;MATCH ;
```

### Enum

```forth
ENUM color
  red
  green
  blue
;ENUM

: PICK-COLOR ( n -- color )
  drop COLOR:GREEN ;

: COLOR>CODE ( color -- n )
  MATCH color
    red OF
      0
    ENDOF
    green OF
      1
    ENDOF
    blue OF
      2
    ENDOF
  ;MATCH ;
```

Rejected:

```forth
: BAD-COLOR ( -- color )
  0 ;
```

because `n` is not `color`.

---

## 24. Diagnostics

Required diagnostics:

```text
unknown type family: result
wrong arity: result expects 2 args, got 1
hidden field type is not public
bad sum declaration: duplicate variant ok
bad sum declaration: empty sum
bad enum declaration: duplicate variant red
bad match: expected sum or enum value on stack
bad match: family mismatch
bad match: unknown variant
bad match: duplicate variant
bad match: missing variant err
bad match: branch output mismatch
bad constructor payload: expected ptr u8, actual n
layout type not allowed in cell-only parameter
linear payload requires explicit match/destructor
layout policy not yet supported
unknown layout policy
missing layout policy name
invalid layout policy for recursive sum
```

The layout-policy rejects above (`E-TDECL-POLICY`) are top-level declaration
diagnostics: they carry the offending policy token and ride the same
declaration-shaped packet as every other `SUMTYPE`/`ENUM`/`PRODUCT` reject (a bad
policy on a `SUMTYPE` renders `bad sumtype declaration '<name>': layout policy
not yet supported at '<policy>'`).

`invalid layout policy for recursive sum` (`E-TDECL-RECURSIVE`) fires when a
variant/field payload names the family being declared: a **direct** self-family
reference (inline `tree<a>`, `ptr tree<a>`, a bare `tree`, or a product self-field)
makes the family recursive, which only the boxed policy can represent (its pointer
indirection breaks the width cycle). Since packed/niche/boxed all reject at the
`POLICY` clause, every family reaching payload parsing is `stack-cell-tag`, so a
self-reference always rejects here; the boxed accept slice will route a boxed
family's self-reference to a pointer layout before this reject. Mutual recursion
(`A` → `B` → `A`) needs a schema cycle walk and is a later boxed sub-slice; this
covers the direct case only.

Diagnostics should show logical types, not hidden fields.

Match/construct rejects carry stable machine codes (item 9 slice 4):
`E-MATCH-UNKNOWN-FAMILY`, `E-MATCH-FAMILY-KIND`, `E-MATCH-SCRUTINEE`,
`E-MATCH-FAMILY-MISMATCH`, `E-MATCH-UNKNOWN-VARIANT`,
`E-MATCH-DUPLICATE-VARIANT`, `E-MATCH-MISSING-OF`, `E-MATCH-NONEXHAUSTIVE`
(with a `missing_variants` JSON field listing the unhandled variant names in
declaration order), `E-MATCH-STRAY`, `E-MATCH-UNTERMINATED`, `E-MATCH-DEPTH`,
`E-MATCH-QUOTATION`, `E-MATCH-OPEN-ARGS`, `E-MATCH-BRANCH-JOIN`, and
`E-CONSTRUCT-UNKNOWN-FAMILY`, `E-CONSTRUCT-FAMILY-KIND`,
`E-CONSTRUCT-UNKNOWN-VARIANT`, `E-CONSTRUCT-UNTERMINATED`. Each carries a
repair class and suggestion through the standard diagnostic JSON, so repair
packets consume them with no schema change.

The `MATCH` reject codes above are compile/check-time. The one *runtime* ADT
diagnostic is the compiled invalid-tag fallback (item 10 slice 3): every lowered
`MATCH` ends with a self-contained die that a well-typed scrutinee never reaches.
A forged tag (only reachable through a `TRUSTED:` constructor that fabricates an
out-of-range tag) writes `hb: bad <family> tag\n` to fd 2 with the family name
copied INLINE into the compiled word — never a live pointer into the growable,
relocatable `TF-STR` pool — and exits `E-BAD-TAG` (process exit status 85,
`src/habu/layout.f`). There is no normal continuation past the die.

Top-level declaration diagnostics are not fake word-definition diagnostics. A
bad `SUMTYPE` or `TYPEFAMILY` reports a declaration-shaped packet with its source
span and ADT fields; it does not invent a declared stack effect or require
definition-only fields such as `definition_source`, `source_excerpt`,
`return_stack`, `expected`, or `actual`.

Example:

```text
habu: in RESULT>CODE: at ;MATCH
  branch output mismatch
  ok branch leaves:  ptr u8
  err branch leaves: n
```

---

## 25. Tests

### 25.1 Type-family registry tests

- `TYPEFAMILY foo 2` registers `foo`.
- `foo<n,n>` parses.
- `foo<n>` is rejected for wrong arity.
- unknown `bar<n>` is rejected.
- existing PTX parametric types still parse after migration.

### 25.2 Sum declaration tests

- `SUMTYPE result 2 ...` registers family and variants.
- an arbitrary third sum such as `packet<a,b>` registers, constructs, and
  matches so the implementation is not result/option-specialized.
- duplicate variant is rejected.
- empty sum is rejected.
- arity greater than the old `PARAM-MAX-ARGS` parses through growable schema
  storage.
- payload with unknown type is rejected.

### 25.3 Constructor tests

Accepted:

```forth
: T ( n -- result<n,n> ) RESULT:OK ;
```

Rejected:

```forth
: T ( n -- result<ptr u8,n> ) RESULT:OK ;
```

Accepted:

```forth
: T ( -- color ) COLOR:RED ;
```

Accepted:

```forth
: T ( ptr u8 -- packet<ptr u8,n> ) PACKET:DATA ;
```

Rejected:

```forth
: T ( -- color ) 0 ;
```

### 25.4 Match tests

Accepted exhaustive match:

```forth
: T ( result<n,n> -- n )
  MATCH result
    ok OF ENDOF
    err OF ENDOF
  ;MATCH ;
```

Rejected non-exhaustive match:

```forth
: T ( result<n,n> -- n )
  MATCH result
    ok OF ENDOF
  ;MATCH ;
```

Accepted generic third-family match:

```forth
: T ( packet<ptr u8,n> -- n )
  MATCH packet
    data OF drop 1 ENDOF
    code OF ENDOF
  ;MATCH ;
```

Rejected duplicate branch:

```forth
: T ( result<n,n> -- n )
  MATCH result
    ok OF ENDOF
    ok OF ENDOF
    err OF ENDOF
  ;MATCH ;
```

Rejected branch join:

```forth
: T ( result<ptr u8,n> -- n )
  MATCH result
    ok OF ENDOF
    err OF ENDOF
  ;MATCH ;
```

### 25.5 Runtime invalid-tag test

Use a checked test-only object/AOT entry that enters the generated `MATCH` test
with raw physical stack cells. The test-entry support must seed payload cells
and call the generated helper in checked Habu using only the existing
image-writer trust rows. Do not introduce any new ADT `TRUST`, `TRUSTED:`,
`set-check`, or `TRUSTED.md` row to forge payload slots plus an invalid tag:

```text
payload-slot0 ... payload-slotN invalid-tag
```

Then execute generated `MATCH` helpers and assert they die on the invalid-tag
path for both the native self-hosted compiler output and the Gforth-recovered
bootstrap output. Emitted no-continuation sequence checks are additional
evidence, not a substitute for executing the bad-tag path. Cover one-payload sums, wider
max-payload sums, and zero-payload enum/sum layouts, including at least one
arbitrary family not named result/option/color, so fallback cleanup cannot
silently drop only the tag or only the first payload cell. AOT closure roots and
object-cache metadata include the selected entry, helper root, seeded stack
cells, layout/test mode, ABI, and source digest; stale normal-`MAIN` objects
cannot satisfy preseeded bad-tag runs, and helpers cannot be stripped by a
`MAIN`-only closure.

### 25.6 Layout-aware stack op tests

For `result<n,n>` and other width > 1 families:

```forth
: T ( result<n,n> -- result<n,n> result<n,n> ) dup ;
: T ( result<n,n> -- ) drop ;
: T ( result<n,n> n -- n result<n,n> ) swap ;
```

Check physical lowering for every generic stack surface: `dup`, `drop`, `swap`,
`over`, `nip`, `rot`, `-rot`, `tuck`, `2dup`, `2drop`, `2swap`, `2over`,
locals, return-stack transfers, constants, `depth`, `.s`, optimized native
shuffles, fallback spilled calls, and the Gforth bootstrap mirror. `?dup` must
reject layout values until a checked truthiness/niche policy exists. Hidden
fields must not bind to ordinary primitive effects, optimized one-cell lowering,
scalar control predicates (`if`, `while`, `until`, `case`, `of`, `do`, `?do`,
`+loop`), arithmetic/comparison/unary/float optimized paths (`VOP*`, `VCMP`,
`VUN`, `FOP`), higher-order effect application (`execute`, `catch`, defer calls,
and combinators), nested `evaluate`, `catch`/`throw`, `run-in-stack` frame
save/restore paths, or field coercions outside constructors and `MATCH`.

---

## 26. Implementation phases

### Phase 1: Type-family registry

Implement:

```forth
TFAM-ADD-INTERNAL
TFAM-FIND-INTERNAL
TFAM-KIND@
TFAM-ARITY@
TFAM-LAYOUT?
TYPEFAMILY
```

Replace hard-coded parametric constructor lookup with internal TFAM lookup.

Register existing constructors during boot.

Expected result: no semantic behavior change.

### Phase 2: Layout families and hidden fields

Implement:

```forth
LAYOUT-TYPE?
LAYOUT-WIDTH
LAYOUT-PUSH-FIELDS
HIDDEN-FIELD-TYPE
PUSH-LOGICAL
```

Change stack signature parsing to use `PUSH-LOGICAL`.

Reject public use of hidden names.

Add renderer compaction.

### Phase 3: `SUMTYPE`

Implement:

```forth
SUMTYPE
VARIANT
;VARIANT
;SUMTYPE
```

Add:

```forth
SUMV registry
schema-node registry
variant schema parser
family finalization
max-payload-width computation
```

Generate constructor metadata and internal token handlers only. Do not publish
user-callable layout constructors until layout-aware stack operations and
primitive isolation can preserve whole bundles.

Do not reserve or replace `ENUM` in this phase; legacy `ENUM` is migrated in the
later enum-family phase.

### Phase 4: Layout-aware stack operations

Make checked generic stack words operate on logical values before any public ADT
surface can expose hidden fields.

Lower:

```forth
dup drop swap over nip rot -rot tuck 2dup 2drop 2swap 2over
```

according to logical layout width. Reject `?dup` for layout values until a
checked truthiness/niche policy exists.

This is the phase that makes ADTs safe to expose.

### Phase 5: Checked `MATCH`

Add checker control-flow support:

```forth
CF-MATCH
MATCH
OF
ENDOF
;MATCH
```

Implement:

```text
payload refinement
branch accumulation
exhaustiveness checking
duplicate checking
family mismatch checking
invalid-tag no-return edge
```

### Phase 6: Compiler lowering for `MATCH`

Emit:

```text
tag peek
compare/branch chain
branch prologues
branch epilogues
invalid-tag fallback
join label
```

### Phase 7: Product families

Implement:

```forth
PRODUCT
FIELD
;PRODUCT
```

Then decide whether existing `VALUE-RECORD` becomes sugar over `PRODUCT` or remains as a compatibility feature.

Landed (item 15): the grammar plus generated `PKG:MAKE`/`PKG:UNMAKE`; decided
by evidence that `VALUE-RECORD` remains a typed compatibility feature (§9.4).

### Phase 8: Layout policies

Add optional policies:

```forth
POLICY stack-cell-tag
POLICY packed-tag
POLICY niche-null
POLICY boxed
```

Keep default as `stack-cell-tag`.

---

## 27. Non-goals for v1

Do not implement these in the first pass:

- recursive unboxed ADTs;
- implicit niche optimization;
- fully layout-polymorphic type parameters;
- packed memory ABI for GPU arrays;
- automatic deriving of equality/order/hash — REVISED: opt-in `DERIVE eq`
  (S1+S2, §9.3.1) and `DERIVE hash` (S3, §9.3.2) landed for enums, payload
  sums, and products; `order` remains deferred;
- user-defined custom layout code;
- public construction from raw tags;
- unsafe enum casts.

These can come later once the core family/layout/match mechanism is correct.

---

## 28. Final design decision

Use **type families** as the generic mechanism.

Implement **sum families**, **enum families**, and **product families** as layout-bearing type families.

Do not leave `Result` halfway implemented as a parsed type expression or a quotation-based combinator.

A complete Habu ADT implementation must provide:

```text
1. logical type syntax
2. hidden physical layout
3. generated constructors
4. checked pattern matching
5. runtime tag validation
6. exhaustiveness checking
7. branch-local payload refinement
8. branch row joining
9. layout-aware stack operations
10. clean logical diagnostics
```

The user writes algebraic data types. The checker owns refinement and exhaustiveness. The compiler emits efficient stack-cell layout and real tag checks.

That is the whole job.
