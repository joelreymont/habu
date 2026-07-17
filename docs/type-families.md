# Habu Type Families and Algebraic Data Types

**Proposed repository path:** `docs/type-families.md`  
**Status:** PLANNED design — not yet implemented (owning epic `habu-epic-one-structure-04f9804f`)  
**Primary goal:** implement generic, efficient, checked algebraic data types in Habu without turning `Result` into a one-off special case.

> **This document is a design specification for a surface that does not yet
> exist in the shipped engine.** It describes the planned MODEL-CAD-V2 unified
> `STRUCTURE … ;STRUCTURE` / `ENUM … ;ENUM` grammar and its
> `E-REMOVED-TYPE-SYNTAX` tombstones. Neither ships today: loading a `STRUCTURE`
> declaration fails `E-UNDEFINED: STRUCTURE` (exit 70), and
> `E-REMOVED-TYPE-SYNTAX` appears nowhere in `src/`. The live composite-type
> surface is still `TYPEFAMILY`, `SUMTYPE`, `PRODUCT`, `ENUM`, `VALUE-RECORD`,
> and `BEGIN-STRUCTURE`; none of them is removed. The single source of truth for
> what actually ships — including the real error codes and the positional
> `SUMTYPE` / bare-names `ENUM` split — is [`docs/forth.md`](forth.md)
> § Structures And Enums. The cutover is owned by epic
> `habu-epic-one-structure-04f9804f` (implementation chain in
> `MODEL-CAD-V2-PLAN.md` § 3.1). Everything below specifies that target design in
> normative voice; read it as "will", not "does".

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

These reserved-name rejects split into two diagnostic codes by mechanism, and
the split is deliberate — do not "fix" it by folding `ptr` into the reserved-name
class. A **reserved concrete-cell or grammar token** — a single-letter signature
var `a`..`z` (so the builtins `n`/`f`/`r`), `field`, atom prefixes, control
words, and grammar keywords — is caught by the name gate (`TDECL-RESERVED?`,
`src/core/sumtype.f`) *before* any family row is created, and rejects
`E-TDECL-NAME` (7110, "reserved name"). A token that is itself a **live
registered parametric family tail** — `ptr` (seeded arity 2), `span`, `matrix`,
and the other cell families seeded in `src/core/type-family.f` — is not a
reserved-name token at all: it passes the name gate and instead collides at
registration (`TFAM-DECL`), rejecting `E-TFAM-DUP` (7102, "duplicate family").
So `TYPEFAMILY n 0` reports reserved-name while `TYPEFAMILY ptr 0` reports
duplicate-family, and both are correct: `ptr` genuinely *is* a registered family,
so redeclaring it is a real same-scope duplicate, not a reserved-name shadow.
(`test/type-decl-suite.f` pins both codes.) The one case where a live family tail
still reports `E-TDECL-NAME` is a family declared *inside a package* whose tail
shadows a *global* family — the in-package scope does not own the global row, so
the diagnostic is "shadows a global family"; only a top-level redeclaration of a
global tail is the same-scope duplicate.

Use the generic term internally and the specific terms externally:

| Public concept | Internal kind | Example |
|---|---:|---|
| Ordinary parametric type | `cell-family` | `span<space-global,t,e>` |
| Product type / by-value record | `product-family` | `pair<a,b>` |
| Sum type / tagged union | `sum-family` | `result<a,b>` |
| Enum | `enum-family` | `color` |
| Capability / proof / evidence token | `evidence-family` | `aligned<ptr,t,align-16>` |

The sole public composite/type-family declaration blocks are:

```forth
STRUCTURE pair 2 ... ;STRUCTURE
ENUM result 2 ... ;ENUM
```

`STRUCTURE` covers ordinary cell families, pointer-layout records, and by-value
products through one typed field schema. `ENUM` covers payloadless enums and
payload-bearing sums; the compiler selects the payloadless representation when
every variant has no fields. All prior declaration words are to be removed by
the cutover, not aliased (they are still live definers today — see the banner
above and `docs/forth.md` § Structures And Enums).

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

## 2. Normative hard-cutover grammar

### 2.1 Lexical grammar

```text
structure-decl = STRUCTURE type-name arity header-clause* field* ;STRUCTURE
enum-decl      = ENUM type-name (full-enum | compact-enum) ;ENUM
full-enum      = arity header-clause* variant-block+
compact-enum   = compact-variant+

header-clause  = POLICY policy-name
               | DERIVE derive-name+

field          = FIELD field-name type-expr

variant-block  = VARIANT variant-name field* ;VARIANT
compact-variant = variant-name
```

`type-name`, `field-name`, `variant-name`, and every family tail inside a
`type-expr` are lowercase. Package qualifiers remain uppercase project package
names. `arity` is mandatory for `STRUCTURE` and full `ENUM`; arity parameters
are the positional lowercase tokens `a`, `b`, and so on. Compact `ENUM`
omits both arity and header clauses and is implicitly arity zero. Header clauses
occur after the arity in full declarations. A clause may occur at most once;
`DERIVE` features are order-independent and duplicates reject.

The compact `ENUM name v0 v1 ... ;ENUM` form is legal only when every variant
is payloadless. The first token after the enum name selects the form: a decimal
arity selects full block mode; a bare variant selects compact mode. A header
clause without a preceding arity is a malformed full declaration. Bare variant
tails and `VARIANT` blocks cannot mix. A block variant accepts only zero or
more named `FIELD` clauses before `;VARIANT`; anonymous payload tokens are
invalid.

Examples:

```forth
STRUCTURE point 0
  FIELD x n
  FIELD y n
;STRUCTURE

ENUM color red green blue ;ENUM

ENUM message 0
  VARIANT quit ;VARIANT
  VARIANT move
    FIELD x n
    FIELD y n
  ;VARIANT
  VARIANT write
    FIELD text ptr u8
    FIELD len n
  ;VARIANT
  VARIANT change-color
    FIELD red n
    FIELD green n
    FIELD blue n
  ;VARIANT
;ENUM
```

### 2.2 `STRUCTURE` semantics

A structure is one nominal, single-shape type family. A zero-field declaration
is an opaque one-cell family and publishes no generic raw constructor,
destructor, cast, or field operation. This is the authority-safe replacement
for `TYPEFAMILY`. A declaration with fields is a product; its fields are the
sole schema source for checker expansion, stack width, storage size, alignment,
field offsets, codecs, reflection, snapshot rows, and AOT metadata.
Field order is declaration order, deepest stack field first. Field types may be
arity parameters, concrete checker types, pointers, or closed nested families.
Direct recursion, unknown types, duplicate fields, open nested
applications, and layout-policy cycles reject transactionally.

A public declaration with fields publishes one closed generated package:

```text
POINT:MAKE      ( field... -- point )
POINT:UNMAKE    ( point -- field... )
POINT:X         ( ptr point -- ptr n )
```

`FAMILY:FIELD` is the sole field-address spelling. For a generic declaration its
effect is `( ptr family<a,...> -- ptr field-type )`; callers use the field
type's normal checked load/store operations. The generated words are ordinary
checked words or compiler-certified metadata operations; none is a raw cast.
The accessor is available only where the storage/layout policy gives an
addressable field.
Private declarations expose generated operations only inside their owning
package. Generated packages are sealed after publication.

Former `TYPEFAMILY` declarations become zero-field structures. Authority-bearing
ids keep raw representation refinement and decoding private to the validating
owner; there is no universal `n` cast or generated `MAKE`. Former
pointer-layout structures use the same field schema and typed address
projections instead of byte-size-threading definers.
Former `VALUE-RECORD` and `PRODUCT` declarations become ordinary structures;
there is no second record registry or compatibility layer.

### 2.3 `ENUM` semantics

An enum is one nominal sum-family. Every variant has a named structure-shaped
payload; a payloadless variant has an empty field list. The compiler derives
the kind from the schema:

- all variants payloadless: enum layout;
- any variant with fields: tagged-sum layout.

The distinction is metadata, never public syntax. Physical width is the
selected policy's tag plus the widest variant payload. Padding is canonical
zero. Nested structures and enums contribute their full physical widths.

A public declaration publishes one closed generated package with one checked
constructor per variant (`MESSAGE:QUIT`, `MESSAGE:MOVE`, and so on). Constructor
inputs and `MATCH` payload bindings follow field declaration order, deepest
first. Private constructors resolve only in the owning package.

Generated package spelling is unchanged by the cutover. Every package and
family segment uses the existing injective uppercase escape/join algorithm: a
top-level `point` derives `POINT`; `pxevid` inside `PX-PROBE` derives
`PX--PROBE-PXEVID`. Structure operation tails and enum variant tails retain
their declaration spellings after uppercase folding. This preserves existing
closed-package identity, snapshot rows, and AOT references while replacing the
declaration registry.

### 2.4 Unified field registry

Both declarations write one field registry keyed by:

```text
(family-id, optional-variant-id, field-tail)
```

`optional-variant-id` is absent for a structure field and present for an enum
payload field. A row stores declaration slot, schema root, physical width,
alignment, byte offset, visibility, and source span. Variant rows retain tag and
payload-start/count metadata, but payload order comes exclusively from their
field rows. Reflection enumerates field names and types in declaration order;
constructors, `UNMAKE`, `MATCH`, codecs, hashing, snapshots, and AOT metadata
all consume that same order. No product-field, value-record-field, anonymous
sum-payload, or pointer-layout field registry survives the cutover.

`src/core/type-field.f` is the shared provider. Its protected `TYPE-FIELD`
package owns nominal roles for family, variant, schema, slot, cell count, byte
layout, source span, visibility, field count, and committed `field-id` handles.
Linear `field-tx` and `field-draft` roles enforce the staged builder protocol.

The declaration-prefix friend surface builds a row through `OPEN`, `START` or
`START-VARIANT`, `SCHEMA`, `LAYOUT`, `SOURCE`, and `ADD`, then `COMMIT` or
`ROLLBACK`. `OPEN` accepts only a family owned by the lexically active package;
cross-package mutation throws `TYPE-FIELD:E-VISIBILITY`. Errors abort the whole
strict-LIFO transaction stack and restore row, copied-name, and draft
watermarks. `ADD` returns only the transaction token: a `field-id` is minted
only by `FIND`, `FIND-VARIANT`, or `EACH` after the outer commit publishes its
rows. A rolled-back slot therefore cannot leak a stale handle when reused.

`START` copies parser/name storage before canonicalization or any parser-adjacent
string operation. Names are canonical lowercase, case-insensitively unique
within the exact owner key, and may be one character; declaration and generated
operation names are reserved. Flags are exposed only through named public/private
and byte-addressable constructors.

Reflection is role typed: `COUNT`, `FIND`, `FIND-VARIANT`, `FAMILY@`,
`VARIANT?`, `VARIANT@`, `NAME`, `SCHEMA@`, `SLOT@`, `CELLS@`, `BYTE-OFF@`,
`BYTE-SIZE@`, `ALIGN@`, `FLAGS@`, `VIS@`, `SOURCE@`, and `EACH`. `FIND` throws
`E-ID` when absent and `E-VISIBILITY` when the family or field is hidden. `NAME`
copies into caller-owned storage; no arena pointer or raw id conversion escapes.

### 2.5 Header clauses and transactions

`POLICY` selects a registered layout policy. `DERIVE` requests generated
operations such as `eq`, `hash`, `order`, or canonical codecs. Both blocks use
the same registry transaction: parse and validate the entire declaration,
reserve names, register schema/layout metadata, generate operations, certify
them, seal the generated package, then publish atomically. Any failure restores
all family, schema, wordlist, signature, reflection, snapshot, and AOT rows.

### 2.6 Bootstrap cycle

The bootstrap cycle is removed rather than solved with a second declaration
machine. Records needed before the checker hook are private implementation
layouts with:

- named cell or byte offsets and a named record stride;
- ordinary accessor words, never a macro or definer;
- load-time assertions for every expected offset, stride, alignment, and
  pointer-field role; and
- exact native/recovery layout-parity tests.

These records have no family ids, reflection, constructors, parser, definer,
descriptor arena, adoption transaction, snapshot rows, or AOT rows. `CELL` is
owned by the earliest bootstrap constant layer rather than by legacy structure
support.

The canonical load order in both native and recovery sources is:

```text
utilities
checker private layouts
lower-certificate base
type-schema private layouts
type-family private layouts
render support
checker hook
shared field declarer arena
legacy core / unified STRUCTURE and ENUM consumer
remaining core
```

The final `STRUCTURE`/`ENUM` parser is therefore the only executable
composite/type-family declaration language and always runs through the
installed checker hook. There
is no cold parser, transient descriptor format, adoption path, bootstrap-only
source spelling, or raw public layout definer.

### 2.7 Removed syntax

> **Planned — not shipped.** In the current engine every token below is a live,
> heavily-used definer (`src/core/sumtype.f`, `roles.f`, `structures.f`,
> `enums.f`), and `E-REMOVED-TYPE-SYNTAX` exists nowhere in `src/`. The removals
> and tombstones described here take effect only when the cutover
> (`habu-epic-one-structure-04f9804f`) lands.

After cutover these words are to have no executable compatibility definition:

```text
BEGIN-STRUCTURE END-STRUCTURE +FIELD PTR-FIELD: CFIELD:
VALUE-RECORD END-VALUE-RECORD
TYPEFAMILY PRODUCT ;PRODUCT SUMTYPE ;SUMTYPE ENUM+ ENUM4+
```

The compiler is to keep only error tombstones so each removed token reports
`E-REMOVED-TYPE-SYNTAX` with its `STRUCTURE` or `ENUM` replacement. A tombstone
cannot define, replay, lower, or mutate metadata. Explicitly allowlisted
negative fixtures may contain removed spellings only as non-executable test
data. Inside new blocks, legacy
closers/field words, anonymous variant payloads, mixed compact/block variants,
and a missing arity on `STRUCTURE` or full `ENUM` reject at the exact token.
The final source lint requires zero occurrences in live executable or generated
source outside the tombstone table and explicitly allowlisted negative fixtures.

The exact source census and migration owner for every old surface is
`docs/census-type-dsl-cutover.md`.

### 2.8 Design position

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
ENUM result 2
  VARIANT ok  FIELD value a ;VARIANT
  VARIANT err FIELD error b ;VARIANT
;ENUM
```

Physical layout:

```text
slot0 tag
```

```forth
ENUM option 1
  VARIANT none ;VARIANT
  VARIANT some FIELD value a ;VARIANT
;ENUM
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
ENUM result 2
  VARIANT ok  FIELD value a ;VARIANT
  VARIANT err FIELD error b ;VARIANT
;ENUM
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
ENUM parse-result 1
  VARIANT ok FIELD value a ;VARIANT
  VARIANT err
    FIELD message ptr u8
    FIELD len n
  ;VARIANT
;ENUM
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

## 9. Pre-cutover declaration inventory

This section records the implementation being removed so registry and checker
behavior are not lost during migration. None of its declaration spellings is a
post-cutover public API. The normative grammar is §2.

### 9.1 Removed `TYPEFAMILY`

Pre-cutover ordinary one-cell parametric types used:

```forth
TYPEFAMILY span 3
TYPEFAMILY matrix 4
TYPEFAMILY gridctx 3
TYPEFAMILY tile 3
TYPEFAMILY uniform 1
```

This replaces hard-coded parser knowledge.

An arity-zero `TYPEFAMILY` is a package-scoped nominal cell kind:

```forth
package CAD-KIND
public
TYPEFAMILY design-id 0
TYPEFAMILY node-id 0
;package
```

Inside the package, signatures use `design-id`; outside, they use
`CAD-KIND:design-id`. The resolved family id is the identity, so an equally
spelled tail in another package does not unify. Ordinary typed `@` and `!`
preserve the family through `ptr CAD-KIND:design-id` storage.

Do not add universal `n` casts for authority-bearing ids. The allocator,
decoder, or table owner keeps any raw-representation refinement private and
validates its range, generation, schema, or provenance first. Use `DEFTYPE`
only when a global nominal plus its generated raw converter pair is the desired
contract.

### 9.2 Removed `SUMTYPE`

Pre-cutover syntax:

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

### 9.3 Pre-cutover payloadless `ENUM`

Pre-cutover syntax omitted the mandatory arity and could not carry named
payload fields:

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
ENUM color 0 DERIVE eq
  VARIANT red   ;VARIANT
  VARIANT green ;VARIANT
  VARIANT blue  ;VARIANT
;ENUM
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

### 9.4 Removed `PRODUCT`

Pre-cutover syntax:

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
legal, lowercase canon enforced, duplicates reject). Field and variant payload
types are positional letter params within arity, concrete cell types, `ptr T`,
or a closed, non-linear, arity-0 layout family. The family form becomes an
SC-APP carrying the resolved family id. Its physical width is the referenced
family's full `WIDTH`, so `PF.SLOT`, product width, sum padding, constructors,
destructors, and `MATCH` preserve nested layouts rather than counting the
schema root as one cell. Parametric, linear, and direct self-recursive families
reject; explicit parameterized application syntax is not yet part of the
declaration grammar. `MAKE`/`UNMAKE` consume and produce the field under its
family type, so same-width family swaps remain checker errors. Pinned in
`test/type-decl-suite.f` and `test/lower-txn-large.f`.

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

Hard-cutover verdict: `VALUE-RECORD` and `PRODUCT` are both subsumed by
`STRUCTURE`. Their distinct registries and coercions are migration inputs, not
compatibility contracts. The unified checker must preserve every sound
construction, destruction, projection, linearity, and nested-width invariant
before both old paths are deleted.

---

## 10. Hidden physical field types

Sections 10-25 describe the existing registry, checker, layout, and lowering
invariants that the unified implementation must preserve. Any old declaration
spelling shown there is pre-cutover evidence; translate it through §2 before
using it in source.

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

**Implementation status (PLAN items 7 and 12).** `PUSH-LOGICAL` is the
signature-parse seam. A closed non-linear sum/enum/product family expands to its
`W` checker-owned hidden physical fields in slot order, with the tag last and on
top; width-aware generic transport and compiler pass 2 preserve that bundle as
one logical value. An open family application stays one logical `T-PARAM` cell,
and a possibly-linear application stays fail-closed for transport until TFAM 11
provides whole-bundle linear accounting. Cell families remain one cell. The
public parser still rejects hidden `@family.slotN`/`@family.tag` names, so
physical fields cannot appear in public signatures even though the checker uses
them internally.

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
elided, keeping the compiled word minimal — the diagnostic and the `ENGINE-ERROR:BAD-TAG`
(85) exit are the observable contract (see §24). **The `bootstrap/cg/forth.fs`
stage0 mirror landed in slice 3b** (byte-identical fixpoint from the
Gforth-recovered engine), and slice 5 landed AOT/object persistence of matched
definitions: `tools/hb-build.f --preseed-entry NAME --preseed-seed HEX` builds a
selected non-`MAIN` entry that seeds a forged value-stack bundle before calling the
matched helper, so the persisted-then-restored artifact reaches the same
`ENGINE-ERROR:BAD-TAG` die; the entry/seed/mode axis is folded into the artifact key, the
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

**Implementation status (PLAN item 12, slices 1–3b).** Checked generic stack ops
treat a layout value as one whole logical bundle. Closed non-linear layouts are
physically expanded to `W` hidden row cells, and the compiler lowers transport
from checker-recorded width facts. The transport set — `dup drop swap over nip
rot -rot tuck 2dup 2drop 2swap 2over`, the return-stack transfers `>r r> r@
2>r 2r> 2r@`, and `{: :}` locals capture — moves the complete group under the
token-scoped `LAYOUT-XPORT` mode. `!` and `@` are the only value-touching
primitives with a separate typed-layout rule; other inspection keeps the
fail-closed boundary: `?dup`, scalar control predicates, arithmetic/compare/
unary, `execute`/`catch`/defer quotation operands, `constant`'s value pop,
`throw`, and hidden `@family.*` names in public signatures. A stack-preserving
quotation still passes a layout value through untouched, and `evaluate` stays
rejected in checked bodies. A layout whose args contain a linear con, or an
unresolved arg that may later become linear, cannot transport or cross memory;
identity flow remains legal until TFAM 11 supplies whole-bundle linear counting.

**Width facts for emitters (slice 2).** The checker computes logical widths per
§18 — `TFAM-WIDTH@ ( id -- n )` in the registry (sum: slots+tag, enum: tag,
product: field cells, cell/evidence: 1) and `T-WIDTH ( type -- n )` on resolved
type terms — and records, per checked definition, a width-fact table: one row
per LAYOUT operand of each transport op and locals capture, holding (body token
index, operand position 0=top, family-id, registry logical width). Absence of a
row means every operand is one cell. Query surface (checker-modeled, callable
from checked code): `WF-N@`, `WF-TOKIX@`, `WF-POS@`, `WF-FAM@`, `WF-WIDTH@`.
Typed layout `!`/`@` also record one row at operand position 0; this position
names the bundle width, not the one-cell address. The table is per-`CHECK`
scratch: valid from a definition's verdict until the next `CHECK`, never
persisted, never rolled back.

**Pass-2 ordering (slice 3b, landed).** The native compiler's first pass captures
the body and runs the checker hook at publish time. If `CHECK` records any wide
fact, `EM-P2-TRIGGER` replays the captured body with the fact table available;
the second pass selects width-aware lowering by body-token index and does not
register the definition twice. This replay supplies checker-before-emission
ordering without making every scalar definition pay for a separate parse.

**Storable layouts S1/S2 (dot habu-checker-capability-typed-a480c423, landed).**
Typed `!`/`@` through a `ptr family<..>` address move the pointee's whole closed
non-linear logical value. The address carries family identity: bare `ptr a`, a
mismatched family, scalar-to-layout storage, and layout-to-scalar fetch all
reject. Ordinary `variable`, `create`, pointer arithmetic, and a declared
`ptr a` result cannot refine to `ptr family`; only the sealed generative storage
boundary may introduce a family-typed pointer.

At `W = 1`, the ordinary scalar `!`/`@` instruction is already the exact
lowering. At `W > 1`, the checker records the operation's compile-time width and
pass 2 emits a fixed-width loop: store pops the typed address and writes
`slot0 .. tag` to ascending cell addresses; fetch reads ascending cells and
reconstructs `slot0 .. tag`, with the tag again on top. A zero-width product has
no addressable representation and rejects. Open or possibly-linear applications
also reject until TFAM 11 whole-bundle linear accounting can discharge their
ownership obligations.

`count LAYOUT-BUFFER NAME family<args>` owns backing capacity and publishes one
checked accessor `( n -- ptr family<args> )`. The family application must be
closed, non-linear, addressable, and non-zero-width; `count` must be positive
and its `count * width * CELL` extent must fit. Because count is a stack input,
named capacity constants remain the single source of truth. The buffer is zero-initialized,
indexing is fixed-stride, and either signed bound throws `E-LAYOUT-BOUNDS`.
Source generation completes before allocation, and a rejected generated
definition rolls the allocation back. This is the only typed-layout pointer
introduction form.

**Nominal scalars.** An arity-0 CELL family (a nominal scalar such as the
CAD-KIND ids) is also admitted, with width 1 and no variants: `LAYOUT-BUFFER`
is likewise the only checked introduction of `ptr <nominal-scalar>`. Inside a
pointee, a type variable may not absorb a nominal-scalar family outside the
armed generated-accessor window (`NOMPTR-BLOCK?`, the mirror of the layout
pointee-bind rule), so a plain `variable`, `create`, `data-base`, pointer
arithmetic, or a `ptr a` cast never certifies as the family pointer, while
value-position uses stay ordinary one-cell flow. Typed `!`/`@` take the same
memory arm as layouts with param-to-param rows but record no width fact:
every raw bit pattern is a valid value of a nominal-scalar family, so the
operation lowers as a plain scalar cell with no fetch-validation program, and
the zero image reads as family id 0 — a valid id, the same semantics as an
enum column's zero image reading as its first variant. Cells that need a
"no value yet" state keep a liveness guard beside the buffer, exactly as raw
cells did.

Zero initialization proves the initial image only. Untyped code can reconstruct
a DATA address and corrupt tags through a raw alias, so typed fetch validates
the root tag and every active nested tag before publishing the logical value.
Inactive SUM payload/padding is preserved and is not interpreted.

Every destination cell of a wide store executes the same two-band protected-
store guard as scalar `!`. Guarding only the base would be unsound because a
later cell could cross into a sealed band. The current lowering therefore fails
at the first protected destination; the focused later-cell/protected-byte
immutability runtime pin is tracked separately by
`habu-pin-wide-adt-31f1639c`.

`test/type-decl-suite.f` pins checker width facts, mismatched/scalar/linear/open
rejections, and W=2/W=3/W=4 memory values. `test/layout-buffer.f` pins the
generative storage boundary, raw-pointer rejection, bounds, stride, zero image,
and transactional allocation. `test/type-layout-lower-pending.f`
pins exact store/fetch instruction sequences and constructor-produced runtime
round trips. Recovery-chain parity includes the Gforth host mirror in
`bootstrap/cg/forth.fs`: its pass-2 width lookup, typed-fetch validator, and
wide store/fetch emitters build the current native source. The recovered
`hb-stdin` passes both suites plus compiler dispatch, bootstrap codegen,
signature-scan emitter, and seal-absence tests.

**Convenience storage definers (dot habu-nominal-storage-typed).** `TYPED-VARIABLE`
and `TYPED-BUFFER` add a sound uppercase surface for typed scalar/pointer storage
outside a fixed layout owner, built on the SAME generative boundary as
`LAYOUT-BUFFER` (`src/core/layout-buffer.f`; the armed generated-accessor window,
allocation, zero image, and transactional rollback are shared):

```forth
TYPED-VARIABLE NAME <type>          \ one typed cell,     accessor ( -- ptr <type> )
count TYPED-BUFFER NAME <type>      \ typed capacity,     accessor ( n -- ptr <type> )
```

The admissibility gate is `CHECKER-STORAGE-INFO`, a superset of the
`CHECKER-LAYOUT-INFO` gate that `LAYOUT-BUFFER` keeps unchanged. The two
capabilities stay distinct:

| Stored `<type>` | `LAYOUT-BUFFER` | `TYPED-BUFFER` / `TYPED-VARIABLE` |
|---|:---:|:---:|
| closed non-linear layout family (`res<n,n>`) | admit | admit |
| arity-0 nominal scalar (`CAD-KIND:node-id`) | admit | admit |
| closed typed pointer (`ptr fam`, `ptr res<n,n>`, `ptr ptr fam`) | reject | **admit** |
| open type var / bare `ptr a` / `ptr n` | reject | reject |
| quotation / linear value / hidden field | reject | reject |
| non-positive `count`, unresolved args, duplicate name | reject | reject |

A "closed typed pointer" is a `ptr` whose pointee chain bottoms out at a nominal
scalar or a closed non-linear layout family — the family identity a plain `ptr a`
could otherwise never re-acquire (the pointee-bind seal). The stored type may be
a `ptr* base` multi-token span (`TYPED-VARIABLE SLOT ptr target-kind`). Width is
one cell for scalars and pointers, and the registry width for a layout. Typed
constants still require a checked producer: a raw `n` cannot initialize a typed
`constant`, because `constant`/`variable`/`create` publish RAW effects
(`TVK-RAW`) that reject a nominal family in value position — the definers are the
sound alternative to that laundering, not a bypass. Same-family `!`/`@` through a
definer accessor certifies and executes; cross-family, `E-LAYOUT-BOUNDS` (index),
`E-LAYOUT-BUFFER` (admissibility/overflow), and `E-DUP-DEFINITION` (duplicate)
reject, and a rejected declaration rolls the allocation back and defines nothing.
The gate path is the verify-source scanner (`RECORD-TYPED-BUFFER` /
`RECORD-TYPED-VARIABLE` → `CHECKER-DEFTYPED-BUFFER` / `CHECKER-DEFTYPED-VARIABLE`),
mirroring `RECORD-LAYOUT-BUFFER`; `test/typed-storage-test.f` pins the surface.

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
ENUM result 2
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

Store parameter kinds now even though the hard-cutover grammar initially
exposes only cell parameters. A later parameter-kind header feature must extend
the one grammar; it may not add a third declaration form.

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

`TYPE-FIELD:SNAPSHOT-HOOK` chains that path and relocates the complete capacity
of every row, draft, transaction, and copied-name arena. Snapshot capture is
rejected while a field transaction or draft remains active. The focused suite
grows all four arenas beyond their boot capacities before persistence; the AOT
gate resolves and reflects the sixth committed field from the stripped image.

Stripped AOT persistence is narrower than full snapshot preparation. It copies
only the family, schema, and field runtime arenas into the compact DATA image,
then emits entry code that restores their fixed-address pointer, capacity, and
high-water cells before `MAIN`. Capturing the complete checker snapshot would
retain compiler work arenas and exceed the stripped image's bounded code/data
budget.

These hooks run through the checker's `REG-EXT-PERSIST` path, on the same snapshot
preparation path that persists concrete types, value records, symbols, user
signatures, and no-return metadata. That hook also drops the transient rollback
frame arenas back to their baked boot stores (`RBF-SNAP-RESET`,
`TFAM-RBF-SNAP-RESET`, `SCHEMA-RBF-SNAP-RESET`) — frames are process-local and
always at depth 0 at snapshot time, like the `HIDX` mapping.

---

## 22. Runtime layout policies

### 22.0 The `POLICY` header clause

A family selects its physical representation with an optional `POLICY <name>`
clause after the mandatory arity and before the first `VARIANT` or `FIELD`:

```forth
ENUM option 1 POLICY stack-cell-tag
  VARIANT none ;VARIANT
  VARIANT some FIELD value a ;VARIANT
;ENUM
```

`POLICY` is a reserved token: it may not name a family, variant, or field.

Policy is bound **per family**, chosen once at declaration; it is never a
per-use-site decision. A missing clause defaults to `stack-cell-tag` (§22.1).

v1 grammar surface (item 16 foundation):

- `stack-cell-tag` — accepted; the universal default, the only stack layout v1
  lowers.
- `packed-tag` — accepted; the stack representation stays IDENTICAL to
  `stack-cell-tag` (§22.2 — constructors, `MATCH`, and stack ops behave exactly
  as the default, pinned differentially in test/type-family-suite.f), and the
  declaration close bakes the `PACKED-DESC` memory ABI descriptor into the
  `LAY-*` registry.
- `niche-null`, `boxed` — recognised policy names, but the grammar rejects them
  today with `layout policy not yet supported` (§24). They ship as separate
  checked extensions, each with constructor/match/stack-op/invalid-tag tests,
  before being exposed publicly — a physical-layout policy that CHANGES the
  stack shape must not be selectable before its lowering support exists (PLAN
  item 16 risk).
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

Examples: payloadless `ENUM` of 3 -> `size 1, align 1, tagw 1`;
payload-bearing `ENUM` of 2 variants with `M=1` ->
`size 16, align 8, tagw 1`; `STRUCTURE` of two cell fields -> `size 16,
align 8, tagw 0`. These land in the `LAY-*` registry (`LAY.SIZE/ALIGN/TAGW`) —
this is the ABI the maki store/fetch capability reads; habu defines it, the
capability marshals against it. A mixed narrow-width payload tier (an explicit
`payload-offsets` table) is a later refinement. The descriptor is pure
compile-time metadata: no heap, no per-value runtime cost.

Staging: `PACKED-DESC` computes the descriptor for any family regardless of its
declared policy. `POLICY packed-tag` is ACCEPTED (item 16 sub-slice 2): the
declaration close wires `PACKED-DESC` → `LAY-ADD`, so a packed family carries
its size/align/tagw row; `stack-cell-tag` families bake no row. Buffer
marshalling that CONSUMES the descriptor is the separate maki capability
(the ABI contract above is what it reads).

### 22.3 Niche optimization

Later:

```forth
ENUM option 1 POLICY niche-null
  VARIANT none ;VARIANT
  VARIANT some FIELD value nonnull-ptr<a> ;VARIANT
;ENUM
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
ENUM tree 1 POLICY boxed
  VARIANT leaf FIELD value a ;VARIANT
  VARIANT node
    FIELD left ptr tree<a>
    FIELD right ptr tree<a>
  ;VARIANT
;ENUM
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
ENUM result 2
  VARIANT ok  FIELD value a ;VARIANT
  VARIANT err FIELD error b ;VARIANT
;ENUM

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
ENUM option 1
  VARIANT none ;VARIANT
  VARIANT some FIELD value a ;VARIANT
;ENUM

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
relocatable `TF-STR` pool — and exits `ENGINE-ERROR:BAD-TAG` (process exit status 85,
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

## 26. Historical implementation phases

These phases record how the pre-cutover substrate landed. They are superseded
by the unified hard-cutover dot chain in §2 and
`docs/census-type-dsl-cutover.md`; none authorizes a compatibility surface.

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

The hard cutover replaces both this grammar and `VALUE-RECORD` with
`STRUCTURE`; no sugar or compatibility word remains.

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
