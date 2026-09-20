# The Habu Type System

This is the plain-English map of how types work in Habu today: what the checker
proves, what the type vocabulary is, how you define your own types, where the
escape hatches are and why each one exists. Deep design detail lives in the
reference docs listed at the end; this document is the one you read first.

Everything here was checked against the tree it ships with. Where the document
says the checker accepts or rejects something, that was run, and the message it
produced is quoted so you can reproduce it.

One naming note before we start. The system spells the idea "what numeric kind
a tensor element is" as `datatype`. The older `dtype` family spelling is
retired.

## 1. The one-sentence version

Every Forth word declares what it takes and leaves on the stack; the checker
proves the body matches that declaration when the file loads, and a program
that lies does not run — the load stops with exit code 70 and a message naming
the word and the token where the story fell apart.

So the stack-effect comment is not a comment. In

```forth
: SQUARE ( n -- n ) dup * ;
```

the `( n -- n )` is a machine-checked signature. The tokens inside it are
**types, not names** — writing `( got expected -- bool )` does not name two
inputs, it invents two type variables called `got` and `expected`, and the
mistake shows up later as a baffling mismatch. Names belong in locals
(`{: got :}`); signatures hold types.

## 2. Stack effects as types

A signature `( in1 in2 -- out1 )` says: the word consumes the top two stack
items, of these types, and produces one of that type. Everything below the
declared inputs is invisible to the word, and the checker enforces that a body
cannot reach deeper than its declaration. That last rule is what "row
polymorphism" means here: the rest of the stack is an anonymous row that flows
through untouched, and a body must not borrow from it.

Lower-case single letters (`a`, `b`) are **type variables** — a placeholder
that stands for whatever type the caller supplies. `( a -- a a )` is `dup`'s
signature: whatever came in, two of the same go out. A quotation (an anonymous
block of code) carries its own bracketed effect inside the signature, so
`( ptr a n [ a a -- bool ] -- )` is a sorter that takes a comparison function.
The checker verifies quotation parameters all the way through call chains and
loops. Passing functions around is an ordinary checked capability here, not a
special case that needs an escape hatch.

Type variables come with one guarantee that is easy to forget is even a rule. If you declare
`( a -- a )`, the body must genuinely work for any `a`. It may not quietly
decide that `a` is one specific type. A body that pins the variable down is
rejected at definition time with `E-NONPARAMETRIC-EFFECT` and a message that
names the offending variable and the type it got pinned to:

```
E-NONPARAMETRIC-EFFECT habu: in bad: declared type variable 'a' is specialized
to family 'esnp2'; a declared effect must stay parametric over its quantifier
```

(Verified: a word declared `( a -- a )` whose body calls a word declared
`( esnp2 -- esnp2 )` is rejected, exit 70.) Aliasing two declared variables to
each other — declaring `( a b -- a )` and then collapsing them — is rejected
the same way. The reason this rule earns its keep is that without it, a plain
`:` definition could conjure a value of a protected type out of thin air, which
is exactly what the trusted boundaries in § 8 exist to make visible and rare.

## 3. The base vocabulary

- **`n`** — a machine cell holding an integer.
- **`bool`** — a real boolean, and a different type from `n`. Produce one with
  a comparison or a typed helper. Storing a raw `0` where a `bool` is declared
  is rejected: `expected: bool actual: n`, exit 70.
- **Sized integers** — `u8`, `u16`, `u32` widen to `n` on their own when
  nothing is lost. Going the other way, or changing sign at the same width,
  needs an explicit conversion.
  A quotation argument reads the lattice from the other side: its inputs are
  supplied by whoever executes it, so `[ u8 -- ]` does not satisfy a
  `[ i64 -- ]` parameter while `[ i64 -- ]` satisfies `[ u8 -- ]`; quotation
  outputs widen in the ordinary direction.
- **Roles** — `idx`, `len`, `fd`, `rc`, `pid` and friends are integers with a
  job title. They never silently become each other or bare `n`. This is the
  cheapest defense the system has: a file descriptor handed where a length
  belongs is a load-time error rather than a runtime mystery.
- **Pointers** — `ptr` is a constructor that consumes the next token. `ptr u8`
  is a byte pointer (read with `c@`, write with `c!`), `ptr a` is a cell
  pointer (`@` and `!`), and `ptr ptr u8` is a cell that stores a byte pointer.
  A bare `( -- ptr )` is malformed; a pointer always says what it points at.

The element type of a pointer is now enforced at call boundaries, which was not
true a few weeks ago. Handing a `ptr n` to a word that declared `ptr u8` is
rejected: `expected: ptr u8 actual: ptr n`, exit 70. The old hole where an
element type was only a documentation promise is closed.

## 4. Declaring your own scalar types

When you want a byte offset to be a different type from a serial number even
though both are integers, you declare a **nominal** — a type that is a cell at
runtime and its own distinct thing to the checker.

**`NEWTYPE name 0`** is the form to reach for. It registers a nominal cell
type owned by the package it is declared in (a zero-parameter *family* — § 5
defines that word), so two packages may each have a type called `index`
without any confusion between them. `lib/num-types.f`
is the reference example: package `NUM` declares ten of them —
`byte-len`, `item-count`, `cell-count`, `index`, `byte-off`, `cell-off`,
`alignment`, `positive-divisor`, `alloc-byte-len`, `alloc-cell-count` — and
that one file is why a byte count cannot be passed where a cell count belongs
anywhere downstream of it.

Strictness is the whole point, and the probe that settles it is always the
same. Write a checked word that returns a raw `7` where the nominal is
declared, and watch it fail: `expected: num:byte-len<> actual: n`, exit 70.

`NUM` also shows the discipline that makes nominals worth having. Its
constructors validate, and there is **no public inverse** anywhere — no word
that turns a `byte-len` back into a bare `n`. Each consumer that genuinely
needs the raw cell owns its own private, audited projection and says so.
`lib/memory.f` has exactly three (`ALLOC-BYTES>N`, `ALLOC-CELLS>N`,
`BYTE-LEN>N`), each with a note saying which primitive has to start accepting
the role directly before it can be deleted.

**`DEFTYPE NAME`** (`lib/type/deftype.f`) is the same generation on the same
substrate — a package-scoped nominal exactly like a `NEWTYPE`, probed: two
packages may each declare `DEFTYPE SERIAL` and both load. The difference is
what it derives: `DEFTYPE` generates the converter pair (`>NAME` and `NAME>N`)
for free, so anyone may cross the boundary and every crossing is visible in
source; a `NEWTYPE` derives nothing, so construction is whatever the owning
package writes. Use `DEFTYPE` for "these two integers must never be mixed up";
use `NEWTYPE` for "holding this value proves my validation ran". (An older,
genuinely global `DEFTYPE` once lived in the core roles table; it is retired,
and the current word deliberately replaced it on the package-scoped substrate —
the decision record is `docs/value-nominal-substrate.md`.)

A nominal cell family is also how a package embeds a **proof token**: a field
whose only constructor is private to the package, so possessing a filled-in
record is evidence that the package's own validating constructor built it.
This is a workaround, not architecture — construction control faked with a
magic field because generated constructors are always public — and it is
scheduled to be deleted: the `CONSTRUCT owner` flag (TYPE-FIXES-PLAN.md)
controls construction directly and every proof token evaporates with it.
`GPT2:cfg-proof` in Loom's `maki/infer/gpt2-config.f` and `GPT2:layer-proof`
in its `maki/infer/gpt2-tensor.f` are the two live examples. Both files are honest
in their own headers about the limit of that evidence, and § 9 explains it.

## 5. Families: records, alternatives, and generics

**"Family" is the engine's word for a declared type. It appears in checker
messages, so here is what it means.** Every type you declare —
with `STRUCTURE`, `ENUM`, or `NEWTYPE` — becomes one row in a single registry
inside the engine: the type's name, the package that owns it, how many type
parameters it takes, its fields or variants, and which derived operations it
opted into. That row is called a type family. "Family" rather than "type"
because one declaration can stand for many concrete types: `option` is
declared once with one parameter, and every use picks a payload —
`option<n>`, `option<NUM:index>` — each a different concrete type from
the same declaration. A declaration with zero parameters is still a family;
it just has exactly one member, and the checker prints it with an empty
parameter list — when an error message says `maki:datatype<>`, that trailing
`<>` is the checker naming a zero-parameter family instance, not a typo.

So: records and tagged alternatives are families, the generic containers are
families, and the nominal wrappers of § 4 are families too — one substrate,
several declaring words.

- **`STRUCTURE name arity … FIELD f type … ;STRUCTURE`** declares a record:
  several named fields travelling together as one value. `GPT2:config` carries
  eleven semantic fields plus its temporary construction proof.
- **`ENUM name … ;ENUM`** declares a set of alternatives. In its short form the
  body is bare variant names and nothing else, which gives you a plain tag set:
  `MAKI:datatype` (Loom's `maki/tensor.f`) is five names, `df32` through `di32`, and
  is the single authority on element datatypes for the whole tensor layer. In
  its full form, with an arity token and `VARIANT … ;VARIANT` clauses, each
  alternative may carry named fields — `SAFET:map-take` is `moved` carrying a
  mapping, or `empty` carrying nothing.
- **`SUMTYPE`** and **`PRODUCT`** are the older spellings of the same two
  ideas, with positional rather than named payloads on the sum side. Plenty of
  live code uses them (`lib/process.f`, `lib/adt/result.f`), but new code
  should use `STRUCTURE` and `ENUM`.
- **`DERIVE eq`** on a public family generates its typed identity comparison,
  so consumers compare values instead of raw tags. `MAKI-DATATYPE:EQ` is one.

Declaring a family generates a **constructor** (`MAKE`) and a **destructurer**
(`UNMAKE`), plus `MATCH … ;MATCH` for the alternatives, which the checker
requires to be exhaustive — every variant needs an arm. The generated names are
mechanical: the package name, then the family name with internal hyphens
doubled. `STRUCTURE config` inside `package GPT2` produces
`GPT2-CONFIG:MAKE`. This spelling rule creates one trap:
`ENUM map-take` inside `package SAFET` produces `SAFET-MAP--TAKE:MOVED`, which
reads as though it belongs to `package SAFET-MAP` — a real package declared
earlier in the same file. There is also a readability cap of thirty-two
characters on the generated spelling (`TF-CTOR-NAME-LIMIT` in
`src/core/type-family.f`); past it the name is built from a hash instead, so a
long, fully spelled-out constructor simply does not resolve. Wrap the long ones
in short private words.

**Multi-cell values.** A record with several fields is one logical value that
occupies several stack cells. The checker tracks the whole bundle, and two
rules follow from that:

- A non-linear multi-cell value binds **whole** to a local: `{: p :}` infers
  its type and `{: p:pair :}` asserts it. Every reference reloads every cell.
  An annotation uses the signature type grammar, including nested families
  and the definition's own type variables: `( opt<a> -- opt<a> )
  {: o:opt<a> :} o`. Wrong families, arguments and arities are refused.
  Keep a value whole while passing it between words; use `UNMAKE` or `MATCH`
  when its fields participate in the computation. The runtime and refusal
  cases are in `test/wide-typed-local-probe.f`, run at both tiers.
- A word that returns a multi-cell value **cannot be called at the interpreter
  prompt**. The interpreter would shuffle one physical cell of a multi-cell
  bundle without knowing it, so such words are marked at definition time and
  the attempt fails closed: `hb: interpret-mode layout value: P11:MK`, exit 70.

**Generic containers.** `lib/adt/option.f` declares `option<a>` — a value is
either `some` carrying one thing or `none` carrying nothing — and
`lib/adt/result.f` declares `result<a,b>` — either `ok` carrying a success
value or `err` carrying a reason. These replace the old habits of returning
`-1` for "not found" or a value-plus-flag pair for "it worked, or here is why
it did not". The gain is that `MATCH` forces every caller to handle the absent
or failing case; a missing branch no longer type-checks. `SAFET` uses
`option<n>` for every reader addressed by tensor id, and `result<n,n>` for the
outcome of ending a mapping's life.

There is a real limit on what can go inside a generic container today, and it
is described in § 9.

## 6. Linear owners: values you must use exactly once

A type declared with **`DEFLINEAR`** is a resource. The checker enforces that a
value of that type is neither copied nor thrown away: every path through the
code must consume it exactly once. Where an owning handle flows through code —
a mapped file, an allocated buffer, an open transaction — this turns leaks and
double frees into load-time errors. Dropping one is rejected at the `drop`,
exit 70.

The worked example is Loom's checkpoint loader, `maki/infer/safetensors.f`. It
declares three owners and a chain of transitions between them:

- **`SAFET:session`** — one open, unpublished load transaction. `OPEN` creates
  it, `MAP-FILE` or `ADOPT` gives it an image, `PARSE` validates the header.
- **`SAFET:file`** — one published, validated safetensors file owner with an
  immutable tensor index. `DETACH` consumes
  a validated session and produces it. `CLOSE` consumes a session that will
  never be published.
- **`SAFET:mapping`** — the file mapping, moved out of a file owner so the bytes
  can outlive the description of them. `DETACH-MAPPING` performs that move and
  returns `map-take`: `moved` the first time, `empty` ever after, so a second
  attempt cannot fabricate a second owner.

Because these are linear, the ordering rules are enforced by the type system
rather than by a runtime flag. A session cannot be both closed and published, a
file owner cannot be released twice, and a mapping cannot be read after it was
unmapped, because in each case the token that named it is gone.

Linearity composes with everything above. An alternative whose payload is a
linear value makes the whole thing one linear unit; constructing it consumes
the payload, and a `MATCH` arm re-introduces the payload and must consume or
re-wrap it. A record field may be linear too, and so may a payload field that
names such a record, so an owner can be carried several layers deep and stay
one. Binding a linear value to a local is refused, because a local would let the
same resource be named twice: `E-LINEAR-LOCAL habu: in bad: linear value cannot
be bound to a local; keep it on the stack`, exit 70.

**Disposal that cannot half-fail.** `MEM:RELEASE-BYTES` and `MEM:UNMAP` in
`lib/memory.f` return nothing at all. If the underlying `munmap` fails, the
process dies immediately with the message `memory: unmap failed` and exit code
71 (`lib/memory.f:168-173`). That is deliberate: a disposal word with no result
cannot be ignored, cannot be half-handled, and cannot leave a caller believing
memory was returned when it was not. The promise "this owner is consumed, so
its memory is released" is either true or the process is not running.

Not every cleanup path is fatal today. `SAFET:UNMAP-MAPPING` returns
`result<n,n>` instead, on purpose, so that code disposing of several owners in
sequence can see one failure without unwinding past the owners it has not
disposed of yet. It frees its own record before making the syscall, so a
failing unmap still cannot leak the record.

## 7. Quotations, `catch`, and deferred words

- Quotations `[: … ;]` are execution tokens, **not closures**. They cannot read
  the enclosing word's locals. A value the quoted code needs travels in on the
  data stack and comes back out on every branch.
- `catch` takes a quotation and requires it to be **stack-preserving**: its
  inputs must equal its outputs, so the stack has the same shape whether the
  body completed or threw. Combined with the no-closures rule, that is also how
  a value crosses the boundary — you pass it in and get it back. Several words
  in the loader exist purely to give `catch` a stack-preserving shape, and say
  so in their own comments (`UNMAP-BODY`, `PARSE-BODY`).
- Function-valued state uses typed **`defer`** words. `defer ACTION ( in -- out )`
  declares the vector's public effect and `[: IMPL ;] is ACTION` installs an
  implementation; the checker proves the installed quotation's effect matches
  the declaration exactly. Raw execution-token cells fetched and executed lose
  the effect and are not used in checked code.

## 8. The escape hatches, and the ledger that keeps them small

The checker cannot express everything. Three escape hatches exist, and all of
them are deliberately loud.

- **`TRUSTED: NAME ( effect ) … ;`** — "believe this effect, do not check the
  body." Machine-code emitters, syscall wrappers, and the private words that
  turn a raw address into a typed pointer live here.
- **`0 set-check`** — turns checking off for a span of source. Legitimate only
  as a named, tested boundary. Experience says most such spans exist because
  one primitive lacked a declared effect, so try a single trusted row before
  accepting a whole unchecked region.
- **`PRIM:` and `PPRIM:` axiom rows** — teach the checker the effect of an
  engine primitive, which turns what would otherwise be a trusted call site
  into an ordinary checked call.

Every trusted site is explicit debt. Its source-local comment states why the
effect cannot be inferred and names the retirement owner; a focused test pins
the asserted behavior through its production path.

The trust surface only shrinks deliberately, and the pattern that keeps it
small is visible in the history: before adding trust, try a checked factoring.
A word that looked like it had to be a primitive turned out to certify as
`: BYTE+ ( ptr u8 n -- ptr u8 ) + ;`, and the trusted row was deleted. A single
axiom row has retired whole unchecked spans. Passing functions around, which
once looked like it needed an unchecked boundary, turned out to be a fully
checked capability.

**Sealed wordlists.** There is one more protection that is not about types at
all but backs them up. A package can seal its wordlists so that no later file
can reopen it. `SAFET-MAP` does this, and its own comment explains why: without
the seal, a later file could execute `package SAFET-MAP` and republish the
private word that turns an `mmap` result into a typed pointer, handing out raw
addresses. Non-resolution from outside proves only that a name is not visible;
it never proves the package cannot be reopened and drained. Attempting the
reopen aborts with exit code 84 (`SEAL-PACKAGE`) — verified, and the same
attempt on an unsealed package succeeds, so the seal is doing the work.

## 9. Known gaps

These are the places where the type system does not yet say what we would like
it to say. Each is real, each has been reproduced, and none of them is papered
over with a runtime guard.

**Destructure and rebuild defeats a proof token.** A record whose validating
constructor is the only way to build it honestly can still be taken apart with
the public `UNMAKE` and put back together with `MAKE`, keeping the original
proof field and substituting a bogus value for everything else. Verified: a
word outside the owning package that unmakes a validated record, keeps its
proof, and remakes it around `-999` certifies cleanly, exit 0. Both live proof
tokens are honest about this in their own file headers. Closing it needs the
sealed-destructure capability, tracked by dot
`habu-checker-sealed-destructure-d967fc03`. Until then, the packages that care
defend themselves a second way: the tensor layer revalidates a rebuilt layer
index against the configuration's bounds before it does any address
arithmetic, so a forged index cannot reach a wrong row.

**A tagged family cannot instantiate a generic parameter.** A tagged family
is one declared in variants (`ENUM`, or the older `SUMTYPE`): its values carry
a tag saying which arm they are. An untagged one (`STRUCTURE`, `NEWTYPE`) has
exactly one shape and needs no tag. The gap: `option<NUM:index>` works and is used in
production (`lib/float.f`); so does `option<T>` over a `STRUCTURE`, complete
with a `MATCH` that unmakes the record inside the `some` arm. But
`option<MAKI:datatype>` — an `option` over a plain tag `ENUM` — is rejected at the
constructor: `expected: a actual: maki:datatype<>`, exit 70. The same rejection
happens with a payload-free `SUMTYPE`, and with a tag family declared in the
same package as the consumer, so this is not about package boundaries. It is
about tagged families specifically: `NEWTYPE` and `STRUCTURE` instantiate a
generic parameter, `ENUM` and `SUMTYPE` do not. This is implementation debt, not
design — a tag value is one cell like any nominal; the instantiation code was
never taught about variant families. The fix is scheduled in the type
conversion (TYPE-FIXES-PLAN.md item 14).

**Pointers carry no lifetime — on purpose.** A pointer type says what it
points at, and nothing else: not which allocation it came from, how long it is
valid, or how far it extends. This is a decision, not a gap (Joel,
2026-07-30). The safety that matters is carried by linear owners — a mapping,
a store, a session is used exactly once and disposed explicitly, and the
checker enforces that. A borrowed span inside a scoped word is advisory:
stashing it past its owner's death reads freed memory and crashes loudly,
exactly as in C, and that failure is review's to catch. We are Forth; a
region-and-borrow system was considered and rejected as machinery for a
threat model we do not have.

**`?dup` is not part of the checked vocabulary.** Its result depends on its
input's value rather than its type, so it has no signature. It is not merely
uncheckable — it does not resolve at all; a body naming it fails to load with
`E-UNDEFINED`. Branch on an explicit comparison instead.

**Linear values still cannot live in locals.** Wide and parametric local
bindings are supported (§ 5); the linear-owner refusal in § 6 remains.
`test/engine-suite.f` rejects even a local referenced exactly once
(`CBAD-OWN-LOCAL-ONCE`). Keep an owner on the stack and factor a consumer when
its fields need processing.

## 10. Declared memory records

Everything above is about **values**: a `STRUCTURE` is a bundle of cells that
travels on the stack. This section is about the other kind of record — a
contiguous region of memory, addressed by one pointer, whose fields are read and
written in place. The tree is full of them: a vector header, a reader state, a
task control block, a connection record. None of them is declared today, and
this section is the design for declaring them.

It is a design, not a description. Where it says something was measured, it was
run on the engine this document ships with; where it says a word will be
generated, nothing generates it yet.

### 10.1 The problem in one sentence

A memory record that mixes a **pointer field** with **scalar fields** has no
declared form, so the tree expresses it by casting one of the two halves — and
each cast is a hole the raw-storage rule (`docs/effects.md`, "Raw storage never
holds an address") cannot close.

**Cast A — `ptr-field`.** The primitive's row is `( ptr a n -- ptr ptr b )` with
`b` free, so it manufactures a fully typed pointer out of any base. Applied to a
base the checker only knows as a parameter, it launders an integer:

```forth
create RLP-RAW 3 cells allot
: RLP-FIELD-A ( ptr a -- ptr ptr u8 ) 0 ptr-field ;   \ what PTR-FIELD: generates
: L1 ( n -- u8 ) RLP-RAW ! RLP-RAW RLP-FIELD-A @ c@ ; \ certifies
```

`src/core/structures.f` `PTR-FIELD:` generates exactly that helper, so the
legitimate form and the bypass are the same shape. The token-level refusal that
did land only sees a base named *at* the `ptr-field` token, and a parameter
hides it.

**Cast B — `byte-view` / `cell-view`.** Both are type-level renames, so the pair
reads a declared pointer cell as a scalar cell. That is how the tree reaches the
count cells behind a declared head, and it is also how an integer gets into the
very cell the rule prescribes:

```forth
PTR-VARIABLE RLP-HEAD 0 , 0 ,
: L3 ( n -- ) RLP-HEAD BYTE-VIEW CELL-VIEW ! ;                \ certifies
: L4 ( n -- u8 ) RLP-HEAD BYTE-VIEW CELL-VIEW ! RLP-HEAD @ c@ ; \ certifies
```

`test/record-launder-probe.f` pins both launders, the refusals that did land,
and the six legitimate field reads a record must preserve. The same two are
suite rows `V8` and `V9` of
`test/typed-storage-structural-test.f`. Dots `habu-refuse-ptr-field-331a9731`
and `habu-refuse-a-scalar-030be3ad`.

### 10.2 What already exists, measured

Four facts decided the design. Each was run on the release engine.

**A mixed record is already declarable and storable.** A layout family with a
pointer field and scalar fields loads, in a public or a private package, and the
storage definers accept it:

```forth
STRUCTURE rec 0  FIELD data ptr u8  FIELD len n  FIELD cap n  ;STRUCTURE
TYPED-VARIABLE ZRV ZZ:rec        \ ( -- ptr ZZ:rec )
1 TYPED-BUFFER ZRB ZZ:rec        \ ( n -- ptr ZZ:rec )
TYPED-VARIABLE ZRP ptr ZZ:rec    \ a cell holding a record pointer
```

**A record pointer already refuses both casts.** A layout pointee does not unify
with a one-cell type variable, so neither row admits it:

```
habu: in z2: at 'ptr-field' expected: ptr a n actual: ptr zz:rec<> n
habu: in z3: at 'BYTE-VIEW' expected: ptr a actual: ptr zz:rec<>
```

So the fence the two dots are trying to build already stands on the far side of
a declaration. What is missing is not the fence; it is the door.

**The only field access today is whole-value.** `( ptr ZZ:rec -- n ) @
ZZ-REC:UNMAKE drop nip` certifies, including for a caller-supplied base, and
writing one field means `UNMAKE`, rebuild with `MAKE`, store the whole record.
That is why no mixed record in the tree is declared: the declaration costs a
full load and store per field access and forbids in-place mutation. Indexing is
gone too — `( ptr ZZ:rec n -- ptr ZZ:rec ) 3 cells * +` is refused, because
`+`'s pointer row is `( ptr a n -- ptr a )`.

**The checker half of the field door is already implemented.**
`field-project` is a reserved checker operation (`src/core/checker.f`, the
`FIELD-PROJ-STEP` window; hook bound in `src/core/type-family.f`). Inside an
armed window it consumes `ptr family<args>` plus a baked byte-offset literal and
produces `ptr <instantiated field type>`, deriving the field's owning family,
offset, extent, role and schema from the committed field id by `TYPE-FIELD`
reflection. `test/field-proj-suite.f` pins the positives — a cell field, a
byte-offset field, a **pointer** field (`FIELD p ptr u8` projects as
`( ptr fpptr -- ptr ptr u8 )`), a generic field substituted at the caller's
instantiation — and the negatives, red-first: unarmed use, a forged offset, an
offset past the family width, a non-layout pointer, a foreign family, a wrong
arity, a role or output mismatch, an uncommitted id. `docs/type-families.md` §2.2
documents it and names the missing half: the accessor generator, tracker id
`habu-structure-generate-field-b9dc52f8`.

So this design is not a new facility. It is the generator, three small words
around it, and a migration.

### 10.3 The survey

Every memory record in the tree that mixes a pointer field with a scalar field,
measured 2026-09-18 by reading the definers and loading probes on `bin/hb`.
"Tier" is where the file sits in the boot prefix, because that decides what a
record may use:

- **T1** — the pre-arm prefix, `src/core/util.f` through `src/core/include.f`.
  `include.f` arms `TDECL-EVAL-ARMED` at the end of the file, and a family's
  constructor generation dies without it, so **no record facility is usable
  anywhere in T1** — including `src/core/checker.f` (the 9th file, loaded before
  families exist at all) and `src/core/dynamic-storage.f` (in
  `PFX-LOAD-CORE-FILES`, before `include.f`).
- **T2** — the post-arm prefix, `src/core/enums.f` through
  `src/core/layout-buffer-seal.f`, then the boot stdlib.
- **T3** — everything else: `src/compiler/`, `src/habu/*`, `src/arch/`, `lib/`,
  `tools/`, `test/`.

Totals: **31 distinct mixed records** (14 under `src/`, 16 under `lib/`, 1 under
`tools/`), plus 13 `tools/` pointer-slot families over ~60 call sites, plus the
reserved engine-layout cells read through the same cast — 17 sites in `src/`,
1 in `tools/`, 10 in `test/`. Cast-A call sites whose base is a declared
`( ptr a … )` / `( ptr n … )` parameter: **~75**. Cast-B sites over a
pointer-bearing base: ~70 in `src/`, ~40 in `lib/`, ~25 in `tools/` and `test/`.

#### src/

| record | fields (cell offset : type) | cast | tier | persisted |
|---|---|---|:--:|---|
| SYM row, `checker.f:4370-4467` | 0 `ptr u8`, 1 n, 2 `ptr u8`, 3 n, 4 enum | A×2, B×3 | T1 | yes — `REG-PERSIST-BUF`, `ptr-cell-mark` per row |
| CWIN call window, `checker.f:1309` + `cell-effects.f:43` | 0 `ptr n`, 1 n, 2 n | B×8 | T1 | in DATA across capture |
| declaration-owner record + cells `$360`/`$368`, `checker.f:6-7,118-127` | two head cells `ptr u8`; record: magic/len at −$10/−$8, xts at $48/$70/$78 | A×7, B×7 | T1 head, T3 readers | yes — `ptr-cell-mark`, AOT-captured |
| transaction state, `declaration-transaction.f:80-105` | 0 `ptr n`, 1–10 n, 11 xt | A×1 | T1 | yes (xt cells) |
| DYNAMIC-STORAGE registry, `dynamic-storage.f:44-58` | 0 n capacity, 1..n `ptr ptr a` | A×1 | T1 | no |
| DYNAMIC-BUFFER control, `dynamic-storage.f:1-11`, generated by `layout-buffer.f:517-535` | 0 `ptr a` declared head, 1 n cap, 2 n slot | B (`CTL`, and the generated `cell+ byte-view cell-view @`) | T1 runtime | no — unmapped before capture |
| include frame, `include.f:695-724` | 0 `ptr u8`, +8 byte flag, +16 buffer | A×2 | T1 | no |
| startup DATA, `env-base.f:6-24` | $3670 n, $3678 `ptr ptr u8`, $3680 `ptr ptr u8` | A×4 | T1 | engine DATA |
| generated-decl frame row, `generated-declaration-dictionary.f:25-43` | 0 n, 1 n, 2 `ptr a` | A×1 | T2 | persisted head; DP nulled before capture |
| NSTR pool owner, `compiler/native/string.f:22-37` | 0 `ptr u8`, 1 `ptr u8`, 2–4 n, $28 arrays | A×2, B×5 | T3 | yes — hand `ptr-cell-mark` |
| IR arena descriptor, `compiler/ir/arena.f:142-154` | 5 `ptr u8`; 0–4, 6, 7 n | A×1, B×31 | T3 | persisted head |
| IR symbol index, `compiler/ir/symbol.f:306-316` | 2 `ptr u8`; 0, 1 n | A×1, B | T3 | no |
| AOT dict record (48 B), `aot-closure.f:105-107,223` | 0 code `ptr u8`, 8 n, 16 n, 24 bytes, 40 wid | A×1, B×4 | T3 | AOT-captured |
| address-cell vector header, `address-cells.f:159-195` | magic/mode/cap n, base as a DATA offset | B | T3 | the relocation machinery itself |
| engine-layout pointer/count pairs | `CK-AOT-SIG-POOL`+`-LEN`; `TCSIG-A`/`TSIG-A`+`TSIG-U`; `AOT-SPAN:TABLE`+`N`; `AOT-SPAN:BASE` (read **both** ways, `aot-closure.f:242` vs `:243`); stepper INP/INE; `SIGNAL-ABI` FD-PTR/STUB; `NULL-PTR-CELL` itself | A — 17 `data-base <const> + … ptr-field` sites in `src/`, 1 in `tools/`, 10 in `test/` | T1 and T3 | several `ptr-cell-mark`ed |

#### lib/ — all T3

| record | pointer fields | scalar fields | cast | storage | persisted |
|---|---|---|:--:|---|---|
| `BUF` header, `byte-buffer.f:26,52-56` | 0 `ptr u8` | 1 len, 2 cap | A, B | caller `create`d; also embedded at cells 8–10 of the xmodem session | no |
| `VEC` header, `vector.f:6-10` | 0 `VEC.DATA` | 1 len, 2 cap | `PTR-FIELD:`, B | `create`d **and** `TYPED-BUFFER … ptr u8` elements | no |
| `EDIT` header + row, `byte-edit.f` | hdr 0, row 2 | hdr 1–6, row 0/1/3 | A×7 | caller `create`d | no |
| `JR` reader, `json-read.f:148-170` | 0 and 8 | 1–7, 9–17, ctx stack | A×12 | caller `create`d | no |
| `XML` reader, `xml/state.f:12-37` | 0 | 1–20 + arrays | A×3 | caller `create`d | no |
| xmodem session, `serial-xmodem.f:29-45` | 12 `ptr u8`, 15 `ptr a` | 0–7, 11, 13–18, nested `BUF` at 8–10 | A×2, B×13 | caller `create`d | no |
| ZIP node + entry/buffer overlays, `zip-state.f`, `zip-raw.f` | 0, 2, 3, 4, 6, 8, 10, 18, 22 | 1, 5, 7, 9, 11–17, 19–21, 23–27 | A×11 | **mmap** per node | closed at `IMAGE-LIFECYCLE` |
| ZIP member, `zip-raw.f:47-57` | 7 | 0–6, 8–10 | A, B | mmap | no |
| `MAP` slot + header, `map.f` | slot 2 | slot 0/1/3/4, hdr 0–2 | A×2, B | caller `create`d | no |
| `TBL` pair, `table.f:68-87` | field n | field n+1 len | A×2, B | caller-supplied | no |
| `BUILD` step, `build.f:30-41` | 0, 2, 4, 6, 8 | 1, 3, 5, 7, 9, 10 | A×2 | caller `create`d | no |
| **TCB**, `task.f:61-91` | STACK, REGION, RSTACK, LSTACK, MSG-SENDER | ~20 scalars **plus inline semaphore byte blobs** | `PTR-FIELD:`×5, B | definer-compiled dictionary | in the image; pointers nulled on release |
| queue index, `queue.f:44-49` | a table of record addresses | Q-REC itself is scalar-only | A behind `CELL-VIEW` | `create`d | no |
| DYNAMIC-BUFFER instances (8) | head | cap, slot | B | generated | no |
| `pq` connection/result, `db/pq.f:92-112` | `CONN-PG`, `CONN-ARENA`, `RES-PG` | 13 parallel tables, plus the arena's own mixed extent (`:381`) | A behind `BYTE-VIEW` | `create`d + mmap arena | no |
| `SA-ACT` sigaction, `signal.f:101-102` | offset 0 holds a **code address kept as a scalar** | flags at `SA-FLAGS-OFF` | B | `create`d | no |

#### tools/ and test/ — all T3

| site | shape | cast |
|---|---|:--:|
| `LINT-SLAB`, `lint/text.f:110-166`, **7 `create`d instances** (`lint/text.f:173`, `error-code-lint-core.f:90`, `aot-section-reach-lint.f:47`, `aot-lint-core.f:13`, `public-signatures-core.f:45,46`, `lint/shadow-lint.f:34`) | 0 `ptr u8`, 1 cap, 2 len | A + plain `+ @` |
| 13 `*-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )` families, ~60 call sites (`json.f`, `check-core.f`, `build-fixpoint.f:222`, `hb-build-lib.f`, `examples-test.f`, `lint/diff.f`, three `json-only*.f`, three `repair-*.f`) | one `variable` pointer slot beside a *separate* `variable` length — a two-field record spelled as two words | A only |
| `tools/check-core.f:137-144` | an indexed cast-A helper with **no caller anywhere** | delete, do not migrate |
| `test/` | the two documented-open rows `V8`/`V9`, the landed refusals in `test/compiler/raw-cell-pointer-refusals.f`, ~10 engine-layout readers, and `test/record-launder-probe.f` | A and B |

Two facts from the survey shape the migration more than the totals do. First,
the recurring shape is not exotic: a pointer at offset 0 followed by a length
and a capacity, or a pointer immediately followed by its length, appears in at
least eleven independent hand-rolled encodings. Second, the form the raw-storage
rule *prescribes* — a declared head with counts allotted behind it — does not
remove the cast, it moves it from A to B: `CWIN`, `cell-effects.f` `STATE` and
every `DYNAMIC-BUFFER` control record read their own counts through a view.

### 10.4 The design

#### The declaration

A memory record is an ordinary layout family that opts into an address surface:

```forth
package BUF
STRUCTURE header 0
   DERIVE addr
   FIELD data ptr u8
   FIELD len  n
   FIELD cap  n
;STRUCTURE
;package
```

`DERIVE addr` is opt-in for two reasons: a value family such as `GPT2:config`
should not acquire an address surface it never uses, and the engine should not
carry accessor words nothing calls. It joins `DERIVE eq` and `DERIVE hash` as a
third derive code in `src/core/structure-decl.f`'s header-clause grammar.

There is **no second record registry**. The field offsets, widths, generic
substitution, linear-containment rules and declaration transaction are the ones
`STRUCTURE` already has; `docs/type-families.md` §2.2 rules a parallel registry
out by name, and the checker's projection window already reads this one.

#### What is generated

Inside the one declaration transaction, for a family `F` with fields
`f₀ … fₙ`. **A family's visibility picks the spelling and the wordlist**, and the
two forms are `TF-CTOR-PKG$` and `TF-CTOR-PRIV$` in `src/core/type-family.f`:

| | public `F` | private `F` |
|---|---|---|
| spelling | `PKG-FAMILY:member` | `FAMILY-member` |
| derived from | package name, then the family tail, internal hyphens doubled, capped at `TF-CTOR-NAME-LIMIT` = 32 characters with a SHA-256 fallback past the cap | the family tail, `-`, the member — uppercased, no escaping, no cap, no hash |
| lands in | the reserved constructor **namespace**, which is global | the **declaring package's private wordlist** |
| resolvable from | anywhere, qualified | that one package only |

The private form drops the package segment and the escaping because every use of
it is inside the one package that declared the family, where the package name
says nothing new, and a hashed spelling written on every line of a library would
be the wrong trade. It is not injective across packages and does not need to be:
a collision with an existing private word, or between two families of one
package, is refused by the generator's own "generated declaration already
defined" die. A qualified private spelling is not merely unchosen but
structurally impossible — see "Why a private generated word wears no colon"
below.

The member set is the same either way; the table below writes the public form.

| word | effect | body |
|---|---|---|
| `F:f` (one per field) | `( ptr F -- ptr T )` — the field's **address** | `<byte-offset> field-project` |
| `F:AT` | `( ptr F n -- ptr F )` | `record-at` — stride by the family's committed width |
| `F:BYTES`, `F:CELLS` | `( -- n )` | the size constants `BEGIN-STRUCTURE` publishes today |

The accessor yields an address, not a value, so the existing `@`, `!` and `c@`
rows do the access and a pointer field reads back as a pointer:
`BUF-HEADER:DATA @` is `( ptr BUF:header -- ptr u8 )`. One word per field, not
two, and the same shape `PTR-FIELD:` and `+FIELD` already generate — with the
offset and the type carried into the checker instead of thrown away.

Generation goes into the owning package's wordlist, and for a **private** family
into its private wordlist: most of the records in the survey are package-private
(`BUF`, `JR`, `XML`, `EDIT`, `LINT-SLAB`), so a public-only generator would
serve almost nothing. `src/core/structure-decl.f` used to gate `MAKE`/`UNMAKE`
generation on a public family and call package-scoped private generation
deferred type-DSL work; that work landed as the first implementation dot below
(`habu-generate-a-private-80272413`), and visibility now decides the spelling
and the wordlist rather than whether anything is generated at all.

##### Why a private generated word wears no colon

A generated word that must be package-private cannot carry a qualifier, at
either layer:

- the engine routes a **qualified definition name** to the named namespace's
  PUBLIC wid (`src/habu/habu2.f` `C-QUALIFY-DEF`: "a qualified name cannot land
  in a private wordlist"), and
- the checker records **any** name with one non-edge colon as that package's
  public symbol before it consults the open package at all
  (`src/core/checker.f` `CHECKER-RECORD-SYM`).

So `PKG:PRIVATE-WORD` does not resolve even from inside `PKG` — the rule
`docs/forth.md` states for hand-written packages — and the generator obeys the
same rule rather than working around it. Nothing switches wordlists to achieve
this: when `;STRUCTURE` runs, the declaring package is open and its private
wordlist is already current, so an unqualified generated definition evaluated
through `TDECL-EVAL-XT` lands there by itself.

##### Private generated words are protected by name, not by wordlist

A public family's constructor namespace is a wordlist nothing else may publish
into, so it is marked protected (`prot-wid-add`) and the checker refuses both an
extra tail in it and an `undefine` of a word in it. A private family has no
namespace of its own; its words live in a wordlist the package keeps defining
into. Marking **that** wid protected would refuse every later definition the
package makes — in a sealed engine `EMIT-STORE-DEF-NAME` exits 84
`ENGINE-ERROR:SEAL-PACKAGE` on a publish into a protected wid — so generation
stages no wordlist for a private family, and the guarantee narrows to a
**name-keyed** one: `TFAM-CTOR-WORD?` recognises the private spelling while the
declaring package is open, and the checker's `CTOR-WORD?-XT` undefine guard
refuses it with `E-CTOR-PROTECTED`. It recognises the member set that family's
generator published — the `TF-PRIV-MEMBER$` list, `make` and `unmake` today —
and not every name spelled from the family tail, so a family that generates
nothing (a private SUM or ENUM, an opaque zero-field product) protects nothing.
There is no "extra tail" rule for a private family because there is no reserved
package for a stray tail to extend. This is a real, deliberate weakening
relative to the public path; it is the price of the words being private at all.

#### How the accessors are minted

Per accessor, the generator arms the sealed window and evaluates one definition:

```
FIELD-PROJ! ( accessor-name-addr accessor-name-len field-id byte-offset -- )
: F:f ( ptr F -- ptr T ) <byte-offset> field-project ;
```

The window is single-shot, keyed on the accessor's name, and disarms at the
`field-project` token even on a reject. The **committed field id is the sole
authority** — the checker derives the owning family, the committed offset, the
extent, the role and the schema from it, instantiates that schema over the input
pointer's family arguments, and cross-checks the baked offset. A generator that
lies about an offset, a family, an arity, a role or an output type is refused,
each case pinned in `test/field-proj-suite.f`.

Two small pieces sit beside the generator; both landed with it, at global scope
in `src/core/structure-make.f`, because a generated body names them unqualified
from whatever package declared the record.

`field-project ( ptr a n -- ptr a ) + ;` is the accessor body's **production
runtime word**. Outside the armed window that row is exactly `+` — it preserves
the pointee and retypes nothing, so the word is not a capability, and the checker
replaces its effect only inside the window.

`record-at ( ptr a n -- ptr a ) cells + ;` is a second row, and it needs no
window: it consumes `ptr F` and a cell count and yields `ptr F`, and the
generated `F:AT` bakes the family's committed width as the multiplier — the row
itself scales nothing. Because it **preserves the family**, it forges nothing;
bounds remain the caller's, the same contract `cells +` has today. It exists
because `+` and `cell+` reject a layout pointer outright, which is what makes a
record pointer safe in the first place. On a layout pointee it refuses an
instantiated width that is not the committed width: an argument wider than one
cell expands the product and moves every field past the first, which is what both
the baked multiplier and a baked field offset assume away.

#### Instantiating a record

| where the bytes live | form | notes |
|---|---|---|
| a single global record | `TYPED-VARIABLE NAME F` | accessor `( -- ptr F )`, zero image, name-guarded |
| a fixed array of records | `count TYPED-BUFFER NAME F` | accessor `( n -- ptr F )` |
| a growable mapped region | `DYNAMIC-BUFFER NAME F` | accessor `( n -- ptr F )` over `map-anon`, plus `NAME-RESERVE` / `NAME-RELEASE` |
| a caller-supplied region | a `( ptr F … )` parameter | measured: a caller-supplied record pointer reads and writes normally |
| a persisted arena | `PERSISTED-PTR-*` head over a boot table, records inside it | the pointer fields need marking — see below |

Individually `MEM:ALLOC-CELLS`-ed nodes are the one shape with no direct answer:
the allocator returns `ptr a`, and there is no sound cast from `ptr a` to
`ptr F` (`CAST` refuses a `ptr` operand, and minting one is the launder this
whole section is about). Such a record either becomes an element of a
`DYNAMIC-BUFFER` — an indexed arena instead of a linked list of separate
mappings — or acquires a typed allocation crossing of its own, one audited word
per family. The ZIP node (`lib/zip-state.f`) is the live case, and its migration
entry below is where that choice is made.

#### The engine prefix

T1 has nine mixed records and cannot have a record facility, so **the pre-arm
prefix becomes record-free**. Two mechanisms do that work.

*Columns instead of records.* A T1 record splits into one declared cell or table
per field — the shape `src/core/checker.f` already uses for `ATOMA`/`ATOMU`/
`ATOMK` and `src/core/layout-valid.f` uses for its twelve certificate columns.
For SYM this is a strict improvement rather than a tax: the two pointer columns
become `PERSISTED-PTR-U8-TABLE-VARIABLE`s, so the snapshot marks **one cell per
table** instead of walking every live row to `ptr-cell-mark` two fields of each,
and the three scalar columns become plain tables.

*A declarator for a reserved engine cell.* Seventeen `src/` sites read a fixed
DATA offset as a pointer through `data-base <const> + 0 ptr-field`, and
`NULL-PTR` in `src/core/pointer-storage.f` is one of them. No landed definer
covers them — all four definers in that file `create` their own body, and
nothing in the tree names a fixed offset — so the design adds one, in the same
clause style, per pointee:

```forth
: RESERVED-PTR-U8-CELL ( n -- )
   create , does> ( -- ptr ptr u8 ) @ data-base swap + 0 ptr-field ;
```

The field view at the end is not decoration: `data-base <off> +` answers the
untyped `ptr n` view of the DATA region, and `0 ptr-field` is what says the cell
there holds a `ptr u8`. A clause without it computes the same address with the
wrong type and the engine's own check refuses the definition — measured as
`E-NCOMP-VERDICT` out of the native build, which is where a pre-checker
definition is checked. Measured for the landed form: the clause certifies, the
cell the created word answers sits at exactly the offset it was given from
`data-base` (`test/pointer-storage-test.f`), and the declared pointee holds in
both directions — `ptr ptr u8` in, `ptr ptr n` refused. The clause spells its
pointee out, so `trust-raw` seals no
variable and the raw discipline never looks at it — the same reason
`PTR-U8-TABLE` works before the checker exists. Being a pre-checker definer, it
needs **two effect rows**, not one: a `TRUST` row in `src/core/cell-effects.f`
so the seal does not mark it `DNAME-INT`, and a row in
`src/habu/verify-source.f`'s definer table so the source scanner agrees with
what the native path publishes. `tools/lint/def.f`'s case table is a third list
of the same names and should gain it too, as a lint surface rather than a gate.

Three T1 sites need a named answer beyond those two mechanisms:

- **The DYNAMIC-BUFFER control record** is the only T1 record whose three cells
  must be reachable from one pointer, because the registry stores that pointer.
  It dissolves the same way: the generated declaration publishes
  `NAME#base` (the declared head), `NAME#cap` and `NAME#slot` as separate
  declared cells, `DYNAMIC-STORAGE:RESERVE` / `RELEASE` take the three, and the
  registry keeps three parallel columns instead of one pointer to a record. The
  cheaper alternative, if the build allows it, is to move
  `src/core/dynamic-storage.f` after `src/core/include.f` in the prefix and
  declare the control record properly; the migration dot decides which, and the
  view narrowing lands after it either way.
- **`ARENA-SNAP-BOOT`'s zero pass** views a `PTR-U8-TABLE` boot buffer as cells
  to zero it. The typed form writes `NULL-PTR` through the table's own accessor
  in a loop — a snapshot-time cost only.
- **The byte-copy growers** (`ARENA-BYTES-GROW`, `TV-GROW-ONE`, `TVK-GROW-ONE`)
  view buffers whose cells may hold `ptr u8`. The parametric cell copy already
  in the tree — `src/core/dynamic-storage.f` `COPY ( ptr a ptr a n -- )` —
  is the typed replacement.

#### What `ptr-field` and the view rows become

Once a record has a field door, neither cast has a job left, and both narrow.

**`ptr-field` admits only a declared pointer cell.** Its base must be storage
declared to hold an address — `PTR-VARIABLE`, `PERSISTED-PTR-VARIABLE`,
`PTR-U8-TABLE`, `PERSISTED-PTR-U8-TABLE-VARIABLE`, `TYPED-VARIABLE NAME ptr t`,
`TYPED-BUFFER`, a reserved cell declared by the definer above, or an element of
one of those tables. A base that is a raw cell is already refused
(`MD-RAW-FIELD`); a base that is only a parameter, or a declared cell holding a
scalar, is refused by a new code:

```
E-RAW-CELL-PTR  MD-FIELD-BASE-KIND  (the next free MD- code when it lands)
reason:  ptr-field: base is <kind>, not a declared pointer cell
repair:  declare_record_field — declare the record (STRUCTURE … DERIVE addr)
         and take the field through its generated accessor; ptr-field
         addresses a declared pointer cell only.
```

`<kind>` names what the base actually was — a parameter, a scalar-pointee cell,
a raw cell — which is the "third MD- code naming `ptr-field`'s base kind" the
dot asks for. `PTR-FIELD:` inside `BEGIN-STRUCTURE` stops generating a free
pointee and is retired with the rest of that definer.

**`byte-view` refuses a pointer pointee.** A base whose pointee is a pointer is
a declared cell or a record head, and a scalar cell behind one is a record
field:

```
E-RAW-CELL-PTR  MD-VIEW-PTR-BASE  (the next free MD- code when it lands)
reason:  byte-view: base holds an address; a scalar behind a pointer head is
         a record field
repair:  declare_record_field — declare the record and read the field through
         its accessor. A view may not reinterpret a cell declared to hold an
         address.
```

Both codes stay in the `E-RAW-CELL-PTR` family because both are the same
statement: a cell that holds an address is declared, and nothing reinterprets
it. Neither rule needs to see an offset, which is what killed the earlier
attempt — a declared record base was never in either row, so a migrated site
leaves the rule's reach entirely rather than arguing with it.

The rows the narrowing must **not** disturb are pinned as `K1`–`K6` in
`test/record-launder-probe.f`: a count behind a declared head (until its record
is declared), a `PTR-U8-TABLE` field, a declared cell's fetch, and the two
unrelated pointees one declared cell accepts today — which is the last
observation worth keeping, because it says why a declared cell alone is not a
record: its pointee is open, and a declared field pins it.

#### Migration order and cost

Prefix first, because `src/core/checker.f` cannot use a post-arm definer and
the engine has to keep building. The two rules land after the last wave, not
between them: a narrowing refuses every unmigrated site at once.

| wave | what | cost |
|---|---|---|
| **P** | T1 record-free: SYM to five columns; CWIN and `cell-effects.f` `STATE` to a head plus two variables; transaction state, include frame and registry to columns; the DYNAMIC-BUFFER control record to three declared cells; the reserved-cell declarator plus its two effect rows for the 17 engine-layout sites; the zero pass and the growers to typed loops | the largest wave — SYM touches ~20 call sites, the declarator 17, the rest are single-file |
| **1** | `tools/`: `LINT-SLAB` (one declaration, 7 `create … allot` sites become `TYPED-VARIABLE`, 6 accessor words are generated away) and the 13 pointer-slot families (one two-field record each, ~60 call sites rewritten from `X-A X-FIELD @` to `X SLOT:A @`); delete the dead helper at `check-core.f:137-144` | mechanical, no ABI |
| **2** | `lib/` readers and headers: `BUF`, `VEC`, `EDIT`, `JR`, `XML`, `MAP`, `TBL`, `BUILD` — each publishes a size constant its callers `create … allot` against, so each becomes a family plus a caller-side `TYPED-VARIABLE`/`TYPED-BUFFER` change | 10–20 field words per record, generated away; caller edits are one line each |
| **3** | `lib/` with storage questions: the xmodem session (nested `BUF` field — supported, a family field may name another family), the ZIP node and member (mmap — becomes a `DYNAMIC-BUFFER` arena or gains a typed allocation crossing), `pq` (struct-of-arrays plus the parameter arena) | design per record, not mechanical |
| **4** | `src/` T3: NSTR pool owner, IR arena descriptor, IR symbol index, AOT dict record, address-cell header | each is persisted or captured; see the marking dot |
| **after byte fields** | **TCB** (`lib/task.f`, inline semaphore byte blobs), `SA-ACT` (`lib/signal.f`, a foreign `struct sigaction` whose offset 0 holds a code address), the `CFIELD:` users in `src/arch/tic6x/` | blocked, see below |

#### Known gaps in this design

**Byte and inline-blob fields are outside the first cut.** A layout family's
fields occupy cell slots; the TCB carries inline semaphore byte blobs and
`CFIELD:` declares byte fields. The shape this expects is either a byte-extent
field kind — `TYPE-FIELD` already records `BYTE-OFF@`, `BYTES@` and `ALIGN@`, so
the schema has room — or a nested family standing for the blob. Until one
exists, every byte-field user stays on `BEGIN-STRUCTURE`, and `BEGIN-STRUCTURE`
therefore cannot be retired by this design alone.

**A foreign record is not a Habu record.** `struct sigaction` and the kernel's
argv vector have layouts someone else owns. A declared record is the right
description of them, but the design does not claim a declaration proves the
foreign layout; that stays what it is today — an asserted boundary with a test.

**Whole-value and field access must agree.** `MAKE`/`UNMAKE` and the field
accessors address the same cells, and nothing yet pins that they agree for every
field kind. The generator's acceptance below asks for that pin.

### 10.5 Implementation order

Each entry is one dot, with what it must prove. The order is a dependency
order, and the one constraint worth stating plainly is that **the two rule
narrowings land last**: they refuse ~75 cast-A sites and ~135 cast-B sites, so
every one of those has to be gone first — including the byte-field holdouts,
because `lib/task.f`'s TCB reaches its five pointer fields through the very row
`ptr-field` is narrowing.

1. **Private family generation** — *landed*, dot
   `habu-generate-a-private-80272413`. Package-scoped generation for a private
   family, into its own private wordlist, inside the declaration transaction.
   Nothing in `lib/` could use the facility without it. *Acceptance:* a private
   `STRUCTURE` in a package publishes its generated words privately; a second
   package cannot resolve them; a reject rolls the whole declaration back
   byte-identically. Pinned as cases 13-14 of
   `test/structure-decl-suite.f`.
2. **The accessor generator** — *landed*, dot
   `habu-generate-typed-field-ba63866e` (tracker id
   `habu-structure-generate-field-b9dc52f8`), the production `field-project`
   runtime word, and `record-at` with its generated `F:AT`. `DERIVE addr` is the
   third derive code; a public family publishes `PKG-FAMILY:field`,
   `PKG-FAMILY:AT`, `PKG-FAMILY:BYTES` and `PKG-FAMILY:CELLS`, a private one
   `FAMILY-FIELD`, `FAMILY-AT`, `FAMILY-BYTES` and `FAMILY-CELLS` in the
   declaring package's private wordlist. The words are minted by a **sixth
   declaration-transaction participant** (`ORDER-ADDRESS` 830,
   `src/core/structure-make.f`) in the COMMIT phase, because an accessor is armed
   with a **committed** field id and `DECL-EVENT` (800) commits the field rows
   first; `;STRUCTURE` only arms the family. The arming rides the plan row that
   names the word, so the candidate preflight and the evaluator each arm the
   single-shot window for their own reading. A field named like a generated
   member (`make`, `unmake`, `at`, `bytes`, `cells`, `eq`, `hash`, `tag`) is
   refused at its own token when the family derives `addr`, and a fieldless
   family cannot derive it at all. The same lift makes `DERIVE eq` and
   `DERIVE hash` work for a private family, in the private spelling. *Acceptance,
   pinned as case 15 of `test/structure-decl-suite.f`:* `DERIVE addr` publishes
   one accessor per field with the declared effect; a pointer field projects as
   `ptr ptr t` and a generic field at the caller's instantiation; every negative
   in `test/field-proj-suite.f` still rejects when reached through a generated
   accessor; `F:AT` strides by the committed width and preserves the family; a
   field read through an accessor equals the same field read through `UNMAKE`,
   for a pointer field, a scalar field and a nested family field; a rejected
   declaration rolls back byte-identically. Replay publishes the accessors too,
   which item 3 below owns.
3. **The verify-source replay arm** — *landed*. The address participant (830)
   takes the replay branch its constructor sibling (820) already had:
   `TDECL-ADDR-REPLAY` renders the SAME plan the live path evaluates and stops
   after its first reading, so each row is armed and checked and the accessor's
   effect is registered under its own name, and no row reaches the evaluator, so
   no code is emitted and no runtime dictionary entry appears. Nothing restates a
   spelling: the names and signatures come from the plan the generator renders.
   The arming is the live one — this participant commits after `DECL-EVENT` (800)
   has advanced the committed field watermark on the replay path exactly as on
   the live one, so the field ids the plan bakes are committed ids and the
   field-projection window reads them; an unarmed reading would reject the
   accessor body, which is how the fixture measures it. The reserved-cell definer
   is in the verify-source definer table and in `src/core/cell-effects.f`, with
   its case in `tools/lint/def.f`. *Acceptance, pinned as case 6 of
   `test/decl-replay-verify-source.f` and in `test/pointer-storage-test.f`:* a
   file that uses its own record accessors — public spelling and private — passes
   the pre-scan, while a use that disagrees with the registered effect is
   refused, and the private surface stays private; a pre-checker definer with
   both rows is reachable from checked source and its sibling with neither
   (`NULL-PTR-CELL`, same file, same phase) is not. The seal runs when the engine
   is built, so the rowless half is measured on that real sibling rather than on
   a clone written in a test, which would be ordinary checked source.
4. **`ptr-cell-mark` for a persisted record's pointer fields.** *Acceptance:* a
   declared record in a persisted arena has each pointer field marked exactly
   once; a restored image reads them back; a scalar field is not marked; a
   record whose pointer column became a declared table is marked once per table.
5. **The T1 record-free migration**, including SYM's five columns, the
   DYNAMIC-BUFFER control cells, the reserved-cell declarator over the 17
   engine-layout sites, the typed zero pass, and the typed growers.
   *Acceptance:* no mixed record remains in the pre-arm prefix; the engine
   rebuilds to a byte fixpoint; the snapshot marks one cell per persisted table
   instead of two per live SYM row.
6. **The `tools/` migration** — `LINT-SLAB` and the 13 pointer-slot families,
   and the dead helper at `tools/check-core.f:137-144` deleted rather than
   migrated. *Acceptance per site:* the record is declared, every accessor is
   generated, no cast remains, and the lint and check suites assert exactly what
   they asserted before.
7. **The `lib/` migrations, one dot per record** (`BUF`, `VEC`, `EDIT`, `JR`,
   `XML`, `MAP`, `TBL`, `BUILD`, then the xmodem session, the ZIP node and
   member — whose dot also decides arena-or-crossing — and `pq`), then **`src/`
   T3** (NSTR pool owner, IR arena descriptor, IR symbol index, AOT dict record,
   address-cell header). *Acceptance per record:* declared, accessors generated,
   no `ptr-field` or view cast left for it, its own suite unchanged in what it
   asserts, and for a captured record its marking pinned.
8. **Byte and inline-blob fields.** *Acceptance:* a byte-extent field kind or a
   nested-blob family; `CFIELD:`'s users converted; TCB declared with its five
   pointer fields and its semaphore blobs; `BEGIN-STRUCTURE` retired.
9. **The two rule narrowings.** `MD-FIELD-BASE-KIND` and `MD-VIEW-PTR-BASE`,
   taking the next free `MD-` codes when they land (the vocabulary grows in
   between: `MD-UNDERFLOW` took 28 on 2026-09-18). *Acceptance:* `V8` and `V9` in
   `test/typed-storage-structural-test.f` and `L1`–`L4` in
   `test/record-launder-probe.f` flip from certified to rejected, each with its
   own code, reason and repair class; `K1`–`K6` and every control in
   `test/compiler/raw-cell-pointer-refusals.f` keep their verdicts; a
   rule-hosted generation build completes; `test/run.f` is green.

## 11. Bounded pointers

A `ptr t` says what it points at and nothing about how far it reaches, so every
copy into a buffer and every indexed read is bounded by a hand-written
`u cap > if throw` or by nothing at all. `lib/span.f` (package `SPAN`) gives the
reach a type. A span is one two-cell value — a base `ptr t` and a reach — and it
travels on the stack like any other family value, so nothing in the engine
changed to carry it:

```forth
STRUCTURE span 1
   FIELD base ptr a
   FIELD len n
;STRUCTURE
```

**The reach is counted in bytes, and that is a soundness decision.** A family
argument unifies with the ordinary integer widening lattice rather than
strictly, so `span<u8>` IS accepted where `span<cell>` is declared — measured,
and pinned in `lib/span-test.f`. If the reach counted elements, a 64-byte
`span<u8>` handed to a word declared over `span<cell>` would pass `i < len` at
i=7 and cell-read bytes 56..63, eight times past its end: the exact overrun the
type exists to refuse. Counting bytes removes it — `SPAN:CELL-AT` checks
`i < reach/CELL`, every other accessor checks bytes, and the widened call stays
inside the buffer. `n` is worse than `cell` here: it is the universal integer, so
`span<n>` accepts every integer-element span in both directions, which is why no
word in the file is declared over it. Closing the widening itself belongs to the
checker (the campaign's band 3), not to a declaration.

The mint follows the same widening: `( ptr u8 n -- span<cell> ) SPAN:MAKE`
certifies, while `( ptr cell n -- span<u8> )` is refused. The `STW-MINT` and
`STW-MINT2` cases in `lib/span-test.f` pin both directions. Use `SPAN:BYTES`
for an explicit byte view of a cell span.

**One audited crossing.** `SPAN:MAKE ( ptr t n -- span<t> )` is where an address
and a number become a reach, and it takes that reach on the caller's word: the
checker knows the pointee, never the extent behind it. It is admitted in `lib/`
and `src/` only, where the producers live — `n SPAN-BUFFER: NAME`,
`n SPAN-CELLS: NAME`, `MEM:ALLOC-SPAN` — and `tools/lint/bare-copy-lint.f`
reports it, and a bare `BYTE-COPY`, anywhere else, naming the site and the
replacement. It needs no `TRUST`: the body is the generated constructor plus a
sign check, and the audit is the lint.

**Narrowing never widens.** `SPAN:SKIP`, `SPAN:TAKE` and `SPAN:SUB` refuse an
offset past the reach with `E-SPAN-RANGE` and can only produce a span inside the
one they were given; `SPAN:COPY` refuses a source longer than the destination's
reach with `E-SPAN-CAPACITY` and copies nothing at all in that case; a negative
reach at the mint or a negative source length is `E-SPAN-LENGTH`.

**What is deliberately absent.** A generic element-indexed accessor: `+` on a
`ptr t` is byte arithmetic and checked code has no element size for a type
parameter, so an `AT` that steps by elements is not expressible — the byte set
(`AT`, `U8@`, `U8!`) works over `span<u8>`, the cell set (`CELL-AT`, `CELL@`,
`CELL!`, `CELL-LEN`) over `span<cell>`, and the narrowings are byte offsets for
every element type. There is no `TYPED-BUFFER` span form either: its generated
accessor is an index function `( n -- ptr t )`, not a base-and-reach pair, and
adding one means changing the generated accessor set at the sealed generative
storage boundary (`src/core/layout-buffer.f`). A span over a nominal element is
already reachable — hand a `TYPED-BUFFER` accessor result to `SPAN:MAKE`, which
is how `lib/span-test.f` builds its `span<NUM:index>`.

An open instantiation (`span<a>`) can be bound to a local and duplicated: its
argument occurs only as a pointee, so every instance occupies the same two
cells and carries no linear owner. `lib/span-test.f`'s `T-OPEN-TRANSPORT` pins
both operations. A family whose width depends on an unresolved argument still
has a conservative one-cell representation. The generic narrowings use the
return stack deliberately to avoid a local frame, as `lib/span.f` explains.

**Cost.** Summing 4096 bytes, 200 passes, aarch64, engine `7c8b9db7`: `SPAN:U8@`
24 ns/byte, the same loop with a hand-written bounds check 17 ns/byte, a bare
`c@` with no check 10 ns/byte (five runs, identical to the nanosecond). So the
checked span read costs 2.4x a bare byte read and 1.4x the hand-written check it
replaces. Static elimination of a bounds check whose index is already known to be
in range is a later optimisation; release quality comes first.

## 12. Where the deep detail lives

- `docs/forth.md` — the working standard: naming, packages, factoring, the
  checker and type model section, testing, and the commit gate. Note that its
  Structures And Enums section still describes the unified `STRUCTURE` opener
  as unimplemented; it is implemented and in production use, and a declaration
  loads cleanly today.
- `docs/type-families.md` — the full family design and its history.
- `docs/value-nominal-substrate.md` — why nominal cell families are shaped the
  way they are.
- `docs/extent-substrate.md` — the extent nominal substrate that Loom's tensor
  and kernel code builds on.
- `docs/effects.md` — the effect language itself.
- `docs/typed-top-level.md` — what may and may not run at the interpreter.
The best worked examples in the tree, if you would rather read code:
`lib/num-types.f` for nominals, `lib/adt/option.f` and `lib/adt/result.f`
for generics, and in Loom (`../loom/maki/`) `infer/safetensors.f` for linear owners
and transition chains, `infer/gpt2-config.f` for a validating constructor and a
proof token, and `infer/gpt2-pin.f` for the smallest possible authority — a
package of constants and three typed facts that a configuration file leaves
unsaid.
