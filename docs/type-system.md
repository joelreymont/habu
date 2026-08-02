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
without any confusion between them. `lib/cad-num-types.f`
is the reference example: package `CAD-NUM` declares ten of them —
`byte-len`, `item-count`, `cell-count`, `index`, `byte-off`, `cell-off`,
`alignment`, `positive-divisor`, `alloc-byte-len`, `alloc-cell-count` — and
that one file is why a byte count cannot be passed where a cell count belongs
anywhere downstream of it.

Strictness is the whole point, and the probe that settles it is always the
same. Write a checked word that returns a raw `7` where the nominal is
declared, and watch it fail: `expected: cad-num:byte-len<> actual: n`, exit 70.

`CAD-NUM` also shows the discipline that makes nominals worth having. Its
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
`MDLCFG:cfg-proof` in `maki/infer/model-config.f` and `GPT2TENSOR:layer-proof`
in `maki/infer/gpt2-tensor.f` are the two live examples. Both files are honest
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
`option<n>`, `option<CAD-NUM:index>` — each a different concrete type from
the same declaration. A declaration with zero parameters is still a family;
it just has exactly one member, and the checker prints it with an empty
parameter list — when an error message says `maki:datatype<>`, that trailing
`<>` is the checker naming a zero-parameter family instance, not a typo.

So: records and tagged alternatives are families, the generic containers are
families, and the nominal wrappers of § 4 are families too — one substrate,
several declaring words.

- **`STRUCTURE name arity … FIELD f type … ;STRUCTURE`** declares a record:
  several named fields travelling together as one value. `MDLCFG:mcfg` is a
  eleven-field example.
- **`ENUM name … ;ENUM`** declares a set of alternatives. In its short form the
  body is bare variant names and nothing else, which gives you a plain tag set:
  `MAKI:datatype` (`maki/tensor.f:123`) is five names, `df32` through `di32`, and
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
doubled. `STRUCTURE mcfg` inside `package MDLCFG` produces
`MDLCFG-MCFG:MAKE`. This spelling rule creates one trap:
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

- A multi-cell value **cannot be bound to a typed local**. Writing
  `{: p:pair :}` for a `STRUCTURE pair` is rejected with `unknown type
  'p:pair' in signature`, exit 70. Consume it straight off the stack with
  `UNMAKE` or `MATCH` instead, deepest field first. Single-cell families are
  fine in locals — `{: tok:cfg-proof :}` and `{: dt:MAKI:datatype :}` both appear
  throughout `maki/infer/model-config.f`.
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

The worked example is the checkpoint loader, `maki/infer/safetensors.f`. It
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
exactly one shape and needs no tag. The gap: `option<CAD-NUM:index>` works and is used in
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

**Multi-cell and linear values cannot live in locals.** Both refusals are
correct (§ 5, § 6), but together they push some code into stack discipline that
reads worse than it should. The factoring idioms in `docs/forth.md` are the
answer for now: when the juggling gets deep, the real fix is almost always
another small word whose entry consumes the bundle.

## 10. Where the deep detail lives

- `docs/forth.md` — the working standard: naming, packages, factoring, the
  checker and type model section, testing, and the commit gate. Note that its
  Structures And Enums section still describes the unified `STRUCTURE` opener
  as unimplemented; it is implemented and in production use, and a declaration
  loads cleanly today.
- `docs/type-families.md` — the full family design and its history.
- `docs/value-nominal-substrate.md` — why nominal cell families are shaped the
  way they are.
- `docs/extent-substrate.md` — the shape and extent layer used by the tensor
  and PTX code.
- `docs/effects.md` — the effect language itself.
- `docs/typed-top-level.md` — what may and may not run at the interpreter.
The best worked examples in the tree, if you would rather read code:
`lib/cad-num-types.f` for nominals, `lib/adt/option.f` and `lib/adt/result.f`
for generics, `maki/infer/safetensors.f` for linear owners and transition
chains, `maki/infer/model-config.f` for a validating constructor and a proof
token, and `maki/infer/gpt2-pin.f` for the smallest possible authority — a
package of constants and three typed facts that a configuration file leaves
unsaid.
