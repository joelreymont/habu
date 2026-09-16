# Scoped borrows: the ownership model for checked Habu

Status: proposal for campaign C2 (`habu-campaign-c2-mem-c3d7662b`), child
`habu-write-the-checked-035516db`. It asks Joel to narrow one standing
decision; nothing here is implemented.

## The standing decision and why it is revisited

[type-system.md](type-system.md) section 9 records the decision of
2026-07-30: a pointer says what it points at and nothing about how long it is
valid; safety is carried by linear owners; a borrowed span inside a scoped word
is advisory; a region-and-borrow system was rejected as machinery for a threat
model we do not have.

The threat model changed on 2026-09-16: the code is to be generated, at
volume, by a model, and read by people only when it fails. Two failure classes
are already on record and both pass the checker today:

1. A span handed to a quotation by a scoped owner is stored and read after
   the owner is released: the safetensors crash after `RELEASE` cited by
   `habu-checker-ptr-lifetime-f59d1e9d`, and Tender's lesson about quotation
   locals overwritten by later pushes.
2. A word's hidden package scratch is overwritten by a nested or recursive
   call while the caller still needs it: the compiler and emitter helpers cited
   by `habu-add-lexical-mutable-725b49eb`.

Neither needs regions or lifetime arithmetic. Both are stopped by one rule.

## The rule

A borrow flows down the stack and never anywhere else.

A scoped owner word (today `MEM:WITH-BYTES`, tomorrow every `WITH-*` word)
gives its quotation a value of a new nominal kind, `borrow<T>` for shared
read access or `mut<T>` for unique access, instead of a bare `ptr u8 n`. The
checker enforces, at the point of use:

| Attempt | Verdict |
| --- | --- |
| pass the borrow to a callee as an input | allowed; the callee's effect names the borrow kind |
| copy a `borrow<T>` with `dup` or into a local | allowed |
| copy a `mut<T>` | rejected `E-BORROW-DUP`: it is linear inside the body |
| store either kind into any cell (`!`, field store, `+USER` cell) | rejected `E-BORROW-ESCAPE` |
| leave either kind in the quotation's outputs | rejected `E-BORROW-RETURN` |
| capture either kind in a deferred word, xt cell or `is` | rejected `E-BORROW-CAPTURE` |
| hold a `mut<T>` while the same owner yields a second borrow | rejected `E-BORROW-OVERLAP` |

Pointers stay lifetime-free: `ptr u8 n` remains the raw type for foreign
boundaries, image capture and code that has not moved yet. The rule is about
a value kind that cannot be stored, not about annotating every pointer. That
is the "second-class reference" design: Rust's early references, Hylo's
`let` and `inout` parameters. It costs no runtime and no lifetime solving,
because a value that cannot be stored cannot outlive the stack frame that
received it.

Scratch state is the same rule applied to packages. A word that uses a
package-private scratch buffer declares it in its effect as `mut<scratch>`;
the checker treats the package scratch as one owner, so a nested call that
declares the same scratch while the caller holds it is `E-BORROW-OVERLAP`.
Recursion through a scratch-holding word is refused unless the word takes a
fresh scoped buffer instead.

## What changes where

- `src/core/checker.f`: two new nominal kinds with the table above; the
  quotation checker's output rule; the store rule; the capture rule.
- `lib/memory.f`: `WITH-BYTES` yields `mut<bytes>`; a new `WITH-READ` yields
  `borrow<bytes>` for readers; `RELEASE-BYTES` and `UNMAP` are unchanged.
- `lib/fs.f` and the mapping words: mapped spans become borrows.
- `docs/type-system.md`: section 9's paragraph is replaced by a section that
  states this rule; section 6 gains the two kinds beside linear owners.
- `docs/forth.md`: one paragraph on when to write `borrow<T>` and `mut<T>`.
- Tender's OPC module: the proof. Its intrusive buffer lists become one owner
  per package plus borrows handed to readers; the manual free list goes.

Existing dots absorbed by this design: `habu-add-immutable-lexical-28b79e06`
(read borrows), `habu-add-lexical-mutable-725b49eb` (scratch frames),
`habu-checker-ptr-lifetime-f59d1e9d` (the escape), `habu-add-linear-capture-172b29da`
(phases become `mut<phase>` handed along the capture chain), and the scoped
memory work Cedar integrated.

## Acceptance

- Positive fixtures: a reader body that copies a `borrow`, a callee that takes
  a `mut`, nested scoped owners of different kinds.
- Negative fixtures, one per verdict in the table, each failing with its code
  at the offending token.
- The safetensors escape reproducer and the compiler scratch overwrite both
  reject at `CHECK!`.
- Tender's OPC rewrite loads and its suite passes; the count of `ptr u8 n`
  parameters in `lib/` drops and the drop is reported.

## Decision needed

Joel confirms narrowing the 2026-07-30 note to: pointers carry no lifetime,
borrows cannot be stored. Then the C2 children are amended to this vocabulary
and dispatched in the order: checker kinds, memory words, fs words, OPC proof.
