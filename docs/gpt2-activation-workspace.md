# GPT-2 activation workspace (forward leaf 8)

Frozen design for the activation workspace named as leaf 8 in
`.blackboard/gpt2-forward-leaf-design-20260727.md`. That document remains the
authority for weight ownership (one WSTORE-backed model plus one linear
widened `weight-set`); this one adds nothing to it and changes none of it.

Scope is the activation workspace only. Out of scope by instruction: any
TENSOR migration, any WSTORE change, any executor migration, any second
storage registry, and any generic public pointer-plus-count constructor.

## The owner

```
DEFLINEAR GPT2-FORWARD:arena           \ linear one-cell owner: the base pointer

STRUCTURE workspace 0
   FIELD arena GPT2-FORWARD:arena      \ linear by containment
   FIELD tmax  n                       \ T: tokens the layout was sized for
   FIELD width n                       \ E: embedding width
   FIELD heads n                       \ H
   FIELD vocab n                       \ V
;STRUCTURE
```

`workspace` is linear because it transitively owns `arena`, the landed
pattern `WSTORE:store` already uses for `SAFET:mapping`
(`maki/infer/weight-store.f:177-180`, containment pinned to depth three at
`test/structure-decl-suite.f:570-594`). The geometry fields are plain `n`;
only the arena is linear, so `MAKE`/`UNMAKE` conserve the owner while the
numbers stay copyable.

**The geometry is the capacity witness.** Total size is a pure function of
`(T, E, H, V)`, so release recomputes exactly what construction allocated and
no stored length can drift from the real allocation. This is the discipline
`WSTORE:BLK-FREE` already uses (it derives a block's length from the block's
own slot count, `weight-store.f:382-387`).

There is no public constructor from a pointer and a length, and no arm for
adopted memory: the arena is allocated inside the mint and released by the
single exit.

## Regions

Regions are **private to the package**. They never appear in a public effect,
so "borrowed only while the owner is present" holds structurally: every
accessor threads the workspace, and no region value can be built, stored, or
transported by a caller.

```
private
STRUCTURE region 0                     \ private; MAKE/UNMAKE are private
   FIELD base ptr                      \ cell-addressed (ptr a)
   FIELD cells n
;STRUCTURE
```

Eleven named regions, each a pure function of the geometry. `D = E/H`.

| region | shape | cells | lifetime / reuse |
|---|---|---|---|
| `STREAM` | [T,E] | `T*E` | the residual carrier; live for the whole forward, updated in place |
| `NORMED` | [T,E] | `T*E` | pre-norm output; dies at the end of each sublayer |
| `FUSED` | [T,3E] | `3*T*E` | fused QKV projection, per-token Q then K then V; dies after the split |
| `Q` `K` `V` | [H][T,D] | `T*E` each | head-major, so each per-head [T,D] pane is contiguous |
| `SCORE` | [T,T] | `T*T` | one pane REUSED per head; contents die at each head boundary |
| `CONTEXT` | [T,E] | `T*E` | accumulates head by head at head offsets |
| `ATTN-OUT` | [T,E] | `T*E` | output projection target |
| `MLP-HID` | [T,F] | `T*F` | F = 4E for GPT-2 |
| `LOGITS` | [T,V] | `T*V` | final projection |

Region accessors, all private, all threading the owner:

```
: STREAM ( workspace -- workspace region )
```
…and identically for the other ten. Per-head panes are derived privately from
`Q`/`K`/`V`/`CONTEXT` by head index, with the index proven against `H` before
the offset is formed.

**F is 4E by architecture, not by assumption.** `MDLCFG`'s `arch` ENUM gives
the llama arm an explicit `ffn-dim` field and the gpt2 arm none
(`maki/infer/model-config.f:70-73`), so for GPT-2 the feed-forward width is
defined as four times the embedding width. The leaf states this derivation
explicitly; it does not read a field that does not exist.

## Layout formula

All regions are f64 cells, so every size is cell-aligned by construction and
no padding is inserted. Offsets are the running prefix sum in the table's
order; non-overlap is therefore structural rather than asserted, and the
preflight proves containment and the absence of arithmetic overflow.

```
total_cells = T*(10E + F + V) + T*T          \ F = 4E  =>  T*(14E + V) + T²
```

Every product and sum is computed with the landed overflow-checked CAD
arithmetic — `CAD-NUM:MUL-ITEMS`, `SCALE-CELLS`, `ADD-CELLS`
(`lib/cad-num-arithmetic.f:206-217`) — which return `numeric-result<role>`
rather than a raw cell, so an overflow is a value and never a wrapped number.

Note for the leaf, stated rather than silently decided: `LOGITS` dominates.
At GPT-2 small geometry (T=1024, E=768, V=50257) it is 51.5M of the 63.5M
total cells, about 412 MB of 508 MB. If a later leaf establishes that decode
needs only the final row, narrowing `LOGITS` to [1,V] changes this formula and
nothing else — no interface moves.

## Construction and release

```
: WORKSPACE-NEW ( MDLCFG:mcfg n -- MDLCFG:mcfg workspace-result )
      \ n = requested max tokens
: WORKSPACE-RELEASE ( workspace -- )                          \ TOTAL

ENUM workspace-result 0
   VARIANT ready  FIELD ws workspace ;VARIANT                 \ linear payload
   VARIANT failed FIELD code n ;VARIANT
;ENUM
```

`WORKSPACE-NEW` reads geometry through the public config accessors
(`MDLCFG:MC-COMMON`, `MC-ARM`), never through constants.

**`WORKSPACE-RELEASE` is total** — it consumes the owner, recomputes the byte
length from the geometry, and calls `MEM:RELEASE-BYTES`, which after
`habu-make-owned-release-79de2b5c` cannot return a recoverable failure. So
this leaf **depends on that release leaf landing**; until it does, the release
path has an edge that can throw past a consumed owner, and the workspace must
not be built on it.

### Failure ordering

Every refusal happens before the allocation, so no failure path can leave
memory owned by nobody, and no refusal can occur after a write:

1. reject non-positive T, E, H, V; reject requested T greater than the
   config's context length
2. reject H that does not divide E (D would not be whole)
3. reject a non-F32 dtype and untied embeddings, per the frozen forward
   design's config semantics
4. compute every region size and the total with the overflow-checked CAD
   operations; any `overflow` arm becomes `failed(code)`
5. convert the total to `CAD-NUM:alloc-byte-len`; a failure here is
   `failed(code)`
6. allocate once via `MEM:ALLOC-BYTES`; on failure return `failed(code)`
   having allocated nothing
7. only now `MAKE` the workspace and publish `ready`

Nothing is read back out of a `catch` argument cell anywhere in this
sequence. `catch` does **not** restore pre-call argument values on a throw
path — `docs/forth.md:1104` claims it does and that claim is false, measured
and frozen in `habu-prove-catch-restores-2f368434`.

Named throw codes are allocated from the forward package's block by the error
code registry at implementation time; this document does not invent numbers
that could collide.

## The one audited crossing, named rather than hidden

`MEM:ALLOC-CELLS ( CAD-NUM:alloc-cell-count -- ptr a )` discards its extent
and **there is no `RELEASE-CELLS`** — the only release is
`RELEASE-BYTES ( ptr u8 CAD-NUM:alloc-byte-len -- )` (`lib/memory.f:185-213`).
A workspace allocated in cells therefore could not be released at all.

So the arena is allocated with `ALLOC-BYTES` and held as `ptr u8`, and the
package carries exactly one private `TRUSTED:` byte-to-cell base conversion,
the mirror of `WSTORE:BLK>BYTES` (`weight-store.f:261`). It is applied once
per region accessor, never exposed, and it is the single unchecked boundary
in this design. A dot records the missing typed capability — a cell-addressed
region role, or a `MEM:RELEASE-CELLS` sibling funnelling into the same fatal
sink — and the boundary is removed when it lands.

## Corrections to the frozen forward design

Today's review supersedes three of its primitive leaves. The 22-leaf chain,
its numbering, and its dependencies are otherwise unchanged.

- Leaves 1, 3, and 4 must **not** publish `LN-ROWS-AFFINE`,
  `TOKEN-EMBED-ROWS`, or `GELU-ROWS` as public raw pointer-and-count
  mutators. Each becomes a private row loop inside the owning package,
  operating on an owner-issued region and composing the existing public
  scalar and per-row words. No new public unchecked mutator surface is added
  by the forward chain.
- Leaf 2 (`MAKI:MATMUL-RIGHT-T`) proceeds **only** as consolidation of the
  existing raw matmul family. It is not the activation authority and gains no
  new role.
- Every capacity, overlap, token-bound, and shape refusal happens before the
  first write, in all three leaves.

## Leaf split

Each is under thirty minutes and independently reviewable.

| # | leaf | owned result | depends |
|---|---|---|---|
| 8a | geometry and layout arithmetic | the pure functions from `(T,E,H,V)` to every region size and the total, on overflow-checked CAD operations; no allocation, no owner. Tests: zero and negative extents, H not dividing E, T above context length, product overflow, the maximum accepted total and maximum-plus-one | MDLCFG accessors |
| 8b | the linear owner | `arena`, `workspace`, `workspace-result`, `WORKSPACE-NEW`, total `WORKSPACE-RELEASE`, the audited byte-to-cell crossing and its dot. Tests: dropped owner fails certification, double release fails certification, allocation failure allocates nothing and publishes no partial state, every refusal precedes allocation | 8a; `habu-make-owned-release-79de2b5c` |
| 8c | region accessors | the eleven private accessors and the per-head pane derivation. Tests: every region lies inside the arena; adjacent regions do not overlap; the last cell of the last region is the last cell of the allocation; a head index at or above H refuses before any offset is formed | 8b |

Leaves 10 through 15 consume 8c unchanged from the frozen chain.
