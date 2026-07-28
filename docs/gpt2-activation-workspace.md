# GPT-2 activation workspace (forward leaf 8)

Frozen design for the activation workspace named as leaf 8 in
`.blackboard/gpt2-forward-leaf-design-20260727.md`. That document remains the
authority for weight ownership; this one adds nothing to it and changes none
of it.

Scope is the activation workspace only. Out of scope by instruction: any
TENSOR migration, any WSTORE change, any executor migration, any second
storage registry, and any public constructor from a pointer and a length.

## The owner

```
DEFLINEAR GPT2-FORWARD:workspace
```

One public linear token, and nothing else public that describes storage.
There is no public `STRUCTURE`, so there are no generated public
`MAKE`/`UNMAKE` and no way for a caller to build a workspace value at all.
This is the correction that matters most: an earlier draft made the workspace
a public structure whose geometry fields release recomputed from, which let a
caller rebuild the record with different geometry and drive release to unmap
a span that was never allocated. Geometry supplied by a caller is not an
allocation witness, and no field a caller can write may reach the release
path.

The token is represented by the base of **one** allocation laid out as a
private aligned header followed by the arena:

```
[ header : 27 cells ][ arena : region data ]
```

| header cell | contents |
|---|---|
| 0 | the **exact** allocation byte length returned by `MEM:ALLOC-BYTES` |
| 1–4 | validated T, E, H, V |
| 5–26 | eleven validated (offset, cells) pairs, one per region, offsets absolute from the allocation base |

Everything in the header is written **before the token is minted**, from
values already validated. Nothing outside the package can read or write it.

### The only representation crossings

Three private `TRUSTED:` words, the shape `WSTORE` already uses
(`MINT-TBUILDER` / `TAKE-TABLE`, `maki/infer/weight-store.f:221-231`):

```
TRUSTED: MINT-WORKSPACE ( ptr u8 -- workspace )        \ base -> token
TRUSTED: WS-BASE  ( workspace -- workspace ptr u8 )    \ non-consuming read
TRUSTED: WS-TAKE  ( workspace -- ptr u8 )              \ consuming, release only
```

No other word converts between the token and an address. The header
constructors and the layout arithmetic are private. `MINT-WORKSPACE` is
called at exactly one site, after the header is complete.

## Construction and release

```
: WORKSPACE-NEW     ( MDLCFG:mcfg n -- MDLCFG:mcfg workspace )   \ n = max tokens
: WORKSPACE-RELEASE ( workspace -- )                             \ TOTAL
```

### Allocation failure, from real MEM behavior

`MEM:ALLOC-BYTES` has **no failure return**: it reaches `MEM-ALLOC-PTR`,
which throws `E-MEM-MAP` when `mmap` fails (`lib/memory.f:54-55`, `:185-187`).
So `WORKSPACE-NEW` **throws** and there is no result union. That is the
proved mechanism rather than a promised one.

This is owner-safe by ordering, not by recovery: every refusal and the
allocation itself happen **before** `MINT-WORKSPACE`, so at every throw point
no workspace token exists and nothing can be stranded. Nothing is caught, and
nothing is read back out of a `catch` argument cell — `catch` does not
restore pre-call argument values on a throw path (`docs/forth.md:1104` claims
it does and is false; measured and frozen in
`habu-prove-catch-restores-2f368434`).

### Failure ordering

1. reject non-positive T, E, H, V; reject requested T above the config's
   context length
2. reject H that does not divide E
3. reject a non-F32 dtype and untied embeddings, per the frozen design's
   config semantics
4. compute every region size, offset, and the total through the landed
   overflow-checked CAD operations — `CAD-NUM:MUL-ITEMS`, `SCALE-CELLS`,
   `ADD-CELLS` (`lib/cad-num-arithmetic.f:206-217`) — which return
   `numeric-result<role>`, so an overflow is a value and never a wrapped
   number; any `overflow` arm throws a named code
5. narrow the total to `CAD-NUM:alloc-byte-len` (`MEM:BYTES-ALLOC-LEN`
   throws `E-MEM-SIZE` on zero, negative, or overflow)
6. allocate once via `MEM:ALLOC-BYTES`; on failure it throws `E-MEM-MAP` with
   nothing allocated and no token in existence
7. write the header: the exact returned length, the validated geometry, the
   validated offsets
8. `MINT-WORKSPACE` — the first moment a workspace exists

### Release

`WORKSPACE-RELEASE` consumes the token via `WS-TAKE`, reads the exact length
**from header cell 0**, and calls `MEM:RELEASE-BYTES`. It never reconstructs
a length from geometry and never touches a caller-visible value, so the span
released is always exactly the span allocated.

It is **total**: after `habu-make-owned-release-79de2b5c`,
`MEM:RELEASE-BYTES` cannot return a recoverable failure. This leaf therefore
**depends on that leaf landing**; until it does, the release path can throw
past a consumed owner and the workspace must not be built on it.

## Regions

Region identity is a compact enum, never a value carrying an address:

```
private
ENUM region-id  stream normed fused q k v score context attn-out mlp-hid logits ;ENUM
```

Views are private and bounded:

```
: VIEW ( workspace region-id -- workspace ptr a n )   \ private
```

`VIEW` reads the region's validated (offset, cells) pair from the header — it
performs no arithmetic on caller-supplied numbers — and is used only inside
the package's own compute words. **No region, view, pointer, or count appears
in any public effect.** The public forward and decode words thread only the
workspace token.

`D = E/H`. Per-head panes are derived privately from `Q`/`K`/`V`/`CONTEXT`
with the head index proven against `H` before any offset is formed.

| region | shape | cells | lifetime / reuse |
|---|---|---|---|
| `stream` | [T,E] | `T*E` | residual carrier; live for the whole forward, updated in place |
| `normed` | [T,E] | `T*E` | pre-norm output; dies at the end of each sublayer |
| `fused` | [T,3E] | `3*T*E` | fused QKV, per-token Q then K then V; dies after the split |
| `q` `k` `v` | [H][T,D] | `T*E` each | head-major, so each per-head [T,D] pane is contiguous |
| `score` | [T,T] | `T*T` | one pane reused per head; contents die at each head boundary |
| `context` | [T,E] | `T*E` | accumulates head by head at head offsets |
| `attn-out` | [T,E] | `T*E` | output projection target |
| `mlp-hid` | [T,F] | `T*F` | F = 4E |
| `logits` | **[1,V]** | `V` | the final-position row only |

**`logits` is the final-position row.** The frozen chain's "all sixteen tiny
logits" means that row's sixteen vocabulary values, not a [T,V] plane. An
earlier draft sized it [T,V], which would waste about 404 MB at GPT-2 small
and scale catastrophically. Observation of intermediate boundaries uses the
existing probe sink, not a resident plane.

**F is 4E by architecture, not assumption.** `MDLCFG`'s `arch` ENUM gives the
llama arm an explicit `ffn-dim` and the gpt2 arm none
(`maki/infer/model-config.f:70-73`), so for GPT-2 the feed-forward width is
defined as four times the embedding width.

## Layout formula

All regions are f64 cells, so every size is cell-aligned and no padding is
inserted between them. Offsets are the running prefix sum in table order
after the header, so non-overlap is structural; the preflight proves
containment and the absence of overflow.

```
total_cells = header_cells + T*(10E + F) + T*T + V        \ F = 4E
            = 27 + T*14E + T² + V
```

At GPT-2 small (T=1024, E=768, V=50257) that is about 12.1M cells, 97 MB —
against 508 MB for the rejected [T,V] shape.

## Corrections to the frozen forward design

Today's review supersedes three primitive leaves. The 22-leaf chain, its
numbering, and its dependencies are otherwise unchanged.

- Leaves 1, 3, and 4 must **not** publish `LN-ROWS-AFFINE`,
  `TOKEN-EMBED-ROWS`, or `GELU-ROWS` as public raw pointer-and-count
  mutators. Each becomes a private row loop inside the owning package,
  operating on a private view and composing the existing public scalar and
  per-row words. The forward chain adds no new public unchecked mutator
  surface.
- Leaf 2 (`MAKI:MATMUL-RIGHT-T`) proceeds **only** as consolidation of the
  existing raw matmul family. It is not the activation authority.
- Every capacity, overlap, token-bound, and shape refusal happens before the
  first write.

## Leaf split

| # | leaf | owned result | depends |
|---|---|---|---|
| 8a | pure checked layout | the pure functions from `(T,E,H,V)` to every region size, every offset, the header size, and the total, on the overflow-checked CAD operations, with `logits` as [1,V]. No allocation, no owner, no address. Tests: zero and negative extents, H not dividing E, T above context length, product overflow, maximum accepted total and maximum-plus-one | MDLCFG accessors |
| 8b | one allocation, header, linear mint and release | `DEFLINEAR workspace`; the 27-cell header; the three private crossings; `WORKSPACE-NEW` with the throw-ordered failure sequence; total `WORKSPACE-RELEASE` reading the stored exact length | 8a; `habu-make-owned-release-79de2b5c` |
| 8c | private bounded views | `region-id`, `VIEW`, the per-head pane derivation, and the private row loops that consume them | 8b |

### Negative checks required

- `MINT-WORKSPACE`, `WS-BASE`, `WS-TAKE`, `VIEW`, and every header word
  reject from outside the package, bare and qualified, in the global,
  package-public, and package-private word lists — the three-list runtime
  matrix, since a checker probe outside a package cannot see its private
  definitions
- dropping a workspace fails certification (linear)
- duplicating a workspace fails certification (linear)
- releasing twice fails certification (linear)
- after release the token is gone, so no view can be taken through it

**Named residue, not hidden:** linearity makes the token impossible to drop,
duplicate, or release twice, and makes use-after-release of the *token*
impossible. It does not prevent a raw pointer obtained from `VIEW` earlier in
a word from being used after a release in that same word body. That is
intra-package discipline today, bounded by the fact that no view escapes the
package, and it closes with the region/lifetime checker capability the
advisory-span dots already track.
