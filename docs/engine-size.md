# Where the engine's bytes go

`bin/hb` is 5,374,144 bytes. This is what they are, measured rather than
estimated, and what the measurement says about making the engine smaller.

Reproduce it with the tool that produced every number below:

```
bin/hb --load tools/engine-size.f -- bin/hb
```

`tools/engine-size.f` walks the image file itself. It refuses to print a budget
unless the classes it names add up to the file's own length, so the table is an
accounting identity, not a set of estimates. Everything here is the engine this
tree builds, sha256
`ad40305af2e98226fe3c4d52ead96d6410ef1bdab213146b4a10eb3afa78536d`,
5,374,144 bytes.

## The budget

| class | bytes | % | what it is |
| --- | ---: | ---: | --- |
| `elf/header` | 64 | 0.0 | ELF file header |
| `elf/program-headers` | 224 | 0.0 | four program headers |
| `elf/dynamic-metadata` | 189 | 0.0 | PT_INTERP, hash, dynsym, dynstr, relocations |
| `elf/header-pad` | 3,619 | 0.0 | zero pad up to the code offset |
| `engine/code` | 123,244 | 2.2 | every instruction the engine emitters bake: boot, primitives, the interpreter's assembly half |
| `engine/primitive-names` | 184 | 0.0 | the primitive names too long to sit in a record |
| `engine/primitive-count` | 8 | 0.0 | the count cell in front of the seeded table |
| `engine/primitive-records` | 9,888 | 0.1 | 206 boot-seeded dictionary records, 48 B each |
| `source/baked` | 0 | 0.0 | an application image bakes its source here; an engine bakes none |
| `aot/framing-cells` | 120 | 0.0 | the payload's fifteen count cells |
| `aot/code-blob` | 1,887,900 | 35.1 | the captured native code of everything written in Habu |
| `aot/dictionary-records` | 313,360 | 5.8 | 15,668 compact records, 20 B each |
| `aot/call-sites` | 99,504 | 1.8 | 12,438 call sites, each a blob offset and the callee's dictionary index, 8 B |
| `aot/name-pool` | 155,072 | 2.8 | the deduplicated `[len][bytes]` name pool |
| `aot/data-sites` | 71,740 | 1.3 | 17,935 DATA literals the boot rebases |
| `aot/address-cells` | 254,768 | 4.7 | 31,846 declared address cells |
| `aot/data-run-rows` | 679,152 | 12.6 | 84,894 `(offset, length)` headers for the captured DATA |
| `aot/data-run-bytes` | 1,723,932 | 32.0 | the captured DATA bytes those headers describe |
| `aot/code-sites` | 36 | 0.0 | 9 code literals |
| `aot/named-code-sites` | 0 | 0.0 | none in this engine |
| `aot/boot-run-entries` | 4 | 0.0 | the boot-run entry list, empty in an engine image |
| `aot/protected-wordlists` | 604 | 0.0 | 151 sealed wordlist ids |
| `aot/checker-sidecar` | 0 | 0.0 | absent: this engine bakes a seeded runtime |
| `image/text-pad` | 50,340 | 0.9 | zero pad rounding the text segment to 64 KB |
| `container/rw-segment` | 192 | 0.0 | DYNAMIC plus the two loader slots |
| **total** | **5,374,144** | **100.0** | |

Three facts follow from the table.

**Nearly half the engine is its captured DATA heap.** `aot/data-run-rows` plus
`aot/data-run-bytes` is 2,403,084 bytes, 44.7% of the file: 1,723,932 bytes of
content carried in 84,894 runs, an average run of 20.3 bytes. The image stores
the boot DATA as its non-zero runs, and a run costs an eight-byte header, so a
table of cells holding small numbers used to break into one run per cell and
cost more in headers than in bytes. Merging runs across short zero gaps
(`habu-merge-short-zero-512051d8`) turned 258,476 runs into 84,894 and took
460 KB off the file; the remaining 679,152 bytes of headers are 12.6% of it.

**A third of the engine is the code of everything written in Habu**
(`aot/code-blob`, 1,887,900 bytes). That is the compiler, the checker, the
loader, the library and the tools the prefix carries.

**The dictionary is 8.7% of the engine, not the megabytes it is usually blamed
for.** Records plus names are 468,432 bytes. The 0.76 MB figure that circulates
is the *runtime* cost: the boot expands each 20-byte record to 48 bytes in the
dictionary region. In the file it is 20 bytes per record.

**The zero pad is a real place for bytes to hide.** The text segment rounds up
to 64 KB, so a saving smaller than the current pad does not change the file's
length at all — it moves into `image/text-pad`. Compare content, not `ls`.

## The largest DATA owners

Each owner is charged from its own base up to the next owner's base — the rule
`tools/data-table-census.f` uses, because `allot` only moves DP forward, so
consecutive bases partition the heap exactly. A block allotted after a variable
is therefore charged to that variable: the name is a locator, not an accusation.

| owner | extent | runs | bytes | image cost |
| --- | ---: | ---: | ---: | ---: |
| `TR-LASTZERO` | 2,657,859 | 57,797 | 1,236,941 | 1,699,317 |
| `SYMS-BOOT` | 655,360 | 26,501 | 112,501 | 324,509 |
| `SYM-STR-BOOT` | 393,232 | 1 | 236,154 | 236,162 |
| `PES` | 12,288 | 2 | 11,234 | 11,250 |
| `EC-TV-BOOT` | 10,240 | 1 | 10,240 | 10,248 |
| `EC-RV-BOOT` | 10,240 | 0 | 10,240 | 10,240 |

`TR-LASTZERO` (`src/core/top-row.f`) is the last `create`d word below the DP
heap, so the row is the heap above every named table: 2.66 MB of span holding
1,236,941 bytes in 57,797 runs, which cost 1,699,317 bytes of image — 31.6% of
the engine, one row.

Identified by hand from the payload's own address-cell table: the only DATA
pointer cell that targets this region is `FP` (`src/core/checker.f`), the cursor
the checker's signature scans walk `USIGS-USER` with. So that 1.7 MB is the
checker's grown user-signature store — the effect signature of every definition
the build compiled.

`SYMS-BOOT` and `SYM-STR-BOOT` (`src/core/checker.f`) are the checker's symbol
table and its string arena: another 560,671 bytes. The checker's stores are the
engine's DATA.

## The dictionary the image ships

| class | records | record bytes | name bytes | code bytes |
| --- | ---: | ---: | ---: | ---: |
| global | 4,781 | 95,620 | 56,860 | 496,296 |
| package-public | 2,695 | 53,900 | 27,470 | 267,640 |
| package-private | 8,021 | 160,420 | 83,614 | 1,090,376 |
| package rows | 171 | 3,420 | 2,024 | 0 |

Names are deduplicated, so the name bytes above double-count a name two records
share. The number that matters for dropping private records is exclusive:
**71,737 bytes of name pool are reachable only from private records**.

So retiring every private record from the shipped dictionary is worth
160,420 + 71,737 = **232,157 bytes, 4.3% of the engine** — and, at runtime,
8,021 × 48 = 385,008 bytes of dictionary region plus their hash-index entries
that the boot no longer has to build.

## The baked call sites

```
  sites 12438, bound to seeded primitives 12438, to payload records 0
  distinct callees 83
```

A site is a call in the baked code whose callee is outside the captured window.
The capture records it by name and scope, because a name is the only identity a
host dictionary record has that survives into another process. **The image does
not carry that name.** The build resolves it once, against the two tables the
image itself bakes — the seeded primitive records and the payload's own records
— and the row carries the callee's index in the dictionary the boot builds
(`src/habu/habu2.f` `EMIT-AOT-SITES`). The boot loads `dict[k][0]` and
subtracts: no lookup, no name, no sealed-WID gate, because nothing is resolved
against the booting engine's dictionary any more.

All 12,438 sites name one of 83 engine primitives; a call between two captured
words needs no site at all, because the blob moves rigidly and keeps its own
displacements. The 656 bytes of pooled callee names are now referenced by
nothing in the image and show up in the tool's unreferenced-names row; they go
when the pool is rebuilt for the private-record work.

What replaces the lookup is three checks. The index space is asserted once —
`NDICT` must be the primitive count plus the payload's record count when the
pass starts — then every index is bounded against `NDICT`, and a row naming a
package row (whose `[0]` is a wordlist id, not a code entry) is refused. A
tampered row exits 82 with a named diagnostic rather than wiring a call to a
number; both refusals were exercised against a patched image.

With the sites bound, an engine image resolves **no name at all** while it
seeds: its boot-run entry list is empty (one terminator byte) and it carries no
named code sites. The name-resolving passes remain for the images that do use
them — a maker or application image with boot-run entries.

## What nothing reaches

A stripped application gets the closure walk (`src/habu/aot-closure.f`); the
engine does not, so every word the build ever compiled ships. The tool marks
from roots over the same call graph — direct `B`/`BL` edges in the baked code
plus the code addresses the payload's relocation tables name — with two root
sets, because "dead" means two different things.

**Dictionary surface** (every global and package-public word is a root, since a
program can name it):

| | records | code bytes | record bytes | name bytes |
| --- | ---: | ---: | ---: | ---: |
| reachable | 12,636 | 1,781,356 | | |
| unreachable | 2,861 | 72,956 | 57,220 | 24,501 |

All 2,861 are package-private. They are private code no public word can reach:
the exact, mechanical version of "dead code". Their top owners:

| package | file | records | code bytes |
| --- | --- | ---: | ---: |
| NELAB | `src/compiler/native/elaborate.f` | 193 | 5,308 |
| A64RA | `src/compiler/native/regalloc.f` | 170 | 5,172 |
| A64SEL | `src/compiler/native/select.f` | 158 | 4,352 |
| A64RAV | `src/compiler/native/regalloc-verify.f` | 150 | 4,264 |
| TFAM | `src/core/type-family.f` | 155 | 3,924 |
| ENGINE-INTERNAL | `src/core/internal-mark.f` | 34 | 3,280 |
| TYPE-DECL | `src/core/sumtype.f` | 100 | 3,100 |
| A64EMIT | `src/compiler/native/emit.f` | 111 | 2,832 |
| IR-SCHEMA | `src/compiler/ir/schema.f` | 134 | 2,460 |
| A64SPILL | `src/compiler/native/spill.f` | 84 | 2,312 |

63 more packages follow. Many of the unreachable records are constants whose
value the compiler inlines at every call site, so the routine that returns it is
never called — real dead code, and cheap per record.

**Engine entry** (only the engine's own entry points are roots: its boot-run
entry words and the code addresses its DATA cells hold):

| | records | code bytes | record bytes | name bytes |
| --- | ---: | ---: | ---: | ---: |
| reachable | 7,847 | 1,432,052 | | |
| unreachable | 7,650 | 422,260 | 153,000 | 76,774 |

This is a **lower bound on what a tree-shaken engine could keep, not a strip
list**: the interpreter resolves user tokens by name, so a public word outside
this closure is still callable from source. It says that half the shipped
records and 23% of the baked code exist only because the dictionary is the
language surface.

## What the size work is worth

Measured against this engine, so the numbers are bounds, not hopes:

- **Bind baked call sites at build time** (`habu-bind-baked-call-e4d5b58f`):
  done. The site row went from 12 bytes to 8 and the boot stopped resolving
  12,438 names — 49,752 bytes of content, which the 64 KB text rounding
  currently hides in `image/text-pad`, and about 1.4 ms of a 30 ms start
  (2.6 million fewer instructions retired on a trivial program).
- **Drop private dictionary records** (`habu-ship-no-dictionary-2fee2dea`):
  232,157 bytes, 4.3%. Add the code of the 2,861 unreachable private words and
  it is 305,113 bytes, 5.7%.
- **The DATA image**: 2,403,084 bytes, 44.7%, and the checker's stores are most
  of it. That is the table-fill lane's ground
  (`habu-merge-short-zero-512051d8`), not this one's.

## What the measurement does not say

- **Code is attributed to a record, and a record to its package; there is no
  per-file attribution in the image.** Nothing in a dictionary record, a compact
  record or the tier table names a source file. The file column above is the
  package's declaration site, found by searching the tree; a package reopened in
  several files has one row here. Global-wordlist words belong to no package and
  appear only in the class totals.
- **Reachability is over-approximated in the safe direction.** Code no record
  owns (quotation bodies, padding) is scanned as a root region, and every
  address the payload records is a root. A word reported unreachable is
  unreachable; the unreachable set is a floor.
- **An owner is charged up to the next owner**, so an anonymous heap block lands
  under the name below it.
- The tool measures baked engine images. It refuses a snapshot image, whose
  dictionary and DATA travel verbatim behind a trailer, and a stripped
  application, which carries no seeded dictionary at all.
