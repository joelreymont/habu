# Where the engine's bytes go

`bin/hb` is 5,832,896 bytes. This is what they are, measured rather than
estimated, and what the measurement says about making the engine smaller.

Reproduce it with the tool that produced every number below:

```
bin/hb --load tools/engine-size.f -- bin/hb
```

`tools/engine-size.f` walks the image file itself. It refuses to print a budget
unless the classes it names add up to the file's own length, so the table is an
accounting identity, not a set of estimates. Everything here is the release
engine of 2026-09-16, sha256
`7e490c6031cc317c02d9db551c07ab8a8b0328da919900c8224a495341f452c9`,
5,832,896 bytes.

## The budget

| class | bytes | % | what it is |
| --- | ---: | ---: | --- |
| `elf/header` | 64 | 0.0 | ELF file header |
| `elf/program-headers` | 224 | 0.0 | four program headers |
| `elf/dynamic-metadata` | 189 | 0.0 | PT_INTERP, hash, dynsym, dynstr, relocations |
| `elf/header-pad` | 3,619 | 0.0 | zero pad up to the code offset |
| `engine/code` | 117,612 | 2.0 | every instruction the engine emitters bake: boot, primitives, the interpreter's assembly half |
| `engine/primitive-names` | 184 | 0.0 | the eight primitive names too long to sit in a record |
| `engine/primitive-count` | 8 | 0.0 | the count cell in front of the seeded table |
| `engine/primitive-records` | 9,648 | 0.1 | 201 boot-seeded dictionary records, 48 B each |
| `source/baked` | 0 | 0.0 | an application image bakes its source here; an engine bakes none |
| `aot/framing-cells` | 120 | 0.0 | the payload's fifteen count cells |
| `aot/code-blob` | 1,887,128 | 32.3 | the captured native code of everything written in Habu |
| `aot/dictionary-records` | 312,820 | 5.3 | 15,641 compact records, 20 B each |
| `aot/call-sites` | 149,496 | 2.5 | 12,458 call sites the boot resolves by name, 12 B each |
| `aot/name-pool` | 154,688 | 2.6 | the deduplicated `[len][bytes]` name pool |
| `aot/data-sites` | 71,732 | 1.2 | 17,933 DATA literals the boot rebases |
| `aot/address-cells` | 254,256 | 4.3 | 31,782 declared address cells |
| `aot/data-run-rows` | 2,063,672 | 35.3 | 257,959 `(offset, length)` headers for the captured DATA |
| `aot/data-run-bytes` | 794,832 | 13.6 | the captured DATA bytes those headers describe |
| `aot/code-sites` | 36 | 0.0 | 9 code literals |
| `aot/named-code-sites` | 0 | 0.0 | none in this engine |
| `aot/boot-run-entries` | 4 | 0.0 | the boot-run entry list |
| `aot/protected-wordlists` | 604 | 0.0 | 151 sealed wordlist ids |
| `aot/checker-sidecar` | 0 | 0.0 | absent: this engine bakes a seeded runtime |
| `image/text-pad` | 11,768 | 0.2 | zero pad rounding the text segment to 64 KB |
| `container/rw-segment` | 192 | 0.0 | DYNAMIC plus the two loader slots |
| **total** | **5,832,896** | **100.0** | |

Three facts follow from the table.

**Half the engine is its captured DATA heap, and three quarters of that half is
framing.** `aot/data-run-rows` plus `aot/data-run-bytes` is 2,858,504 bytes,
49.0% of the file — and 2,063,672 of it is run headers for 794,832 bytes of
content, an average run of 3.1 bytes. The image stores the boot DATA as its
non-zero runs, so a table of cells holding small numbers breaks into one run per
cell and costs eight bytes of header for one to three bytes of payload.
`tools/data-table-census.f` has warned about exactly this shape for the live
heap; this is the price being paid in the shipped file.

**A third of the engine is the code of everything written in Habu**
(`aot/code-blob`, 1,887,128 bytes). That is the compiler, the checker, the
loader, the library and the tools the prefix carries.

**The dictionary is 8% of the engine, not the megabytes it is usually blamed
for.** Records plus names are 467,508 bytes. The 0.76 MB figure that circulates
is the *runtime* cost: the boot expands each 20-byte record to 48 bytes in the
dictionary region. In the file it is 20 bytes per record.

## The largest DATA owners

Each owner is charged from its own base up to the next owner's base — the rule
`tools/data-table-census.f` uses, because `allot` only moves DP forward, so
consecutive bases partition the heap exactly. A block allotted after a variable
is therefore charged to that variable: the name is a locator, not an accusation.

| owner | extent | runs | bytes | image cost |
| --- | ---: | ---: | ---: | ---: |
| `TR-LASTZERO` | 2,657,859 | 199,876 | 357,228 | 1,956,236 |
| `SYMS-BOOT` | 655,360 | 37,221 | 37,221 | 334,989 |
| `SYM-STR-BOOT` | 1,048,592 | 1 | 235,768 | 235,776 |
| `NORET-BOOT` | 98,312 | 8,069 | 13,438 | 77,990 |
| `SPA-BOOT` | 65,536 | 4,698 | 8,407 | 45,991 |
| `SEEN-BOOT` | 16,392 | 1 | 16,384 | 16,392 |

`TR-LASTZERO` (`src/core/top-row.f`) is the last `create`d word below the DP
heap, so the row is the heap above every named table: 2.66 MB of span holding
357,228 bytes in 199,876 runs, which cost 1,956,236 bytes of image — 33.5% of
the engine, one row. The runs are cell-spaced and one to three bytes long, which
is the signature of an array of cells holding small numbers.

Identified by hand from the payload's own address-cell table: the only DATA
pointer cell that targets this region is `FP` (`src/core/checker.f:4054`), the
cursor the checker's signature scans walk `USIGS-USER` with. So the megabyte and
a half is the checker's grown user-signature store — the effect signature of
every definition the build compiled — travelling as one sparse run per cell.

`SYMS-BOOT` and `SYM-STR-BOOT` (`src/core/checker.f:4378`) are the checker's
symbol table and its string arena: another 570,765 bytes. The checker's stores
are the engine's DATA.

## The dictionary the image ships

| class | records | record bytes | name bytes | code bytes |
| --- | ---: | ---: | ---: | ---: |
| global | 4,753 | 95,060 | 56,462 | 495,396 |
| package-public | 2,695 | 53,900 | 27,470 | 267,640 |
| package-private | 8,022 | 160,440 | 83,628 | 1,090,504 |
| package rows | 171 | 3,420 | 2,024 | 0 |

Names are deduplicated, so the name bytes above double-count a name two records
share. The number that matters for dropping private records is exclusive:
**71,751 bytes of name pool are reachable only from private records**, and
another 656 bytes only from call sites.

So retiring every private record from the shipped dictionary is worth
160,440 + 71,751 = **232,191 bytes, 4.0% of the engine** — and, at runtime,
8,022 × 48 = 385,056 bytes of dictionary region plus their hash-index entries
that the boot no longer has to build.

The tool also measures what those call sites name:

```
  sites 12458, in the global wordlist 12458, in a package wordlist 0
  distinct callee names 83, of them seeded primitives 83
```

Every one of the 12,458 baked call sites resolves in the global wordlist, and
all 83 distinct callees are engine primitives (`+`, `!`, `die`, `throw`, …): a
call between two captured words needs no site at all, because the blob moves
rigidly and keeps its own displacements. No call site names a private word, so
dropping private records does not break boot call resolution.

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
| reachable | 12,608 | 1,780,456 | | |
| unreachable | 2,862 | 73,084 | 57,240 | 24,515 |

All 2,862 are package-private. They are private code no public word can reach:
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
| reachable | 7,846 | 1,431,696 | | |
| unreachable | 7,624 | 421,844 | 152,480 | 76,404 |

This is a **lower bound on what a tree-shaken engine could keep, not a strip
list**: the interpreter resolves user tokens by name, so a public word outside
this closure is still callable from source. It says that half the shipped
records and 23% of the baked code exist only because the dictionary is the
language surface.

## What the two size dots are worth

Measured against this engine, so the numbers are bounds, not hopes:

- **Drop private dictionary records** (`habu-ship-no-dictionary-2fee2dea`):
  232,191 bytes, 4.0%. Add the code of the 2,862 unreachable private words and
  it is 305,275 bytes, 5.2%.
- **Bind baked call sites at build time** (`habu-bind-baked-call-e4d5b58f`):
  the site rows shrink from 12 bytes to 8 if a site names a record instead of a
  name and a scope (49,832 bytes), and the 656 bytes of site-only names go. The
  dot's real prize is start time, not size: 12,458 name lookups happen at every
  boot.
- **The DATA image**: 2,858,504 bytes, 49.0%, of which 2,063,672 is framing for
  content that is mostly cells holding small numbers. Nothing in either dot
  touches it. This is where the engine's size actually is.

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
- The tool refuses a snapshot image: its dictionary and DATA travel verbatim
  behind a trailer, which is a different budget with a different owner.
