# Where the engine's bytes go

## macOS representation reductions

The ARM64 Mach-O product shrank from 3,385,207 to 2,972,407 bytes (12.2%)
by interning checker package names, storing checker strings as arena offsets,
encoding unused checker bindings as zero, and binding primitive BL targets in
their instructions. Four-byte call-site offsets still identify the instructions
that startup must relocate for the actual mapping distance. Partial captures
retain their generic call rows.

These are measured file sections, including each section's alignment:

| Section | Before | After |
|---|---:|---:|
| Engine code | 120,804 | 121,332 |
| Captured Habu code | 1,536,124 | 1,536,660 |
| Call-site rows | 115,656 | 38,560 |
| Address relocation rows | 270,552 | 3,528 |
| DATA bitmap | 71,180 | 69,196 |
| DATA values | 852,956 | 787,548 |
| Complete signed file | 3,385,207 | 2,972,407 |

The largest reduction is 33,378 string-pointer relocation rows removed by
arena-relative offsets. Zero now represents an unused binding in the four
checker scratch maps; no startup reconstruction pass is needed. Bound calls
validate their encoded target against the immutable primitive dictionary before
relocation. This changes representation, not the retained language surface.

The resulting engine SHA-256 is
`2daeb34544e8c081209f2437485d76f606c61ca5ac7eafe8cc28ddf438845639`.
The baseline is
`d3b3bdfd65c835c4275006bc40274635b7272e22d45591301ddcf62197279f8f`.
Run `bin/hb --load tools/engine-size.f -- bin/hb` on a rebuilt product to
measure subsequent changes; the table is evidence, not a fixed size assertion.

## Shared effects and frame reloads

The next ARM64 Mach-O product is 2,889,847 bytes, down 82,560 bytes from
2,972,407. Its SHA-256 is
`730f69dac961702ea8593685d5f7641df32cf2d8d20725ba996b38f422e63aae`.
Every effect binding and history remains; a 40-byte binding now references
interned immutable 64-byte content. Authority stays on the binding. Owner
transfer and serialized effect graphs retain their 96-byte wire representation.
The isolated effect allocation falls from 321,883 to 239,570 encoded bytes,
including its bitmap; the actual file reduction includes implementation costs.

The native emitter also removes a frame reload immediately following a store
of the same register to the same slot in one IR block. It retains the store,
and one predicate controls layout and emission. The isolated change removes
4,644 code bytes net, although alignment absorbs its entire standalone file
saving. The combined product has these measured sections:

| Section | Previous product | Combined product |
|---|---:|---:|
| Captured Habu code | 1,536,660 | 1,538,232 |
| DATA bitmap | 69,196 | 52,908 |
| DATA values | 787,548 | 723,712 |
| Mach-O text padding | 6,680 | 60 |
| Complete signed file | 2,972,407 | 2,889,847 |

Added checker machinery makes the combined code section slightly larger; its
metadata reduction is the net win. Two native generations are byte-identical,
and the integrated native gate passes all 490 suites.
Maki's native REPL image falls from 25,264,640 to 22,687,328 bytes without
snapshot compression. Existing routing/geometry and negotiation checks produce
byte-identical KiCad board artifacts. This does not measure external KiCad DRC.

Of Maki's 2,577,312-byte reduction, 2,495,588 bytes are removed from the raw
warm DATA window, 75,300 from embedded native sections, 1,400 from the warm
code region and 5,024 from the signature. Padding is unchanged. The raw window
falls from 16,732,124 to 14,236,536 bytes; it is still copied verbatim, with
12,875,588 zero bytes. A smaller captured span, not a new snapshot codec,
accounts for most of this reduction. The section census does not establish
individual allocator provenance.

## Eliminate unnecessary DATA before encoding it

The current stripped builder still saves unreachable initialized storage.
On the preceding product, an empty MAIN emits 2,156 code bytes and 427 DATA
value bytes. Adding an unreferenced private 65,536-byte array filled at build
time leaves code size unchanged but raises value bytes to 74,155. Both native
executables run successfully. Sparse encoding already exists on this path and
does not establish DATA reachability.

Repeating that comparison on source `f1b61d80` and engine `730f69da` produces
33,276-byte and 99,324-byte Mach-O executables: 66,048 extra file bytes for the
unused array. The retained sources, executables, complete size reports and
hashes are in `~/.cache/tmp/habu-opt-round3/data/RESULTS.md`.

Allocation extents and literal address sites are insufficient to remove those
bytes safely. A checked program can compute `TARGET BASE - constant GAP` at
build time and later read `BASE GAP + c@`. The retained `hidden-offset.f`
builds and executes successfully on that same source/engine pair. Pointer types
carry no allocation bounds, and native lowering can inline the arithmetic;
scanning calls to `+` cannot establish the missing proof. Ordinary pointer
tables and relative deferred-column offsets also carry references outside the
address-cell registry.

Safe general DATA elimination therefore needs producer-owned extents, lifetime
invalidation and current bounded-access facts, with unknown accesses retaining
their possible targets. A conservative first version that rejects every generic
memory operation would improve empty MAIN but provide no warm Maki saving.
That is not the next optimization: remove storage at existing, explicit owner
lifetime boundaries first. The broader DATA issue remains open as
`habu-prove-the-closure-5b7d02bb`.

Warm snapshot zero counts likewise do not prove dead allocations. Maki's
preceding image has 15,218,805 zero bytes within its 16,732,124-byte DATA
window. Its final USIGS/NORETS stores contain 4,194,576 used bytes and only
65,264 bytes of spare capacity. Earlier copies reserve another 3,014,656
bytes, but releasing them requires proving that their name borrows and cached
pointers have ended. Command argument/result buffers also carry retained API
state; they cannot simply be reset because the measured instance is empty.

Capture needs exact allocation extents, reference ownership and lifecycle
boundaries. CREATE/ALLOT and address-relocation rows do not currently provide
a complete object graph. Preserve unknown escapes conservatively. Compression
and snapshot repacking remain held while these allocation causes are resolved;
retaining runtime capacity does not itself justify storing every reserved byte.

## Allocate checker scratch when needed

Lazy SYMS and NORETS storage, owned SPA/TV/SEEN mappings, and removal of the
full-image driver/signers from warm capture reduce Maki's signed REPL image
from 22,687,328 to **21,702,368 bytes**: 984,960 bytes (4.34%) smaller.
Semantic registries still persist, scratch mappings are released at capture,
and the stripped linker loads its own driver and signer dependencies.
No snapshot codec or general DATA reclamation is involved.

| Measured section | Before | After |
|---|---:|---:|
| Raw warm DATA window, including absorbed padding | 14,236,536 | 13,252,884 |
| Warm code band | 1,508,260 | 1,506,636 |
| Warm out-of-line record names | 19,900 | 19,860 |
| Embedded native sections, net change | — | +2,276 |
| Mach-O code signature | 44,528 | 42,608 |
| Complete signed Maki file | 22,687,328 | 21,702,368 |

The DATA-window reduction is 983,652 bytes; it includes the writer's absorbed
alignment padding and is not an allocator-by-allocator attribution. Removed
dictionary records become unused capacity, so that region's total is unchanged.
Former persisted semantic copies can still remain after growth or recapture.

The standalone native engine **grows** from 2,889,847 to 2,906,359 bytes.
Its sparse DATA encoding already omitted most removed zero storage; the new
owner code and alignment cost more physical bytes. Two native generations and
their name sidecars are byte-identical. Engine SHA-256:
`e2754336b266146d7bb33de3da8d64b66bdff2459c7fe829b0f928853c9363a6`.
The Maki image SHA-256 is
`2b070253266b4dd26d819ba66cf501deac55a564cd3aa7650d8889c2ccf72f83`.

Existing native Maki routing/geometry and negotiation checks pass; both KiCad
board exports are byte-identical to the preceding product, and the image's
strict signature check passes. The integrated native gate passes all 490 suites.
Its first run exposed an invalid timeout fixture that evaluated compile-only
loop words; the corrected fixture compiles and runs an endless word while
preserving the original deadline and timeout assertion. External KiCad DRC,
connectivity and IPC remain
unexercised. Frozen sources, generations, measurements and repeatable artifacts
are retained at `~/.cache/tmp/habu-opt-round3/integrated/`.

## Historical Linux measurements

The block below records one Linux engine measurement. It is an example, not
the size of the current build: compiler changes and the Mach-O container change
these rows. The gate checks the reader's accounting and reachability behavior
in `tools/engine-size-test.f`; it does not compare a changing binary with a
documentation snapshot. The prose and tables explain what the measurement
means for making the engine smaller.
[Where an application image's bytes go](#where-an-application-images-bytes-go)
asks the same question of what `tools/hb-build.f` writes, which is a much
larger file and a different answer.

Reproduce it with the tool that produced every number below:

```
bin/hb --load tools/engine-size.f -- bin/hb
```

<!-- ENGINE-SIZE-ROWS-BEGIN -->
class	bytes	percent
elf/header	64	0.0
elf/program-headers	224	0.0
elf/dynamic-metadata	200	0.0
elf/header-pad	3608	0.1
engine/code	130088	3.6
engine/primitive-names	184	0.0
engine/primitive-count	8	0.0
engine/primitive-records	10128	0.2
source/baked	0	0.0
aot/framing-cells	136	0.0
aot/code-blob	1656092	46.7
aot/dictionary-records	163040	4.6
aot/call-sites	149280	4.2
aot/name-pool	88680	2.5
aot/data-sites	70592	1.9
aot/address-cells	270360	7.6
aot/data-cell-bitmap	71116	2.0
aot/data-cell-values	852264	24.0
aot/code-sites	36	0.0
aot/named-code-sites	0	0.0
aot/code-spans	42696	1.2
aot/boot-run-entries	4	0.0
aot/protected-wordlists	676	0.0
aot/checker-sidecar	0	0.0
image/text-pad	29468	0.8
container/rw-segment	192	0.0
total	3539136	100.0

dictionary the image ships
class	records	record bytes	name bytes	code bytes
global	4962	99240	59619	438780
package-public	2981	59620	30257	254880
package-private	11	220	114	1596
unmapped-wordlist	0	0	0	0
package rows	198	3960	2294	0
name pool entries reachable only from private records, bytes 114
name pool entries reachable only from named code sites, bytes 0
name pool entries nothing in the image references, bytes 606

baked call sites
  sites 12440, bound to seeded primitives 12440, to payload records 0, left as names 0
  distinct bound callees 72

reachability from the dictionary-surface roots
  reachable	7954	695256 code bytes
  unreachable	0	0 code bytes, 0 record bytes, 0 name bytes
  fall-through edges modeled	1277
  spans reachable	5337	926908 code bytes
  spans unreachable	0	0 code bytes

reachability from the engine-entry roots
  reachable	4034	542760 code bytes
  unreachable	3920	152496 code bytes, 78400 record bytes, 39826 name bytes
  fall-through edges modeled	1270
  global	2727	54540	75296
  package-public	1192	23840	76916
  package-private	1	20	284
  package	records	code bytes
  IR-ATTR	59	9808
  IR-SCHEMA	51	5636
  IR-TYPE	23	3984
  NEFF	30	3600
  NUM	40	3352
  A64IR	13	3348
  NMACH	8	3280
  TFAM	77	2876
  SOURCE-ROOT	13	2220
  IR-FUN	23	2212
  HIR	11	2152
  A64ASM	46	1724
  IR-BUILD	29	1576
  NTAPE	11	1572
  NSTR	10	1316
  IR-OP	22	1288
  (121 more packages)
  spans reachable	4913	862928 code bytes
  spans unreachable	424	63980 code bytes
  largest unreachable spans (blob offset, bytes, optional sidecar name)
    873908	3184
    809352	1836
    834696	1200
    590340	712
    836808	644
    618100	584
    653836	540
    836284	524
    620808	524
    631060	520
    626728	508
    619884	496
    1039024	484
    690500	480
    610732	480
    1038292	456

captured DATA heap: 8671248 bytes of span, 306012 present cells in 71113 bitmap bytes, 923375 bytes of image
  owners 967, unowned value bytes 52793, unowned cells 8649
  owner	offset	extent	cells	bytes	image cost
  DONE	4208128	4463120	254297	438576	496352
  SYM-STR-BOOT	1825640	393216	31385	282464	286508
  EC-RV-BOOT	769424	10240	1280	12800	12960
  EC-TV-BOOT	759184	10240	1280	12800	12960
  RVT-BOOT	707984	10240	1280	12800	12960
  TVT-BOOT	697744	10240	1280	12800	12960
  TDECL-PROT-WID-ARMED	2591688	8520	734	6402	6535
  RDP	658000	39720	1433	5329	5758
  REQUIRE-PATHS	2643088	524800	443	3571	5451
  PES	2222072	12288	1499	2744	2937
  DFERS	2234840	65536	200	1169	1299
  DISC-TOK-U	3236848	768	94	838	850
  STR-MIN-I64$	3240424	944872	56	317	774
  EI-AK	2219024	512	64	640	648
  DBUF-W	2610704	23768	170	213	585
  VRDEF-I	2639128	2296	117	544	580
  (441 more owners)
<!-- ENGINE-SIZE-ROWS-END -->
`tools/engine-size.f` is the command line; `tools/image-size-lib.f` is the walk,
in a library because `tools/hb-build.f` runs it too. It walks the image file
itself, reads which of the three image classes the file is out of the file, and
refuses a budget unless the physical spans behind its classes meet in file order
and cover the entire file, and the class totals add up to that length. Framing
cells are checked at their interleaved positions before being reported as one
class; the snapshot's three band classes partition one checked span. The header
metadata extent comes from the program headers and dynamic entries, including
trailing zero fields, rather than from scanning for the last nonzero byte.
The historical examples above and below describe different Linux builds.
The prose and tables below use the engine of sha256
`91715a33e5cff0b0b876322fb360ede4f030a1f3ed59c722ca53206ee0b68bc9`,
3,735,744 bytes.

## The budget

| class | bytes | % | what it is |
| --- | ---: | ---: | --- |
| `elf/header` | 64 | 0.0 | ELF file header |
| `elf/program-headers` | 224 | 0.0 | four program headers |
| `elf/dynamic-metadata` | 189 | 0.0 | PT_INTERP, hash, dynsym, dynstr, relocations |
| `elf/header-pad` | 3,619 | 0.0 | zero pad up to the code offset |
| `engine/code` | 128,964 | 3.4 | every instruction the engine emitters bake: boot, primitives, the interpreter's assembly half |
| `engine/primitive-names` | 184 | 0.0 | the primitive names too long to sit in a record |
| `engine/primitive-count` | 8 | 0.0 | the count cell in front of the seeded table |
| `engine/primitive-records` | 9,984 | 0.2 | 209 boot-seeded dictionary records, 48 B each |
| `source/baked` | 0 | 0.0 | an application image bakes its source here; an engine bakes none |
| `aot/framing-cells` | 136 | 0.0 | the payload's seventeen count cells |
| `aot/code-blob` | 1,813,172 | 48.5 | the captured native code of everything written in Habu |
| `aot/dictionary-records` | 160,620 | 4.2 | 7,835 compact records, 20 B each |
| `aot/call-sites` | 148,704 | 3.9 | 12,392 call sites, each a blob offset, the callee's target and its scope, 12 B |
| `aot/name-pool` | 87,112 | 2.3 | the deduplicated `[len][bytes]` name pool |
| `aot/data-sites` | 76,552 | 2.0 | 18,724 DATA literals the boot rebases |
| `aot/address-cells` | 267,112 | 7.1 | 32,557 declared address cells |
| `aot/data-cell-bitmap` | 67,712 | 1.8 | a presence map over 64-byte groups of the cell bitmap, then the groups holding a present cell |
| `aot/data-cell-values` | 842,600 | 22.5 | one unsigned LEB128 per present cell, in cell order |
| `aot/code-sites` | 36 | 0.0 | 9 code literals |
| `aot/named-code-sites` | 0 | 0.0 | none in this engine |
| `aot/code-spans` | 67,072 | 1.7 | 8,224 `(blob offset, code span)` rows for the words the image ships no record for, 8 B |
| `aot/boot-run-entries` | 4 | 0.0 | the boot-run entry list, empty in an engine image |
| `aot/protected-wordlists` | 668 | 0.0 | 156 sealed wordlist ids |
| `aot/checker-sidecar` | 0 | 0.0 | absent: this engine bakes a seeded runtime |
| `image/text-pad` | 752 | 0.0 | zero pad rounding the text segment to 64 KB |
| `container/rw-segment` | 192 | 0.0 | DYNAMIC plus the two loader slots |
| **total** | **3,735,744** | **100.0** |

Five facts follow from the table.

**Two fifths of the engine is the code of everything written in Habu**
(`aot/code-blob`, 1,813,172 bytes, 48.5%). That is the compiler, the checker,
the loader, the library and the tools the prefix carries. The records the image
still ships own 708,820 bytes of it; the rest is the code of words that travel
without a record, accounted for by the 8,224 rows of `aot/code-spans` — eight
bytes each instead of a 20-byte record and the 48 the boot would expand it to.

**A quarter of the engine is the captured DATA heap, and almost all of it is
content.** THE THREE CELL ROWS ABOVE, AND THE DATA-OWNER SECTION BELOW, ARE
MEASURED ON THE ENGINE THIS FORMAT BUILDS (3,735,744 bytes); the pinned engine
at the head of this page predates the format and the rest of its table is still
that older measurement. `aot/data-cell-bitmap` plus `aot/data-cell-values` is
910,312 bytes, 24.4% of that file, where the varint `(gap, length)` extent rows
this replaced cost 1,285,456 bytes for the same heap. The framing used to be the
offender — a fixed eight-byte `(offset u32, length u32)` header per run, then
two varints — and it is now one bit a cell, with the all-zero runs of those bits
dropped as well.

**A cell is one unsigned LEB128, and the bitmap says which cells are there**:
one bit per 8-byte cell of the window, cell k in bit k mod 8 of bitmap byte
k div 8, then the value of every present cell in cell order
(`src/habu/aot-decl.f`, package AOT-WINDOW). Seven bits a byte, low group first,
high bit set while more groups follow; a whole cell needs at most ten bytes,
which is `VMAX`. The image states no cell count and no value length: a varint
array cannot be found from a count, and the present-cell count is what the walk
that validates the bitmap counts, as the value bytes are what its widths sum to.
The bitmap stops after the last present cell, so a window's trailing zeros cost
nothing at all.

**Most of that bitmap is zero, so the image carries it in groups.** A group is
64 bitmap bytes — 512 cells, 4,096 bytes of DATA — and the image writes a
presence map of one bit a group, then only the groups that hold a present cell
(`GROUP-BYTES`, `BM-COMPACT`). A group of pure `allot`ed room costs one bit
instead of 64 bytes. This engine's window is 1,046,386 cells in 2,044 groups, of
which 990 hold nothing: 256 bytes of presence map and 1,054 stored groups,
67,712 bytes where the flat bitmap was 130,792 — 48.2% of the row, and the
63,080 bytes it saves are a whole 64 KB grain of the file. The same two numbers
frame the bitmap in both images that carry one, and both decoders
(`src/habu/habu2.f` `APPLY-CELLS`, `src/habu/aot-lib.f` `EMIT-DATA-COPY`) skip an
absent group with one branch and one add for 4,096 bytes of DATA. A capture, an
artifact section and a merge keep the flat bitmap: there a cell's bit has to
stay at a fixed place, because appending a window is a concatenation.

**A bit per cell beats a header per extent.** The window's non-zero bytes come
in ones and twos: 772,892 content bytes in 256,128 maximal extents, about 1.7
bytes each, so the varint `(gap, length)` rows this replaced spent 512,563 bytes
on headers alone. A bitmap spends one bit on a cell whether it is present or
not, and measured over this engine's window the four candidates cost 1,285,454
(extent rows), 964,570 (this format), 2,535,017 (bitmap plus raw cells) and
843,675 (a per-4-KiB-page choice between the first two). The page-tagged hybrid
is 9.4% of the DATA class cheaper and costs two decoders and a tag byte a page,
so the one encoding is the bitmap — and grouping it recovers most of that
difference with one decoder. Measured over the flat 130,792-byte bitmap the
release engine had before the grouping, a 32-byte group would have saved 64,705
bytes and a 128-byte group 60,672 against the 63,104 of the 64-byte group chosen
here; all three save the same single grain of file, so the size that wins is the
one whose skip is a page of DATA and whose map is 256 bytes.

**The zero pad is a real place for bytes to hide.** The text segment rounds up
to 64 KB, so a saving smaller than the current pad — 45,200 bytes — does not
change the file's length at all: it moves into `image/text-pad`. Compare
content, not `ls`.

## The largest DATA owners

Each owner is charged from its own base up to the next owner's base — the rule
`tools/data-table-census.f` uses, because `allot` only moves DP forward, so
consecutive bases partition the heap exactly. A block allotted after a variable
is therefore charged to that variable: the name is a locator, not an accusation.
An owner is also a record the image still carries, and private records no longer
travel, so a private table is invisible here and its bytes charge to the nearest
shipped name below it.

The heap is 8,371,088 bytes of span holding 303,943 present cells in 67,712
bitmap bytes, 910,312 bytes of image, across 961 owners, with 52,186 value bytes
in 8,562 cells below the first owner.

| owner | offset | extent | cells | bytes | image cost |
| --- | ---: | ---: | ---: | ---: | ---: |
| `DONE` | 4,170,280 | 4,200,808 | 252,562 | 431,706 | 486,353 |
| `SYM-STR-BOOT` | 1,817,328 | 393,216 | 31,152 | 280,360 | 284,340 |
| `EC-RV-BOOT` | 769,424 | 10,240 | 1,280 | 12,800 | 12,960 |
| `EC-TV-BOOT` | 759,184 | 10,240 | 1,280 | 12,800 | 12,960 |
| `RVT-BOOT` | 707,984 | 10,240 | 1,280 | 12,800 | 12,960 |
| `TVT-BOOT` | 697,744 | 10,240 | 1,280 | 12,800 | 12,960 |
| `TDECL-PROT-WID-ARMED` | 2,550,520 | 8,520 | 731 | 6,378 | 6,512 |
| `RDP` | 658,000 | 39,720 | 1,433 | 5,329 | 5,758 |
| `REQUIRE-PATHS` | 2,601,912 | 524,800 | 443 | 3,571 | 5,451 |
| `PES` | 2,213,720 | 12,288 | 1,496 | 2,738 | 2,930 |
| `DFERS` | 2,226,488 | 65,536 | 200 | 1,169 | 1,235 |
| `DISC-TOK-U` | 3,195,672 | 768 | 94 | 838 | 850 |
| `STR-MIN-I64$` | 3,203,464 | 944,000 | 56 | 317 | 696 |
| `EI-AK` | 2,210,712 | 512 | 64 | 640 | 648 |
| `STGT-START` | 2,569,528 | 23,768 | 169 | 212 | 585 |
| `VRDEF-I` | 2,597,952 | 2,296 | 117 | 544 | 580 |

439 more owners follow.

An owner's `bytes` is what its present cells encode to, and its image cost is
that plus the bitmap bytes covering its span, charged whole to the owner the
byte's first cell lands on — but only for the groups the image carries. An owner
whose extent holds no present cell costs nothing beyond its share of the
presence map, and that is what sorted this table: `SYMS-BOOT` and `NORET-BOOT`
are entirely zero and have left it, 23 owners have dropped out of the ranking
altogether, and `STR-MIN-I64$` fell from third place to thirteenth — 56 present
cells in 944,000 bytes of span cost 317 bytes of value against 379 of bitmap,
where the flat bitmap charged it 14,750.

`DONE` (`src/habu/repl.f`) is a global variable in the REPL, and the last owner
whose record the image still ships, so the row is the heap above every named
table the census can still see: 4.20 MB of span holding 252,562 present cells
that encode to 431,706 bytes, which cost 486,353 bytes of image — 13.0% of the
engine, one row. Two
things land in it. The packages that load after the REPL keep their tables here,
because their private records were dropped at capture. And the checker's baked
user-signature store has no name to charge to at all: `USIGS-SNAPSHOT-PERSIST`
(`src/core/checker.f`) allots the grown store at `here` when it bakes it,
rounded up to a 64 KB grain, so it lands in this row under the REPL's name.
Nothing in the image says how much of the row it is — the census can only charge
an anonymous block to the word below it.

The row's shape says what kind of store it is. Its present cells encode to 1.7
bytes each, so the non-zero bytes are scattered over cells that mostly hold
small numbers — and a small number now costs its own byte and one bit, where an
extent row charged two framing bytes on top of it. The same row cost 773,687
bytes of image under those rows. `SYMS-BOOT` (`src/core/checker.f`) is that
shape taken to its limit and the clearest measure of the change: 655,360 bytes
of span with no present cell at all now cost nothing at all, because all 160 of
their groups are absent — the flat bitmap still charged 10,240 bytes for them,
and the extent rows 115,224 for 38,408 one-byte runs. Its string arena
`SYM-STR-BOOT` is the opposite, and the one owner the extent rows carried more
cheaply: 31,152 present cells of packed text encode to 280,360 bytes — nine a
cell, because a cell with a byte in its top octet needs nine groups — costing
284,340 against 242,081 for the single run they made of it. The checker's two
symbol stores are 284,340 bytes of image between them, all of it the arena, down
from 357,305.

## The dictionary the image ships

| class | records | record bytes | name bytes | code bytes |
| --- | ---: | ---: | ---: | ---: |
| global | 4,860 | 97,200 | 58,227 | 466,140 |
| package-public | 2,964 | 59,280 | 30,044 | 266,512 |
| package-private | 11 | 220 | 114 | 1,684 |
| unmapped-wordlist | 0 | 0 | 0 | 0 |
| package rows | 196 | 3,920 | 2,261 | 0 |

Names are deduplicated, so the name bytes above double-count a name two records
share. The exclusive figures the tool prints are small now: 114 bytes of name
pool are reachable only from private records, none only from named code sites,
and 606 bytes nothing in the image references at all.

The private records are gone. Dropping them was measured at 232,157 bytes on the
engine this document last described, which still carried 8,021 of them; eleven
remain, holding 220 bytes of record and 114 bytes of name, and their code —
along with the code of every other record the capture dropped — travels in
`aot/code-spans`. What is left is 240,760 bytes of records plus names, 6.0% of
the engine, for 7,835 records. In the file it is 20 bytes per record; at boot it
is 48, so the same dictionary costs 374,592 bytes of dictionary region plus its
hash index once the engine is up.

## The baked call sites

```
  sites 12392, bound to seeded primitives 12392, to payload records 0, left as names 0
  distinct bound callees 75
```

A site is a call in the baked code whose callee is outside the captured window.
The capture records it by name and scope, because a name is the only identity a
host dictionary record has that survives into another process. **This image does
not carry any of those names.** The build resolves each one against the two
tables the image itself bakes — the seeded primitive records and the payload's
own records — and the row carries the callee's index in the dictionary the boot
builds (`src/habu/habu2.f` `EMIT-AOT-SITES`). The boot loads `dict[k][0]` and
subtracts: no lookup, no name, no sealed-WID gate, because nothing is resolved
against the booting engine's dictionary any more.

All 12,392 sites name one of 75 engine primitives; a call between two captured
words needs no site at all, because the blob moves rigidly and keeps its own
displacements. The row is three u32 — blob offset, target, scope — and stays
twelve bytes wide because the width is a property of the section, not of the
site: a partial capture, a stripped application or a chain capture calls words
no index of its payload can name, and such a row keeps its pooled name and its
resolving scope. In this image none does, and the tool's `left as names 0` is
what says so.

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

A stripped application gets the closure walk in `src/habu/aot-closure.f`.
Native engine capture also traces and compacts its code graph, in
`src/habu/aot-capture.f`. Its dictionary surface remains a root because the
interpreter must resolve words used by programs loaded later. The size tool
independently marks direct `B`/`BL` edges and the code addresses named by the
payload's relocation tables, using two root sets. The historical tables below
illustrate that distinction; run the tool for the current image's counts.

**Dictionary surface** (every global and package-public word is a root, since a
program can name it):

| | records | code bytes | record bytes | name bytes |
| --- | ---: | ---: | ---: | ---: |
| reachable | 7,832 | 734,712 | | |
| unreachable | 1 | 300 | 20 | 12 |

This historical sample leaves one package-private record in `NSTR` unreachable.
Code whose dictionary record was removed travels in anonymous code spans;
the current census reports those spans separately. Dictionary counts alone
therefore do not account for all retained code.

**Engine entry** (only the engine's own entry points are roots: its boot-run
entry words and the code addresses its DATA cells hold):

| | records | code bytes | record bytes | name bytes |
| --- | ---: | ---: | ---: | ---: |
| reachable | 3,499 | 528,100 | | |
| unreachable | 4,335 | 206,224 | 86,700 | 48,326 |

| class | records | record bytes | code bytes |
| --- | ---: | ---: | ---: |
| global | 3,047 | 60,940 | 122,456 |
| package-public | 1,287 | 25,740 | 83,468 |
| package-private | 1 | 20 | 300 |

Their top owners:

| package | file | records | code bytes |
| --- | --- | ---: | ---: |
| IR-ATTR | `src/compiler/ir/attr.f` | 70 | 13,244 |
| IR-SCHEMA | `src/compiler/ir/schema.f` | 46 | 5,236 |
| TFAM | `src/core/type-family.f` | 89 | 4,232 |
| IR-TYPE | `src/compiler/ir/type.f` | 23 | 4,152 |
| NUM | `lib/num-types.f` | 47 | 3,832 |
| A64IR | `src/compiler/native/a64ir.f` | 13 | 3,668 |
| NEFF | `src/compiler/native-effect.f` | 30 | 3,876 |
| IR-BUILD | `src/compiler/ir/build.f` | 40 | 2,500 |
| IR-FUN | `src/compiler/ir/fun.f` | 23 | 2,260 |
| HIR | `src/compiler/native/hir.f` | 11 | 2,240 |
| A64ASM | `src/arch/arm64/asm.f` | 46 | 1,932 |
| TYPE-DECL | `src/core/sumtype.f` | 20 | 1,772 |
| NTAPE | `src/compiler/native/tape.f` | 11 | 1,680 |
| DATA-CLAIMS | `src/habu/layout.f` | 20 | 1,608 |
| IR-OP | `src/compiler/ir/op.f` | 22 | 1,296 |
| PRIM-SPEC | `src/habu/prims.f` | 29 | 1,268 |

106 more packages follow.

This is a **lower bound on what a tree-shaken engine could keep, not a strip
list**: the interpreter resolves user tokens by name, so a public word outside
this closure is still callable from source. It says that 54% of the shipped
records and 29% of the code those records own exist only because the dictionary
is the language surface.

## What the size work is worth

Measured against this engine, so the numbers are bounds, not hopes:

- **Bind baked call sites at build time** (`habu-bind-baked-call-e4d5b58f`):
  done. Every one of the 11,792 rows carries an index, none carries a name, and
  the boot resolves nothing while it seeds.
- **Drop private dictionary records** (`habu-ship-no-dictionary-2fee2dea`):
  done. Eleven private records remain; the code of the rest travels in 8,224
  code-span rows at 8 bytes instead of 20 bytes of record and 48 of booted
  dictionary.
- **Declare the engine's surface** (`habu-declare-the-surface-89e9aed0`): the
  engine-entry closure is the bound. 4,220 records the engine's own entries never
  reach are 84,400 bytes of record plus 47,174 bytes of name, and their code is
  202,300 bytes; a record that becomes a code span gives back 12 of its 20 bytes
  and all of its name.
  The same caveat applies — a word outside the closure is still callable by name,
  so the list decides what goes, not the walk.
- **The DATA image**: 970,164 bytes, 26.0%, and the framing is finished. A
  present cell costs its own varint and one bit, and an absent one costs the
  bit, which is the floor a per-cell format has; what is left to win is content
  and layout. The checker's stores are where both are: 250,138 present cells in
  the `DONE` row costing 492,418 bytes of image, and `SYM-STR-BOOT`'s packed
  text costing 284,767 for 278,623 bytes of value. A store laid out without
  interleaved zero cells is the lever (`habu-ship-only-the-d7d38629`, item 4),
  not a denser cell format; the measured alternative to this one, a per-4-KiB
  page choice between cells and extent rows, was 9.4% of the class and two
  decoders.

## What the measurement does not say

- **Code is attributed to a record, and a record to its package; there is no
  per-file attribution in the image.** Nothing in a dictionary record, a compact
  record or the tier table names a source file. The file column above is the
  package's declaration site, found by searching the tree; a package reopened in
  several files has one row here. Global-wordlist words belong to no package and
  appear only in the class totals.
- **Reachability is over-approximated in the safe direction, and more so now.**
  Code no record owns is scanned as a root region, and every address the payload
  records is a root. Since the capture stopped shipping private records, the code
  of every dropped word is exactly such a region: 1.05 MB of the blob, most of
  it. A word reported unreachable is unreachable; the unreachable set is
  a floor, and a lower one than it was.
- **An owner is a record the image carries, charged up to the next owner.** An
  anonymous heap block lands under the name below it, and so does every table a
  dropped private record used to own.
- **A snapshot's payload lengths are the trailer's own statement, and only their
  shape is checked.** `REGLEN` and `DATALEN` are validated against this engine's
  band geometry and against the page boundary the donor's text ends on, so every
  single-field edit — either length, the record count, the version, the magic —
  is refused by name. An edit that raises one length by a page and lowers the
  other by the same page keeps every one of those invariants, and the walk
  reports a table whose region/DATA boundary is off by a page. Nothing in the
  file is a second witness to where that boundary falls; only a checksum over the
  payloads would be.
- **A record charged in the code band is charged for the ground it covers
  first.** An `EXPORT` alias or a `does>` clause that shares a span is charged
  nothing, and a record whose span nests inside another's is charged nothing:
  the bytes belong to whichever record's span reached them first in band order.
- **The reachability census above is a question only a baked engine has**, so it
  does not run on an application image. A `--repl` image ships the interpreter,
  which resolves user tokens by name, so every word it carries is reachable by
  construction; a stripped image already had the closure walk
  (`src/habu/aot-closure.f`) run against it at build time, and what that walk
  dropped is not in the file to find.

## Where an application image's bytes go

`tools/hb-build.f` writes two other image classes, and the question "why is this
application 24 MB on an engine of 4 MB" is not answered by either the code or
the dictionary. It is answered by the zero bytes.

Every class below is reported with a **zero** column beside its byte count,
because the difference between what an image carries and what it says is the
whole story for a snapshot. The classes still have to sum to the file's length
or nothing is printed.

The worked example is this application, built with the engine above:

```forth
package IMGFIX

create TABLE 4096 allot
variable COUNTER
defer HOOK ( -- )

: BUMP ( -- )
   COUNTER @ 1+ COUNTER ! ;

: BIND ( -- )
   [: BUMP ;] is HOOK ;

BIND

public

: GREET ( -- )
   HOOK
   s" imgfix" type cr ;

;package

: MAIN ( -- )
   IMGFIX:GREET ;
```

### A `--repl` snapshot image

```
bin/hb --load tools/hb-build.f -- --repl app.f -o app-repl
bin/hb --load tools/engine-size.f -- app-repl
```

23,920,832 bytes, of which **16,675,521 — 69.7% — are zero**.

| class | bytes | zero | % | what it is |
| --- | ---: | ---: | ---: | --- |
| `elf/header` … `elf/header-pad` | 4,096 | 3,970 | 0.0 | the header page, as in any image |
| `engine/code` | 129,036 | 18,466 | 0.5 | the donor engine's own emitted code |
| `engine/primitive-*` | 10,224 | 7,419 | 0.0 | its boot-seeded primitive dictionary |
| `aot/*` | 3,809,140 | 527,537 | 15.9 | its AOT payload, the twenty rows the engine table itemises |
| `engine/text-pad` | 45,200 | 45,200 | 0.2 | the donor's own text pad, now interior to this file |
| `region/dict-records` | 465,696 | 307,730 | 1.9 | 9,702 live 48-byte dictionary records |
| `region/dict-unused` | 2,680,032 | 2,680,032 | 11.2 | the rest of the 65,536-slot array, never written |
| `region/cf-stack` | 4,096 | 4,096 | 0.0 | the control-flow stack inside the dictionary band |
| `region/record-names` | 5,788 | 460 | 0.0 | the names too long to sit in a record, written in the band |
| `region/code-band` | 839,376 | 122,873 | 3.5 | the code the live records own, attributed by package below |
| `region/code-unowned` | 1,059,396 | 145,097 | 4.4 | code in the band no record owns |
| `data/window` | 14,868,512 | 12,812,446 | 62.1 | the DATA window, copied byte for byte |
| `snapshot/trailer` | 48 | 31 | 0.0 | magic, text base, ndict, region length, data length, version |
| `container/rw-segment` | 192 | 164 | 0.0 | DYNAMIC plus the two loader slots |
| **total** | **23,920,832** | **16,675,521** | 100.0 | |

Three facts follow.

**Five sixths of the file is the application's half, and four fifths of that is
zero.** The engine travels whole — 3,997,696 bytes, the same text a baked engine
carries, which is why the engine's own walkers measure it unchanged — and the
snapshot adds 19,922,944 bytes on top. 16,072,765 of those are zero.

**The DATA window is copied verbatim, zeros included.** It is 62.1% of the file
and 86% of it is zero. `src/habu/snap-lib.f` writes `[data-base, DP)` as bytes,
so a table `allot`ed at its declared capacity and filled a tenth of the way
travels at full size, and a store laid out with interleaved zero cells travels
as those cells. Nothing compresses it and nothing skips it: a restore is a
`read` into the window. This is the lever, and it is the same lever the engine's
`aot/data-cell-*` sections describe from the other side — the AOT capture does
encode cells, which is why the engine pays under 1 MB for a 8.3 MB heap while
the snapshot pays 14.9 MB for a 14.9 MB one.

**The dictionary slot array is the second lever, at 2,680,032 bytes of nothing.**
`DICT-CAP` is 65,536 records and this application publishes 9,702; the other
55,834 slots are written to the file because they are inside the region the
trailer's `REGLEN` covers. They cost more than every record the image actually
ships.

#### The code band, by package

A live record's wordlist id names its package the same way a compact record's
does, so the band is attributed with the map `BUILD-WID-MAP` already builds —
from the namespace records, which spend their two code slots on the public and
private wordlist ids their package publishes. The out-of-line names are
separated from the code first: a name too long for a record is written at `CP`
like code is (`src/habu/habu2.f C-STORE-NAME`), so both live in the band.

Every band byte is charged at most once. The spans are sorted and walked in
order, and a record is charged what its own span covers that no earlier span
already covered — an `EXPORT` alias and a `does>` clause put a second record over
ground the first already answers for (12 spans, 184 bytes here). What no record
covers is the `unowned` row.

```
band 1904560 bytes: code 839376, out-of-line names 5788, unowned 1059396
records with code in the band 9282, in the donor engine's text 209, names there 708
```

| package | records | code bytes |
| --- | ---: | ---: |
| `TFAM` | 220 | 23,740 |
| `TASK` | 253 | 18,456 |
| `IR-ATTR` | 75 | 14,076 |
| `IR-FUN` | 68 | 12,020 |
| `A64IR` | 72 | 10,964 |
| `IR-SCHEMA` | 85 | 10,132 |
| `IR-BUILD` | 121 | 9,792 |
| `FFI-DECL` | 98 | 8,548 |
| `IR-OP` | 70 | 8,492 |
| `HIR-WORD` | 49 | 8,440 |

188 more packages follow, and 528,236 bytes belong to global-wordlist words,
which are in no package at all.

**The band is the build's own compiler, not the application.** 9,282 of the
9,702 records have their code in the region and only 209 in the donor engine's
text, for a twenty-line program. A `--repl` build runs a maker child over stdin
— `require tools/app-build.f`, which loads `src/habu/app-image.f` under
`1 set-tier` — so the child compiles the whole tier-1 optimizer stack from
source (the `IR-*`, `A64*`, `HIR-*`, `TFAM` and `TASK` rows above are it) before
the application is loaded at all, and the snapshot keeps every word of it. That
is where a `--repl` image's region goes, and it is a property of how the image is
made rather than of what was compiled: a prebuilt maker, or one whose loader is
already captured in the donor engine, would cut it without touching the
application. The 1,059,396 unowned bytes are the same story from the other end —
stored quotation bodies, hidden bodies, and the code a definition abandoned
where it stood (`src/habu/snap-lib.f`: the retained region carries them).

#### The DATA window, by owner

An owner is a record the definer stamped `DKIND:ADDR` — `create` and `variable`,
the only records whose body pushes a DATA address — and it is charged from its
own base up to the next owner's, so a static `allot` lands on the word that
declared it. The stamp is the claim and the body has to back it: `does>` clears
the stamp in the same window it patches the body, so a stamped record whose body
is not the four-instruction `MOVZ`/`MOVK` chain, or whose chain names an address
outside this image's DATA, is refused rather than skipped. Verbatim makes the
arithmetic trivial: an owner's image cost *is* its extent, and the only question
left is how much of that extent carries anything.

```
owners 1114, below the first one 3514248 bytes (3179006 zero)
```

| owner | package | offset | extent | written | zero |
| --- | --- | ---: | ---: | ---: | ---: |
| `COUNTER` | `IMGFIX` | 10,102,584 | 4,765,928 | 792,337 | 3,973,591 |
| `DONE` | — | 7,030,040 | 2,794,208 | 376,834 | 2,417,374 |
| `STR-MIN-I64$` | — | 6,067,504 | 939,336 | 9,720 | 929,616 |
| `SYMS-BOOT` | — | 4,020,672 | 655,360 | 202,359 | 453,001 |
| `REQUIRE-PATHS` | — | 5,466,456 | 524,800 | 4,382 | 520,418 |
| `SYM-STR-BOOT` | — | 4,676,032 | 393,216 | 264,649 | 128,567 |
| `FS-DIR-BUF` | — | 9,857,016 | 131,072 | 0 | 131,072 |
| `NORET-BOOT` | — | 5,155,392 | 98,304 | 0 | 98,304 |

1,106 more owners follow. `COUNTER` is the application's own `variable`, the
last owner in the window, so the row is every byte of heap above the last name
the image carries: 4.77 MB of which 3.97 MB is zero — a fifth of the file under
one name, and a locator rather than an accusation. `TABLE`, the fixture's
`create TABLE 4096 allot`, is charged 4,096 bytes exactly. The 3,514,248 bytes
below the first owner are the engine's own fixed DATA bands, which no `create`
names.

### A stripped image

```
bin/hb --load tools/hb-build.f -- app.f -o app-strip
bin/hb --load tools/engine-size.f -- app-strip
```

65,728 bytes, of which 62,251 are zero — and almost all of that is one class.

| class | bytes | zero | % | what it is |
| --- | ---: | ---: | ---: | --- |
| `elf/header` … `elf/header-pad` | 4,096 | 3,980 | 6.2 | the header page |
| `app/code` | 3,364 | 534 | 5.1 | the startup, the closure of `MAIN`, the crash and signal handlers |
| `app/data-cell-bitmap` | 285 | 264 | 0.4 | two u32, groups and stored bytes, then the presence map and the groups it keeps |
| `app/data-cell-values` | 477 | 0 | 0.7 | one varint per present cell |
| `app/row-align-pad` | 2 | 2 | 0.0 | the blob rounded up to the rows' four-byte boundary |
| `app/relocation-rows` | 8 | 3 | 0.0 | one 8-byte row per declared address cell |
| `image/text-pad` | 57,304 | 57,304 | 87.1 | the text segment rounded up to 64 KB |
| `container/rw-segment` | 192 | 164 | 0.2 | DYNAMIC plus the two loader slots |
| **total** | **65,728** | **62,251** | 100.0 | |

```
restored DATA window: 660144 bytes from 477 carried in 95 cells; 659667 bytes do not travel
  relocation rows 1, declared address cells this image rebinds at startup
```

**A stripped image carries no zero byte of its DATA at all.** Its window is
encoded as present cells (`src/habu/aot-lib.f BUILD-SPARSE-DATA`), the same
encoding the engine's `aot/data-cell-*` sections use, which is why the
snapshot's 14.9 MB and this image's hundreds of bytes describe comparable
things. The zero-filled span is reported beside the table rather than as a
class, because none of those bytes is in the file to attribute. The bitmap is
grouped here exactly as it is in an engine: 285 bytes carry a 660 KB window
because only the four groups holding a present cell travel, where the flat
bitmap of the same window was 10,315 bytes.

**Almost the whole file is the page round.** 87.1% is `image/text-pad`: the text
segment rounds up to 64 KB and this program needs 3.4 KB of it. A saving smaller
than the pad does not change the file's length at all — the same warning the
engine's own pad carries, in a much louder form.

### What frames the two classes

A snapshot says so at a fixed offset: the 48-byte trailer is the last thing
inside the authenticated text extent, and `src/habu/layout.f` is the single
owner of its size and every field offset, which `src/habu/snap-lib.f`,
`src/habu/habu2.f EM-SNAPSHOT-RESTORE`, `tools/imgdump.f` and this tool all read
from. Its `REGLEN` and `DATALEN` place the two payloads exactly, so nothing is
searched for and the walk cannot land in the wrong place.

A stripped image frames nothing, so the walk takes the emitter's own two
statements about it:

- **the blob** is named by the startup's single `ADR x9`
  (`src/habu/aot-lib.f EMIT-DATA-COPY`; `test/gate-aot-image.f` already admits
  exactly one), and then frames itself — a u32 of encoded row bytes, those rows,
  and the bytes they describe;
- **the relocation row count** is the `MOVZ`/`MOVK` chain `EMIT-XT-CELLS` loads
  into x11, found behind the three-instruction idiom that rounds the byte cursor
  up to the rows' four-byte boundary and confirmed by the `ADR x12` after it,
  which must name this image's own code base.

The row count is read and never inferred. A row is `(u32 location, u32 target)`
and a target below 65,536 leaves the row's last two bytes zero: the image above
has exactly one row, target 1,252, and ending the content at the last non-zero
byte would lose eight bytes of the file to the pad.

### What every build prints

`tools/hb-build.f` measures what it just wrote and prints one line:

```
hb-build size: app-repl 23920832 bytes = code 3861636, names 3402492, data 2811109 written + 12812703 zero, padding 48819, other 984073 (repl-snapshot)
```

The six terms plus `other` are the file's own length, so the line is an identity
and `other` is whatever no class claimed — the table itemises it. `code` is
every byte of executable code, `names` every dictionary record and its names
(the slot array included, which is why it is large), `data written` and `data
zero-filled` the two halves of every data-carrying class, and `padding` the
header pad, the text pads and the alignment pads.

`--size-report` prints the whole table above the line. `--report-json`
suppresses the line and puts the same numbers in the report object instead, so
the JSON stays a single parseable object:

```json
"size":{"total":65728,"code":3176,"names":0,"data_written":19,
        "data_zero_filled":0,"padding":61832,"other":701}
```

### Tender's images

> The numbers this section was asked for — Tender's scraper (24.6 MB), product
> CLI (28.4 MB) and server (29.0 MB) — belong here, measured by their owner
> against their own tree. Run `bin/hb --load tools/engine-size.f -- <image>` on
> each and paste the three tables; the shape above says what to expect, but the
> split between `region/code-band`, `region/dict-unused` and `data/window` is a
> property of each application and is not worth guessing.
