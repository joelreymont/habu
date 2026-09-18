# Where the engine's bytes go

`bin/hb` is 3,932,352 bytes. This is what they are, measured rather than
estimated, and what the measurement says about making the engine smaller.
[Where an application image's bytes go](#where-an-application-images-bytes-go)
asks the same question of what `tools/hb-build.f` writes, which is a much
larger file and a different answer.

Reproduce it with the tool that produced every number below:

```
bin/hb --load tools/engine-size.f -- bin/hb
```

`tools/engine-size.f` is the command line; `tools/image-size-lib.f` is the walk,
in a library because `tools/hb-build.f` runs it too. It walks the image file
itself, reads which of the three image classes the file is out of the file, and
refuses to print a budget unless the classes it names add up to the file's own
length, so the table is an accounting identity, not a set of estimates.
Everything in the engine sections is the engine this tree builds, sha256
`d5e871c07a39b891b4e314024480f168dbc38a248fc987c0e4b567acc19a56f1`,
3,932,352 bytes.

## The budget

| class | bytes | % | what it is |
| --- | ---: | ---: | --- |
| `elf/header` | 64 | 0.0 | ELF file header |
| `elf/program-headers` | 224 | 0.0 | four program headers |
| `elf/dynamic-metadata` | 189 | 0.0 | PT_INTERP, hash, dynsym, dynstr, relocations |
| `elf/header-pad` | 3,619 | 0.0 | zero pad up to the code offset |
| `engine/code` | 127,880 | 3.2 | every instruction the engine emitters bake: boot, primitives, the interpreter's assembly half |
| `engine/primitive-names` | 184 | 0.0 | the primitive names too long to sit in a record |
| `engine/primitive-count` | 8 | 0.0 | the count cell in front of the seeded table |
| `engine/primitive-records` | 9,984 | 0.2 | 208 boot-seeded dictionary records, 48 B each |
| `source/baked` | 0 | 0.0 | an application image bakes its source here; an engine bakes none |
| `aot/framing-cells` | 128 | 0.0 | the payload's sixteen count cells |
| `aot/code-blob` | 1,746,960 | 44.4 | the captured native code of everything written in Habu |
| `aot/dictionary-records` | 153,480 | 3.9 | 7,674 compact records, 20 B each |
| `aot/call-sites` | 138,924 | 3.5 | 11,577 call sites, each a blob offset, the callee's target and its scope, 12 B |
| `aot/name-pool` | 83,644 | 2.1 | the deduplicated `[len][bytes]` name pool |
| `aot/data-sites` | 73,572 | 1.8 | 18,393 DATA literals the boot rebases |
| `aot/address-cells` | 255,992 | 6.5 | 31,999 declared address cells |
| `aot/data-run-rows` | 492,036 | 12.5 | 245,874 varint `(gap, length)` rows framing the captured DATA |
| `aot/data-run-bytes` | 738,728 | 18.7 | the captured DATA bytes those rows describe |
| `aot/code-sites` | 36 | 0.0 | 9 code literals |
| `aot/named-code-sites` | 0 | 0.0 | none in this engine |
| `aot/code-spans` | 64,568 | 1.6 | 8,071 `(blob offset, code span)` rows for the words the image ships no record for, 8 B |
| `aot/boot-run-entries` | 4 | 0.0 | the boot-run entry list, empty in an engine image |
| `aot/protected-wordlists` | 604 | 0.0 | 151 sealed wordlist ids |
| `aot/checker-sidecar` | 0 | 0.0 | absent: this engine bakes a seeded runtime |
| `image/text-pad` | 41,332 | 1.0 | zero pad rounding the text segment to 64 KB |
| `container/rw-segment` | 192 | 0.0 | DYNAMIC plus the two loader slots |
| **total** | **3,932,352** | **100.0** | |

Five facts follow from the table.

**Two fifths of the engine is the code of everything written in Habu**
(`aot/code-blob`, 1,746,960 bytes, 44.4%). That is the compiler, the checker,
the loader, the library and the tools the prefix carries. The records the image
still ships own 699,164 bytes of it; the rest is the code of words that travel
without a record, accounted for by the 8,071 rows of `aot/code-spans` — eight
bytes each instead of a 20-byte record and the 48 the boot would expand it to.

**Not quite a third of the engine is the captured DATA heap, and content is now
the larger term.** `aot/data-run-rows` plus `aot/data-run-bytes` is 1,230,764
bytes, 31.3% of the file: 738,728 bytes of content carried in 245,874 runs, an
average run of 3.0 bytes, framed by 492,036 bytes of rows. The framing used to
be the offender — a fixed eight-byte `(offset u32, length u32)` header per run —
and it no longer is.

**A run row is two unsigned LEB128 varints**: the gap in zero bytes from the
previous run's end, then the run's length (`src/habu/aot-decl.f`, package
AOT-WINDOW). Seven bits a byte, low group first, high bit set while more groups
follow; the first row's gap counts from offset zero. A gap is unsigned, so a row
that went backwards or overlapped its predecessor is not a shape the format can
express rather than one a reader has to refuse. The image stores the table's
encoded byte length and not a row count, because a varint array cannot be found
from a count; the run count is what the walk that validates the table counts.
This engine's table encodes to 492,033 bytes over 245,874 rows — 2.001 bytes a
row — so nearly every gap and every length fits in one byte.

**A zero gap under two bytes travels inside a run.** Carrying g zero bytes costs
g payload bytes; splitting the run around them costs one more row, and no row is
narrower than two one-byte varints. So a gap of one is cheaper to carry, a gap
of exactly two breaks even, and RUN-GAP-MIN is the row's own arithmetic rather
than a tuned constant. It fell from 8 to 2 with the fixed row it was sized for
(3afce20f): re-split at that threshold, the engine of that landing went from
85,285 rows to 245,194 and its two classes together from 2,417,592 bytes to
1,228,047.

**The zero pad is a real place for bytes to hide.** The text segment rounds up
to 64 KB, so a saving smaller than the current pad — 41,332 bytes — does not
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

The heap is 6,858,520 bytes of span holding 245,874 runs in 492,033 row bytes,
1,230,758 bytes of image, across 932 owners, with 41,224 bytes in 3,314 runs
below the first owner.

| owner | offset | extent | runs | bytes | image cost |
| --- | ---: | ---: | ---: | ---: | ---: |
| `DONE` | 4,130,680 | 2,727,840 | 201,471 | 360,165 | 763,117 |
| `SYM-STR-BOOT` | 1,778,080 | 393,216 | 1 | 237,993 | 237,999 |
| `SYMS-BOOT` | 1,122,720 | 655,360 | 37,639 | 37,639 | 112,917 |
| `EC-TV-BOOT` | 720,088 | 10,240 | 1 | 10,240 | 10,246 |
| `TVT-BOOT` | 658,648 | 10,240 | 1 | 10,240 | 10,244 |
| `EC-RV-BOOT` | 730,328 | 10,240 | 0 | 10,240 | 10,240 |
| `RVT-BOOT` | 668,888 | 10,240 | 0 | 10,240 | 10,240 |
| `TDECL-PROT-WID-ARMED` | 2,509,632 | 8,456 | 27 | 5,634 | 5,690 |
| `PES` | 2,174,472 | 12,288 | 1,442 | 2,126 | 5,011 |
| `REQUIRE-PATHS` | 2,568,200 | 524,800 | 106 | 2,520 | 2,838 |
| `NAMES` | 2,559,672 | 4,096 | 1 | 1,350 | 1,354 |
| `NAMES-U` | 2,563,768 | 1,248 | 2 | 1,240 | 1,246 |
| `DFERS` | 2,186,960 | 65,536 | 103 | 956 | 1,162 |
| `TAB` | 2,565,016 | 1,808 | 223 | 331 | 777 |
| `DISC-TOK-U` | 3,161,960 | 752 | 1 | 729 | 734 |
| `VRDEF-I` | 2,556,984 | 2,136 | 76 | 404 | 559 |

303 more owners follow.

`DONE` (`src/habu/repl.f`) is a global variable in the REPL, and the last owner
whose record the image still ships, so the row is the heap above every named
table the census can still see: 2.73 MB of span holding 360,165 bytes in 201,471
runs, which cost 763,117 bytes of image — 19.4% of the engine, one row. Two
things land in it. The packages that load after the REPL keep their tables here,
because their private records were dropped at capture. And the checker's baked
user-signature store has no name to charge to at all: `USIGS-SNAPSHOT-PERSIST`
(`src/core/checker.f`) allots the grown store at `here` when it bakes it,
rounded up to a 64 KB grain, so it lands in this row under the REPL's name.
Nothing in the image says how much of the row it is — the census can only charge
an anonymous block to the word below it.

The row's shape says what kind of store it is. Its content averages 1.8 bytes a
run, so the non-zero bytes are scattered over cells that mostly hold small
numbers, and its framing costs more than its content: 402,952 bytes of rows
against 360,165 bytes of content. `SYMS-BOOT` (`src/core/checker.f`) is the same
shape taken to its limit — 37,639 runs of exactly one byte each, 112,917 bytes
of image for 37,639 bytes of content. Its string arena `SYM-STR-BOOT` is the
opposite and the cheapest thing in the table: 237,993 bytes in a single run,
costing six bytes to frame. The checker's two symbol stores are 350,916 bytes of
image between them.

## The dictionary the image ships

| class | records | record bytes | name bytes | code bytes |
| --- | ---: | ---: | ---: | ---: |
| global | 4,700 | 94,000 | 56,181 | 449,680 |
| package-public | 2,785 | 55,700 | 28,332 | 247,800 |
| package-private | 11 | 220 | 114 | 1,684 |
| unmapped-wordlist | 0 | 0 | 0 | 0 |
| package rows | 178 | 3,560 | 2,090 | 0 |

Names are deduplicated, so the name bytes above double-count a name two records
share. The exclusive figures the tool prints are small now: 114 bytes of name
pool are reachable only from private records, none only from named code sites,
and 606 bytes nothing in the image references at all.

The private records are gone. Dropping them was measured at 232,157 bytes on the
engine this document last described, which still carried 8,021 of them; eleven
remain, holding 220 bytes of record and 114 bytes of name, and their code —
along with the code of every other record the capture dropped — travels in
`aot/code-spans`. What is left is 237,124 bytes of records plus names, 6.0% of
the engine, for 7,674 records. In the file it is 20 bytes per record; at boot it
is 48, so the same dictionary costs 368,352 bytes of dictionary region plus its
hash index once the engine is up.

## The baked call sites

```
  sites 11577, bound to seeded primitives 11577, to payload records 0, left as names 0
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

All 11,577 sites name one of 75 engine primitives; a call between two captured
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

A stripped application gets the closure walk (`src/habu/aot-closure.f`); the
engine does not, so every word the build ever compiled ships. The tool marks
from roots over the same call graph — direct `B`/`BL` edges in the baked code
plus the code addresses the payload's relocation tables name — with two root
sets, because "dead" means two different things.

**Dictionary surface** (every global and package-public word is a root, since a
program can name it):

| | records | code bytes | record bytes | name bytes |
| --- | ---: | ---: | ---: | ---: |
| reachable | 7,495 | 698,864 | | |
| unreachable | 1 | 300 | 20 | 12 |

One record, package-private, in `NSTR`. The 2,861 unreachable private records
this walk used to find are not hiding: the capture no longer ships them. The
walk is weaker than it was, too — the code those records owned is now code no
record owns, and every such span is scanned as a root — so the empty row says
less than it would have.

**Engine entry** (only the engine's own entry points are roots: its boot-run
entry words and the code addresses its DATA cells hold):

| | records | code bytes | record bytes | name bytes |
| --- | ---: | ---: | ---: | ---: |
| reachable | 3,350 | 500,248 | | |
| unreachable | 4,146 | 198,916 | 82,920 | 46,317 |

| class | records | record bytes | code bytes |
| --- | ---: | ---: | ---: |
| global | 2,922 | 58,440 | 118,448 |
| package-public | 1,223 | 24,460 | 80,168 |
| package-private | 1 | 20 | 300 |

Their top owners:

| package | file | records | code bytes |
| --- | --- | ---: | ---: |
| IR-ATTR | `src/compiler/ir/attr.f` | 70 | 13,244 |
| IR-SCHEMA | `src/compiler/ir/schema.f` | 46 | 5,324 |
| TFAM | `src/core/type-family.f` | 87 | 4,208 |
| IR-TYPE | `src/compiler/ir/type.f` | 23 | 4,152 |
| NUM | `lib/num-types.f` | 47 | 3,760 |
| A64IR | `src/compiler/native/a64ir.f` | 12 | 3,632 |
| A64EFF | `src/compiler/a64-effect.f` | 40 | 3,324 |
| IR-BUILD | `src/compiler/ir/build.f` | 40 | 2,520 |
| IR-FUN | `src/compiler/ir/fun.f` | 23 | 2,260 |
| HIR | `src/compiler/native/hir.f` | 11 | 2,240 |
| A64ASM | `src/arch/arm64/asm.f` | 45 | 1,916 |
| TYPE-DECL | `src/core/sumtype.f` | 20 | 1,772 |
| NTAPE | `src/compiler/native/tape.f` | 11 | 1,716 |
| DATA-CLAIMS | `src/habu/layout.f` | 20 | 1,608 |
| IR-OP | `src/compiler/ir/op.f` | 22 | 1,296 |
| A64EMIT | `src/compiler/native/emit.f` | 17 | 1,252 |

106 more packages follow.

This is a **lower bound on what a tree-shaken engine could keep, not a strip
list**: the interpreter resolves user tokens by name, so a public word outside
this closure is still callable from source. It says that 55% of the shipped
records and 28% of the code those records own exist only because the dictionary
is the language surface.

## What the size work is worth

Measured against this engine, so the numbers are bounds, not hopes:

- **Bind baked call sites at build time** (`habu-bind-baked-call-e4d5b58f`):
  done. Every one of the 11,577 rows carries an index, none carries a name, and
  the boot resolves nothing while it seeds.
- **Drop private dictionary records** (`habu-ship-no-dictionary-2fee2dea`):
  done. Eleven private records remain; the code of the rest travels in 8,071
  code-span rows at 8 bytes instead of 20 bytes of record and 48 of booted
  dictionary.
- **Declare the engine's surface** (`habu-declare-the-surface-89e9aed0`): the
  engine-entry closure is the bound. 4,146 records the engine's own entries never
  reach are 82,920 bytes of record plus 46,317 bytes of name, and their code is
  198,916 bytes; a record that becomes a code span gives back 12 of its 20 bytes
  and all of its name.
  The same caveat applies — a word outside the closure is still callable by name,
  so the list decides what goes, not the walk.
- **The DATA image**: 1,230,764 bytes, 31.3%, and the framing is finished. Rows
  average 2.001 bytes, one byte a field, which is the floor the format has; what
  is left to win is content and layout. The checker's stores are where both are:
  360,165 bytes scattered across 201,471 runs in the `DONE` row and 37,639
  one-byte runs in `SYMS-BOOT`, together costing 876,034 bytes of image for
  397,804 bytes of content. A store laid out without interleaved zero cells is
  the lever (`habu-ship-only-the-d7d38629`, item 4), not a denser row format.

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
`aot/data-run-*` rows describe from the other side — the AOT capture does encode
runs, which is why the engine pays 1.2 MB for a 6.8 MB heap while the snapshot
pays 14.9 MB for a 14.9 MB one.

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

65,728 bytes, of which 62,887 are zero — and almost all of that is one class.

| class | bytes | zero | % | what it is |
| --- | ---: | ---: | ---: | --- |
| `elf/header` … `elf/header-pad` | 4,096 | 3,980 | 6.2 | the header page |
| `app/code` | 3,176 | 524 | 4.8 | the startup, the closure of `MAIN`, the crash and signal handlers |
| `app/data-run-rows` | 24 | 3 | 0.0 | a u32 of encoded row bytes and the varint `(gap, length)` rows |
| `app/data-run-bytes` | 19 | 0 | 0.0 | the bytes those rows describe |
| `app/row-align-pad` | 1 | 1 | 0.0 | the blob rounded up to the rows' four-byte boundary |
| `app/relocation-rows` | 8 | 3 | 0.0 | one 8-byte row per declared address cell |
| `image/text-pad` | 58,212 | 58,212 | 88.5 | the text segment rounded up to 64 KB |
| `container/rw-segment` | 192 | 164 | 0.2 | DYNAMIC plus the two loader slots |
| **total** | **65,728** | **62,887** | 100.0 | |

```
restored DATA window: 659516 bytes from 19 carried in 7 runs; 659497 zero bytes do not travel
  relocation rows 1, declared address cells this image rebinds at startup
```

**A stripped image carries no zero byte of its DATA at all.** Its window is
encoded as non-zero runs (`src/habu/aot-lib.f BUILD-SPARSE-DATA`), so 659,516
bytes of window arrive from 19 bytes of file. That is the same encoding the
engine's `aot/data-run-*` rows use, and it is why the snapshot's 14.9 MB and
this image's 19 bytes describe comparable things. The zero-filled span is
reported beside the table rather than as a class, because none of those bytes is
in the file to attribute.

**Almost the whole file is the page round.** 88.5% is `image/text-pad`: the text
segment rounds up to 64 KB and this program needs 3.2 KB of it. A saving smaller
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
