# SwiftX cross-compilation, distilled

Sources:

- *SwiftX Cross-Development Software Reference Manual*, FORTH, Inc., copyright
  1998-2010, revision of June 20, 2012. It ships inside the SwiftX evaluation
  installer (<https://www.forth.com/download/>). Section and table numbers
  (§4.6.2, Table 14) are the manual's.
- The ANS Forth cross-compiler wordset proposal, FORTH, Inc.: normative text
  <https://www.forth.com/downloads/ANS/XCtext5.pdf> (cited as x.3.1.4) and
  rationale <https://www.forth.com/downloads/ANS/XCapp5.pdf> (cited as A.x.1).

The manual is proprietary to FORTH, Inc.; this page restates its facts in new
words, as [swiftforth.md](swiftforth.md) does for SwiftForth. It describes
SwiftX only, and only how one source file is split between the machine that
builds and the machine that runs. Habu's own rules are in [xcomp.md](xcomp.md).

## The two machines

The host compiles; the target runs. Target code never executes on the host,
and a finished target needs nothing from the host: it is not required to carry
a compiler or a searchable dictionary (x.5.1.2). The host can ask a connected target to run a word (the
cross-target link, XTL, a debugging aid, §4.9); a build needs no connection.

## Scopes

A scope word in the source sets where the definitions after it go and which
words can be found. It stays in force until another scope word (§4.6.2,
Table 12).

| Scope | Its definitions are | Used for |
|---|---|---|
| `TARGET` (the default) | compiled into the target | the program |
| `INTERPRETER` | host words, run at top level on the target's behalf | defining words, allocation, values the build computes |
| `COMPILER` | host words, run only while a target colon definition compiles | `IF`, `LOOP` and other words that shape a definition |
| `HOST` | host words of the underlying Forth | rare additions to the cross-compiler itself |

`ASSEMBLER` is a fifth scope, for the target's instruction mnemonics.

Where a colon definition can be used (Table 14):

| Defined in | At top level | Inside a colon definition |
|---|---|---|
| `TARGET` | not allowed unless connected | in `TARGET` definitions: compiled as a call |
| `INTERPRETER` | in `TARGET` scope: executed on the host | in `INTERPRETER` definitions |
| `COMPILER` | not allowed | in `TARGET` definitions: executed on the host |
| `HOST` | in `HOST`, `INTERPRETER`, `COMPILER` | in `HOST`, `INTERPRETER`, `COMPILER` |

- A target colon definition typed or loaded at top level without a connection
  is not allowed (§4.6.1, §4.6.3); the proposal calls executing a target
  definition on the host an ambiguous condition (x.3.1.4) and says a program
  must never attempt it at compile time (A.x.1).
- `IMMEDIATE` is not used: `COMPILER` scope replaces it (x.3.1.5).
- `[+TARGET]`, `[+HOST]`, `[+INTERPRETER]` add one scope to the search order
  inside a host definition, and `[PREVIOUS]` removes it (Table 13).

## Top level

Everything outside a colon definition executes on the host.

- A number goes on the host's stack (§4.1.1).
- `EQU` defines a constant on the host only. Inside a target definition its
  value is compiled as a literal (§4.4). It is what `[IF]` tests:
  `0 EQU MEM-MAP` then `MEM-MAP [IF] ... [THEN]` (§4.1.2).
- A computed value needs a host word to compute it: `4 SQUARE CONSTANT N`
  builds only when `SQUARE` was defined in `INTERPRETER` or `HOST` scope.

## Target data

Target memory is declared in sections of three types (§4.3, Table 11):

| Type | Holds | At build time |
|---|---|---|
| `CDATA` | code and initialization tables; may be ROM | the host keeps an image; readable and writable |
| `IDATA` | initialized data, preset at build time and copied in at power-up | the host keeps an image; readable and writable |
| `UDATA` | uninitialized data | allocated only; no access without a connection |

- The host holds an image of every `CDATA` and `IDATA` section (§4.9.1).
  Top-level `,` `C,` `@` `!` `ALLOT` act on that image, so
  `CREATE PRIMES 2 , 3 , 5 ,` lays the table down during the build.
- A target data word can be used at top level, where it does not run its target
  behaviour: a `CONSTANT` returns its value and any other data word returns
  the target address of its data (§4.6.4). The manual calls the host entry a
  twin; `CREATE`, `VARIABLE` and `CONSTANT` make one automatically, and `TWIN`
  makes one for a custom word (§4.7).
- `VARIABLE` and `BUFFER:` allocate in `UDATA`, which no build-time code can
  initialize (§4.4).

## Defining words

A defining word runs on the host to make a definition the target runs, so it
is written in `INTERPRETER` scope (§4.7):

```
INTERPRETER
: PRINTS ( n -- ) CREATE ,  DOES> ( -- ) @ . ;
TARGET
1 PRINTS ONE
```

The part before `DOES>` is host code. The part after `DOES>` is target code
and may name target words, because only the target ever executes it. `ONE`
used at top level without a connection returns the address of its data.

A word needed on both machines is defined on both; the manual's example
defines a host version with `TWIN ... DOES>` in `HOST` scope beside the target
version (§4.7).

## Building

- Libraries are source, compiled "directly from source to executable form
  without a link phase" (§6). A build compiles the whole program from its main
  load file (§3.2.2).
- Strip repeats the build, leaving out each time the words the last build
  found never called, until two builds give the same size (§3.2.2).
- `SAVE-CODE` and `SAVE-DATA` write a section's image as Intel hex, S-records,
  TI text or binary (§4.8).
- The manual describes no per-file object, no dependency tracking and no build
  cache.

## Rules in one place

1. The scope in force decides where a definition goes: `TARGET` into the
   image, the others onto the host.
2. Outside a colon definition everything executes on the host, against the
   host's images of code and initialized data.
3. Inside a target colon definition only `COMPILER` words execute; every other
   word is compiled as a target reference and an `EQU` as a literal.
4. The code after `DOES>` in an `INTERPRETER` defining word is target code.
5. A target colon definition never executes on the host; a target data word
   has a host twin that returns its value or address.
