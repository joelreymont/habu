# Sealing a load to an admitted vocabulary

`lib/policy.f` confines the source a process reads to a vocabulary its harness
chose. It is for a program that loads source it did not write: the harness loads
the packages that source may call, admits them by name, seals, and then loads
the source. From the seal on the engine refuses every token outside the
vocabulary, at the token, before anything it names can run.

AArch64 only. The x86-64 kernel registers both engine words as refusals,
because its token loops do not read the seal.

## Use

```forth
require lib/policy.f
require my/vocabulary.f

: RUN ( -- ) s" MY-VOCAB" POLICY:ALLOW POLICY:SEAL 0 SCRIPT-ARGV$ included ;
RUN
```

```sh
bin/hb --load harness.f -- design.f
```

| word | effect | does |
|---|---|---|
| `POLICY:ALLOW` | `( ptr u8 n -- )` | admits the public words of the package the span names |
| `POLICY:SEAL` | `( -- )` | seals the process |

- Admit, seal and load inside **one compiled word**. The seal governs the
  harness's own file too: a token the engine reads from it after `SEAL` is gated
  like the design's.
- Both words refuse once the process is sealed: `hb: policy: sealed`, exit
  code 107. There is no unseal, and a fresh process is unsealed.
- A name no package owns is `hb: policy: no package <name>`, exit code 107.
- A package whose wordlist id is at or above `PROT-WID-MAX` (8192) has no bit
  in the admission bitmap: `hb: policy: package wid above the bound`, exit
  code 107. A word such a package held at the seal is never in the vocabulary.
- A package with a public word spelled like a keyword row the engine
  dispatches (`DUP`, `+`, `create`; the match folds case) cannot be admitted:
  `hb: policy: package <name> publishes keyword <word>`, exit code 107. Under the
  seal tier 0 would read that name as the row and tier 1 as the word.
- Importing a package with `using` does not admit it, and neither does
  reopening it with `package`.

## The vocabulary

After the seal a token is in the vocabulary when it is one of these.

- A definition: `: NAME ( effect ) … ;`, with `\` and `( … )` comments.
- A number literal: an integer in any spelling the engine reads, or a real such
  as `1.5`.
- A local: a `{: a b:n :}` group and a reference to a name it bound.
- One of twelve keywords: `if` `else` `then`, `{:` `:}`, `s"`, `package`
  `public` `private` `;package`, `using` `;using`.
- A word the sealed source defined itself, in whichever package it put it.
- A public word of an admitted package that checked source may call. A word
  marked internal to the engine is not one, even when an admitted package's
  public wordlist holds it.

Nothing else is. There is no arithmetic, no stack word, no loop, no `recurse`,
no quotation or execution token, no definer other than `:`, no store or fetch,
no other string form, no `require`, `included` or `evaluate`, and no
`trusted:`. The admitted packages supply every operation a design performs.

The seal removes words and changes nothing else. The checker still certifies
every body at `;` and refuses one that breaks its effect, a throw still ends
the process or unwinds to a `catch`, and a program inside the vocabulary runs
as it does unsealed.

## Refusals

The engine's refusal is one line on stderr and exit code
`ENGINE-ERROR:POLICY` (107):

```
hb: not in vocabulary: <token> at <path>:<line>
```

Nothing after the refused token is read or run. When the sealed source was
loaded by `included` inside a `catch`, the refusal is a throw of 107 that the
harness catches.

| the source names | refused token |
|---|---|
| `require lib/ffi-abi.f`, `s" f" included`, `s" 1" evaluate` | `require`, `included`, `evaluate` |
| `trusted: X ( -- ) ;` | `trusted:` |
| `' WORD`, `c" x"`, `." x"` | `'`, `c"`, `."` |
| `1 +`, `!`, `execute` in a body | `+`, `!`, `execute` |
| a public word of a package that was not admitted | the qualified name |
| the same word through `using` | the bare name |
| a private word of an admitted package, qualified or from inside it | the name |
| an internal word of an admitted package | the name |
| `POLICY:ALLOW`, `POLICY:SEAL` | the qualified name |
| `begin`, `do`, `exit`, `>r`, `recurse`, `[:`, `[']`, `[char]`, `[`, `postpone` in a body | the keyword |
| a call to the definition being compiled | its name |
| `: begin ( -- ) ;`, `: dup ( -- n ) 5 ;`, `: create ( -- ) ;` | the name |
| a token nothing defines | the token |

Both tiers refuse at the same token. Tier 1 (`1 set-tier`, the native compiler)
captures a body's text before it compiles it, and the capture admits what tier 0
admits: a local the body declared, a record in the vocabulary, a number, and
`if` `else` `then` `s"` `{:`, whose group ends at its `:}`. The compiler never
reads a token outside the vocabulary.

A definition may not be named like any keyword row the engine dispatches,
because tier 0 would read the name as the row and tier 1 as the definition. A
row outside the twelve keywords is refused as above; one of the twelve
(`: package ( -- ) ;`) is the reserved-name refusal both tiers share,
`hb: compile keyword cannot be a definition name: package`, exit code 70. A
local may take any name: `{: type:n :} type` reads the local at both tiers.

## Why a sealed load terminates

The vocabulary has no back edge. `if`, `else` and `then` branch forward only;
the loop keywords, `recurse`, `defer` and every way to hold or run an execution
token are outside it; a definition cannot name itself, because its record does
not exist until `;`; and no sealed token reads more source. A body can therefore
call only words that already exist when it is compiled, so the calls of a sealed
load form a graph ordered by dictionary index, and every run is bounded by
induction on that index down to the admitted words. No fuel, timeout or
analysis pass is involved.

## What an admitted package must guarantee

The seal confines what a design can name. What the named words do is the
harness's responsibility, and the argument above assumes all of it.

- Every public word of an admitted package terminates on every input. A throw
  counts.
- An admitted parsing word consumes a bounded number of tokens.
- No admitted word stores to an address its caller supplies.
- No admitted word changes what the design can name: none calls `set-current`
  or `prot-wid-add`, or `evaluate` or `included` on data its caller supplies.
  Nor does one hand such data to the interpret loop written in Habu
  (`src/habu/interpret.f` `OUTER:INTERPRET`): it reads no seal, so the data
  would run unsealed.

The admitted public words are the design's whole reach.

## Where it lives

The seal is two cells of the DATA header (`src/habu/layout.f`
`POLICY-NDICT-CELL`, `POLICY-BITS-OFF`): the dictionary size at the seal, and a
bitmap of admitted wordlist ids. `policy-admit` and `policy-seal`
(`src/habu/habu1.f`) are the only writers. The readers are the sites where a
source token is read (`src/habu/habu2.f`): `LPOLICYREC` for a found record,
`LKWCMP` for a matched keyword, `LUNDEF` for a miss, `CAPTURE-IMMEDIATE`'s
predicate for every tier-1 body token, and `EMIT-QUALIFY-DEF` for a definition
name. The name wall and `policy-admit` share `LROWWALK`, a spelling test per
keyword row generated from the rows' own registrars, so a new row is covered
when it is added. The native compiler never reads the seal: it binds only names
the capture already admitted.

`lib/policy-test.f` loads each case under `test/policy/` sealed at both tiers
and writes `build/policy-run.txt`, one line per child.
