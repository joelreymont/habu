\ vocabulary.f - restrict loaded source to admitted packages' public words. A
\ harness loads the packages a design may call, admits each, seals and reads the
\ design inside one compiled word: a token the harness reads after RESTRICT is
\ gated too.
\ STORAGE CLASS. TASK-LOCAL: the seal and require identity live in the sealing
\ task's DATA header.

package VOCABULARY
private

TYPED-VARIABLE REQUIRE-XT [ -- ]
' require REQUIRE-XT !

public

\ Admit the public words of the package this span names. A name no package
\ owns is `hb: policy: no package <name>`.
: ALLOW-PACKAGE ( ptr u8 n -- ) policy-admit ;

\ Opt in to ordinary static require within this canonical searchable directory.
\ The resolver and loader then refuse a canonical path outside it (`..`,
\ absolute, symlink, library fallback) before a registry hit or a read, in checks
\ before RESTRICT and in the sealed load, the harness's `script-required` entry
\ included.
: SOURCE-ROOT! ( ptr u8 n -- ) REQUIRE-XT @ SOURCE-ROOT:POLICY-ROOT ;

\ Scopes the root and the nested-require owner around one callback: restores both
\ on return or throw, nests, refuses once sealed, and throws 107 on leaving if its
\ callback sealed.
: WITH-SOURCE-ROOT ( ptr u8 n [ -- ] -- ) REQUIRE-XT @ SOURCE-ROOT:POLICY-WITH-ROOT ;

\ Seals, with no unseal. The seal is src/habu/layout.f POLICY-NDICT-CELL,
\ POLICY-BITS-OFF and POLICY-REQUIRE-CELL; capture and warm restore clear all
\ three, both native kernels enforce it. From then on the engine refuses every
\ token outside the vocabulary, at the token, before anything it names runs. In
\ it: a `:` definition with `\` and `( ... )` comments; a number literal; a
\ `{: :}` group or a name it bound; thirteen keywords (if else then {: :} s" S\"
\ package public private ;package using ;using); a word the sealed source
\ defined; a public word of an admitted package that checked source may call and
\ the engine has not marked internal; after SOURCE-ROOT!, a top-level global
\ `require` with a literal path. `using` does not admit a package, and `package`
\ refuses one that existed at the seal (inside it its private words would come
\ first); a package the sealed source made opens and reopens as usual. The
\ checker still certifies every body, and an admitted program runs as it does
\ unsealed.
\ No back edge is in the vocabulary (forward branches only; no loop, recursion or
\ xt; no self-name before `;`; no source read under the default seal), so a sealed
\ load's calls are ordered by dictionary index and every run is bounded down to
\ the admitted words; with a root, `require` reads each file once. An admitted
\ package must guarantee that every public word terminates (a throw counts), a
\ parsing word reads boundedly many tokens, no word stores to an address its caller
\ supplies, and no word widens what the design can name (set-current,
\ prot-wid-add, evaluate or included on caller data).
\ Refusals are one stderr line and ENGINE-ERROR:POLICY (107), a throw a `catch`
\ around `included` can take: `hb: not in vocabulary: <token> at <path>:<line>`,
\ at the same token in both tiers; `hb: policy: package <name> predates the seal
\ at <path>:<line>`; `hb: policy: sealed` from any of the four words once sealed;
\ `hb: policy: no package <name>`; `hb: policy: package wid above the bound` (wid
\ >= PROT-WID-MAX has no admission bit); `hb: policy: package <name> publishes
\ keyword <word>` (a public word spelled like a dispatched keyword row, case
\ folded, which tier 0 would read as the row and tier 1 as the word); `hb: policy:
\ source outside root: <path>`. A definition named like one of the thirteen
\ keywords is the reserved-name refusal, exit 70; one named like another row is
\ refused as above.
: RESTRICT ( -- ) policy-seal ;

;package
