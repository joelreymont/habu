\ vocabulary.f - restrict loaded source to admitted packages' public words.
\
\ A harness loads the packages a design may call, admits each by name, seals,
\ and only then reads the design. From the seal on the engine refuses every
\ token outside the vocabulary, at the token, before anything it names can run.
\ The harness does all three steps inside one compiled word, because the seal
\ governs the harness's own file too: a token it reads after RESTRICT is gated
\ like the design's.
\
\    require src/lib/vocabulary.f
\    require my/vocabulary.f
\    using VOCABULARY
\    : RUN ( -- ) s" MY-VOCAB" ALLOW-PACKAGE RESTRICT 0 SCRIPT-ARGV$ included ;
\    RUN
\    ;using
\
\ THE VOCABULARY. After the seal a token is in it when it is: part of a `:`
\ definition, with `\` and `( ... )` comments; a number literal; a `{: :}` local
\ group or a name it bound; one of thirteen keywords (if else then {: :} s" S\"
\ package public private ;package using ;using); a word the sealed source
\ defined itself; a public word of an admitted package that checked source may
\ call and the engine has not marked internal; or, after SOURCE-ROOT!, a
\ top-level global `require` with a literal path. Nothing else: no arithmetic,
\ stack word, loop, recurse, quotation or execution token, definer but `:`,
\ store or fetch, other string form, included, evaluate or trusted:. `using`
\ or reopening a package does not admit it. The checker still certifies every
\ body, and a program inside the vocabulary runs as it does unsealed.
\
\ REFUSALS are one stderr line and ENGINE-ERROR:POLICY (107), a throw a
\ harness `catch` around `included` can take: `hb: not in vocabulary: <token>
\ at <path>:<line>`, at the same token in both tiers (tier 1 captures a body's
\ text and admits what tier 0 admits); `hb: policy: sealed` from any of the
\ four words once sealed (there is no unseal); `hb: policy: no package
\ <name>`; `hb: policy: package wid above the bound` (wid >= PROT-WID-MAX has
\ no admission bit); `hb: policy: package <name> publishes keyword <word>` (a
\ public word spelled like a dispatched keyword row, case folded, which tier 0
\ would read as the row and tier 1 as the word); `hb: policy: source outside
\ root: <path>`. A definition named like one of the thirteen keywords is the
\ reserved-name refusal, exit 70; one named like another row is refused above.
\
\ SOURCE ROOT. SOURCE-ROOT! resolves a canonical searchable directory; from
\ then on the resolver and loader refuse a canonical path outside it (`..`,
\ absolute, symlink, library fallback) before a registry hit or a read, both
\ in checks before RESTRICT and in the sealed load, the harness's
\ `script-required` entry included. WITH-SOURCE-ROOT scopes the root and the
\ nested-require owner around one callback, restores both on return or throw,
\ nests, refuses once sealed, and throws 107 on leaving if its callback sealed.
\
\ TERMINATION. The vocabulary has no back edge: if/else/then branch forward,
\ no loop, recursion or execution token is in it, a definition cannot name
\ itself before `;`, and under the default seal no token reads more source. The
\ calls of a sealed load are therefore ordered by dictionary index and every
\ run is bounded by induction down to the admitted words. With a root,
\ `require` reads each named file once: the registry marks it before loading.
\
\ AN ADMITTED PACKAGE MUST GUARANTEE that every public word terminates on every
\ input (a throw counts), a parsing word consumes a bounded number of tokens,
\ no word stores to an address its caller supplies, and no word widens what
\ the design can name (set-current, prot-wid-add, evaluate or included on
\ caller data). The seal confines naming; the harness owns what names do.
\
\ The seal is src/habu/layout.f POLICY-NDICT-CELL (the watermark),
\ POLICY-BITS-OFF (the admission bitmap) and POLICY-REQUIRE-CELL (the opt-in
\ require record); image capture and warm restore clear all three. Both native
\ kernels enforce it. src/lib/vocabulary-test.f runs each test/policy/ case.
\
\ STORAGE CLASS. TASK-LOCAL: the seal and optional require identity live in the
\ sealing task's DATA header and govern the source that task reads.

package VOCABULARY
private

TYPED-VARIABLE REQUIRE-XT [ -- ]
' require REQUIRE-XT !

public

\ Admit the public words of the package this span names. A name no package
\ owns is `hb: policy: no package <name>`.
: ALLOW-PACKAGE ( ptr u8 n -- ) policy-admit ;

\ Opt in to ordinary static require within this canonical directory.
: SOURCE-ROOT! ( ptr u8 n -- ) REQUIRE-XT @ SOURCE-ROOT:POLICY-ROOT ;

\ Scope the same boundary around a check that may return or throw.
: WITH-SOURCE-ROOT ( ptr u8 n [ -- ] -- ) REQUIRE-XT @ SOURCE-ROOT:POLICY-WITH-ROOT ;

\ Activate the vocabulary restriction; it cannot be widened afterward.
: RESTRICT ( -- ) policy-seal ;

;package
