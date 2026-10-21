\ vocabulary.f - restrict loaded source to admitted packages' public words.
\
\ A harness loads the packages a design may call, admits each by name, seals,
\ and only then reads the design. SOURCE-ROOT! optionally confines static
\ require to a directory before checking or calling RESTRICT. From then on,
\ every source token the
\ engine reads resolves in the design's own definitions, in an admitted
\ package's public words, or in the design built-ins, and anything else is
\ `hb: not in vocabulary: <token> at <path>:<line>` with exit code
\ ENGINE-ERROR:POLICY. docs/vocabulary.md states the vocabulary, the refusals and
\ what an admitted package must guarantee.
\
\ The harness does all three steps inside one compiled word, because the seal
\ governs the harness's own file too: a token it reads after RESTRICT is gated like
\ the design's.
\
\    require src/lib/vocabulary.f
\    require my/vocabulary.f
\    using VOCABULARY
\    : RUN ( -- ) s" MY-VOCAB" ALLOW-PACKAGE RESTRICT 0 SCRIPT-ARGV$ included ;
\    RUN
\    ;using
\
\ All four words refuse once the process is sealed (`hb: policy: sealed`), so
\ nothing a design reaches can widen its own vocabulary. There is no unseal.
\ AArch64 only: the x86-64 kernel refuses both engine words.
\
\ Task-local: the seal and optional require identity live in the sealing
\ task's DATA header and govern the source that task reads.

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
