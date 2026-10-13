\ policy.f - seal the source a process reads to an admitted vocabulary.
\
\ A harness loads the packages a design may call, admits each by name, seals,
\ and only then reads the design. It may set ROOT before checking and sealing
\ to admit confined static require. From the seal on, every source token the
\ engine reads resolves in the design's own definitions, in an admitted
\ package's public words, or in the design built-ins, and anything else is
\ `hb: not in vocabulary: <token> at <path>:<line>` with exit code
\ ENGINE-ERROR:POLICY. docs/policy.md states the vocabulary, the refusals and
\ what an admitted package must guarantee.
\
\ The harness does all three steps inside one compiled word, because the seal
\ governs the harness's own file too: a token it reads after SEAL is gated like
\ the design's.
\
\    require src/lib/policy.f
\    require my/vocabulary.f
\    : RUN ( -- ) s" MY-VOCAB" POLICY:ALLOW POLICY:SEAL 0 SCRIPT-ARGV$ included ;
\    RUN
\
\ All four words refuse once the process is sealed (`hb: policy: sealed`), so
\ nothing a design reaches can widen its own vocabulary. There is no unseal.
\ AArch64 only: the x86-64 kernel refuses both engine words.
\
\ Task-local: the seal and optional require identity live in the sealing
\ task's DATA header and govern the source that task reads.

package POLICY
private

TYPED-VARIABLE REQUIRE-XT [ -- ]
' require REQUIRE-XT !

public

\ Admit the public words of the package this span names. A name no package
\ owns is `hb: policy: no package <name>`.
: ALLOW ( ptr u8 n -- ) policy-admit ;

\ Opt in to ordinary static require within this canonical directory.
: ROOT ( ptr u8 n -- ) REQUIRE-XT @ SOURCE-ROOT:POLICY-ROOT ;

\ Scope the same boundary around a check that may return or throw.
: WITH-ROOT ( ptr u8 n [ -- ] -- ) REQUIRE-XT @ SOURCE-ROOT:POLICY-WITH-ROOT ;

\ Seal the process: every definition made from here on is the design's own.
: SEAL ( -- ) policy-seal ;

;package
