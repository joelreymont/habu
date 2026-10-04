\ policy.f - seal the source a process reads to an admitted vocabulary.
\
\ A harness loads the packages a design may call, admits each by name, seals,
\ and only then reads the design. From the seal on, every source token the
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
\    require lib/policy.f
\    require my/vocabulary.f
\    : RUN ( -- ) s" MY-VOCAB" POLICY:ALLOW POLICY:SEAL 0 SCRIPT-ARGV$ included ;
\    RUN
\
\ Both words refuse once the process is sealed (`hb: policy: sealed`), so
\ nothing a design reaches can widen its own vocabulary. There is no unseal.
\ Both native kernels enforce admission and the seal through these words.
\
\ Task-local: the seal is two cells of the sealing task's DATA header, and it
\ governs the source that task reads.

package POLICY
public

\ Admit the public words of the package this span names. A name no package
\ owns is `hb: policy: no package <name>`.
: ALLOW ( ptr u8 n -- ) policy-admit ;

\ Seal the process: every definition made from here on is the design's own.
: SEAL ( -- ) policy-seal ;

;package
