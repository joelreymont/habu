\ gpt2-entry-test.f - the GPT-2 production entry commands load and run.
\
\ WHY THIS FILE EXISTS. maki/infer/gpt2-generate.f published its three caps
\ through the generic `constant` definer. That definer records an open cell, so
\ the CAD-NUM role the value was built with is dropped at the store: a byte-len
\ went in and an untyped cell came out. Every checked reader then asked the cap
\ for a role it could no longer supply, and both production commands died on the
\ load path - `bin/hb --load tools/gpt2.f --` and `bin/hb --load
\ tools/gpt2-serve.f --` each exited 70 with `non-certified definition`. Nothing
\ went red, because not one GPT-2 production entry was scheduled in any suite
\ (dot habu-gpt-2-prod-ed55d98c).
\
\ WHAT IT PROVES, in two halves that each catch that defect on their own.
\
\ 1. The caps still carry their roles. CHECK-QUIET-CANDIDATE! hands the live
\    checker a one-word body per cap - in the role the cap owes, and in two
\    roles it must refuse. The refusals are the half a generic `constant`
\    passes: an open cell certifies AS `n` and is refused AS a CAD-NUM role,
\    the exact inversion of the verdicts below. So a cap that loses its role
\    fails here whichever way it is spelled.
\
\ 2. The commands reach their own entry word. Each entry is spawned as the real
\    command, through the engine ENGINE-CANDIDATE:PATH$ resolves, with no model
\    argument. An entry that loaded runs and hits its OWN guard, refusing with
\    its OWN code; an entry whose load path is broken never gets there and
\    reports the checker's rc instead.
\
\ SCOPE. These are load-path smoke cases and need no checkpoint, so they belong
\ in the ordinary suite. The behavioural GPT-2 proofs - real generation, the
\ guard-page reader, the post-service owner census - need a real checkpoint and
\ stay standalone; maki/test.f names them and their commands beside this suite,
\ and docs/ablation.md carries the members that need a device as well.
\
\ COST: four engine boots, about three seconds.

require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require test/checker-assert.f
require maki/infer/gpt2-generate.f

package GPT2-ENTRY
private

1024 constant CAP-N                \ per-stream capture; a refusing entry says little
120000 constant ENTRY-MS           \ one engine boot plus a load, generously bounded

\ Sysexits EX_USAGE, which maki/infer/gpt2-cli.f and maki/infer/gpt2-serve.f
\ each hold as their own private E-USAGE. It is what a production entry answers
\ when it has loaded, run, and found no argument to work with.
64 constant RC-USAGE

\ The engine's deterministic exit for a top-level throw nobody caught
\ (UNCAUGHT-RC, src/habu/habu1.f). The token-guard child refuses that way.
67 constant RC-THROW

create CAP-OUT CAP-N allot
create CAP-ERR CAP-N allot

variable CHILD-RC
variable OUT-U
variable ERR-U
variable EXITED

: STORE! ( len len outcome -- )
   MATCH outcome
      exited OF CHILD-RC ! true EXITED ! ENDOF
      signaled OF CHILD-RC ! false EXITED ! ENDOF
      timeout OF 0 CHILD-RC ! false EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !
   LEN>N OUT-U ! ;

: SPAWN-ENTRY ( ptr u8 n -- ) {: src:ptr srcu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   src srcu >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   CAP-OUT CAP-N >LEN CAP-ERR CAP-N >LEN ENTRY-MS T-BUDGET-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME STORE! ;

\ The entry ran to its own refusal: it exited rather than dying, answered the
\ code that refusal owes, and printed nothing on stdout.
: RAN-WITH ( ptr u8 n n -- ) {: src:ptr srcu:n want:n :}
   src srcu SPAWN-ENTRY
   EXITED @ TTRUE
   CHILD-RC @ want T=
   OUT-U @ 0 T= ;

\ An entry that refuses with nothing to say on either stream.
: QUIET-ENTRY ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu RC-USAGE RAN-WITH
   ERR-U @ 0 T= ;

\ An entry that refuses by letting the engine report its uncaught throw.
: NOISY-ENTRY ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu RC-THROW RAN-WITH
   ERR-U @ 0 > TTRUE ;

\ Half one: each cap carries the role its readers ask for and refuses every
\ role it does not own - including the raw cell a generic `constant` hands out.
: CAP-ROLES ( -- )
   s" MAX-TOKENS is an item-count, and nothing else" T-LABEL
   s" GE-A ( -- CAD-NUM:item-count ) GPT2:MAX-TOKENS" CHECK-QUIET-CANDIDATE! -1 T=
   s" GE-B ( -- CAD-NUM:byte-len ) GPT2:MAX-TOKENS" CHECK-QUIET-CANDIDATE! 0 T=
   s" GE-C ( -- n ) GPT2:MAX-TOKENS" CHECK-QUIET-CANDIDATE! 0 T=
   s" PROMPT-CAP is a byte-len, and nothing else" T-LABEL
   s" GE-D ( -- CAD-NUM:byte-len ) GPT2:PROMPT-CAP" CHECK-QUIET-CANDIDATE! -1 T=
   s" GE-E ( -- CAD-NUM:item-count ) GPT2:PROMPT-CAP" CHECK-QUIET-CANDIDATE! 0 T=
   s" GE-F ( -- n ) GPT2:PROMPT-CAP" CHECK-QUIET-CANDIDATE! 0 T=
   s" OUTPUT-CAP is a byte-len, and nothing else" T-LABEL
   s" GE-G ( -- CAD-NUM:byte-len ) GPT2:OUTPUT-CAP" CHECK-QUIET-CANDIDATE! -1 T=
   s" GE-H ( -- CAD-NUM:item-count ) GPT2:OUTPUT-CAP" CHECK-QUIET-CANDIDATE! 0 T=
   s" GE-I ( -- n ) GPT2:OUTPUT-CAP" CHECK-QUIET-CANDIDATE! 0 T= ;

\ tools/gpt2.f runs GPT2-CLI:RUN, whose first act is its own argument-count
\ guard, so reaching that guard proves the whole generation chain certified.
: GEN-ENTRY ( -- )
   s" the generation command loads and reaches GPT2-CLI:RUN" T-LABEL
   s" tools/gpt2.f" QUIET-ENTRY ;

\ tools/gpt2-serve.f runs GPT2-SERVE:RUN, which guards its argument count before
\ it opens anything, so no checkpoint is needed to prove its load path whole.
: SERVE-ENTRY ( -- )
   s" the service command loads and reaches GPT2-SERVE:RUN" T-LABEL
   s" tools/gpt2-serve.f" QUIET-ENTRY ;

\ The service-cleanup census loads the service command and then reads the
\ safetensors owner and mapping counts back; it refuses the same way.
: CLOSE-ENTRY ( -- )
   s" the service-cleanup census loads over the service command" T-LABEL
   s" maki/infer/gpt2-serve-close-test.f" QUIET-ENTRY ;

\ The guard-page child is production source of its own: the page it maps is the
\ only place GPT2:ENCODE's chunk walk is proved to stay in bounds. Its first act
\ is an argument-count guard it throws from, uncaught.
: GUARD-ENTRY ( -- )
   s" the guard-page child loads and reaches its own guard" T-LABEL
   s" maki/infer/gpt2-token-guard-child.f" NOISY-ENTRY ;

: T-RUN ( -- )
   T-RESET
   CAP-ROLES
   GEN-ENTRY
   SERVE-ENTRY
   CLOSE-ENTRY
   GUARD-ENTRY
   T-REPORT ;

T-RUN

;package
