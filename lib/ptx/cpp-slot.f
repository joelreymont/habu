\ cpp-slot.f - checked cp.async pipeline-slot typestate protocol (package CPPSLOT).
\
\ The emit-time discipline of the cp.async double-buffered software pipeline
\ (cg-matmul-emit.f MM-PIPE-KLOOP-WITH, the decomposed CPP-* protocol steps):
\ a staged-buffer slot is ISSUED by the async copy, its group is COMMITTED, then
\ WAITED (drained + bar.sync fenced) before the staged tile may be READ. This
\ vocabulary makes that ordering a TYPE, so a checked kernel that composes the
\ steps out of order is rejected fail-closed - the checker EXPRESSES the pipeline
\ protocol instead of trusting a fused emitter body to keep it.
\
\ TYPESTATE (families in src/core/type-family.f, over a symbolic buffer parity p):
\   cpp-pending<p>    issued: cp.async copies in flight, group not yet committed.
\   cpp-committed<p>  commit_group closed; wait_group + bar.sync not yet done.
\   cpp-ready<p>      waited + bar.sync fenced: the staged tile is block-visible.
\ The slot threads pending -> committed -> ready; each transition consumes the
\ prior state, so the checker's ordinary stack-effect unification rejects:
\   read-before-wait  READ demands cpp-ready<p>, a cpp-pending<p> does not unify;
\   missing-commit    WAIT demands cpp-committed<p>, a cpp-pending<p> does not;
\   double-wait       WAIT demands cpp-committed<p>, a cpp-ready<p> does not;
\   parity mismatch   READ-STAGE ties the ready slot's parity p to the staged
\                     tile's parity, so a cross-parity read is a type error.
\ Negatives pinned in lib/ptx/cpp-slot-neg-test.f, positive in cpp-slot-test.f.
\
\ SCOPE (dot habu-checker-cp-async-6ba788a5): SAME-BODY parity consistency only -
\ the runtime loop-carried parity ALTERNATION across $KLOOP iterations lives in an
\ emitted-PTX runtime register (%r15 xor), outside an emit-time stack-effect
\ checker (that alternation stays the trusted MM-PIPE-KLOOP-WITH boundary). The
\ ISSUE mint of the initial cpp-pending<p> is the pipeline's existing trusted
\ boundary (the cp.async issue in MM-PIPE-KLOOP-WITH), so this vocabulary's entry
\ is a pending slot supplied by the issuer: the audited mint core stays where it
\ is, the PROTOCOL becomes checked.
\
\ WAIT is a bar.sync fence, so committed<p> -> ready<p> is flagged CTL-BARRIER by
\ the checker (PTX-CPWAIT-ROWS?) and a WAIT reached under divergent control
\ rejects (E-DIVERGENT-BARRIER), composing with the M5 barrier model rather than
\ duplicating it. Load after lib/ptx/cg-matmul-emit.f (the CPP-* step emitters).

require lib/ptx/cg-matmul-emit.f

package CPPSLOT
public

\ Retirement owner for both checked-ordering state transitions:
\ habu-ptx-phantom-preserving-3df9db92.
\ COMMIT: close the current cp.async issue group; the in-flight copies become a
\ committed group (cpp-pending<p> -> cpp-committed<p>). Wraps the CPP-COMMIT step
\ emitter; the pending slot's phantom register is the audited coercion boundary
\ (the cp.async issue owns the mint, this word only advances the state).
TRUSTED: COMMIT ( cpp-pending<p> -- cpp-committed<p> )   drop CPP-COMMIT 0 ;

\ WAIT: drain the committed group and bar.sync-fence it so every lane sees the
\ staged tile (cpp-committed<p> -> cpp-ready<p>). The bar.sync makes this a block
\ barrier: committed->ready is CTL-BARRIER (checker.f PTX-CPWAIT-ROWS?) and a WAIT
\ under divergent control rejects, exactly like a BLOCK-MAX collective (M5).
TRUSTED: WAIT ( cpp-committed<p> -- cpp-ready<p> )   drop 0 CPP-WAIT CPP-SYNC 0 ;

\ READ: consume the ready slot - the pipeline stage is now readable. A read of a
\ pending or committed slot is a fail-closed type error (read-before-wait /
\ missing-commit). Checked: the ready witness is a plain cell, no mint.
: READ ( cpp-ready<p> -- )   drop ;

\ READ-STAGE: consume a ready slot to license reading THE staged tile at the
\ matching buffer parity - the slot's parity p must unify with the mmstage's, so a
\ read of the wrong buffer (a ready<p> against an mmstage of parity q) rejects.
\ Ties the pipeline typestate to the real staged operand (lib/ptx/tile-pipe.f).
: READ-STAGE ( mmstage<t,b,l,w,p> cpp-ready<p> -- mmstage<t,b,l,w,p> )   drop ;

;package
