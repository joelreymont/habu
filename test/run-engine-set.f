\ run-engine-set.f - what "the engine" means to a gate phase key.
\
\ A phase runs `bin/hb`, but the binary is not the whole engine. bin/hb bakes
\ only primitives; the checker, the core registries and the seeded stdlib are
\ re-read from the checkout at every process start (docs/bootstrap.md,
\ src/habu/habu2.f PFX-LOAD-BASE-FILES). So an edit under src/core changes what
\ every phase observes while leaving the binary byte-identical.
\
\ That is why this set exists. The per-phase PASS-stamp cache (test/run-lib.f)
\ used to key on `bin/hb` alone, so a stamp earned before a boot-prefix edit was
\ served after it: the tree that moved SCHEMA-N@ into package SCHEMA-REG turned
\ the prop/debug phase red while its key stood still, and the gate reported
\ PASS (cached) on a red tree (incident habu-incident-master-red-750d7ee7).
\
\ tools/boot-pin.f BP-EACH is the one place the prefix list lives, and
\ test/boot-pin-test.f holds it to src/habu/habu2.f, so this set inherits that
\ single source of truth instead of restating the paths. Whatever BP-EACH names
\ is keyed the moment it is named.
\
\ FILES has the shape of a test/run-files.f TR-FILES: set, so a caller folds it
\ into a key exactly the way it folds a declared file set.

require tools/boot-pin.f

package ENGINE-SET

public

\ typed-local-lint: allow-bare-local - q keeps the quotation effect from the stack signature.
: FILES ( [ ptr u8 n -- ] -- ) {: q :}
   s" bin/hb" q execute
   q BP-EACH ;

;package
