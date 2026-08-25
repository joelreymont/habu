\ native-runtime.f - the complete source-owned native runtime.
\
\ native-build.f evaluates this manifest inside the single AOT window.  The
\ sources before include.f were loaded directly; replaying their paths here
\ gives the target loader, rather than the discarded build host, authoritative
\ ownership of the complete runtime closure.

s" src/core/util.f" provided
s" src/core/cell.f" provided
HB-TARGET-LINUX? if
   s" src/os/linux/target.f" provided
   s" src/os/linux/layout.f" provided
else HB-TARGET-MACOS? if
   s" src/os/macos/target.f" provided
   s" src/os/macos/layout.f" provided
else
   s" native-runtime: unknown target" 76 die
then then
s" src/habu/layout.f" provided
s" src/core/pointer-storage.f" provided
s" src/core/engine-error.f" provided
s" src/core/exec-vector.f" provided
s" src/core/checker.f" provided
s" src/core/engine-error-effects.f" provided
s" src/core/type-schema.f" provided
s" src/core/type-family.f" provided
s" src/core/render.f" provided
s" src/core/sumtype.f" provided
s" src/core/layout-buffer.f" provided
s" src/core/check-hook.f" provided
s" src/core/roles.f" provided
s" src/core/cell-effects.f" provided
s" src/core/declaration-transaction.f" provided
s" src/core/generated-declaration.f" provided
s" src/core/decl-event.f" provided
s" src/core/structure-make.f" provided
s" src/core/structure-decl.f" provided
s" src/core/enum-decl.f" provided
s" src/core/structures.f" provided
s" src/core/bytes.f" provided
s" src/os/env-base.f" provided
s" src/core/include.f" provided

s" src/core/enums.f" required
s" src/core/sha256.f" required
s" src/core/type-family-sha.f" required
s" src/core/combinators.f" required
s" src/habu/xref.f" required
s" src/core/generated-declaration-dictionary.f" required
s" src/core/generated-declaration-protection.f" required
s" src/core/layout-buffer-seal.f" required
s" lib/prelude.f" required
s" lib/errors.f" required
s" lib/adt/option.f" required
s" lib/cad-num-types.f" required
s" lib/cad-num-arithmetic.f" required
s" lib/string.f" required
s" lib/memory.f" required
s" lib/vector.f" required
s" src/os/script-argv.f" required
s" src/compiler/native/compiler.f" required
HB-TARGET-LINUX? if
   s" src/os/linux/repl-term.f" required
else HB-TARGET-MACOS? if
   s" src/os/macos/repl-term.f" required
else
   s" native-runtime: unknown target" 76 die
then then
s" src/habu/repl.f" required
s" src/habu/debug-watch.f" required
s" src/habu/stepper.f" required
s" src/habu/debug.f" required
s" src/core/internal-mark.f" required
s" src/core/top-row.f" required

package NATIVE-RUNTIME

\ The manifest leaves this private xt across ;package and executes it as its
\ final token.  The checker observes execute before the body runs, so
\ CHECKER-CAPTURE-PREPARE remains the last checker operation before the
\ non-querying seal primitive captures the runtime.
: PREPARE ( -- )
   REQUIRE-BOOT-FREEZE
   INCLUDE-SNAPSHOT-PREPARE
   LBUF-CAPTURE-PREPARE
   XREF-SNAPSHOT-PREPARE
   NCOMP:CAPTURE-PREPARE
   SHA256-SNAPSHOT-PREPARE
   ENV-SNAPSHOT-PREPARE
   ['] NCOMP:COMPILE data-base NCOMP-DISPATCH:XT-CELL + xt!
   CHECKER-CAPTURE-PREPARE
   SEAL-CAPTURE ;

' PREPARE
;package
execute
