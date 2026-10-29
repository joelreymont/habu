\ native-runtime.f - the complete source-owned native runtime.
\
\ native-build.f evaluates this manifest inside the single AOT window.  The
\ sources before include.f were loaded directly; replaying their paths here
\ gives the target loader, rather than the discarded build host, authoritative
\ ownership of the complete runtime closure.
\
\ WHAT BELONGS HERE is the closure of the compiler, the JIT, the REPL and the
\ product's checked C2 storage, and on x86-64 the Habu loop that is its
\ evaluate (src/habu/runtime-x64.f). No other package or type signature reaches
\ the image unless one of those entry points requires it. tools/manifest-lint.f checks that
\ against this file - every row is either a declared entry point, with its reason
\ on the row there, or a file some file in the closure requires - so a row added
\ here for convenience fails the lint rather than growing the binary quietly.
\ src/lib/vector.f, a row here by choice rather than by closure, has left: nothing in
\ the closure requires it. The debugger trio (debug-watch.f, stepper.f, debug.f,
\ over code-bytes.f) is baked as the debugger the REPL offers, without debug.f's
\ breakpoints on x86-64; a session reaches it with `using DEBUG` or `DEBUG:`
\ names. src/lib/num-*.f stay, because src/lib/memory.f states the allocator's
\ own contract in NUM roles and the compiler allocates;
\ src/lib/adt/option.f stays, because src/lib/string.f returns option<n>; src/lib/float.f and
\ src/lib/fmt.f stay, because src/habu/habu2.f requires fmt for number text.
\
\ The manifest IS the engine's boot registry, so it opens that registry here and
\ CAPTURE-PREPARE below freezes it. Between the two, src/core/include.f keeps
\ each row by PORTABLE name - relative to the tree - instead of canonicalising it
\ against whatever directory this build ran in, which the emitted engine then
\ carried byte for byte (dot habu-bake-prefix-src-1047b604).

REQUIRE-BOOT-OPEN
s" src/core/util.f" provided
s" src/core/cell.f" provided
package NATIVE-RUNTIME

: PROVIDE-TARGET ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" provided
      s" src/os/linux/layout-constants.f" provided
      s" src/os/linux/layout.f" provided
      exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/target.f" provided
      s" src/os/macos/layout.f" provided
      exit
   then
   HB-TARGET-LINUX-X86-64? if
      s" src/os/linux-x86-64/target.f" provided
      s" src/os/linux/layout-constants.f" provided
      s" src/os/linux/layout.f" provided
      exit
   then
   s" native-runtime: unknown target" 76 die ;

' PROVIDE-TARGET
;package
execute
s" src/habu/stack-abi.f" provided
s" src/habu/layout.f" provided
s" src/core/pointer-storage.f" provided
s" src/core/engine-error.f" provided
s" src/core/exec-vector.f" provided
s" src/core/checker-fetch-abi.f" provided
s" src/core/checker-owner-extension.f" provided
s" src/core/checker-owner-abi.f" provided
s" src/habu/created-body-abi.f" provided
s" src/habu/prims.f" provided
s" src/core/does-clause.f" provided
s" src/core/checker.f" provided
s" src/core/engine-error-effects.f" provided
s" src/core/lower-cert-base.f" provided
s" src/core/type-schema.f" provided
s" src/core/type-family.f" provided
s" src/core/render.f" provided
s" src/core/sumtype.f" provided
s" src/core/layout-buffer.f" provided
s" src/core/layout-valid.f" provided
s" src/core/check-hook.f" provided
s" src/habu/native-body-abi.f" provided
s" src/habu/native-body.f" provided
s" src/habu/engine-origin-early.f" provided
s" src/habu/engine-origin-checked.f" provided
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
s" src/core/quotation-storage.f" provided
s" src/core/include.f" provided

s" src/core/enums.f" required
s" src/core/sha256.f" required
s" src/core/source-producer.f" required
s" src/core/combinators.f" required
s" src/habu/code-span.f" required
s" src/habu/xref.f" required
s" src/core/cell-body.f" required
s" src/habu/engine-origin.f" required
ENGINE-ORIGIN:INSTALL
ENGINE-ORIGIN:ARM
s" src/core/generated-declaration-dictionary.f" required
s" src/core/generated-declaration-protection.f" required
s" src/core/layout-buffer-seal.f" required
s" src/core/dynamic-storage.f" required
s" src/core/checker-owner-guard.f" required
s" src/core/lower-cert-seal.f" required
s" src/lib/prelude.f" required
s" src/lib/errors.f" required
s" src/lib/span.f" required
s" src/core/layout-buffer-address.f" required
s" src/lib/image-lifecycle.f" required
s" src/lib/adt/option.f" required
s" src/lib/num-types.f" required
s" src/lib/num-arithmetic.f" required
s" src/lib/string.f" required
s" src/lib/memory.f" required
s" src/os/script-argv.f" required
\ Seal the pre-checker definitions before compiling the checked toolchain.
\ Later checked publications record their own visibility and minimum arity.
s" src/core/internal-mark.f" required
\ The checker symbols the capture keeps, decided against the sealed dictionary.
s" src/core/checker-surface.f" required
s" src/compiler/native/compiler.f" required
package NATIVE-RUNTIME

: LOAD-REPL-TERM ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/repl-term.f" required
      exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/repl-term.f" required
      exit
   then
   HB-TARGET-LINUX-X86-64? if
      s" src/os/linux-x86-64/repl-term.f" required
      exit
   then
   s" native-runtime: unknown target" 76 die ;

' LOAD-REPL-TERM
;package
execute
s" src/habu/repl.f" required
s" src/habu/code-bytes.f" required
s" src/habu/debug-watch.f" required
s" src/habu/stepper.f" required
package NATIVE-RUNTIME

\ Breakpoints plant A64 BRK words, and only the ARM64 engine's SIGTRAP handler
\ (src/habu/habu2.f EMIT-TRAPH) serves them; the x86-64 kernel sends SIGTRAP
\ to its crash dump (src/habu/boot-x64.f INSTALL-CRASH,). That product bakes
\ the watch cells and the stepper without them.
: LOAD-BREAKPOINTS ( -- )
   HB-TARGET-LINUX-X86-64? if exit then
   s" src/habu/debug.f" required ;

' LOAD-BREAKPOINTS
;package
execute
s" src/core/top-row.f" required
s" src/lib/c2-owner.f" required
s" src/lib/executable-build.f" required

package CHECKER-REG
public
\ The target's own installs, run by SEAL on x86-64 and filled there by
\ src/habu/runtime-x64.f; no other target loads a filler or calls it.
defer SEAL-TARGET ( -- )
;package

package NATIVE-RUNTIME

\ The Habu loop is x86-64's evaluate, startup and command line; ARM64 keeps
\ the kernel's bodies for them and captures none of the loop's files.
: LOAD-HABU-LOOP ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" src/habu/runtime-x64.f" required
   then ;

' LOAD-HABU-LOOP
;package
execute

package NATIVE-RUNTIME

public

\ Run outside an open source load. The checker reset must be the last
\ checker operation before copying the image.
: CAPTURE-PREPARE ( -- )
   ENGINE-ORIGIN:CAPTURE-PREPARE
   IMAGE-LIFECYCLE:PREPARE
   REQUIRE-BOOT-FREEZE
   INCLUDE-SNAPSHOT-PREPARE
   LBUF-CAPTURE-PREPARE
   XREF-SNAPSHOT-PREPARE
   NCOMP:CAPTURE-PREPARE
   ENV-SNAPSHOT-PREPARE
   CHECKER-CAPTURE-PREPARE
   ENGINE-ORIGIN:CAPTURE-END ;

;package

package CHECKER-REG

\ Bind this compiler's fixed engine cells to its private declaration record.
\ The entry runs again in each emitted process; capture-time stores alone do
\ not initialize the engine header copied outside the AOT DATA window.
TRUSTED: BIND-OWNER ( -- )
   DECLARATIONS NCOMP-DISPATCH:DECL-A !
   DECLARATIONS NCOMP-DISPATCH:TARGET-DECL-A ! ;

: SEAL ( -- )
   NCOMP:INSTALL
   HB-TARGET-LINUX-X86-64? if SEAL-TARGET then
   BIND-OWNER
   SEAL-CAPTURE ;

data-base NCOMP-DISPATCH:DECL-CELL + ptr-cell-mark
' SEAL
;package
execute
