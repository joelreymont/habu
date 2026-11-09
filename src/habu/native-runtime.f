\ native-runtime.f - the native runtime: the closure of the compiler, T0 on
\ arm64, the JIT, the REPL and the product's checked C2 storage, plus the Habu
\ loop on x86-64 (runtime-x64.f). native-build.f evaluates it inside the one AOT
\ window; replaying the paths of the files loaded before include.f gives the
\ target loader ownership of the whole closure.

\ Rows by choice: the debugger trio (debug-watch.f, stepper.f, debug.f over
\ code-bytes.f), without debug.f's breakpoints on x86-64; src/lib/num-*.f,
\ because src/lib/memory.f states the allocator's contract in NUM roles;
\ src/lib/adt/option.f for src/lib/string.f's option<n>; src/lib/float.f and
\ src/lib/fmt.f, because habu2.f requires fmt for number text.

\ The manifest is the engine's boot registry: opened here, frozen by
\ CAPTURE-PREPARE, and src/core/include.f keeps each row by its tree-relative
\ name in between, never the build directory's path, which the emitted engine
\ would carry byte for byte.

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
s" src/habu/boot-cells.f" provided
s" src/habu/intern-table.f" provided
s" src/habu/wl.f" provided
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
s" src/core/arena.f" provided
s" src/core/heap.f" provided
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
s" src/lib/error.f" required
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

package CHECKER-REG
public
\ The target's own installs, run by SEAL and filled by src/habu/runtime-x64.f
\ on x86-64 and src/habu/runtime-arm64.f on arm64.
defer SEAL-TARGET ( -- )
;package

HB-TARGET-KNOWN? 0= [if] s" native-runtime: unknown target" 76 die [then]
HB-TARGET-LINUX-X86-64? 0= [if] s" src/habu/runtime-arm64.f" required [then]
HB-TARGET-LINUX? [if] s" src/os/linux/repl-term.f" required [then]
HB-TARGET-MACOS? [if] s" src/os/macos/repl-term.f" required [then]
HB-TARGET-LINUX-X86-64? [if] s" src/os/linux-x86-64/repl-term.f" required [then]
s" src/habu/repl.f" required
s" src/habu/code-bytes.f" required
s" src/habu/debug-watch.f" required
s" src/habu/stepper.f" required
\ Breakpoints plant A64 BRK words, and only the ARM64 engine's SIGTRAP handler
\ (src/habu/habu2.f EMIT-TRAPH) serves them; the x86-64 kernel sends SIGTRAP
\ to its crash dump (src/habu/boot-x64.f INSTALL-CRASH,). That product bakes
\ the watch cells and the stepper without them.
HB-TARGET-LINUX-X86-64? 0= [if] s" src/habu/debug.f" required [then]
s" src/core/top-row.f" required
s" src/lib/c2-owner.f" required
s" src/lib/executable-build.f" required
\ The cross-build's linker, which writes arm64 programs (src/habu/link-data.f).
HB-TARGET-LINUX-X86-64? 0= [if] s" src/habu/link-data.f" required [then]

\ The Habu loop is x86-64's evaluate, startup and command line; ARM64 keeps
\ the kernel's bodies for them and captures none of the loop's files.
HB-TARGET-LINUX-X86-64? [if] s" src/habu/runtime-x64.f" required [then]

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

: SEAL ( -- )
   NCOMP:INSTALL
   SEAL-TARGET
   SEAL-CAPTURE ;

' SEAL
;package
execute
