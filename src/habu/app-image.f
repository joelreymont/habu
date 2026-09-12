\ Save the running native dictionary, compiler, checker and REPL.
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/codesign.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f

package APP-IMAGE

: LOAD-TARGET ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/sys.f" required
      s" src/os/image-bytes.f" required
      s" src/os/linux/elf.f" required
      s" src/os/linux/sign.f" required
      exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/sys.f" required
      s" src/os/image-bytes.f" required
      s" src/os/macos/macho.f" required
      s" src/os/macos/sign2.f" required
      exit
   then
   s" app-image: unsupported target" 76 die ;

' LOAD-TARGET
;package
execute

require src/habu/fdio.f
require src/habu/driver-io.f
require src/habu/snap-lib.f

package APP-IMAGE
public

: START! ( [ -- ] -- )
   data-base APP-ENTRY:XT-CELL + xt! ;

\ Invoke from the outer stdin stream after all required files have returned.
\ Capture exits after writing; callbacks may release live process resources.
: SAVE ( ptr u8 n -- )
   SNAP:PATH!
   REPL-ENABLE
   NATIVE-RUNTIME:CAPTURE-PREPARE
   SNAP:PERSIST ;

;package

\ EVERY DEFINITION THE APPLICATION ITSELF MAKES COMPILES ON TIER 1, and this line
\ is where that is decided: the stdin stream that builds an image requires this
\ file first, so selecting the tier at its tail puts the selection ahead of the
\ application's own source without asking every build stream to remember it.
\ WHY IT IS NOT A PREFERENCE. Tier 0 is the legacy JIT; it compiles a definition
\ before the checker has seen it, so it cannot route a checked quotation store
\ through QUOTATION-STORAGE:STORE the way the optimizing tier's elaborator does
\ (src/compiler/native/elaborate.f DO-QUOTATION-STORE). A store it lowers as a
\ plain `!` never declares the cell to the persisted-address table
\ (src/habu/layout.f SNAP-RELOC:XTCELL-*), so the writer leaves the builder's own
\ code address in the image and the restored process jumps into whatever the
\ image mapped there - measured as SIGILL at a pc below the live region base,
\ from one `TYPED-VARIABLE` holding a quotation (test/app-image-subject.f).
\ Selecting the tier after this file's own requires keeps them on tier 0, where
\ every persisted cell they own is declared explicitly through `defer`/`is`/`xt!`.
1 set-tier
