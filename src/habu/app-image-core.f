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
   HB-TARGET-LINUX-X86-64? if
      s" src/os/linux-x86-64/sys.f" required
      s" src/os/image-bytes.f" required
      s" src/os/linux-x86-64/elf.f" required
      s" src/os/linux-x86-64/sign.f" required
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

\ app-image.f owns native selection and the scope around this complete load.
