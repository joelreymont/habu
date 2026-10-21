\ Load the shared stripped linker and the selected target emitter.
package AOT-LINK
private

: LOAD-TARGET ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" src/habu/image-x64.f" required
      s" src/habu/boot-x64.f" required
      s" src/os/linux-x86-64/sys.f" required
      s" src/habu/aot-common.f" required
      s" src/habu/aot-x64.f" required
      exit
   then
   HB-TARGET-LINUX? HB-TARGET-MACOS? or if
      s" src/habu/aot-link-arm.f" required
      exit
   then
   s" aot: unsupported target" 76 die ;

' LOAD-TARGET
;package
execute
