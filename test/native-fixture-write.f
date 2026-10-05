\ Select the source-built fixture writer for the running target. ARM keeps its
\ original capture-host writer; x86-64 builds a full native runtime and merges
\ a partial capture into that runtime when one was supplied.
package NATIVE-FIXTURE-WRITE
private
: LOAD-TARGET ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" test/native-fixture-write-x64.f" required exit then
   HB-TARGET-LINUX? HB-TARGET-MACOS? or if
      s" test/native-fixture-write-arm.f" required exit then
   s" native-fixture: unsupported target" 76 die ;
' LOAD-TARGET
;package
execute
