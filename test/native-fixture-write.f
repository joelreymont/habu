\ Source-built image writer shared by partial-capture fixtures, selected for
\ the running target. test/native-fixture-write-arm.f keeps the capture-host
\ writer; test/native-fixture-write-x64.f builds a full native runtime and
\ merges a partial capture into that runtime when one was supplied. Either is
\ an application: test/fixture-writer.f builds it once per tree into a keyed
\ image through tools/app-build.f, which starts the image at MAIN.
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
