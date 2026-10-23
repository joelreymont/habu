\ The native x86 compiler is already installed by its engine. Other hosts
\ never load this extension, so recovery's AOT window remains compiler-free.
require src/compiler/native/abi.f
require src/compiler/native/shadow.f

package AOT-ARM
private

: OPEN-NATIVE-WINDOW ( -- ) NABI:BINDING NSHADOW:OPEN-AUTO-NATIVE ;

: INSTALL-NATIVE ( -- ) [: OPEN-NATIVE-WINDOW ;] is WINDOW-NATIVE ;
INSTALL-NATIVE

;package
