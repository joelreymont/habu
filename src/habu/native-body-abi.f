\ Relative cells in the optional native-origin-cell capability record.
\ An engine bakes this file (src/habu/native-runtime.f provides it), so a
\ `require` binds the running engine's own indices. Its path and package name
\ are part of that ABI: do not rename them while older hosts still build.
package NATIVE-BODY-ABI
public
0 constant ORIGIN-HOOK
1 constant ORIGIN-TAKE
2 constant PENDING-TAKE
;package
