\ boot-cells.f - the header cells the boot writes. Loaded before checker.f by
\ each core prefix so the checker can read it (native-runtime.f provides it).
package BOOT
public
\ The running engine's heap floor, a DATA offset in the fixed header: a build
\ splits host address rows here (native-build-core.f CHECK-HOST-LAYOUT).
$47E0 constant HEAP-START-CELL
\ The bounds habu1.f DP-CHECK holds a new DP to, absolute: every arm64 start
\ writes DATA + DATA-START and DATA + DATA-SIZE - PROF-CNT-BYTES (prof-abi.f).
$7E0 constant DP-FLOOR-CELL
$7E8 constant DP-CEILING-CELL
\ The global wordlist and a package row's marker (DICT-WL) the lookups use, and
\ their readers: every boot writes 0 and DICT-WL:NAMESPACE.
$7F0 constant GLOBAL-WL-CELL
$7F8 constant NAMESPACE-WL-CELL
: GLOBAL-WL ( -- n ) data-base GLOBAL-WL-CELL + @ ;
: NAMESPACE-WL ( -- n ) data-base NAMESPACE-WL-CELL + @ ;
\ 0, as every arm64 boot writes it, unless a build is writing an image; then the
\ host address of the image's DATA, whose header holds its address-cell table.
$6C8 constant IMAGE-BASE-CELL
\ The user program's end while the engine prefix runs, 0 once EM-COMPILE-EXIT
\ installs it: two streams give the AOT seed a point before any user token.
$3CB0 constant USER-END
;package
