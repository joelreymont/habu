\ boot-cells.f - the header cells the boot writes. Its startup row
\ (boot-files.f FILL) loads it before checker.f, which reads it.
package BOOT
public
\ The running engine's heap floor, a DATA offset in the fixed header.
$47E0 constant HEAP-START-CELL
\ The bounds habu1.f DP-CHECK holds a new DP to, absolute: every arm64 start
\ writes DATA + DATA-START and DATA + DATA-SIZE - PROF-CNT-BYTES (prof-abi.f).
$7E0 constant DP-FLOOR-CELL
$7E8 constant DP-CEILING-CELL
\ The global wordlist and a package row's marker (WL) the lookups use, and
\ their readers: every boot writes 0 and WL:NAMESPACE.
$7F0 constant GLOBAL-WL-CELL
$7F8 constant NAMESPACE-WL-CELL
: GLOBAL-WL ( -- n ) data-base GLOBAL-WL-CELL + @ ;
: NAMESPACE-WL ( -- n ) data-base NAMESPACE-WL-CELL + @ ;
\ The wordlist of type entries, `pkg:tail` (a global type bare), and its reader:
\ 0, never a types wid, while the program has none (src/core/type-family.f).
$2D78 constant TYPES-WL-CELL
: TYPES-WL ( -- n ) data-base TYPES-WL-CELL + @ ;
\ 0, as every arm64 boot writes it, unless a build is writing an image; then the
\ host address of the image's DATA, whose header holds its address-cell table.
$6C8 constant IMAGE-BASE-CELL
\ A claimed cell nothing reads or writes (data-claims.f BOOT-USER-END).
$3CB0 constant USER-END
;package
