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
\ The wid lookups treat as the global wordlist and a package row's marker
\ (DICT-WL): every boot writes 0 and DICT-WL:NAMESPACE.
$7F0 constant GLOBAL-WL-CELL
$7F8 constant NAMESPACE-WL-CELL
\ BOOT:USER-END: the end of the user source stream while the engine-prefix
\ stream runs, 0 once it is installed (one-shot). habu2.f's source setup builds
\ the prefix and the user program (piped bytes, `s" f.f" required` rows, or the
\ baked LSRC) into one mapping; the cold-prefix routine publishes the prefix end
\ as INE and the mode publishes the buffer end here. "Source exhausted"
\ (EM-COMPILE-EXIT, LEX0) thus comes at the end of the engine prefix on every
\ boot, where the engine is complete and no user token has run: the AOT seed
\ runs there, then LEX0 installs [INE, USER-END) and re-enters the interpreter,
\ as EM-REPL-READ does for a typed line. Arming the seed without splitting the
\ streams fired it after the batch program it serves. DATA-INIT zeroes it.
$3CB0 constant USER-END
;package
