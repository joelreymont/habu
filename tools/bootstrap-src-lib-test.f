\ bootstrap-src-lib-test.f - fixtures for the tools/bootstrap.sh source reader.
\ Run: bin/hb --load tools/bootstrap-src-lib-test.f
\
\ The fixture is one synthetic script carrying every shape the reader must accept
\ and every shape it must refuse, driven straight through PARSE. Each decoy is a
\ path a CONTAINS? over the script text finds and no field role admits: a `#`
\ comment, a printf string, another redirect target, a cat with no redirect, a
\ `cat` outside emit_src, an indented block-opener look-alike, and a row guarded
\ for a driver other than the one being asked about.
\
\ The same script is parsed three ways - no driver, the stdin driver, another
\ driver - because the driver is the whole difference between the two consumers'
\ questions and a reader that ignored it would answer both the same.
\
\ Load after lib/test.f and tools/bootstrap-src-lib.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/bootstrap-src-lib.f

package BOOTSTRAP-SRC-TEST

private

$2000 constant TXT-CAP

create TXT TXT-CAP allot
variable TXT-U

: TXT-RESET ( -- )
   0 TXT-U ! ;

: L+ ( ptr u8 n -- ) {: a:ptr u:n :}          \ one script line
   TXT-U @ u + 1 + TXT-CAP > if E-TEST-CAPACITY throw then
   a TXT TXT-U @ + u LINT-BMOVE
   TXT-U @ u + TXT-U !
   $0A TXT TXT-U @ + c!
   TXT-U @ 1 + TXT-U ! ;

: TXT$ ( -- ptr u8 n )
   TXT TXT-U @ ;

\ The synthetic script. Every accepted shape appears once, and every decoy is
\ named `zz-<how it hides>` so a failure says which evasion got through.
: SCRIPT$ ( -- ptr u8 n )
   TXT-RESET
   s" #!/usr/bin/env bash" L+
   s" SRC_COMMON=(" L+
   s"   src/core/roles.f" L+
   S\"   \"$OS_TARGET\"" L+
   S\"   \"$OS_LAYOUT\"" L+
   S\"   \"$OS_SYS\"" L+
   s"   # src/core/zz-array-comment.f" L+
   s"   src/core/bytes.f" L+
   s" )" L+
   s" " L+
   s" emit_src() {" L+
   S\"   local out=\"$1\"" L+
   S\"   local driver=\"$2\"" L+
   S\"   cat src/core/util.f >> \"$out\"" L+
   S\"   # cat src/core/zz-comment.f >> \"$out\"" L+
   S\"   printf 'cat src/core/zz-printf.f >> \"$out\"' >> \"$out\"" L+
   S\"   cat src/core/zz-redirect.f >> \"$other\"" L+
   s"   cat src/core/zz-noredirect.f" L+
   s"   # SRC_COMMON=(" L+
   S\"   for f in \"${SRC_COMMON[@]}\"; do" L+
   S\"     cat \"$f\" >> \"$out\"" L+
   s"   done" L+
   S\"   if [[ \"$driver\" == \"src/habu/stdin.f\" ]]; then" L+
   S\"     cat src/habu/aot-arm.f >> \"$out\"" L+
   s"   fi" L+
   S\"   if [[ \"$driver\" == \"src/habu/snap.f\" ]]; then" L+
   S\"     cat src/core/zz-other-driver.f >> \"$out\"" L+
   s"   fi" L+
   S\"   if [[ -n \"$flag\" ]]; then" L+
   S\"     cat src/core/zz-unknown-cond.f >> \"$out\"" L+
   s"   fi" L+
   S\"   if [[ \"$driver\" == \"src/habu/never.f\" ]]; then" L+
   S\"     cat src/core/zz-then.f >> \"$out\"" L+
   s"   else" L+
   S\"     cat src/core/zz-else.f >> \"$out\"" L+
   s"   fi" L+
   S\"   cat \"$driver\" >> \"$out\"" L+
   s" }" L+
   s" " L+
   S\" cat src/core/zz-outside.f >> \"$out\"" L+
   S\" emit_src \"$T/stage2-src\" src/habu/stdin.f" L+
   TXT$ ;

: NO-DRIVER ( -- )
   SCRIPT$ s" " BOOTSTRAP-SRC:PARSE ;

: STDIN-DRIVER ( -- )
   SCRIPT$ s" src/habu/stdin.f" BOOTSTRAP-SRC:PARSE ;

: SNAP-DRIVER ( -- )
   SCRIPT$ s" src/habu/snap.f" BOOTSTRAP-SRC:PARSE ;

: NEVER-DRIVER ( -- )
   SCRIPT$ s" src/habu/never.f" BOOTSTRAP-SRC:PARSE ;

\ ---- the shapes that ARE rows ------------------------------------------------
: ACCEPTS ( -- )
   STDIN-DRIVER
   \ an unconditional cat row inside emit_src
   s" src/core/util.f"      BOOTSTRAP-SRC:HAS? TTRUE
   \ a plain array entry, and the array really reaches a source
   s" src/core/roles.f"     BOOTSTRAP-SRC:HAS? TTRUE
   s" src/core/bytes.f"     BOOTSTRAP-SRC:HAS? TTRUE
   BOOTSTRAP-SRC:ARRAY-USED? TTRUE
   \ the two target-selected entries normalise to their shared key
   BOOTSTRAP-SRC:OS-TARGET-KEY BOOTSTRAP-SRC:HAS? TTRUE
   BOOTSTRAP-SRC:OS-LAYOUT-KEY BOOTSTRAP-SRC:HAS? TTRUE
   \ a row guarded for THIS driver, and the driver emit_src cats last
   s" src/habu/aot-arm.f"   BOOTSTRAP-SRC:HAS? TTRUE
   s" src/habu/stdin.f"     BOOTSTRAP-SRC:HAS? TTRUE
   \ a call site really builds an emission for this driver
   BOOTSTRAP-SRC:DRIVER-CALLED? TTRUE ;

\ ---- the shapes that are NOT rows --------------------------------------------
\ Each of these paths is in the script text, so a CONTAINS? finds every one.
: DECOYS ( -- )
   STDIN-DRIVER
   s" src/core/zz-comment.f"       BOOTSTRAP-SRC:HAS? TFALSE  \ `#` leads the line
   s" src/core/zz-printf.f"        BOOTSTRAP-SRC:HAS? TFALSE  \ inside a printf string
   s" src/core/zz-redirect.f"      BOOTSTRAP-SRC:HAS? TFALSE  \ another redirect target
   s" src/core/zz-noredirect.f"    BOOTSTRAP-SRC:HAS? TFALSE  \ no redirect at all
   s" src/core/zz-outside.f"       BOOTSTRAP-SRC:HAS? TFALSE  \ outside emit_src
   s" src/core/zz-array-comment.f" BOOTSTRAP-SRC:HAS? TFALSE  \ commented array entry
   \ an $OS_* variable that keys no row on either side contributes none
   s" src/os/*/sys.f"              BOOTSTRAP-SRC:HAS? TFALSE ;

\ ---- the driver decides which guarded rows count ------------------------------
: GUARDS ( -- )
   STDIN-DRIVER
   \ a row guarded for ANOTHER driver is not in this emission
   s" src/core/zz-other-driver.f"  BOOTSTRAP-SRC:HAS? TFALSE
   \ a condition this reader does not model is NOT taken
   s" src/core/zz-unknown-cond.f"  BOOTSTRAP-SRC:HAS? TFALSE
   \ neither half of an if/else is taken when the guard names another driver
   s" src/core/zz-then.f"          BOOTSTRAP-SRC:HAS? TFALSE
   s" src/core/zz-else.f"          BOOTSTRAP-SRC:HAS? TFALSE
   \ ask for the other driver and its guarded row IS in the emission, while the
   \ stdin one is not: the guard is read, not ignored
   SNAP-DRIVER
   s" src/core/zz-other-driver.f"  BOOTSTRAP-SRC:HAS? TTRUE
   s" src/habu/aot-arm.f"          BOOTSTRAP-SRC:HAS? TFALSE
   s" src/habu/snap.f"             BOOTSTRAP-SRC:HAS? TTRUE
   \ no call site builds an emission for it, though
   BOOTSTRAP-SRC:DRIVER-CALLED? TFALSE
   \ the taken half of an if/else is in; the else half never is
   NEVER-DRIVER
   s" src/core/zz-then.f"          BOOTSTRAP-SRC:HAS? TTRUE
   s" src/core/zz-else.f"          BOOTSTRAP-SRC:HAS? TFALSE ;

\ ---- no driver is the driver-independent question -----------------------------
\ This is the set bootstrap-mirror-lint asks for: what EVERY emission compiles.
: DRIVERLESS ( -- )
   NO-DRIVER
   \ unconditional rows and array entries are still in
   s" src/core/util.f"      BOOTSTRAP-SRC:HAS? TTRUE
   s" src/core/bytes.f"     BOOTSTRAP-SRC:HAS? TTRUE
   BOOTSTRAP-SRC:ARRAY-USED? TTRUE
   \ every guarded row is out, and so is the driver itself
   s" src/habu/aot-arm.f"   BOOTSTRAP-SRC:HAS? TFALSE
   s" src/core/zz-other-driver.f" BOOTSTRAP-SRC:HAS? TFALSE
   s" src/core/zz-then.f"   BOOTSTRAP-SRC:HAS? TFALSE
   s" src/habu/stdin.f"     BOOTSTRAP-SRC:HAS? TFALSE
   BOOTSTRAP-SRC:DRIVER-CALLED? TFALSE
   \ and the driverless set is strictly smaller than the stdin one
   BOOTSTRAP-SRC:ROWS {: bare:n :}
   STDIN-DRIVER
   BOOTSTRAP-SRC:ROWS bare > TTRUE ;

\ ---- the real script -----------------------------------------------------------
\ The fixtures above are synthetic, so one leg reads the file the consumers do:
\ the stdin emission must carry every host file of the closure, and the
\ driverless one must not carry the four the stdin conditional guards.
: REAL-SCRIPT ( -- )
   s" tools/bootstrap.sh" s" src/habu/stdin.f" BOOTSTRAP-SRC:LOAD
   s" tools/bootstrap.sh" BOOTSTRAP-SRC:SCRIPT$ T$=
   s" src/habu/aot-decl.f"    BOOTSTRAP-SRC:HAS? TTRUE
   s" src/habu/aot-ident.f"   BOOTSTRAP-SRC:HAS? TTRUE
   s" src/habu/aot-arm.f"     BOOTSTRAP-SRC:HAS? TTRUE
   s" src/habu/aot-capture.f" BOOTSTRAP-SRC:HAS? TTRUE
   s" src/habu/aot-file.f"    BOOTSTRAP-SRC:HAS? TTRUE
   s" src/habu/stdin.f"       BOOTSTRAP-SRC:HAS? TTRUE
   BOOTSTRAP-SRC:DRIVER-CALLED? TTRUE
   s" tools/bootstrap.sh" s" " BOOTSTRAP-SRC:LOAD
   s" src/habu/aot-decl.f"    BOOTSTRAP-SRC:HAS? TTRUE
   s" src/habu/aot-arm.f"     BOOTSTRAP-SRC:HAS? TFALSE
   s" src/habu/aot-capture.f" BOOTSTRAP-SRC:HAS? TFALSE
   s" src/habu/aot-file.f"    BOOTSTRAP-SRC:HAS? TFALSE
   s" src/habu/stdin.f"       BOOTSTRAP-SRC:HAS? TFALSE ;

: MAIN ( -- )
   T-RESET
   ACCEPTS
   DECOYS
   GUARDS
   DRIVERLESS
   REAL-SCRIPT
   T-REPORT ;

public

EXPORT MAIN

;package

BOOTSTRAP-SRC-TEST:MAIN
