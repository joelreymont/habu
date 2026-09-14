\ The application and linker run in one native executable-build scope.
require src/habu/app-image.f
require src/os/script-argv.f
require src/habu/maker-source.f
require src/habu/aot-decl.f
require src/habu/aot-closure.f
require src/habu/aot-lib.f

package AOT-LINK
private

: NATIVE-NIBBLE ( n -- n ) {: c:n :}
   c $30 >= c $39 <= and if c $30 - exit then
   c $61 >= c $66 <= and if c $61 - 10 + exit then
   c $41 >= c $46 <= and if c $41 - 10 + exit then
   s" aot: bad preseed hex digit" 74 die ;


: NATIVE-SEED-CELL ( ptr u8 -- n ) {: a:ptr :}
   0
   16 0 ?do
      4 lshift a i + c@ NATIVE-NIBBLE or
   loop ;


: NATIVE-SEED ( ptr u8 n -- ) {: a:ptr u:n :}
   u 16 mod 0<> if s" aot: preseed hex not cell-aligned" 74 die then
   SEED-RESET
   u 16 / 0 ?do
      a i 16 * + NATIVE-SEED-CELL SEED+
   loop ;


: NATIVE-OPTIONS ( -- )
   SCRIPT-ARGC 2 <> SCRIPT-ARGC 4 <> and if
      s" aot-build: source, JSON flag and optional entry/seed are required" 74 die
   then
   1 SCRIPT-ARGV$ s" 1" STR= if -1 else 0 then JSON-DIAGS !
   SCRIPT-ARGC 4 = if
      2 SCRIPT-ARGV$ ENTRY-NAME!
      3 SCRIPT-ARGV$ NATIVE-SEED
   then ;


: NATIVE-BUILD ( -- )
   NATIVE-OPTIONS
   AOT-DATA-START
   0 SCRIPT-ARGV$ script-required
   LINK ;

public

\ Invoke after this file's include has returned, with package scope closed.
: BUILD-NATIVE ( -- )
   ['] NATIVE-BUILD EXECUTABLE-BUILD:WITH ;

;package
