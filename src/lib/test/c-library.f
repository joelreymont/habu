split-str
\ c-library.f - a C source file a test writes, compiled to a shared library by
\ the system compiler. cc inherits this process's environment: its driver finds
\ ld through PATH. STORAGE CLASS. PROCESS-WIDE: BUILD fills the one argv and
\ environment table process-env.f keeps.

require src/lib/string.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test/assert.f
require src/core/exit-status.f

package C-LIBRARY

private

: FLAGS ( -- )
   s" -shared" >LEN PROC-ARGV+
   s" -fPIC" >LEN PROC-ARGV+
   s" -O2" >LEN PROC-ARGV+
   s" -std=gnu11" >LEN PROC-ARGV+
   s" -Wall" >LEN PROC-ARGV+
   s" -Werror" >LEN PROC-ARGV+ ;

: FAIL ( str -- )
   {: src:^ srcu :}
   SB-RESET s" c-library: cc failed on " SB-APPEND
   src srcu SB-APPEND
   SB$ EXIT-STATUS:FAILURE die ;

: RUN ( -- result<n,n> )
   PROC-ENV-INHERIT-MISSING
   s" /usr/bin/cc" >LEN -1 >FD -1 >FD -1 >FD
   PROC-RUN-ARGV-ENV-IO-RC ;

public

\ Compile the source at the first path to the library at the second.
: BUILD ( str str -- )
   {: src:^ srcu lib:^ libu :}
   PROC-ARGV-ENV-RESET FLAGS
   s" -o" >LEN PROC-ARGV+
   lib libu >LEN PROC-ARGV+
   src srcu >LEN PROC-ARGV+
   RUN MATCH result
      ok OF drop ENDOF
      err OF drop src srcu FAIL ENDOF
   ;MATCH ;

;package
