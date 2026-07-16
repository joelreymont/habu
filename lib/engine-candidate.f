\ engine-candidate.f - validated engine for nested candidate-sensitive work.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package ENGINE-CANDIDATE

private

: HOST$? ( -- ptr u8 n bool )
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" " 0 0= 0=
   else
      0 0=
   then ;

: RAW-OVERRIDE$? ( -- ptr u8 n bool )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if
      LEN>N 0 0=
   else
      2drop HOST$?
   then ;

: RAW$ ( -- ptr u8 n )
   RAW-OVERRIDE$? if
   else
      2drop s" bin/hb"
   then ;

public

: VALIDATE$ ( ptr u8 n -- ptr u8 n )
   2dup EXECUTABLE? 0= if E-FS-OPEN throw then ;

: OVERRIDE$? ( -- ptr u8 n bool )
   RAW-OVERRIDE$? {: a:ptr u:n found:bool :}
   found if
      a u VALIDATE$ 0 0=
   else
      s" " 0 0= 0=
   then ;

: PATH$ ( -- ptr u8 n )
   RAW$ VALIDATE$ ;

;package
