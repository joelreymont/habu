\ warm-run.f - checked helpers for warm fixture subprocesses.
\
\ Load after lib/errors.f, lib/fs.f, and lib/process-argv.f.

: WR-TRUE ( -- bool )
   0 0= ;

: WR-FALSE ( -- bool )
   0 0= 0= ;

: WR-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: WR-EXE? ( ptr u8 n -- bool )
   GETENV dup 0= if 2drop WR-FALSE exit then
   2dup EXECUTABLE? 0= if E-FS-OPEN throw then
   2drop WR-TRUE ;

: WR-EXE$ ( ptr u8 n -- ptr u8 n )
   GETENV dup 0= if 2drop s" bin/hb" exit then
   2dup EXECUTABLE? 0= if E-FS-OPEN throw then ;

: WR-TRUST$ ( ptr u8 n -- ptr u8 n )
   GETENV dup 0= if 2drop E-FS-OPEN throw then
   2dup FILE? 0= if E-FS-OPEN throw then ;

: WR-WARM-LOAD ( ptr u8 n ptr u8 n -- )
   {: tenv:ptr tenvu:n entry:ptr entryu:n :}
   s" --load" WR-ARG+
   tenv tenvu WR-TRUST$ WR-ARG+
   entry entryu WR-ARG+
   s" --" WR-ARG+ ;

: WR-WARM-LOAD2 ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: tenv:ptr tenvu:n first:ptr firstu:n second:ptr secondu:n :}
   s" --load" WR-ARG+
   tenv tenvu WR-TRUST$ WR-ARG+
   first firstu WR-ARG+
   second secondu WR-ARG+
   s" --" WR-ARG+ ;

: WR-TOOLS? ( -- bool )
   s" HABU_WARM_TOOLS" WR-EXE? ;

: WR-TOOLS$ ( -- ptr u8 n )
   s" HABU_WARM_TOOLS" WR-EXE$ ;

: WR-TOOLS-LOAD ( ptr u8 n -- bool ) {: entry:ptr entryu:n :}
   WR-TOOLS? if
      s" HABU_WARM_TOOLS_TRUST" entry entryu WR-WARM-LOAD
      WR-TRUE exit
   then
   WR-FALSE ;

: WR-TOOLS-LOAD2 ( ptr u8 n ptr u8 n -- bool )
   {: first:ptr firstu:n second:ptr secondu:n :}
   WR-TOOLS? if
      s" HABU_WARM_TOOLS_TRUST" first firstu second secondu WR-WARM-LOAD2
      WR-TRUE exit
   then
   WR-FALSE ;
