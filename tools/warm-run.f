\ warm-run.f - checked helpers for warm fixture subprocesses.
\
\ Load after lib/errors.f, lib/fs.f, and lib/process-argv.f.

: WR-TRUE ( -- bool )
   0 0= ;

: WR-FALSE ( -- bool )
   0 0= 0= ;

variable WR-TOOLS-U
variable WR-TOOLS-TRUST-U
variable WR-CHECK-U
variable WR-CHECK-TRUST-U

create WR-TOOLS-BUF FS-PATH-CAP allot
create WR-TOOLS-TRUST-BUF FS-PATH-CAP allot
create WR-CHECK-BUF FS-PATH-CAP allot
create WR-CHECK-TRUST-BUF FS-PATH-CAP allot

: WR-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u up ! ;

: WR-TOOLS! ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n ta:ptr tu:n :}
   exe exeu WR-TOOLS-BUF WR-TOOLS-U WR-COPY!
   ta tu WR-TOOLS-TRUST-BUF WR-TOOLS-TRUST-U WR-COPY! ;

: WR-CHECK! ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n ta:ptr tu:n :}
   exe exeu WR-CHECK-BUF WR-CHECK-U WR-COPY!
   ta tu WR-CHECK-TRUST-BUF WR-CHECK-TRUST-U WR-COPY! ;

: WR-TOOLS-OVERRIDE? ( -- bool )
   WR-TOOLS-U @ 0 > ;

: WR-CHECK-OVERRIDE? ( -- bool )
   WR-CHECK-U @ 0 > ;

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
   WR-TOOLS-OVERRIDE? if WR-TRUE exit then
   s" HABU_WARM_TOOLS" WR-EXE? ;

: WR-TOOLS$ ( -- ptr u8 n )
   WR-TOOLS-OVERRIDE? if WR-TOOLS-BUF WR-TOOLS-U @ exit then
   s" HABU_WARM_TOOLS" WR-EXE$ ;

: WR-TOOLS-TRUST$ ( -- ptr u8 n )
   WR-TOOLS-OVERRIDE? if WR-TOOLS-TRUST-BUF WR-TOOLS-TRUST-U @ exit then
   s" HABU_WARM_TOOLS_TRUST" WR-TRUST$ ;

: WR-TOOLS-LOAD ( ptr u8 n -- bool ) {: entry:ptr entryu:n :}
   WR-TOOLS? if
      s" --load" WR-ARG+
      WR-TOOLS-TRUST$ WR-ARG+
      entry entryu WR-ARG+
      s" --" WR-ARG+
      WR-TRUE exit
   then
   WR-FALSE ;

: WR-TOOLS-LOAD2 ( ptr u8 n ptr u8 n -- bool )
   {: first:ptr firstu:n second:ptr secondu:n :}
   WR-TOOLS? if
      s" --load" WR-ARG+
      WR-TOOLS-TRUST$ WR-ARG+
      first firstu WR-ARG+
      second secondu WR-ARG+
      s" --" WR-ARG+
      WR-TRUE exit
   then
   WR-FALSE ;

: WR-CHECK? ( -- bool )
   WR-CHECK-OVERRIDE? if WR-TRUE exit then
   s" HABU_WARM_CHECK" WR-EXE? ;

: WR-CHECK$ ( -- ptr u8 n )
   WR-CHECK-OVERRIDE? if WR-CHECK-BUF WR-CHECK-U @ exit then
   s" HABU_WARM_CHECK" WR-EXE$ ;

: WR-CHECK-TRUST$ ( -- ptr u8 n )
   WR-CHECK-OVERRIDE? if WR-CHECK-TRUST-BUF WR-CHECK-TRUST-U @ exit then
   s" HABU_WARM_CHECK_TRUST" WR-TRUST$ ;

: WR-CHECK-LOAD ( ptr u8 n -- bool ) {: entry:ptr entryu:n :}
   WR-CHECK? if
      s" --load" WR-ARG+
      WR-CHECK-TRUST$ WR-ARG+
      entry entryu WR-ARG+
      s" --" WR-ARG+
      WR-TRUE exit
   then
   WR-FALSE ;
