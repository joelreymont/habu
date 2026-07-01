\ warm-run.f - checked helpers for explicitly installed warm fixture subprocesses.
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

: WR-TOOLS? ( -- bool )
   WR-TOOLS-OVERRIDE? ;

: WR-TOOLS$ ( -- ptr u8 n )
   WR-TOOLS-OVERRIDE? if WR-TOOLS-BUF WR-TOOLS-U @ exit then
   s" bin/hb" ;

: WR-TOOLS-TRUST$ ( -- ptr u8 n )
   WR-TOOLS-OVERRIDE? if WR-TOOLS-TRUST-BUF WR-TOOLS-TRUST-U @ exit then
   E-FS-OPEN throw ;

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
   WR-CHECK-OVERRIDE? ;

: WR-CHECK$ ( -- ptr u8 n )
   WR-CHECK-OVERRIDE? if WR-CHECK-BUF WR-CHECK-U @ exit then
   s" bin/hb" ;

: WR-CHECK-TRUST$ ( -- ptr u8 n )
   WR-CHECK-OVERRIDE? if WR-CHECK-TRUST-BUF WR-CHECK-TRUST-U @ exit then
   E-FS-OPEN throw ;

: WR-CHECK-LOAD ( ptr u8 n -- bool ) {: entry:ptr entryu:n :}
   WR-CHECK? if
      s" --load" WR-ARG+
      WR-CHECK-TRUST$ WR-ARG+
      entry entryu WR-ARG+
      s" --" WR-ARG+
      WR-TRUE exit
   then
   WR-FALSE ;
