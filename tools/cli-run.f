\ cli-run.f - checked helpers for explicitly installed CLI fixture subprocesses.
\
\ Load after lib/errors.f, lib/fs.f, and lib/process-argv.f.

: CLI-TRUE ( -- bool )
   0 0= ;

: CLI-FALSE ( -- bool )
   0 0= 0= ;

variable CLI-TOOLS-U
variable CLI-TOOLS-TRUST-U
variable CLI-CHECK-U
variable CLI-CHECK-TRUST-U

create CLI-TOOLS-BUF FS-PATH-CAP allot
create CLI-TOOLS-TRUST-BUF FS-PATH-CAP allot
create CLI-CHECK-BUF FS-PATH-CAP allot
create CLI-CHECK-TRUST-BUF FS-PATH-CAP allot

: CLI-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u up ! ;

: CLI-TOOLS! ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n ta:ptr tu:n :}
   exe exeu CLI-TOOLS-BUF CLI-TOOLS-U CLI-COPY!
   ta tu CLI-TOOLS-TRUST-BUF CLI-TOOLS-TRUST-U CLI-COPY! ;

: CLI-CHECK! ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n ta:ptr tu:n :}
   exe exeu CLI-CHECK-BUF CLI-CHECK-U CLI-COPY!
   ta tu CLI-CHECK-TRUST-BUF CLI-CHECK-TRUST-U CLI-COPY! ;

: CLI-TOOLS-OVERRIDE? ( -- bool )
   CLI-TOOLS-U @ 0 > ;

: CLI-CHECK-OVERRIDE? ( -- bool )
   CLI-CHECK-U @ 0 > ;

: CLI-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: CLI-TOOLS? ( -- bool )
   CLI-TOOLS-OVERRIDE? ;

: CLI-TOOLS$ ( -- ptr u8 n )
   CLI-TOOLS-OVERRIDE? if CLI-TOOLS-BUF CLI-TOOLS-U @ exit then
   s" bin/hb" ;

: CLI-TOOLS-TRUST$ ( -- ptr u8 n )
   CLI-TOOLS-OVERRIDE? if CLI-TOOLS-TRUST-BUF CLI-TOOLS-TRUST-U @ exit then
   E-FS-OPEN throw ;

: CLI-TOOLS-LOAD ( ptr u8 n -- bool ) {: entry:ptr entryu:n :}
   CLI-TOOLS? if
      s" --load" CLI-ARG+
      CLI-TOOLS-TRUST$ CLI-ARG+
      entry entryu CLI-ARG+
      s" --" CLI-ARG+
      CLI-TRUE exit
   then
   CLI-FALSE ;

: CLI-TOOLS-LOAD2 ( ptr u8 n ptr u8 n -- bool )
   {: first:ptr firstu:n second:ptr secondu:n :}
   CLI-TOOLS? if
      s" --load" CLI-ARG+
      CLI-TOOLS-TRUST$ CLI-ARG+
      first firstu CLI-ARG+
      second secondu CLI-ARG+
      s" --" CLI-ARG+
      CLI-TRUE exit
   then
   CLI-FALSE ;

: CLI-CHECK? ( -- bool )
   CLI-CHECK-OVERRIDE? ;

: CLI-CHECK$ ( -- ptr u8 n )
   CLI-CHECK-OVERRIDE? if CLI-CHECK-BUF CLI-CHECK-U @ exit then
   s" bin/hb" ;

: CLI-CHECK-TRUST$ ( -- ptr u8 n )
   CLI-CHECK-OVERRIDE? if CLI-CHECK-TRUST-BUF CLI-CHECK-TRUST-U @ exit then
   E-FS-OPEN throw ;

: CLI-CHECK-LOAD ( ptr u8 n -- bool ) {: entry:ptr entryu:n :}
   CLI-CHECK? if
      s" --load" CLI-ARG+
      CLI-CHECK-TRUST$ CLI-ARG+
      entry entryu CLI-ARG+
      s" --" CLI-ARG+
      CLI-TRUE exit
   then
   CLI-FALSE ;
