\ process-command.f - checked command-owned process runner.
\
\ Load after lib/errors.f, lib/fs.f, lib/process.f, lib/process-argv.f,
\ and lib/process-env.f.

PROC-ARGV-MAX 1- constant PROC-CMD-ARG-MAX
PROC-ARGV-BUF-CAP constant PROC-CMD-ARG-BUF-CAP
PROC-ENV-MAX constant PROC-CMD-ENV-MAX
PROC-ENV-BUF-CAP constant PROC-CMD-ENV-BUF-CAP
131072 constant PROC-CMD-IN-CAP
32768 constant PROC-CMD-OUT-CAP
32768 constant PROC-CMD-ERR-CAP

create PROC-CMD-ARG-TABLE PROC-CMD-ARG-MAX cells allot
create PROC-CMD-ARG-BUF PROC-CMD-ARG-BUF-CAP allot
create PROC-CMD-ENV-TABLE PROC-CMD-ENV-MAX cells allot
create PROC-CMD-ENV-BUF PROC-CMD-ENV-BUF-CAP allot
create PROC-CMD-IN PROC-CMD-IN-CAP allot
create PROC-CMD-OUT PROC-CMD-OUT-CAP allot
create PROC-CMD-ERR PROC-CMD-ERR-CAP allot

variable PROC-CMD-ARG-N
variable PROC-CMD-ARG-OFF
variable PROC-CMD-ENV-N
variable PROC-CMD-ENV-OFF
variable PROC-CMD-IN-LEN
variable PROC-CMD-OUT-LEN
variable PROC-CMD-ERR-LEN
variable PROC-CMD-OUTCOME-KIND
variable PROC-CMD-OUTCOME-CODE
variable PROC-CMD-RC
variable PROC-CMD-INHERIT

: PROC-CMD-CAPTURE-RESET ( -- )
   0 >LEN PROC-CMD-OUT-LEN !
   0 >LEN PROC-CMD-ERR-LEN !
   PROC-OUTCOME-EXIT PROC-CMD-OUTCOME-KIND !
   0 PROC-CMD-OUTCOME-CODE !
   0 >RC PROC-CMD-RC ! ;

: PROC-CMD-RESET ( -- )
   0 >COUNT PROC-CMD-ARG-N !
   0 >OFF PROC-CMD-ARG-OFF !
   0 >COUNT PROC-CMD-ENV-N !
   0 >OFF PROC-CMD-ENV-OFF !
   0 >LEN PROC-CMD-IN-LEN !
   1 PROC-CMD-INHERIT !
   PROC-CMD-CAPTURE-RESET
   PROC-ARGV-RESET
   PROC-ENV-RESET ;

: PROC-CMD-ARG-SLOT ( idx -- ptr a ) {: idx :}
   idx IDX>N 0 < if E-PROC-OUTPUT throw then
   idx IDX>N PROC-CMD-ARG-MAX >= if E-PROC-OUTPUT throw then
   idx IDX>N cells PROC-CMD-ARG-TABLE + ;

: PROC-CMD-CHECK-ARG-EXTRA ( -- )
   PROC-CMD-ARG-N @ COUNT>N PROC-CMD-ARG-MAX >= if E-PROC-OUTPUT throw then ;

: PROC-CMD-ARG-ZCOPY ( ptr u8 len -- ptr u8 ) {: a:ptr u :}
   u LEN>N 0 < if E-PROC-OUTPUT throw then
   PROC-CMD-ARG-OFF @ {: off :}
   off OFF>N u LEN>N 1 + + PROC-CMD-ARG-BUF-CAP > if E-PROC-OUTPUT throw then
   a u PROC-CMD-ARG-BUF off OFF>N + PROC-CMD-ARG-BUF-CAP off OFF>N - >LEN
   PROC-ZCOPY {: z:ptr :}
   off OFF>N u LEN>N 1 + + >OFF PROC-CMD-ARG-OFF !
   z ;

: PROC-CMD-ARG-INSTALL-Z ( ptr u8 -- )
   PROC-CMD-ARG-N @ COUNT>N >IDX PROC-CMD-ARG-SLOT !
   PROC-CMD-ARG-N @ COUNT>N 1+ >COUNT PROC-CMD-ARG-N ! ;

: PROC-CMD-ARG+ ( ptr u8 len -- ) {: a:ptr u :}
   PROC-CMD-CHECK-ARG-EXTRA
   a u PROC-CMD-ARG-ZCOPY PROC-CMD-ARG-INSTALL-Z ;

: PROC-CMD-ENV-SLOT ( idx -- ptr a ) {: idx :}
   idx IDX>N 0 < if E-PROC-ENV throw then
   idx IDX>N PROC-CMD-ENV-MAX >= if E-PROC-ENV throw then
   idx IDX>N cells PROC-CMD-ENV-TABLE + ;

: PROC-CMD-CHECK-ENV-EXTRA ( -- )
   PROC-CMD-ENV-N @ COUNT>N PROC-CMD-ENV-MAX >= if E-PROC-ENV throw then ;

: PROC-CMD-ENV-STORE-Z ( ptr u8 len -- ptr u8 ) {: a:ptr u :}
   u LEN>N 0 < if E-PROC-ENV throw then
   PROC-CMD-ENV-OFF @ {: off :}
   off OFF>N u LEN>N 1 + + PROC-CMD-ENV-BUF-CAP > if E-PROC-ENV throw then
   a PROC-CMD-ENV-BUF off OFF>N + u LEN>N BYTE-COPY
   0 PROC-CMD-ENV-BUF off OFF>N + u LEN>N + c!
   off OFF>N u LEN>N 1 + + >OFF PROC-CMD-ENV-OFF !
   PROC-CMD-ENV-BUF off OFF>N + ;

: PROC-CMD-ENV-INSTALL-Z ( ptr u8 -- )
   PROC-CMD-ENV-N @ COUNT>N >IDX PROC-CMD-ENV-SLOT !
   PROC-CMD-ENV-N @ COUNT>N 1+ >COUNT PROC-CMD-ENV-N ! ;

: PROC-CMD-ENV-ENTRY+ ( ptr u8 len -- ) {: a:ptr u :}
   a u PROC-ENV-CHECK-ENTRY
   PROC-CMD-CHECK-ENV-EXTRA
   a u PROC-CMD-ENV-STORE-Z PROC-CMD-ENV-INSTALL-Z ;

: PROC-CMD-ENV+ ( ptr u8 len ptr u8 len -- ) {: name:ptr nameu val:ptr valu :}
   name nameu PROC-ENV-CHECK-NAME
   valu LEN>N 0 < if E-PROC-ENV throw then
   PROC-CMD-CHECK-ENV-EXTRA
   PROC-CMD-ENV-OFF @ {: off :}
   off OFF>N nameu LEN>N valu LEN>N + 2 + + PROC-CMD-ENV-BUF-CAP > if
      E-PROC-ENV throw
   then
   name PROC-CMD-ENV-BUF off OFF>N + nameu LEN>N BYTE-COPY
   PROC-ENV-EQUAL PROC-CMD-ENV-BUF off OFF>N + nameu LEN>N + c!
   val PROC-CMD-ENV-BUF off OFF>N + nameu LEN>N + 1 + valu LEN>N BYTE-COPY
   0 PROC-CMD-ENV-BUF off OFF>N + nameu LEN>N + 1 + valu LEN>N + c!
   PROC-CMD-ENV-BUF off OFF>N + PROC-CMD-ENV-INSTALL-Z
   off OFF>N nameu LEN>N valu LEN>N + 2 + + >OFF PROC-CMD-ENV-OFF ! ;

: PROC-CMD-ENV-INHERIT ( -- )
   1 PROC-CMD-INHERIT ! ;

: PROC-CMD-ENV-HERMETIC ( -- )
   0 PROC-CMD-INHERIT ! ;

: PROC-CMD-IN-RESET ( -- )
   0 >LEN PROC-CMD-IN-LEN ! ;

: PROC-CMD-IN! ( ptr u8 len -- ) {: a:ptr u :}
   u LEN>N 0 < if E-PROC-OUTPUT throw then
   u LEN>N PROC-CMD-IN-CAP > if E-PROC-OUTPUT throw then
   a PROC-CMD-IN u LEN>N BYTE-COPY
   u PROC-CMD-IN-LEN ! ;

: PROC-CMD-LOAD-ARG ( idx -- ) {: idx :}
   idx PROC-CMD-ARG-SLOT @ {: z:ptr :}
   z z ZLEN >LEN PROC-ARGV+ ;

: PROC-CMD-LOAD-ARGS ( -- )
   0 begin dup PROC-CMD-ARG-N @ COUNT>N < while
      dup >IDX PROC-CMD-LOAD-ARG
      1+
   repeat drop ;

: PROC-CMD-LOAD-ENV ( idx -- ) {: idx :}
   idx PROC-CMD-ENV-SLOT @ {: z:ptr :}
   z z ZLEN >LEN PROC-ENV-ENTRY+ ;

: PROC-CMD-LOAD-ENVS ( -- )
   0 begin dup PROC-CMD-ENV-N @ COUNT>N < while
      dup >IDX PROC-CMD-LOAD-ENV
      1+
   repeat drop ;

: PROC-CMD-PREPARE ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-CMD-LOAD-ARGS
   PROC-CMD-LOAD-ENVS
   PROC-CMD-INHERIT @ 0 <> if PROC-ENV-INHERIT-MISSING then ;

: PROC-CMD-CHECK-RUN ( ptr u8 len ms -- ) {: path:ptr pathu timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   timeout MS>N 0 < if E-PROC-TIMEOUT throw then ;

: PROC-CMD-STORE-RUN ( len len n n -- ) {: outu erru kind code :}
   outu PROC-CMD-OUT-LEN !
   erru PROC-CMD-ERR-LEN !
   kind PROC-CMD-OUTCOME-KIND !
   code PROC-CMD-OUTCOME-CODE !
   kind code PROC-OUTCOME>RC PROC-CMD-RC ! ;

: PROC-CMD-RUN-OUTCOME ( ptr u8 len ms -- n n ) {: path:ptr pathu timeout :}
   path pathu timeout PROC-CMD-CHECK-RUN
   PROC-CMD-CAPTURE-RESET
   PROC-CMD-PREPARE
   path pathu PROC-CMD-IN PROC-CMD-IN-LEN @
   PROC-CMD-OUT PROC-CMD-OUT-CAP >LEN
   PROC-CMD-ERR PROC-CMD-ERR-CAP >LEN timeout
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME {: outu erru kind code :}
   outu erru kind code PROC-CMD-STORE-RUN
   kind code ;

: PROC-CMD-RUN-RC ( ptr u8 len ms -- rc )
   PROC-CMD-RUN-OUTCOME 2drop PROC-CMD-RC @ ;

: PROC-CMD-OUT$ ( -- ptr u8 n )
   PROC-CMD-OUT PROC-CMD-OUT-LEN @ LEN>N ;

: PROC-CMD-ERR$ ( -- ptr u8 n )
   PROC-CMD-ERR PROC-CMD-ERR-LEN @ LEN>N ;

: PROC-CMD-OUTCOME@ ( -- n n )
   PROC-CMD-OUTCOME-KIND @ PROC-CMD-OUTCOME-CODE @ ;

: PROC-CMD-RC@ ( -- rc )
   PROC-CMD-RC @ ;
