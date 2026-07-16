\ diff-capture-test-support.f - real-jj bulk-content provider.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require src/core/sha256.f
require tools/diff-capture-core.f

package DIFF-META
public

: TEST-OLD-KIND ( n -- n )
   R-OLD-KIND REC@ ;

: TEST-NEW-KIND ( n -- n )
   R-NEW-KIND REC@ ;

: TEST-ABSENT-KIND ( -- n )
   KIND-ABSENT ;

: TEST-FILE-KIND ( -- n )
   KIND-FILE ;

: TEST-GITLINK-KIND ( -- n )
   KIND-GITLINK ;

;package

package DIFF-CMD
private

8000 constant TEST-PEEK-CAP
create TEST-PEEK-BUF TEST-PEEK-CAP allot
create TEST-OLD-DIGEST $20 allot
create TEST-NEW-DIGEST $20 allot
variable TEST-OLD-SIZE
variable TEST-NEW-SIZE
variable TEST-OLD-BINARY
variable TEST-NEW-BINARY
variable TEST-PEEK-FD
variable TEST-PEEK-BINARY
PTR-VARIABLE TEST-PEEK-PATH-A
variable TEST-PEEK-PATH-U

: HEX-C ( n -- n )
   dup 10 < if $30 + else 10 - $61 + then ;

: FILESET$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   JW-RESET
   S\" root-file:\q" JW-RAW
   0 begin dup u < while
      dup a + c@ {: c:n :}
      $5C JW-C
      $78 JW-C
      c 4 rshift HEX-C JW-C
      c $F and HEX-C JW-C
      1+
   repeat drop
   $22 JW-C
   JW$ ;

: CONTENT-ARGS ( ptr u8 n ptr u8 n -- )
   {: rev:ptr revu:n path:ptr pathu:n :}
   PINNED-ARGS
   s" file" ARG s" show" ARG
   s" -r" ARG rev revu ARG
   s" --" ARG path pathu FILESET$ ARG ;

: PEEK-BODY ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   path pathu FS-PATHZ 0 0 open dup TEST-PEEK-FD !
   dup 0 < if drop E-FS-OPEN throw then
   TEST-PEEK-BUF TEST-PEEK-CAP read {: got:n :}
   got 0 < if E-FS-IO throw then
   0 begin dup got < while
      dup TEST-PEEK-BUF + c@ 0= if drop true exit then
      1+
   repeat drop
   false ;

: PEEK-RUN ( -- )
   TEST-PEEK-PATH-A @ TEST-PEEK-PATH-U @ PEEK-BODY TEST-PEEK-BINARY ! ;

: FILE-BINARY? ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   path TEST-PEEK-PATH-A ! pathu TEST-PEEK-PATH-U !
   -1 TEST-PEEK-FD !
   false TEST-PEEK-BINARY !
   [: PEEK-RUN ;] catch {: code:n :}
   TEST-PEEK-FD @ dup 0 >= if close else drop then
   code 0<> if code throw then
   TEST-PEEK-BINARY @ if true else false then ;

: EMPTY-CONTENT ( ptr u8 ptr n ptr n -- )
   {: digest:ptr sizep:ptr binaryp:ptr :}
   s" " digest SHA256
   0 sizep !
   0 binaryp ! ;

: FILE-CONTENT ( DIFF-CAPTURE:command-phase ptr u8 n ptr u8 n n ptr u8 ptr n ptr n -- )
   {: phase:DIFF-CAPTURE:command-phase rev:ptr revu:n path:ptr pathu:n kind:n
      digest:ptr sizep:ptr binaryp:ptr :}
   kind DIFF-META:TEST-ABSENT-KIND = kind DIFF-META:TEST-GITLINK-KIND = or if
      digest sizep binaryp EMPTY-CONTENT
      exit
   then
   rev revu path pathu CONTENT-ARGS
   phase OUT-PATH OUT-PATH-U @ RUN-JJ
   OUT-PATH OUT-PATH-U @ FILE-SIZE sizep !
   OUT-PATH OUT-PATH-U @ digest SHA256-FILE dup 0<> if throw then drop
   kind DIFF-META:TEST-FILE-KIND = if
      OUT-PATH OUT-PATH-U @ FILE-BINARY?
   else
      false
   then
   if 1 else 0 then binaryp ! ;

: CONTENT-ROW ( n -- ) {: row:n :}
   DIFF--CAPTURE-COMMAND--PHASE:OLD-CONTENT
   FROM-A @ FROM-U @ row DIFF-META:OLD$ row DIFF-META:TEST-OLD-KIND
   TEST-OLD-DIGEST TEST-OLD-SIZE TEST-OLD-BINARY FILE-CONTENT
   DIFF--CAPTURE-COMMAND--PHASE:NEW-CONTENT
   TO-A @ TO-U @ row DIFF-META:NEW$ row DIFF-META:TEST-NEW-KIND
   TEST-NEW-DIGEST TEST-NEW-SIZE TEST-NEW-BINARY FILE-CONTENT
   row
   TEST-OLD-SIZE @ TEST-OLD-BINARY @ 0<> TEST-OLD-DIGEST $20
   TEST-NEW-SIZE @ TEST-NEW-BINARY @ 0<> TEST-NEW-DIGEST $20
   DIFF-CONTENT:CONTENT-ROW! ;

: CONTENT-PROVIDER ( -- )
   0 begin dup DIFF-META:COUNT < while
      dup CONTENT-ROW
      1+
   repeat drop ;

public

: TEST-CONTENT-ON ( -- )
   [: CONTENT-PROVIDER ;] DIFF-CONTENT:CONTENT-PROVIDER! ;

;package
