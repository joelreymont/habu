\ diff-capture-frame.f - validated raw-diff framing.

require lib/errors.f
require lib/prelude.f
require lib/memory.f
require tools/lint/diff-frame.f
require tools/lint/diff-frame-write.f
require tools/diff-capture-metadata.f

package DIFF-FRAME
private

PTR-VARIABLE RAW-A
variable RAW-U
variable RAW-CUR
PTR-VARIABLE FROM-A
variable FROM-U
PTR-VARIABLE TO-A
variable TO-U
PTR-VARIABLE FRAME-A
variable FRAME-U
variable FRAME-CAP

: BYTE>STATUS ( n -- DIFF:status )
   case
      0 of DIFF-STATUS:MODIFIED endof
      1 of DIFF-STATUS:ADDED endof
      2 of DIFF-STATUS:REMOVED endof
      3 of DIFF-STATUS:RENAMED endof
      4 of DIFF-STATUS:COPIED endof
      E-DIFF-SYNTAX throw
   endcase ;

: FORM>BYTE ( DIFF:form -- n )
   MATCH DIFF:form
      text    OF 0 ENDOF
      binary  OF 1 ENDOF
      mode    OF 2 ENDOF
      empty   OF 3 ENDOF
      pure    OF 4 ENDOF
      gitlink OF 5 ENDOF
   ;MATCH ;

: NEXT-PATHS ( n bool -- bool ptr u8 n bool ptr u8 n )
   {: row:n next?:bool :}
   next? if
      row 1+ DIFF-META:OLD? row 1+ DIFF-META:OLD$
      row 1+ DIFF-META:NEW? row 1+ DIFF-META:NEW$
      exit
   then
   false s" " false s" " ;

: SCAN-ROW ( n bool -- ) {: row:n next?:bool :}
   row DIFF-META:STATUS BYTE>STATUS
   row DIFF-META:OLD? row DIFF-META:OLD$
   row DIFF-META:NEW? row DIFF-META:NEW$
   next?
   row next? NEXT-PATHS
   RAW-A @ RAW-CUR @ + RAW-U @ RAW-CUR @ -
   DIFF:SCAN-SECTION {: used:n shape:DIFF:form body:bool mode:bool :}
   row RAW-CUR @ used DIFF-META:RAW-RANGE!
   shape FORM>BYTE row DIFF-META:FORM <> if E-DIFF-SYNTAX throw then
   body if 1 else 0 then row DIFF-META:BODY <> if E-DIFF-SYNTAX throw then
   mode if 1 else 0 then row DIFF-META:MODE <> if E-DIFF-SYNTAX throw then
   RAW-CUR @ used + RAW-CUR @ < if E-DIFF-SYNTAX throw then
   RAW-CUR @ used + RAW-CUR ! ;

: SPLIT-ROWS ( -- )
   0 RAW-CUR !
   0 begin dup DIFF-META:COUNT < while
      dup dup DIFF-META:COUNT 1- < SCAN-ROW
      1+
   repeat drop
   DIFF-META:COUNT 0= if
      RAW-U @ 0<> if E-DIFF-SYNTAX throw then
   else
      RAW-CUR @ RAW-U @ <> if E-DIFF-SYNTAX throw then
   then ;

: FRAME-SIZE ( -- n )
   FROM-U @ TO-U @ DIFF-WRITE:HEADER-SIZE FRAME-CAP !
   0 begin dup DIFF-META:COUNT < while
      dup {: row:n :}
      FRAME-CAP @
      row DIFF-META:OLD$ nip
      row DIFF-META:NEW$ nip
      row DIFF-META:RAW-U
      DIFF-WRITE:SECTION-SIZE FRAME-CAP !
      1+
   repeat drop
   FRAME-CAP @ DIFF-WRITE:FINISH-SIZE ;

: BYTE>FORM ( n -- DIFF:form )
   case
      0 of DIFF-FORM:TEXT endof
      1 of DIFF-FORM:BINARY endof
      2 of DIFF-FORM:MODE endof
      3 of DIFF-FORM:EMPTY endof
      4 of DIFF-FORM:PURE endof
      5 of DIFF-FORM:GITLINK endof
      E-DIFF-SYNTAX throw
   endcase ;

: EMIT-ROW ( n -- ) {: row:n :}
   row DIFF-META:STATUS BYTE>STATUS
   row DIFF-META:FORM BYTE>FORM
   row DIFF-META:BODY?
   row DIFF-META:MODE?
   row DIFF-META:OLD? row DIFF-META:OLD$
   row DIFF-META:NEW? row DIFF-META:NEW$
   RAW-A @ row DIFF-META:RAW-OFF + row DIFF-META:RAW-U
   DIFF-WRITE:SECTION ;

: BUILD-FRAME ( -- )
   FRAME-SIZE {: cap:n :}
   cap MEM-ALLOC-BYTES drop FRAME-A !
   FRAME-A @ cap FROM-A @ FROM-U @ TO-A @ TO-U @ DIFF-WRITE:START
   0 begin dup DIFF-META:COUNT < while
      dup EMIT-ROW
      1+
   repeat drop
   DIFF-WRITE:FINISH FRAME-U ! drop
   FRAME-U @ cap <> if E-DIFF-FRAME-CAP throw then ;

public

: BUILD ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: raw:ptr rawu:n from:ptr fromu:n to:ptr tou:n :}
   raw RAW-A ! rawu RAW-U !
   from FROM-A ! fromu FROM-U !
   to TO-A ! tou TO-U !
   SPLIT-ROWS
   BUILD-FRAME
   FRAME-A @ FRAME-U @ ;

;package
