\ gpt2-serve.f - persistent framed GPT-2 service.

require lib/adt/option.f
require lib/adt/result.f
require lib/cad-num-arithmetic.f
require lib/fs.f
require lib/fs-path.f
require lib/process.f
require maki/infer/gpt2-generate.f

package GPT2-SERVE

public

-5668 constant E-FRAME

private

64 constant E-USAGE
4 constant U32-N
8 constant I64-N
1 constant TAG-N
0 constant TAG-OK
1 constant TAG-ERR
U32-N TAG-N + I64-N + constant REFUSAL-N

CAST: BL>N ( CAD-NUM:byte-len -- n ) ;

using GPT2-GEN
using RESULT
using OPTION

PROMPT-CAP BL>N U32-N + constant BODY-CAP

create HEAD U32-N allot
create TAG-BUF TAG-N allot
create ERROR I64-N allot
create PROMPT PROMPT-CAP allot
create OUTPUT OUTPUT-CAP allot

variable IO-OFF
variable REQ-MAX
variable REQ-PROMPT-U

: U32! ( n ptr u8 -- ) {: value:n dst:ptr :}
   value dst c!
   value 8 rshift dst 1 + c!
   value 16 rshift dst 2 + c!
   value 24 rshift dst 3 + c! ;

: U32@ ( ptr u8 -- n ) {: src:ptr :}
   src c@
   src 1 + c@ 8 lshift or
   src 2 + c@ 16 lshift or
   src 3 + c@ 24 lshift or ;

: I64! ( n ptr u8 -- ) {: value:n dst:ptr :}
   value dst c!
   value 8 rshift dst 1 + c!
   value 16 rshift dst 2 + c!
   value 24 rshift dst 3 + c!
   value 32 rshift dst 4 + c!
   value 40 rshift dst 5 + c!
   value 48 rshift dst 6 + c!
   value 56 rshift dst 7 + c! ;

: IO-ERR ( n -- result<n,n> )
   ERR ;

: IO-OK ( -- result<n,n> )
   0 RESULT:OK ;

: READ-EXACT ( fd ptr u8 n -- result<n,n> )
   {: fd:fd dst:ptr len:n :}
   0 IO-OFF !
   begin IO-OFF @ len < while
      fd FD>N dst IO-OFF @ + len IO-OFF @ - read {: got:n :}
      got 0 < if E-FS-IO IO-ERR exit then
      got 0= if E-FRAME IO-ERR exit then
      got len IO-OFF @ - > if E-FS-IO IO-ERR exit then
      IO-OFF @ got + IO-OFF !
   repeat
   IO-OK ;

: WRITE-EXACT ( fd ptr u8 n -- result<n,n> )
   {: fd:fd src:ptr len:n :}
   0 IO-OFF !
   begin IO-OFF @ len < while
      fd FD>N src IO-OFF @ + len IO-OFF @ - write {: put:n :}
      put 0 <= if E-FS-IO IO-ERR exit then
      put len IO-OFF @ - > if E-FS-IO IO-ERR exit then
      IO-OFF @ put + IO-OFF !
   repeat
   IO-OK ;

: PREFIX-ERR ( n -- result<option<n>,n> )
   ERR ;

: PREFIX-OK ( n -- result<option<n>,n> )
   SOME RESULT:OK ;

: PREFIX-EOF ( -- result<option<n>,n> )
   NONE RESULT:OK ;

: READ-PREFIX ( fd -- result<option<n>,n> )
   {: fd:fd :}
   0 IO-OFF !
   begin IO-OFF @ U32-N < while
      fd FD>N HEAD IO-OFF @ + U32-N IO-OFF @ - read {: got:n :}
      got 0 < if E-FS-IO PREFIX-ERR exit then
      got 0= if
         IO-OFF @ 0= if PREFIX-EOF else E-FRAME PREFIX-ERR then
         exit
      then
      got U32-N IO-OFF @ - > if E-FS-IO PREFIX-ERR exit then
      IO-OFF @ got + IO-OFF !
   repeat
   HEAD U32@ PREFIX-OK ;

: BODY-LEN? ( n -- bool )
   dup U32-N >= swap BODY-CAP <= and ;

: READ-BODY ( fd n -- result<n,n> )
   {: fd:fd bodyu:n :}
   fd HEAD U32-N READ-EXACT
   MATCH result
      err OF IO-ERR ENDOF
      ok OF
         drop
         HEAD U32@ REQ-MAX !
         bodyu U32-N - REQ-PROMPT-U !
         fd PROMPT REQ-PROMPT-U @ READ-EXACT
      ENDOF
   ;MATCH ;

: READ-REQUEST ( fd -- result<option<n>,n> )
   {: fd:fd :}
   fd READ-PREFIX
   MATCH result
      err OF PREFIX-ERR ENDOF
      ok OF
         MATCH option
            none OF PREFIX-EOF ENDOF
            some OF
               {: bodyu:n :}
               bodyu BODY-LEN? 0= if E-FRAME PREFIX-ERR exit then
               fd bodyu READ-BODY
               MATCH result
                  err OF PREFIX-ERR ENDOF
                  ok OF drop bodyu PREFIX-OK ENDOF
               ;MATCH
            ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: WRITE-FRAME ( fd n n ptr u8 n -- result<n,n> )
   {: fd:fd bodyu:n tag:n payload:ptr payloadu:n :}
   bodyu HEAD U32!
   tag TAG-BUF c!
   fd HEAD U32-N WRITE-EXACT
   MATCH result
      err OF IO-ERR ENDOF
      ok OF
         drop
         fd TAG-BUF TAG-N WRITE-EXACT
         MATCH result
            err OF IO-ERR ENDOF
            ok OF
               drop
               fd payload payloadu WRITE-EXACT
            ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: WRITE-REFUSAL ( fd n -- result<n,n> )
   {: fd:fd code:n :}
   code ERROR I64!
   fd TAG-N I64-N + TAG-ERR ERROR I64-N WRITE-FRAME ;

: WRITE-SUCCESS ( fd CAD-NUM:byte-len -- result<n,n> )
   {: fd:fd outu:CAD-NUM:byte-len :}
   outu BL>N {: len:n :}
   fd TAG-N len + TAG-OK OUTPUT len WRITE-FRAME ;

: CONTINUE ( -- result<option<n>,n> )
   NONE RESULT:OK ;

: CLEAN-EOF ( -- result<option<n>,n> )
   0 SOME RESULT:OK ;

: REFUSE ( fd n -- result<option<n>,n> )
   WRITE-REFUSAL
   MATCH result
      err OF PREFIX-ERR ENDOF
      ok OF drop CONTINUE ENDOF
   ;MATCH ;

: TERMINAL-REFUSAL ( fd n -- result<option<n>,n> )
   {: fd:fd primary:n :}
   \ Generation is the primary failure; response I/O cannot replace its code.
   fd primary WRITE-REFUSAL
   MATCH result
      err OF drop primary PREFIX-ERR ENDOF
      ok OF drop primary PREFIX-ERR ENDOF
   ;MATCH ;

using CAD-NUM

: PROMPT-LEN ( n -- result<CAD-NUM:byte-len,n> )
   BYTE-LEN
   MATCH numeric-result
      ok OF RESULT:OK ENDOF
      negative OF E-FRAME ERR ENDOF
      zero OF E-FRAME ERR ENDOF
      overflow OF E-FRAME ERR ENDOF
      underflow OF E-FRAME ERR ENDOF
      bad-alignment OF E-FRAME ERR ENDOF
      misaligned OF E-FRAME ERR ENDOF
   ;MATCH ;

: TOKEN-COUNT ( n -- result<CAD-NUM:item-count,n> )
   ITEM-COUNT
   MATCH numeric-result
      ok OF RESULT:OK ENDOF
      negative OF E-LIMIT ERR ENDOF
      zero OF E-LIMIT ERR ENDOF
      overflow OF E-LIMIT ERR ENDOF
      underflow OF E-LIMIT ERR ENDOF
      bad-alignment OF E-LIMIT ERR ENDOF
      misaligned OF E-LIMIT ERR ENDOF
   ;MATCH ;

: VALID-MAX
   ( CAD-NUM:item-count -- result<CAD-NUM:item-count,n> )
   MAX-TOKENS over SUB-ITEMS
   MATCH numeric-result
      ok OF drop RESULT:OK ENDOF
      negative OF drop E-LIMIT ERR ENDOF
      zero OF RESULT:OK ENDOF
      overflow OF drop E-LIMIT ERR ENDOF
      underflow OF drop E-LIMIT ERR ENDOF
      bad-alignment OF drop E-LIMIT ERR ENDOF
      misaligned OF drop E-LIMIT ERR ENDOF
   ;MATCH ;

;using

: GENERATE-REQUEST
   ( GPT2:model fd CAD-NUM:byte-len CAD-NUM:item-count -- GPT2:model result<option<n>,n> )
   {:
      fd:fd promptu:CAD-NUM:byte-len max:CAD-NUM:item-count
   :}
   PROMPT promptu max OUTPUT OUTPUT-CAP GENERATE
   MATCH result
      err OF
         {: code:n :}
         fd code TERMINAL-REFUSAL
      ENDOF
      ok OF
         {: outu:CAD-NUM:byte-len :}
         fd outu WRITE-SUCCESS
         MATCH result
            err OF PREFIX-ERR ENDOF
            ok OF drop CONTINUE ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: HANDLE-REQUEST
   ( GPT2:model fd -- GPT2:model result<option<n>,n> )
   {: fd:fd :}
   REQ-PROMPT-U @ 0= if fd E-PROMPT REFUSE exit then
   REQ-MAX @ 0= if fd E-LIMIT REFUSE exit then
   REQ-PROMPT-U @ PROMPT-LEN
   MATCH result
      err OF PREFIX-ERR ENDOF
      ok OF
         {: promptu:CAD-NUM:byte-len :}
         REQ-MAX @ TOKEN-COUNT
         MATCH result
            err OF drop fd E-LIMIT REFUSE ENDOF
            ok OF
               VALID-MAX
               MATCH result
                  err OF fd swap REFUSE ENDOF
                  ok OF fd promptu rot GENERATE-REQUEST ENDOF
               ;MATCH
            ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: SERVICE-STEP
   ( GPT2:model -- GPT2:model result<option<n>,n> )
   0 >FD READ-REQUEST
   MATCH result
      err OF PREFIX-ERR ENDOF
      ok OF
         MATCH option
            none OF CLEAN-EOF ENDOF
            some OF drop 1 >FD HANDLE-REQUEST ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: SERVICE ( GPT2:model -- GPT2:model result<n,n> )
   begin
      SERVICE-STEP
      MATCH result
         err OF ERR exit ENDOF
         ok OF
            MATCH option
               none OF ENDOF
               some OF RESULT:OK exit ENDOF
            ;MATCH
         ENDOF
      ;MATCH
   again ;

;using
;using
;using

: CLOSE-CODE ( result<n,n> -- n )
   MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH ;

: SERVICE-CODE ( result<n,n> -- n )
   MATCH result
      ok OF drop 0 ENDOF
      err OF ENDOF
   ;MATCH ;

: FINISH ( GPT2:model result<n,n> -- )
   SERVICE-CODE {: primary:n :}
   GPT2:CLOSE CLOSE-CODE {: closing:n :}
   \ The first stream or generation failure remains authoritative over cleanup.
   primary 0<> if primary throw then
   closing 0<> if closing throw then ;

: RUN-ACT ( ptr u8 n -- )
   1 >FD FD-NOSIGPIPE!
   FS-PATH:MAKE GPT2-GEN:OPEN
   MATCH result
      err OF throw ENDOF
      ok OF SERVICE FINISH ENDOF
   ;MATCH ;

public

: RUN ( -- )
   SCRIPT-ARGC 1 <> if E-USAGE throw then
   0 SCRIPT-ARGV$ RUN-ACT ;

;package
