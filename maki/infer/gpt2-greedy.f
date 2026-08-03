\ gpt2-greedy.f - one-token greedy GPT-2 selection.

require lib/adt/result.f
require lib/cad-num-arithmetic.f
require lib/float32-buffer.f
require maki/infer/gpt2-model.f

package GPT2
private

-5658 constant E-NUMERIC

: G-FINITE? ( n -- bool )
   23 rshift $FF and $FF <> ;

: G-MORE?
   ( ptr u8 CAD-NUM:byte-len n n r bool n -- ptr u8 CAD-NUM:byte-len n n r bool n bool )
   {: row:ptr left:CAD-NUM:byte-len idx:n best:n high:r seen:bool code:n :}
   row left idx best high seen code
   code 0= left 0 M-BYTE-LEN M-BYTES= 0= and ;

: G-STEP-OK
   ( CAD-NUM:byte-len ptr u8 n n r bool -- ptr u8 CAD-NUM:byte-len n n r bool n )
   {: left:CAD-NUM:byte-len row:ptr idx:n best:n high:r seen:bool :}
   row F32-BUF:LOAD {: bits:n :}
   bits G-FINITE? 0= if row left idx best high seen E-NUMERIC exit then
   bits F32:WIDEN {: value:r :}
   seen if
      row 4 + left idx 1+
      value high f> if idx value else best high then true 0
   else
      row 4 + left idx 1+ idx value true 0
   then ;

: G-STEP
   ( ptr u8 CAD-NUM:byte-len n n r bool n -- ptr u8 CAD-NUM:byte-len n n r bool n )
   {: row:ptr left:CAD-NUM:byte-len idx:n best:n high:r seen:bool code:n :}
   code drop
   left 4 M-BYTE-LEN CAD-NUM:SUB-BYTES
   MATCH CAD-NUM:numeric-result
      ok OF row idx best high seen G-STEP-OK ENDOF
      negative OF row left idx best high seen E-NUMERIC ENDOF
      zero OF row left idx best high seen E-NUMERIC ENDOF
      overflow OF row left idx best high seen E-NUMERIC ENDOF
      underflow OF row left idx best high seen E-NUMERIC ENDOF
      bad-alignment OF row left idx best high seen E-NUMERIC ENDOF
      misaligned OF row left idx best high seen E-NUMERIC ENDOF
   ;MATCH ;

: G-RESULT
   ( ptr u8 CAD-NUM:byte-len n n r bool n -- result<n,n> )
   {: row:ptr left:CAD-NUM:byte-len idx:n best:n high:r seen:bool code:n :}
   row drop left drop idx drop high fdrop
   code 0<> if code RESULT:ERR exit then
   seen if best RESULT:OK else E-NUMERIC RESULT:ERR then ;

: G-SCAN ( ptr u8 CAD-NUM:byte-len -- result<n,n> )
   0 0 0.0 false 0
   begin G-MORE? while G-STEP repeat
   G-RESULT ;

public

\ LOGITS commits the input token before G-SCAN can report E-NUMERIC.
: GREEDY
   ( GPT2:model n ptr u8 CAD-NUM:byte-len -- GPT2:model result<n,n> )
   {: tok:n row:ptr bytes:CAD-NUM:byte-len :}
   tok row bytes LOGITS
   MATCH result
      ok OF drop row bytes G-SCAN ENDOF
      err OF RESULT:ERR ENDOF
   ;MATCH ;

;package
