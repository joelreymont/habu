\ maki/eval-tokest.f - deterministic model-token ESTIMATE for generated kernel
\ source (package EVAL).
\
\ GEN-TOK-EST approximates an LLM tokenizer's token count for one candidate
\ source line without calling any external tokenizer: one token per
\ alphanumeric run (BPE merges common short identifiers into ~one token) plus
\ one token per non-whitespace punctuation byte (BPE splits `-`, `<`, `,`, `>`
\ off identifiers, so `GRID-CTX` counts 3 and `span<space-global,f32,extent-n>`
\ counts 12). Whitespace only separates. It is an ESTIMATE by construction -
\ the honest dependency-free stand-in the eval matrix records NEXT TO the raw
\ whitespace source-token proxy, so a generator-reported model count
\ (transcript v1.1 `tokens`, docs/maki/eval.md) can be compared against both.

package EVAL

: TE-WS? ( n -- bool ) {: c:n :}
   c $20 =  c $09 = or  c $0A = or  c $0D = or ;

: TE-DIGIT? ( n -- bool ) {: c:n :}
   c $30 >=  c $39 <=  and ;

: TE-ALPHA? ( n -- bool ) {: c:n :}
   c $41 >=  c $5A <=  and
   c $61 >=  c $7A <=  and  or ;

: TE-ALNUM? ( n -- bool )
   dup TE-DIGIT?  swap TE-ALPHA?  or ;

variable TE-N     \ tokens counted so far
variable TE-RUN?  \ inside an alphanumeric run

\ one byte: open an alnum run once, count each punctuation byte, skip whitespace
: TE-BYTE ( n -- ) {: c:n :}
   c TE-ALNUM? if
      TE-RUN? @ 0= if  1 TE-N +!  -1 TE-RUN? !  then
      exit
   then
   0 TE-RUN? !
   c TE-WS? 0= if 1 TE-N +! then ;

public

\ estimated model tokens of one generated source line (alnum runs + punct bytes)
: GEN-TOK-EST ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 TE-N !  0 TE-RUN? !
   0 begin dup u < while
      dup a + c@ TE-BYTE
      1+
   repeat drop
   TE-N @ ;

;package
