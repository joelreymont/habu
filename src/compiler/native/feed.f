\ feed.f - the stage N0 source-tape producer: the checker's own reader fills the
\ tape as it consumes a definition's tokens.
\
\ It adds no lexer: it hangs on the one reader the engine already runs over every
\ checked definition (src/core/checker.f CHECK-SCAN) and records what it consumes.
\
\ The scanned text is the definition the engine reconstructed - whitespace
\ collapsed, backslash comments and the framing `:` and `;` already gone.
\
\ A string literal's body is the one payload that reaches the tape; a character
\ literal's code point does not (habu-bind-a-recorded-78d51725).
\
\ The unit keeps the text because the checker read it out of the engine's scratch,
\ which the next definition refills; every later presentation is digest-checked.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/compiler/digest.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/source.f
require src/compiler/ir/symbol.f
require src/compiler/ir/build.f
require src/compiler/native/tape.f

package NFEED
private

\ ---- the unit state machine --------------------------------------------------
\ IDLE      no unit is open
\ ARMED     a unit is open and the reader has not started its scan
\ SCANNING  the reader is consuming this unit's tokens
\ BETWEEN   a defining word's parent scan ended before its `does>` clause
\ DONE      the scan ended and its verdict arrived
0 constant ST-IDLE
1 constant ST-ARMED
2 constant ST-SCANNING
3 constant ST-BETWEEN
4 constant ST-DONE

here CELL 1- and CELL swap - CELL 1- and allot
1 TYPED-BUFFER F-CTX IR-CTX:ctx
1 TYPED-BUFFER F-BLD IR-BUILD:builder
1 TYPED-BUFFER F-TAPE IR-ARENA:arena
1 TYPED-BUFFER F-SID IR-ID:ir-source-id
variable F-STATE     ST-IDLE F-STATE !
variable F-TXT                         \ the caller's buffer, holding the text the reader scanned
variable F-CAP                         \ how many bytes that buffer holds
variable F-LEN                         \ how many it was handed
variable F-N                           \ rows appended so far
variable F-VERDICT
variable F-DOES                        \ byte split after `does> `, or zero
variable F-BASE                        \ start of the scan now being recorded

: TXT-FIELD ( -- ptr ptr u8 )
   F-TXT 0 ptr-field ;

: TXT@ ( -- ptr u8 )
   TXT-FIELD @ ;

: TXT! ( ptr u8 -- )
   TXT-FIELD ! ;

: CTX ( -- IR-CTX:ctx )          0 F-CTX @ ;
: BLD ( -- IR-BUILD:builder )    0 F-BLD @ ;
: TAPE ( -- IR-ARENA:arena )     0 F-TAPE @ ;
: SID ( -- IR-ID:ir-source-id )  0 F-SID @ ;

: STATE-CK ( n -- )
   F-STATE @ <> if E-NFEED-STATE throw then ;

: DOES? ( -- bool )
   F-DOES @ 0<> ;

: SCAN-LEN ( -- n )
   F-BASE @ 0= if F-DOES @ 6 - exit then
   F-LEN @ F-DOES @ - ;

: SCAN-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   u SCAN-LEN <> if E-NFEED-SCAN throw then
   TXT@ F-BASE @ + u  a u  STR= 0= if E-NFEED-SCAN throw then ;

\ ---- one token ---------------------------------------------------------------
\ The offset and the bytes have to be the same token, compared against the KEPT
\ text, so one check proves the offset honest and the copy faithful.
: BYTES-CK ( ptr u8 n n -- ) {: a:ptr u:n off:n :}
   u 1 < if E-NFEED-SPAN throw then
   off 0 < if E-NFEED-SPAN throw then
   off u + F-LEN @ > if E-NFEED-SPAN throw then
   TXT@ off + u  a u  STR= 0= if E-NFEED-SPAN throw then ;

\ A string row's bytes are its BODY and its span is the source the payload
\ occupied, so only the span is checkable here; `s" "` is a real empty body.
: SPAN-CK ( n n n -- ) {: u:n off:n ru:n :}
   u 0 < if E-NFEED-SPAN throw then
   ru 0 < if E-NFEED-SPAN throw then
   u ru > if E-NFEED-SPAN throw then
   off 0 < if E-NFEED-SPAN throw then
   off ru + F-LEN @ > if E-NFEED-SPAN throw then ;

\ `:` parses the defined name from the outer interpreter, so the name token was
\ read while interpreting and every later token of the definition while compiling.
: MODE ( n -- NTAPE:mode ) {: first:n :}
   first 0 <> if NTAPE-MODE:INTERPRETING exit then
   NTAPE-MODE:COMPILING ;

\ Asked of the engine's own number reader, so the cell is the cell the engine
\ pushes. A disagreement with the checker's predicates is E-NFEED-LITERAL.
: NUM-VALUE ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   a u num-parse {: v:n flt:bool ok:bool :}
   ok if v flt exit then
   E-NFEED-LITERAL throw ;

: INT-VALUE ( ptr u8 n -- n )
   NUM-VALUE if E-NFEED-LITERAL throw then ;

: REAL-VALUE ( ptr u8 n -- n )
   NUM-VALUE if exit then
   E-NFEED-LITERAL throw ;

\ A tape and a builder of two different modules are refused by NTAPE's owner check.
: APPEND-NAME ( ptr u8 n n n -- n ) {: a:ptr u:n off:n first:n :}
   CTX BLD a u IR-BUILD:INTERN-SYMBOL {: sym:IR-ID:ir-symbol-id :}
   CTX BLD TAPE
   BLD SID off u IR-BUILD:ADD-SPAN  sym  first MODE  NTAPE:NAME-TOKEN
   NTAPE:PUSH-INTO ;

: APPEND-INT ( ptr u8 n n n -- n ) {: a:ptr u:n off:n first:n :}
   CTX BLD a u IR-BUILD:INTERN-SYMBOL {: sym:IR-ID:ir-symbol-id :}
   a u INT-VALUE {: v:n :}
   CTX BLD TAPE
   BLD SID off u IR-BUILD:ADD-SPAN  sym  first MODE  v  NTAPE:INT-TOKEN
   NTAPE:PUSH-INTO ;

: APPEND-REAL ( ptr u8 n n n -- n ) {: a:ptr u:n off:n first:n :}
   CTX BLD a u IR-BUILD:INTERN-SYMBOL {: sym:IR-ID:ir-symbol-id :}
   a u REAL-VALUE {: v:n :}
   CTX BLD TAPE
   BLD SID off u IR-BUILD:ADD-SPAN  sym  first MODE  v  NTAPE:REAL-TOKEN
   NTAPE:PUSH-INTO ;

\ Spelling is the body and span is the source it was written as, so this takes
\ both lengths. A string is never the token a colon definition is named by.
: APPEND-STRING ( ptr u8 n n n -- n ) {: a:ptr u:n off:n ru:n :}
   CTX BLD a u IR-BUILD:INTERN-SYMBOL {: sym:IR-ID:ir-symbol-id :}
   CTX BLD TAPE
   BLD SID off ru IR-BUILD:ADD-SPAN  sym  NTAPE-MODE:COMPILING  NTAPE:STRING-TOKEN
   NTAPE:PUSH-INTO ;

: APPEND-CHAR ( ptr u8 n n n -- n ) {: a:ptr u:n off:n ru:n :}
   u 1 < if E-NFEED-KIND throw then
   CTX BLD a u IR-BUILD:INTERN-SYMBOL {: sym:IR-ID:ir-symbol-id :}
   CTX BLD TAPE
   BLD SID off ru IR-BUILD:ADD-SPAN sym NTAPE-MODE:COMPILING a c@ NTAPE:CHAR-TOKEN
   NTAPE:PUSH-INTO ;

\ A literal class this stage has no tape kind for is refused: recording it as a
\ name would say the elaborator may resolve it, which is false.
: APPEND ( ptr u8 n n n n n -- n ) {: a:ptr u:n off:n ru:n kind:n first:n :}
   kind CHECKER-TAPE:K-NAME = if a u off first APPEND-NAME exit then
   kind CHECKER-TAPE:K-INT = if a u off first APPEND-INT exit then
   kind CHECKER-TAPE:K-REAL = if a u off first APPEND-REAL exit then
   kind CHECKER-TAPE:K-STRING = if a u off ru APPEND-STRING exit then
   kind CHECKER-TAPE:K-CHAR = if a u off ru APPEND-CHAR exit then
   E-NFEED-KIND throw ;

\ They differ only if the tape gained a row this producer did not write.
: ORDER-CK ( n -- )
   F-N @ <> if E-NFEED-ORDER throw then ;

\ ---- what the reader calls ---------------------------------------------------
\ The text is copied into the caller's buffer before it is registered, so the
\ registry's content digest is taken over the bytes that were kept.
: ON-SCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   F-STATE @ ST-ARMED <> if E-NFEED-SCAN throw then
   DOES? if
      a u SCAN-CK
   else
      u F-CAP @ > if E-NFEED-TEXT throw then
      a TXT@ u BYTE-COPY
      u F-LEN !
      CTX BLD TXT@ u IR-BUILD:ADD-SOURCE 0 F-SID !
   then
   ST-SCANNING F-STATE ! ;

: ON-TOKEN ( ptr u8 n n n n n -- ) {: a:ptr u:n off:n ru:n kind:n first:n :}
   ST-SCANNING STATE-CK
   off F-BASE @ + {: goff:n :}
   kind CHECKER-TAPE:K-STRING = kind CHECKER-TAPE:K-CHAR = or
   if u goff ru SPAN-CK else a u goff BYTES-CK then
   a u goff ru kind first APPEND ORDER-CK
   F-N @ 1+ F-N ! ;

\ A verdict for some other text is a verdict for some other tape.
: ON-DONE ( ptr u8 n n -- ) {: a:ptr u:n verdict:n :}
   ST-SCANNING STATE-CK
   DOES? if
      a u SCAN-CK
   else
      u F-LEN @ <> if E-NFEED-SCAN throw then
      TXT@ F-LEN @  a u  STR= 0= if E-NFEED-SCAN throw then
   then
   verdict F-VERDICT !
   DOES? F-BASE @ 0= and if ST-BETWEEN else ST-DONE then F-STATE ! ;

\ Clears this producer's hold on the caller's buffer, not its contents.
: CLEAR ( -- )
   0 F-N !  0 F-LEN !  0 F-CAP !  0 F-VERDICT !
   0 F-DOES !  0 F-BASE !
   NULL-PTR TXT!
   ST-IDLE F-STATE ! ;

: OPEN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder tp:IR-ARENA:arena txt cap:n :}
   ST-IDLE STATE-CK
   cap 0 < if E-NFEED-TEXT throw then
   c 0 F-CTX !
   b 0 F-BLD !
   tp 0 F-TAPE !
   txt TXT!  cap F-CAP !
   0 F-N !  0 F-LEN !  0 F-VERDICT !
   0 F-DOES !  0 F-BASE ! ;

public

\ Both ceilings are the caller's commitment: too many tokens is NTAPE's capacity
\ error and text longer than the buffer is refused here; neither is truncated.
: BEGIN-UNIT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena ptr u8 n -- )
   OPEN
   ST-ARMED F-STATE !
   CHECKER-TAPE:ARM ;

\ A defining word is checked as two bodies, but remains one source and one tape.
\ The engine supplies the exact byte split it recorded while consuming `does>`.
: BEGIN-DOES-UNIT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena ptr u8 n ptr u8 n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder tp:IR-ARENA:arena txt cap:n src su:n cut:n :}
   cut 6 < cut su > or if E-NFEED-SCAN throw then
   src cut 6 - + 5 s" does>" STR=CI 0= if E-NFEED-SCAN throw then
   src cut 1- + c@ 32 <> if E-NFEED-SCAN throw then
   su cap > if E-NFEED-TEXT throw then
   c b tp txt cap OPEN
   src TXT@ su BYTE-COPY
   su F-LEN !  cut F-DOES !
   CTX BLD TXT@ su IR-BUILD:ADD-SOURCE 0 F-SID !
   ST-ARMED F-STATE !
   CHECKER-TAPE:ARM ;

\ Insert the one verified keyword between the two checker bodies and advance the
\ checker's matching token ordinal, then admit the clause scan.
: DOES-CLAUSE ( -- n )
   ST-BETWEEN STATE-CK
   F-DOES @ 6 - {: off:n :}
   F-N @ {: row:n :}
   TXT@ off + 5 off BYTES-CK
   TXT@ off + 5 off 0 APPEND-NAME ORDER-CK
   F-N @ 1+ F-N !
   CHECKER-TAPE:ADVANCE
   F-DOES @ F-BASE !
   ST-ARMED F-STATE !
   row ;

\ Sealing here is where the digest becomes worth sharing: after it the tape
\ refuses every append. The source is not answered separately - every span names it.
: END-UNIT ( -- IR-ARENA:view n )
   ST-DONE STATE-CK
   CHECKER-TAPE:DISARM
   TAPE NTAPE:SEAL
   F-VERDICT @
   CLEAR ;

\ The only route out of a unit whose scan threw. It publishes nothing.
: ABANDON-UNIT ( -- )
   CHECKER-TAPE:DISARM
   CLEAR ;

\ Three events, one authority: the scan's text, each token, and the verdict.
\
\ PUBLIC BECAUSE A CAPTURED CHAIN HAS TO RE-RUN IT, the way
\ A64RAV:DKEEP-HOOK-DEFAULT is. It runs at load time into three declared cells
\ BELOW the capture window - CHECKER-TAPE's own - so no captured byte carries it
\ and a seeded engine reaches ARM with nothing installed. Named on
\ tools/aot-chain-capture.f's boot-run list, which LFIND resolves.
: INSTALL ( -- )
   [: ON-SCAN ;] [: ON-TOKEN ;] [: ON-DONE ;] CHECKER-TAPE:INSTALL ;

INSTALL

private
get-current prot-wid-add

public
get-current prot-wid-add

: CAPTURE-PREPARE ( -- )
   ST-IDLE STATE-CK
   CLEAR ;

;package
