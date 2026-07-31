\ feed.f - the stage N0 source-tape producer: the checker's own reader fills the
\ tape as it consumes a definition's tokens.
\
\ docs/compiler-ir-design.md section 7.1. The tape exists so that checking,
\ elaboration, diagnostics and code generation can prove they are talking about
\ the same tokens, and that proof is worth nothing if the tape is filled by a
\ second lexer that merely agrees with the checker's. So this file adds no
\ lexer. It hangs on the one reader the engine already runs over every checked
\ definition - src/core/checker.f CHECK-SCAN, reached from the engine's check
\ hook at every `;` - and appends one row per token that reader consumes, in
\ consumption order, while it consumes it.
\
\ WHAT A UNIT IS. One checked definition: one scan, one registered source, one
\ tape, one verdict. BEGIN-UNIT arms the reader, the reader fills the tape, and
\ END-UNIT seals it and publishes the result. Recording is never on by itself:
\ outside a unit the observer is disarmed and the checker pays one load and a
\ branch per token.
\
\ WHY THE UNIT IS ONE SCAN AND NOT A WINDOW OF TIME. The checker scans text for
\ reasons other than the definition in front of it - a candidate probe, a
\ `does>` body, a preflight check of an immediate - and every one of those is a
\ different token stream. An open unit therefore accepts exactly one scan and
\ refuses the second by name, so a tape can never be a blend of two readings,
\ and "one row per token, exactly once" is a property of the state machine
\ rather than of the caller's care.
\
\ WHAT THE READER GIVES AND WHAT IT DOES NOT. The text handed to the checker is
\ the definition the engine reconstructed: the name, the declared signature,
\ and the body, with runs of whitespace collapsed and backslash comments
\ already gone. So the tape's source is that text and not the file's bytes,
\ every span is an offset into it, and the recorded token stream is exactly
\ what CHECK-SCAN consumed - a parenthesised comment is not a token, and a
\ string or character literal's payload is not a token either, because the
\ reader steps over both. A later stage that needs those bytes needs a reader
\ that consumes them; it must not guess them from this tape. The opening `:`
\ and the closing `;` are gone before the checker sees anything, so a produced
\ tape has no frame rows at all and the elaborator reads the definition frame
\ off the recorded modes instead - the name is the one token consumed while
\ interpreting. Two consequences are still open work rather than hidden: a
\ literal payload never reaches the tape's string-literal and char-literal
\ kinds (habu-put-str-and-0750ac90), and a recorded unit is not yet tied to the
\ file it came from (habu-bind-a-recorded-78d51725).
\
\ WHY THE UNIT KEEPS THE TEXT. IR-SOURCE records a source as its length and the
\ digest of its bytes, never the bytes, and the buffer the checker read from is
\ the engine's own scratch - it is refilled by the next definition the process
\ compiles. A later stage that has to present the same text again, as
\ instruction selection does when it carries the spans into a second module,
\ would therefore have to reconstruct it from the source it came from and hope.
\ So a unit is opened with a byte buffer the caller owns, the scan is copied
\ into it as it opens, and the caller reads the length back off the frozen
\ source registry. The copy is not taken on trust either: every stage that
\ presents these bytes is checked against the registry's content digest.
\
\ WHAT IT REFUSES. A foreign or second scan, a scan longer than the caller's
\ text buffer, a token whose bytes are not the bytes at the offset it claims,
\ an append that lands anywhere but the next ordinal, a literal class this
\ stage has no tape kind for, and an integer literal whose value it cannot read
\ back. Refusals another authority owns keep that authority's name: a full tape
\ is E-NTAPE-CAP, a span outside its source is IR-SOURCE's, and a tape that
\ belongs to another module than the builder is NTAPE's own owner check at the
\ first token.
\
\ WHAT IT DOES NOT DECIDE. What a token means, whether the definition is well
\ typed, and what the verdict should be. It classifies nothing itself: the
\ reader says which tokens are literals, because it is the reader that has to
\ claim precisely the tokens the runtime claims.

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
require src/compiler/native/cert.f

package NFEED
private

\ ---- the unit state machine --------------------------------------------------
\ IDLE      no unit is open
\ ARMED     a unit is open and the reader has not started its scan
\ SCANNING  the reader is consuming this unit's tokens
\ DONE      the scan ended and its verdict arrived
0 constant ST-IDLE
1 constant ST-ARMED
2 constant ST-SCANNING
3 constant ST-DONE

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

\ ---- one token ---------------------------------------------------------------
\ The offset the reader reports and the bytes it hands over have to be the same
\ token, or every span on this tape is a claim about bytes nobody compared. The
\ check is one string compare against the KEPT text - the copy later stages
\ present - so it proves the offset honest and the copy faithful in one step,
\ and it lets a reader change position without silently relabelling the tape.
: BYTES-CK ( ptr u8 n n -- ) {: a:ptr u:n off:n :}
   u 1 < if E-NFEED-SPAN throw then
   off 0 < if E-NFEED-SPAN throw then
   off u + F-LEN @ > if E-NFEED-SPAN throw then
   TXT@ off + u  a u  STR= 0= if E-NFEED-SPAN throw then ;

\ The parser mode the token was consumed in. `:` runs from the outer
\ interpreter and parses the defined name before the parser switches to
\ compiling, so the name token was read while interpreting and every later
\ token of the definition while compiling. The reader says which token is the
\ name; this word only names the two modes.
: MODE ( n -- NTAPE:mode ) {: first:n :}
   first 0 <> if NTAPE-MODE:INTERPRETING exit then
   NTAPE-MODE:COMPILING ;

\ The value an integer-literal token carries. The engine's own parser is what
\ pushes the value at run time and it is not reachable from here, so this reads
\ the spelling back with the stdlib's one decimal reader and REFUSES every
\ spelling that reader declines - a hexadecimal or out-of-range literal is a
\ token this stage cannot record honestly, not a token to record as zero. Dot
\ habu-record-the-engine-79c570ed tracks giving the tape the engine's own
\ value.
: INT-VALUE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u STR>NUMBER? MATCH option
      none OF E-NFEED-LITERAL throw ENDOF
      some OF ENDOF
   ;MATCH ;

\ Append the row. The span is built from the offset the reader reported and the
\ spelling is interned from the bytes it handed over, both against the module
\ the tape was created for: a tape and a builder of two different modules are
\ refused by NTAPE's own owner check here rather than recorded.
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

\ Which append this token's class takes. A real literal this stage has no tape
\ kind for - a float today - is refused: recording it as a name would say the
\ elaborator may resolve it, which is false.
: APPEND ( ptr u8 n n n n -- n ) {: a:ptr u:n off:n kind:n first:n :}
   kind CHECKER-TAPE:K-NAME = if a u off first APPEND-NAME exit then
   kind CHECKER-TAPE:K-INT = if a u off first APPEND-INT exit then
   E-NFEED-KIND throw ;

\ The ordinal an append answers is the count this producer expects. They differ
\ only if the tape gained a row this producer did not write, which is the one
\ way "one row per token, exactly once, in order" can be false.
: ORDER-CK ( n -- )
   F-N @ <> if E-NFEED-ORDER throw then ;

\ ---- what the reader calls ---------------------------------------------------
\ The text is copied into the caller's buffer before it is registered, so the
\ registry's content digest is taken over the bytes that were kept rather than
\ over bytes nothing will hold on to.
: ON-SCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   F-STATE @ ST-ARMED <> if E-NFEED-SCAN throw then
   u F-CAP @ > if E-NFEED-TEXT throw then
   a TXT@ u BYTE-COPY
   u F-LEN !
   CTX BLD TXT@ u IR-BUILD:ADD-SOURCE 0 F-SID !
   ST-SCANNING F-STATE ! ;

: ON-TOKEN ( ptr u8 n n n n -- ) {: a:ptr u:n off:n kind:n first:n :}
   ST-SCANNING STATE-CK
   a u off BYTES-CK
   a u off kind first APPEND ORDER-CK
   F-N @ 1+ F-N ! ;

\ The verdict has to belong to the scan that was recorded, so the text it was
\ reached over is compared with the text the scan opened with. A verdict for
\ some other text is a verdict for some other tape.
: ON-DONE ( ptr u8 n n -- ) {: a:ptr u:n verdict:n :}
   ST-SCANNING STATE-CK
   u F-LEN @ <> if E-NFEED-SCAN throw then
   TXT@ F-LEN @  a u  STR= 0= if E-NFEED-SCAN throw then
   verdict F-VERDICT !
   ST-DONE F-STATE ! ;

\ The text buffer is the caller's, so what is cleared here is this producer's
\ hold on it and not its contents: a caller that read the recorded length off
\ the frozen registry still owns the bytes it committed.
: CLEAR ( -- )
   0 F-N !  0 F-LEN !  0 F-CAP !  0 F-VERDICT !
   ST-IDLE F-STATE ! ;

public

\ Open a unit: this context, this module under construction, this tape, and the
\ buffer the scanned text is kept in. Both ceilings are the caller's
\ commitment - a definition with more tokens than the tape holds is refused by
\ NTAPE with its own capacity error, and one whose text is longer than the
\ buffer is refused here, rather than either being truncated.
: BEGIN-UNIT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder tp:IR-ARENA:arena txt cap:n :} \ typed-local-lint: allow-bare-local - txt keeps the ptr u8 byte-span role
   ST-IDLE STATE-CK
   cap 0 < if E-NFEED-TEXT throw then
   c 0 F-CTX !
   b 0 F-BLD !
   tp 0 F-TAPE !
   txt TXT!  cap F-CAP !
   0 F-N !  0 F-LEN !  0 F-VERDICT !
   ST-ARMED F-STATE !
   CHECKER-TAPE:ARM ;

\ Close it: disarm the reader, seal the tape, and publish the result the sealed
\ tape and the registered source bind. Sealing here rather than in the caller
\ is the point at which the digest becomes worth sharing: after it, the tape
\ handle refuses every append.
: END-UNIT ( -- IR-ARENA:view NCERT:result )
   ST-DONE STATE-CK
   CHECKER-TAPE:DISARM
   TAPE NTAPE:SEAL {: v:IR-ARENA:view :}
   v
   F-VERDICT @  SID
   v NTAPE:DIGEST
   CTX BLD SID IR-BUILD:SOURCE-DIGEST
   NCERT-RESULT:MAKE
   CLEAR ;

\ Give a unit up. The only route out of a unit whose scan threw: the failure has
\ already reached the caller through the checker, and this puts the producer
\ back where a next unit can open. It publishes nothing, so nothing half
\ recorded can be mistaken for a result.
: ABANDON-UNIT ( -- )
   CHECKER-TAPE:DISARM
   CLEAR ;

private

\ The reader is told about this producer once, when this file loads. Three
\ events, one authority: the text a scan opens with, each token as it is
\ consumed, and the verdict it ends with.
: INSTALL ( -- )
   [: ON-SCAN ;] [: ON-TOKEN ;] [: ON-DONE ;] CHECKER-TAPE:INSTALL ;

INSTALL

get-current prot-wid-add

public
get-current prot-wid-add

;package
