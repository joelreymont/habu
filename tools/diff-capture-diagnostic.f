\ diff-capture-diagnostic.f - structured capture diagnostics.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/json-write.f
require lib/fmt.f
require tools/diff-capture-types.f
require tools/diff-capture-command.f
require tools/diff-capture-transaction.f

package DIFF-DIAG
private

$22 constant JSON-QUOTE-C

: HEX-C ( n -- n )
   dup 10 < if $30 + else 10 - $61 + then ;

: PHASE$ ( DIFF-CAPTURE:command-phase -- ptr u8 n )
   MATCH DIFF-CAPTURE:command-phase
      snapshot     OF s" snapshot" ENDOF
      resolve-from OF s" resolve-from" ENDOF
      resolve-to   OF s" resolve-to" ENDOF
      metadata     OF s" metadata" ENDOF
      raw          OF s" raw" ENDOF
      old-content  OF s" old-content" ENDOF
      new-content  OF s" new-content" ENDOF
   ;MATCH ;

: OUTCOME$ ( DIFF-CAPTURE:command-outcome -- ptr u8 n )
   MATCH DIFF-CAPTURE:command-outcome
      succeeded OF s" succeeded" ENDOF
      exited    OF s" exited" ENDOF
      fault     OF s" fault" ENDOF
   ;MATCH ;

: CAPTURE-OUTCOME$ ( DIFF-CAPTURE:capture-outcome -- ptr u8 n )
   MATCH DIFF-CAPTURE:capture-outcome
      ok              OF s" succeeded" ENDOF
      primary-failed  OF s" primary-failed" ENDOF
      cleanup-failed  OF s" cleanup-failed" ENDOF
      combined-failed OF s" combined-failed" ENDOF
   ;MATCH ;

: NUMBER$ ( n -- ptr u8 n )
   SB-RESET SB-INT SB$ ;

: WRITE-HEX ( ptr u8 n -- ) {: a:ptr u:n :}
   JSON-QUOTE-C JW-C
   0 begin dup u < while
      dup a + c@ {: c:n :}
      c 4 rshift HEX-C JW-C
      c $F and HEX-C JW-C
      1+
   repeat drop
   JSON-QUOTE-C JW-C ;

: WRITE-HEX-FIELD ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n bytes:ptr bytesu:n :}
   key keyu JW-KEY
   bytes bytesu WRITE-HEX ;

: REPORT-ARGV ( -- )
   s" argv" JW-KEY
   JW-ARRAY-START
   DIFF-CMD:LAST-EXE$ WRITE-HEX
   0 begin dup DIFF-CMD:ARG-COUNT < while
      JW-COMMA
      dup DIFF-CMD:ARG$ WRITE-HEX
      1+
   repeat drop
   JW-ARRAY-END ;

: RENDER-COMMAND ( -- )
   s" phase" DIFF-CMD:LAST-PHASE PHASE$ JW-FIELD-S JW-COMMA
   s" argv_encoding" s" hex" JW-FIELD-S JW-COMMA
   REPORT-ARGV JW-COMMA
   s" outcome" DIFF-CMD:LAST-OUTCOME OUTCOME$ JW-FIELD-S JW-COMMA
   s" rc" DIFF-CMD:LAST-RC NUMBER$ JW-FIELD-RAW JW-COMMA
   s" code" DIFF-CMD:LAST-CODE NUMBER$ JW-FIELD-RAW JW-COMMA
   s" stdout_code" DIFF-CMD:LAST-OUT-CODE NUMBER$ JW-FIELD-RAW JW-COMMA
   s" stderr_code" DIFF-CMD:LAST-ERR-CODE NUMBER$ JW-FIELD-RAW JW-COMMA
   s" stdout_encoding" s" hex" JW-FIELD-S JW-COMMA
   s" stdout" DIFF-CMD:LAST-OUT$ WRITE-HEX-FIELD JW-COMMA
   s" stderr_encoding" s" hex" JW-FIELD-S JW-COMMA
   s" stderr" DIFF-CMD:LAST-ERR$ WRITE-HEX-FIELD ;

: RENDER ( -- ptr u8 n )
   JW-RESET
   JW-OBJECT-START
   s" capture_outcome" DIFF-TXN:LAST-CAPTURE-OUTCOME CAPTURE-OUTCOME$ JW-FIELD-S JW-COMMA
   s" primary_code" DIFF-TXN:LAST-PRIMARY NUMBER$ JW-FIELD-RAW JW-COMMA
   s" cleanup_code" DIFF-TXN:LAST-CLEANUP NUMBER$ JW-FIELD-RAW JW-COMMA
   s" command_present" DIFF-CMD:COMMAND? JW-FIELD-BOOL
   DIFF-CMD:COMMAND? if JW-COMMA RENDER-COMMAND then
   JW-OBJECT-END
   JW$ ;

public

: REPORT$ ( -- ptr u8 n )
   RENDER ;

;package
