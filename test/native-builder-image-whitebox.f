\ native-builder-image-whitebox.f - a saved native builder asked for the
\ unsealed engine (`-- <out> whitebox`) writes the bytes the source path writes.
\ The reference is test/whitebox-engine.f's keyed engine: tools/native-build.f
\ run with `whitebox` by the same donor in this checkout, which a gate builds
\ once for its whitebox rows. The builder checks the class of the image it
\ wrote before it publishes it, so equal bytes are an unsealed engine.
\ test/native-builder-image-lib.f has the fixture and the other rows; run
\ alone: bin/hb --load test/native-builder-image-whitebox.f

require lib/test.f
require lib/fs.f
require lib/time.f
require tools/chain-run.f
require test/whitebox-engine.f
require test/native-builder-image-lib.f

package NATIVE-BUILDER-IMAGE-TEST

create SAVED-WHITE FS-PATH-CAP allot   variable SAVED-WHITE-U

: SAVED-WHITE$ ( -- ptr u8 n ) SAVED-WHITE SAVED-WHITE-U @ ;

: WHITEBOX-PARITY ( -- )
   s" saved-builder -- output whitebox writes the source path's unsealed engine" T-LABEL
   TIME:MONO-NS {: saved:n :}
   SAVED-WHITE$ true true SAVED-BUILD
   saved s" saved-whitebox" ELAPSED
   SUCCESS
   WHITEBOX-ENGINE:PATH$ SAVED-WHITE$ CHAIN-RUN:SAME-FILES? TTRUE ;

public

: WHITEBOX-MAIN ( -- )
   T-RESET
   s" native-builder-image-whitebox" SETUP
   s" saved-whitebox-hb" SAVED-WHITE SAVED-WHITE-U ROOT-PATH!
   WHITEBOX-PARITY
   T-REPORT ;

;package

NATIVE-BUILDER-IMAGE-TEST:WHITEBOX-MAIN
