\ saved-image-scenario.f - Habu-owned saved-image scenario runner front end.
\
\ This owns saved-image scenario option parsing, path construction, static input
\ validation, and dry-run summaries. Live capture is deliberately refused until
\ the Habu camera capture backend exists.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/render.f
require odin/capture-backend.f
require odin/saved-image-analyzers.f

package SAVEDIMG

70 constant SIS-PENDING-RC
64 constant SIS-USAGE-RC
1024 constant SIS-PATH-CAP
256 constant SIS-TEXT-CAP
 -1 constant SIS-SKIPPED

create SIS-ROOT-BUF FS-PATH-CAP allot
create SIS-SCENARIO-BUF SIS-TEXT-CAP allot
create SIS-RESOLUTION-BUF SIS-TEXT-CAP allot
create SIS-FPS-BUF SIS-TEXT-CAP allot
create SIS-DURATION-BUF SIS-TEXT-CAP allot
create SIS-WARMUP-BUF SIS-TEXT-CAP allot
create SIS-METADATA-BUF SIS-TEXT-CAP allot
create SIS-SAVE-EVERY-BUF SIS-TEXT-CAP allot
create SIS-MAX-SAVED-BUF SIS-TEXT-CAP allot
create SIS-MANUAL-EXPOSURE-BUF SIS-TEXT-CAP allot
create SIS-MANUAL-GAIN-BUF SIS-TEXT-CAP allot
create SIS-EXPOSURE-MANIFEST-BUF FS-PATH-CAP allot
create SIS-LOW-LIGHT-MANIFEST-BUF FS-PATH-CAP allot
create SIS-MOTION-MANIFEST-BUF FS-PATH-CAP allot
create SIS-PRE-DELAY-BUF SIS-TEXT-CAP allot
create SIS-PRE-CUE-BUF SIS-TEXT-CAP allot
create SIS-TAG-BUF SIS-TEXT-CAP allot
create SIS-OUTPUT-ID-BUF SIS-TEXT-CAP allot
create SIS-RESULTS-ROOT-BUF FS-PATH-CAP allot
create SIS-CAPTURE-ABI-BUF FS-PATH-CAP allot
create SIS-CONFIG-BUF FS-PATH-CAP allot
create SIS-P0 SIS-PATH-CAP allot
create SIS-P1 SIS-PATH-CAP allot
create SIS-P2 SIS-PATH-CAP allot

variable SIS-ARG-I
variable SIS-ROOT-U
variable SIS-SCENARIO-U
variable SIS-RESOLUTION-U
variable SIS-FPS-U
variable SIS-DURATION-U
variable SIS-WARMUP-U
variable SIS-METADATA-U
variable SIS-SAVE-EVERY-U
variable SIS-MAX-SAVED-U
variable SIS-MANUAL-EXPOSURE-U
variable SIS-MANUAL-GAIN-U
variable SIS-EXPOSURE-MANIFEST-U
variable SIS-LOW-LIGHT-MANIFEST-U
variable SIS-MOTION-MANIFEST-U
variable SIS-PRE-DELAY-U
variable SIS-PRE-CUE-U
variable SIS-TAG-U
variable SIS-OUTPUT-ID-U
variable SIS-RESULTS-ROOT-U
variable SIS-CAPTURE-ABI-U
variable SIS-CONFIG-U
variable SIS-RUN-EXPOSURE
variable SIS-RUN-LOW-LIGHT
variable SIS-RUN-MOTION-BLUR
variable SIS-RUN-SYNC
variable SIS-REQUIRE-SYNC
variable SIS-REQUIRE-PAIRING
variable SIS-DRY-RUN
variable SIS-I
variable SIS-P0-U
variable SIS-P1-U
variable SIS-P2-U
variable SIS-CAPTURE-STATUS
variable SIS-EXPOSURE-STATUS
variable SIS-LOW-LIGHT-STATUS
variable SIS-MOTION-BLUR-STATUS
variable SIS-SYNC-STATUS
variable SIS-FIRST-FAILURE

: SIS-TRUE ( -- bool ) 0 0= ;
: SIS-FALSE ( -- bool ) SIS-TRUE 0= ;

: SIS-COPY! ( ptr u8 n ptr u8 n ptr a -- ) {: a:ptr u:n dst:ptr cap:n lenp:ptr :}
   u 0 <= if s" empty option value" SIS-USAGE-RC die then
   u cap > if E-STR-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: SIS-ROOT$ ( -- ptr u8 n ) SIS-ROOT-BUF SIS-ROOT-U @ ;
: SIS-SCENARIO$ ( -- ptr u8 n ) SIS-SCENARIO-BUF SIS-SCENARIO-U @ ;
: SIS-RESOLUTION$ ( -- ptr u8 n ) SIS-RESOLUTION-BUF SIS-RESOLUTION-U @ ;
: SIS-FPS$ ( -- ptr u8 n ) SIS-FPS-BUF SIS-FPS-U @ ;
: SIS-DURATION$ ( -- ptr u8 n ) SIS-DURATION-BUF SIS-DURATION-U @ ;
: SIS-WARMUP$ ( -- ptr u8 n ) SIS-WARMUP-BUF SIS-WARMUP-U @ ;
: SIS-METADATA$ ( -- ptr u8 n ) SIS-METADATA-BUF SIS-METADATA-U @ ;
: SIS-SAVE-EVERY$ ( -- ptr u8 n ) SIS-SAVE-EVERY-BUF SIS-SAVE-EVERY-U @ ;
: SIS-MAX-SAVED$ ( -- ptr u8 n ) SIS-MAX-SAVED-BUF SIS-MAX-SAVED-U @ ;
: SIS-MANUAL-EXPOSURE$ ( -- ptr u8 n ) SIS-MANUAL-EXPOSURE-BUF SIS-MANUAL-EXPOSURE-U @ ;
: SIS-MANUAL-GAIN$ ( -- ptr u8 n ) SIS-MANUAL-GAIN-BUF SIS-MANUAL-GAIN-U @ ;
: SIS-EXPOSURE-MANIFEST$ ( -- ptr u8 n ) SIS-EXPOSURE-MANIFEST-BUF SIS-EXPOSURE-MANIFEST-U @ ;
: SIS-LOW-LIGHT-MANIFEST$ ( -- ptr u8 n ) SIS-LOW-LIGHT-MANIFEST-BUF SIS-LOW-LIGHT-MANIFEST-U @ ;
: SIS-MOTION-MANIFEST$ ( -- ptr u8 n ) SIS-MOTION-MANIFEST-BUF SIS-MOTION-MANIFEST-U @ ;
: SIS-PRE-DELAY$ ( -- ptr u8 n ) SIS-PRE-DELAY-BUF SIS-PRE-DELAY-U @ ;
: SIS-PRE-CUE$ ( -- ptr u8 n ) SIS-PRE-CUE-BUF SIS-PRE-CUE-U @ ;
: SIS-TAG$ ( -- ptr u8 n ) SIS-TAG-BUF SIS-TAG-U @ ;
: SIS-OUTPUT-ID$ ( -- ptr u8 n ) SIS-OUTPUT-ID-BUF SIS-OUTPUT-ID-U @ ;
: SIS-RESULTS-ROOT$ ( -- ptr u8 n ) SIS-RESULTS-ROOT-BUF SIS-RESULTS-ROOT-U @ ;
: SIS-CAPTURE-ABI$ ( -- ptr u8 n ) SIS-CAPTURE-ABI-BUF SIS-CAPTURE-ABI-U @ ;
: SIS-CONFIG$ ( -- ptr u8 n ) SIS-CONFIG-BUF SIS-CONFIG-U @ ;

: SIS-ROOT! ( ptr u8 n -- ) SIS-ROOT-BUF FS-PATH-CAP SIS-ROOT-U SIS-COPY! ;
: SIS-SCENARIO! ( ptr u8 n -- ) SIS-SCENARIO-BUF SIS-TEXT-CAP SIS-SCENARIO-U SIS-COPY! ;
: SIS-RESOLUTION! ( ptr u8 n -- ) SIS-RESOLUTION-BUF SIS-TEXT-CAP SIS-RESOLUTION-U SIS-COPY! ;
: SIS-FPS! ( ptr u8 n -- ) SIS-FPS-BUF SIS-TEXT-CAP SIS-FPS-U SIS-COPY! ;
: SIS-DURATION! ( ptr u8 n -- ) SIS-DURATION-BUF SIS-TEXT-CAP SIS-DURATION-U SIS-COPY! ;
: SIS-WARMUP! ( ptr u8 n -- ) SIS-WARMUP-BUF SIS-TEXT-CAP SIS-WARMUP-U SIS-COPY! ;
: SIS-METADATA! ( ptr u8 n -- ) SIS-METADATA-BUF SIS-TEXT-CAP SIS-METADATA-U SIS-COPY! ;
: SIS-SAVE-EVERY! ( ptr u8 n -- ) SIS-SAVE-EVERY-BUF SIS-TEXT-CAP SIS-SAVE-EVERY-U SIS-COPY! ;
: SIS-MAX-SAVED! ( ptr u8 n -- ) SIS-MAX-SAVED-BUF SIS-TEXT-CAP SIS-MAX-SAVED-U SIS-COPY! ;
: SIS-MANUAL-EXPOSURE! ( ptr u8 n -- ) SIS-MANUAL-EXPOSURE-BUF SIS-TEXT-CAP SIS-MANUAL-EXPOSURE-U SIS-COPY! ;
: SIS-MANUAL-GAIN! ( ptr u8 n -- ) SIS-MANUAL-GAIN-BUF SIS-TEXT-CAP SIS-MANUAL-GAIN-U SIS-COPY! ;
: SIS-EXPOSURE-MANIFEST! ( ptr u8 n -- ) SIS-EXPOSURE-MANIFEST-BUF FS-PATH-CAP SIS-EXPOSURE-MANIFEST-U SIS-COPY! ;
: SIS-LOW-LIGHT-MANIFEST! ( ptr u8 n -- ) SIS-LOW-LIGHT-MANIFEST-BUF FS-PATH-CAP SIS-LOW-LIGHT-MANIFEST-U SIS-COPY! ;
: SIS-MOTION-MANIFEST! ( ptr u8 n -- ) SIS-MOTION-MANIFEST-BUF FS-PATH-CAP SIS-MOTION-MANIFEST-U SIS-COPY! ;
: SIS-PRE-DELAY! ( ptr u8 n -- ) SIS-PRE-DELAY-BUF SIS-TEXT-CAP SIS-PRE-DELAY-U SIS-COPY! ;
: SIS-PRE-CUE! ( ptr u8 n -- ) SIS-PRE-CUE-BUF SIS-TEXT-CAP SIS-PRE-CUE-U SIS-COPY! ;
: SIS-TAG! ( ptr u8 n -- ) SIS-TAG-BUF SIS-TEXT-CAP SIS-TAG-U SIS-COPY! ;
: SIS-OUTPUT-ID! ( ptr u8 n -- ) SIS-OUTPUT-ID-BUF SIS-TEXT-CAP SIS-OUTPUT-ID-U SIS-COPY! ;
: SIS-RESULTS-ROOT! ( ptr u8 n -- ) SIS-RESULTS-ROOT-BUF FS-PATH-CAP SIS-RESULTS-ROOT-U SIS-COPY! ;
: SIS-CAPTURE-ABI! ( ptr u8 n -- ) SIS-CAPTURE-ABI-BUF FS-PATH-CAP SIS-CAPTURE-ABI-U SIS-COPY! ;
: SIS-CONFIG! ( ptr u8 n -- ) SIS-CONFIG-BUF FS-PATH-CAP SIS-CONFIG-U SIS-COPY! ;

: SIS-DEFAULTS ( -- )
   0 SIS-ARG-I !
   0 SIS-SCENARIO-U !
   0 SIS-MANUAL-EXPOSURE-U !
   0 SIS-MANUAL-GAIN-U !
   0 SIS-EXPOSURE-MANIFEST-U !
   0 SIS-LOW-LIGHT-MANIFEST-U !
   0 SIS-MOTION-MANIFEST-U !
   0 SIS-PRE-CUE-U !
   0 SIS-OUTPUT-ID-U !
   s" ../Odin" SIS-ROOT!
   s" HD1200" SIS-RESOLUTION!
   s" 60" SIS-FPS!
   s" 10000" SIS-DURATION!
   s" 5000" SIS-WARMUP!
   s" 120" SIS-METADATA!
   s" 60" SIS-SAVE-EVERY!
   s" 20" SIS-MAX-SAVED!
   s" 0" SIS-PRE-DELAY!
   s" manual" SIS-TAG!
   s" results" SIS-RESULTS-ROOT!
   s" libodin_zed_capture.so" SIS-CAPTURE-ABI!
   s" configs/cameras.json" SIS-CONFIG!
   1 SIS-RUN-EXPOSURE !
   1 SIS-RUN-LOW-LIGHT !
   1 SIS-RUN-MOTION-BLUR !
   1 SIS-RUN-SYNC !
   1 SIS-REQUIRE-SYNC !
   1 SIS-REQUIRE-PAIRING !
   0 SIS-DRY-RUN ! ;

: SIS-LINE ( ptr u8 n -- ) type cr ;

: SIS-USAGE ( -- )
   s" usage: odin/saved-image-scenario-cli.f -- <scenario-id> [options] --dry-run" SIS-LINE
   s" options: --odin-root PATH --resolution VALUE --fps VALUE --duration-ms N --warmup-ms N" SIS-LINE
   s"          --metadata-every N --save-every N --max-saved-frames N" SIS-LINE
   s"          --manual-exposure-us N --manual-gain N --exposure-manifest PATH" SIS-LINE
   s"          --low-light-manifest PATH --motion-manifest PATH --pre-capture-delay-s N" SIS-LINE
   s"          --pre-capture-cue TEXT --tag VALUE --output-id VALUE --results-root PATH" SIS-LINE
   s"          --capture-abi PATH --config PATH --camera SERIAL:NAME" SIS-LINE
   s"          --no-exposure --no-low-light --no-motion-blur --no-sync" SIS-LINE
   s"          --allow-frame-phase-offset --allow-sync-characterization" SIS-LINE ;

: SIS-DIE-USAGE ( -- )
   SIS-USAGE
   s" saved-image-scenario usage" SIS-USAGE-RC die ;

: SIS-ARG$ ( -- ptr u8 n ) SIS-ARG-I @ SCRIPT-ARGV$ ;
: SIS-VALUE$ ( -- ptr u8 n )
   SIS-ARG-I @ 1+ SCRIPT-ARGC >= if SIS-DIE-USAGE then
   SIS-ARG-I @ 1+ SCRIPT-ARGV$ ;
: SIS-ADVANCE ( n -- ) SIS-ARG-I @ + SIS-ARG-I ! ;

: SIS-NONNEG? ( ptr u8 n -- bool ) STR-DIGITS? ;

: SIS-ASSERT-NONNEG ( ptr u8 n -- )
   SIS-NONNEG? 0= if s" expected non-negative integer option" SIS-USAGE-RC die then ;

: SIS-ASSERT-OPTIONAL-NONNEG ( ptr u8 n -- )
   dup 0= if 2drop exit then
   SIS-ASSERT-NONNEG ;

: SIS-FIRST-NAME? ( n -- bool ) {: c:n :}
   c 48 >= c 57 <= and
   c 65 >= c 90 <= and or
   c 97 >= c 122 <= and or ;

: SIS-NAME-CHAR? ( n -- bool ) {: c:n :}
   c SIS-FIRST-NAME? if SIS-TRUE exit then
   c 46 = c 95 = or c 45 = or ;

: SIS-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 <= if SIS-FALSE exit then
   a c@ SIS-FIRST-NAME? 0= if SIS-FALSE exit then
   1 SIS-I !
   begin SIS-I @ u < while
      a SIS-I @ + c@ SIS-NAME-CHAR? 0= if SIS-FALSE exit then
      SIS-I @ 1+ SIS-I !
   repeat
   SIS-TRUE ;

: SIS-CHECK-NAME ( ptr u8 n -- )
   SIS-NAME? 0= if s" invalid scenario/tag/output name" SIS-USAGE-RC die then ;

: SIS-OUT-CHECK-ROOM ( -- )
   SIS-OUTPUT-ID-U @ SIS-TEXT-CAP >= if E-STR-CAPACITY throw then ;

: SIS-OUT-C ( n -- ) {: c:n :}
   SIS-OUT-CHECK-ROOM
   c SIS-OUTPUT-ID-BUF SIS-OUTPUT-ID-U @ + c!
   SIS-OUTPUT-ID-U @ 1+ SIS-OUTPUT-ID-U ! ;

: SIS-OUT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 SIS-I !
   begin SIS-I @ u < while
      a SIS-I @ + c@ SIS-OUT-C
      SIS-I @ 1+ SIS-I !
   repeat ;

: SIS-OUT+LOWER ( ptr u8 n -- ) {: a:ptr u:n :}
   0 SIS-I !
   begin SIS-I @ u < while
      a SIS-I @ + c@ ASCII-LOWER SIS-OUT-C
      SIS-I @ 1+ SIS-I !
   repeat ;

: SIS-BUILD-OUTPUT-ID ( -- )
   0 SIS-OUTPUT-ID-U !
   SIS-SCENARIO$ SIS-OUT+
   95 SIS-OUT-C
   SIS-RESOLUTION$ SIS-OUT+LOWER
   95 SIS-OUT-C
   SIS-FPS$ SIS-OUT+
   95 SIS-OUT-C
   SIS-TAG$ SIS-OUT+ ;

: SIS-ABSOLUTE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 > if a c@ 47 = else SIS-FALSE then ;

: SIS-ROOTED+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SIS-ABSOLUTE? 0= if
      SIS-ROOT$ RB+
      SIS-ROOT$ dup 0 > if 1- + c@ 47 <> if 47 RB-C then else 2drop then
   then
   a u RB+ ;

: SIS-ROOTED-SB+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SIS-ABSOLUTE? 0= if
      SIS-ROOT$ SB-APPEND
      SIS-ROOT$ dup 0 > if 1- + c@ 47 <> if 47 SB-APPEND-C then else 2drop then
   then
   a u SB-APPEND ;

: SIS-RUN-DIR$ ( ptr u8 n -- ptr u8 n ) {: kind:ptr kindu:n :}
   SB-RESET
   SIS-RESULTS-ROOT$ SIS-ROOTED-SB+
   47 SB-APPEND-C
   kind kindu SB-APPEND
   47 SB-APPEND-C
   SIS-OUTPUT-ID$ SB-APPEND
   SB$ ;

: SIS-CAPTURE-DIR$ ( -- ptr u8 n ) s" capture" SIS-RUN-DIR$ ;
: SIS-EXPOSURE-DIR$ ( -- ptr u8 n ) s" exposure_adaptation" SIS-RUN-DIR$ ;
: SIS-LOW-LIGHT-DIR$ ( -- ptr u8 n ) s" low_light" SIS-RUN-DIR$ ;
: SIS-MOTION-BLUR-DIR$ ( -- ptr u8 n ) s" motion_blur" SIS-RUN-DIR$ ;
: SIS-SYNC-DIR$ ( -- ptr u8 n ) s" timestamp_sync" SIS-RUN-DIR$ ;

: SIS-PATH-COPY ( ptr u8 n ptr u8 ptr a -- ptr u8 n ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u SIS-PATH-CAP > if E-STR-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp !
   dst u ;

: SIS-P0! ( ptr u8 n -- ptr u8 n ) SIS-P0 SIS-P0-U SIS-PATH-COPY ;
: SIS-P1! ( ptr u8 n -- ptr u8 n ) SIS-P1 SIS-P1-U SIS-PATH-COPY ;
: SIS-P2! ( ptr u8 n -- ptr u8 n ) SIS-P2 SIS-P2-U SIS-PATH-COPY ;

: SIS-CAPTURE-P1$ ( -- ptr u8 n ) SIS-CAPTURE-DIR$ SIS-P1! ;
: SIS-EXPOSURE-P2$ ( -- ptr u8 n ) SIS-EXPOSURE-DIR$ SIS-P2! ;
: SIS-LOW-LIGHT-P2$ ( -- ptr u8 n ) SIS-LOW-LIGHT-DIR$ SIS-P2! ;
: SIS-MOTION-BLUR-P2$ ( -- ptr u8 n ) SIS-MOTION-BLUR-DIR$ SIS-P2! ;
: SIS-SYNC-P2$ ( -- ptr u8 n ) SIS-SYNC-DIR$ SIS-P2! ;

: SIS-COMBINED-P0$ ( -- ptr u8 n )
   SIS-CAPTURE-DIR$ s" combined.ndjson" SIS-P0 JOIN-PATH
   SIS-P0 swap ;

: SIS-RUNNER-SUMMARY-P0$ ( -- ptr u8 n )
   SIS-CAPTURE-DIR$ s" runner_summary.md" SIS-P0 JOIN-PATH
   SIS-P0 swap ;

: SIS-VALIDATE-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   SB-RESET
   a u SIS-ROOTED-SB+
   SB$ FILE? 0= if E-FS-OPEN throw then ;

: SIS-CHECK-NUMBERS ( -- )
   SIS-FPS$ SIS-ASSERT-NONNEG
   SIS-DURATION$ SIS-ASSERT-NONNEG
   SIS-WARMUP$ SIS-ASSERT-NONNEG
   SIS-METADATA$ SIS-ASSERT-NONNEG
   SIS-SAVE-EVERY$ SIS-ASSERT-NONNEG
   SIS-MAX-SAVED$ SIS-ASSERT-NONNEG
   SIS-PRE-DELAY$ SIS-ASSERT-NONNEG
   SIS-MANUAL-EXPOSURE$ SIS-ASSERT-OPTIONAL-NONNEG
   SIS-MANUAL-GAIN$ SIS-ASSERT-OPTIONAL-NONNEG ;

: SIS-FINALIZE ( -- )
   SIS-SCENARIO-U @ 0= if SIS-DIE-USAGE then
   SIS-SCENARIO$ SIS-CHECK-NAME
   SIS-TAG$ SIS-CHECK-NAME
   SIS-OUTPUT-ID-U @ 0= if SIS-BUILD-OUTPUT-ID then
   SIS-OUTPUT-ID$ SIS-CHECK-NAME
   SIS-MANUAL-EXPOSURE-U @ 0 > SIS-MANUAL-GAIN-U @ 0= and if s" 1000" SIS-MANUAL-GAIN! then
   SIS-CHECK-NUMBERS
   SIS-EXPOSURE-MANIFEST$ SIS-VALIDATE-FILE
   SIS-LOW-LIGHT-MANIFEST$ SIS-VALIDATE-FILE
   SIS-MOTION-MANIFEST$ SIS-VALIDATE-FILE ;

: SIS-SET-SCENARIO ( ptr u8 n -- )
   SIS-SCENARIO-U @ 0 <> if 2drop SIS-DIE-USAGE then
   SIS-SCENARIO! ;

: SIS-OPTION ( -- )
   SIS-ARG$ s" -h" STR= if SIS-DIE-USAGE then
   SIS-ARG$ s" --help" STR= if SIS-DIE-USAGE then
   SIS-ARG$ s" --dry-run" STR= if 1 SIS-DRY-RUN ! 1 SIS-ADVANCE exit then
   SIS-ARG$ s" --odin-root" STR= if SIS-VALUE$ SIS-ROOT! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --resolution" STR= if SIS-VALUE$ SIS-RESOLUTION! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --fps" STR= if SIS-VALUE$ SIS-FPS! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --duration-ms" STR= if SIS-VALUE$ SIS-DURATION! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --warmup-ms" STR= if SIS-VALUE$ SIS-WARMUP! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --metadata-every" STR= if SIS-VALUE$ SIS-METADATA! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --save-every" STR= if SIS-VALUE$ SIS-SAVE-EVERY! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --max-saved-frames" STR= if SIS-VALUE$ SIS-MAX-SAVED! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --manual-exposure-us" STR= if SIS-VALUE$ SIS-MANUAL-EXPOSURE! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --manual-gain" STR= if SIS-VALUE$ SIS-MANUAL-GAIN! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --exposure-manifest" STR= if SIS-VALUE$ SIS-EXPOSURE-MANIFEST! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --low-light-manifest" STR= if SIS-VALUE$ SIS-LOW-LIGHT-MANIFEST! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --motion-manifest" STR= if SIS-VALUE$ SIS-MOTION-MANIFEST! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --pre-capture-delay-s" STR= if SIS-VALUE$ SIS-PRE-DELAY! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --pre-capture-cue" STR= if SIS-VALUE$ SIS-PRE-CUE! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --tag" STR= if SIS-VALUE$ SIS-TAG! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --output-id" STR= if SIS-VALUE$ SIS-OUTPUT-ID! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --results-root" STR= if SIS-VALUE$ SIS-RESULTS-ROOT! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --capture-abi" STR= if SIS-VALUE$ SIS-CAPTURE-ABI! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --config" STR= if SIS-VALUE$ SIS-CONFIG! 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --camera" STR= if SIS-VALUE$ HCAP:CAMERA+ 2 SIS-ADVANCE exit then
   SIS-ARG$ s" --no-exposure" STR= if 0 SIS-RUN-EXPOSURE ! 1 SIS-ADVANCE exit then
   SIS-ARG$ s" --no-low-light" STR= if 0 SIS-RUN-LOW-LIGHT ! 1 SIS-ADVANCE exit then
   SIS-ARG$ s" --no-motion-blur" STR= if 0 SIS-RUN-MOTION-BLUR ! 1 SIS-ADVANCE exit then
   SIS-ARG$ s" --no-sync" STR= if 0 SIS-RUN-SYNC ! 1 SIS-ADVANCE exit then
   SIS-ARG$ s" --allow-frame-phase-offset" STR= if 0 SIS-REQUIRE-SYNC ! 1 SIS-REQUIRE-PAIRING ! 1 SIS-ADVANCE exit then
   SIS-ARG$ s" --allow-sync-characterization" STR= if 0 SIS-REQUIRE-SYNC ! 0 SIS-REQUIRE-PAIRING ! 1 SIS-ADVANCE exit then
   SIS-ARG$ s" --" STARTS-WITH? if SIS-DIE-USAGE then
   SIS-ARG$ SIS-SET-SCENARIO
   1 SIS-ADVANCE ;

: SIS-PARSE ( -- )
   SIS-DEFAULTS
   0 SIS-ARG-I !
   SCRIPT-ARGC 0= if SIS-DIE-USAGE then
   begin SIS-ARG-I @ SCRIPT-ARGC < while SIS-OPTION repeat
   SIS-FINALIZE ;

: SIS-BOOL$ ( bool -- ptr u8 n ) if s" yes" else s" no" then ;
: SIS-MAYBE$ ( ptr u8 n -- ptr u8 n ) dup 0= if 2drop s" not supplied" then ;

: SIS-MD-S ( ptr u8 n ptr u8 n -- ) MD-S ;
: SIS-MD-B ( ptr u8 n bool -- ) {: label:ptr labelu:n b:bool :} label labelu b SIS-BOOL$ MD-S ;
: SIS-MD-PATH ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n a:ptr u:n :}
   label labelu a u SIS-MAYBE$ MD-S ;

: SIS-SUMMARY$ ( -- ptr u8 n )
   RB-RESET
   s" # Saved-Image Scenario Runner" RB+ RB-NL RB-NL
   s" execution mode" SIS-DRY-RUN @ if s" dry-run" else s" live-requested" then SIS-MD-S
   s" scenario" SIS-SCENARIO$ SIS-MD-S
   s" output id" SIS-OUTPUT-ID$ SIS-MD-S
   s" Odin root" SIS-ROOT$ SIS-MD-S
   s" results root" SIS-RESULTS-ROOT$ SIS-MD-S
   s" capture ABI" SIS-CAPTURE-ABI$ SIS-MD-S
   s" camera config" SIS-CONFIG$ SIS-MD-S
   s" capture" SIS-CAPTURE-DIR$ SIS-MD-S
   s" exposure" SIS-EXPOSURE-DIR$ SIS-MD-S
   s" low-light" SIS-LOW-LIGHT-DIR$ SIS-MD-S
   s" motion-blur" SIS-MOTION-BLUR-DIR$ SIS-MD-S
   s" sync" SIS-SYNC-DIR$ SIS-MD-S
   s" resolution" SIS-RESOLUTION$ SIS-MD-S
   s" fps" SIS-FPS$ SIS-MD-S
   s" duration ms" SIS-DURATION$ SIS-MD-S
   s" warmup ms" SIS-WARMUP$ SIS-MD-S
   s" metadata every" SIS-METADATA$ SIS-MD-S
   s" save every" SIS-SAVE-EVERY$ SIS-MD-S
   s" max saved frames" SIS-MAX-SAVED$ SIS-MD-S
   s" pre-capture delay s" SIS-PRE-DELAY$ SIS-MD-S
   s" pre-capture cue" SIS-PRE-CUE$ SIS-MD-PATH
   s" manual exposure us" SIS-MANUAL-EXPOSURE$ SIS-MD-PATH
   s" manual gain" SIS-MANUAL-GAIN$ SIS-MD-PATH
   s" exposure manifest" SIS-EXPOSURE-MANIFEST$ SIS-MD-PATH
   s" low-light manifest" SIS-LOW-LIGHT-MANIFEST$ SIS-MD-PATH
   s" motion manifest" SIS-MOTION-MANIFEST$ SIS-MD-PATH
   s" run exposure" SIS-RUN-EXPOSURE @ SIS-MD-B
   s" run low-light" SIS-RUN-LOW-LIGHT @ SIS-MD-B
   s" run motion-blur" SIS-RUN-MOTION-BLUR @ SIS-MD-B
   s" run sync" SIS-RUN-SYNC @ SIS-MD-B
   s" strict sync" SIS-REQUIRE-SYNC @ SIS-MD-B
   s" require timestamp pairing" SIS-REQUIRE-PAIRING @ SIS-MD-B
   RB-NL
   SIS-DRY-RUN @ if
      s" Capture/analyzer processes started: none." RB+ RB-NL
      s" Live execution will use the Habu camera-capture backend." RB+ RB-NL
   else
      s" Capture backend: Habu CameraOne capture." RB+ RB-NL
      s" Analyzer execution: Habu saved-image analyzers." RB+ RB-NL
   then
   RB$ ;

: SIS-ROOTED$ ( ptr u8 n -- ptr u8 n )
   SB-RESET
   SIS-ROOTED-SB+
   SB$ ;

: SIS-CAPTURE-RUN ( -- )
   HCAP:RESET
   SIS-CAPTURE-ABI$ HCAP:ABI!
   SIS-CONFIG$ SIS-ROOTED$ HCAP:CONFIG!
   SIS-CAPTURE-DIR$ HCAP:OUTPUT!
   SIS-RESOLUTION$ HCAP:RESOLUTION!
   SIS-FPS$ HCAP:FPS!
   SIS-DURATION$ HCAP:DURATION-MS!
   SIS-WARMUP$ HCAP:WARMUP-MS!
   SIS-METADATA$ HCAP:METADATA-EVERY!
   SIS-SAVE-EVERY$ HCAP:SAVE-EVERY!
   SIS-MAX-SAVED$ HCAP:MAX-SAVED-FRAMES!
   SIS-MANUAL-EXPOSURE$ HCAP:MANUAL-EXPOSURE!
   SIS-MANUAL-GAIN$ HCAP:MANUAL-GAIN!
   HCAP:RUN ;

: SIS-STATUS-RESET ( -- )
   SIS-SKIPPED SIS-CAPTURE-STATUS !
   SIS-SKIPPED SIS-EXPOSURE-STATUS !
   SIS-SKIPPED SIS-LOW-LIGHT-STATUS !
   SIS-SKIPPED SIS-MOTION-BLUR-STATUS !
   SIS-SKIPPED SIS-SYNC-STATUS !
   0 SIS-FIRST-FAILURE ! ;

: SIS-RECORD-STATUS ( n -- )
   dup SIS-SKIPPED = if drop exit then
   dup 0 <> SIS-FIRST-FAILURE @ 0= and if SIS-FIRST-FAILURE ! else drop then ;

: SIS-CAPTURE-READY? ( -- bool )
   SIS-CAPTURE-STATUS @ 0= if
      SIS-COMBINED-P0$ FILE?
   else
      SIS-FALSE
   then ;

: SIS-RUN-EXPOSURE-ANALYZER ( -- n )
   SIS-COMBINED-P0$ SIS-CAPTURE-P1$ SIS-EXPOSURE-P2$ SIA:ANALYZE-EXPOSURE ;

: SIS-RUN-LOW-LIGHT-ANALYZER ( -- n )
   SIS-COMBINED-P0$ SIS-CAPTURE-P1$ SIS-LOW-LIGHT-P2$ SIA:ANALYZE-LOW-LIGHT ;

: SIS-RUN-MOTION-BLUR-ANALYZER ( -- n )
   SIS-COMBINED-P0$ SIS-CAPTURE-P1$ SIS-MOTION-BLUR-P2$ SIA:ANALYZE-MOTION-BLUR ;

: SIS-RUN-SYNC-ANALYZER ( -- n )
   SIS-COMBINED-P0$ SIS-SYNC-P2$ SIS-REQUIRE-SYNC @ SIS-REQUIRE-PAIRING @ SIA:ANALYZE-SYNC ;

: SIS-CALL-EXPOSURE ( -- )
   SIS-RUN-EXPOSURE-ANALYZER SIS-EXPOSURE-STATUS ! ;

: SIS-CALL-LOW-LIGHT ( -- )
   SIS-RUN-LOW-LIGHT-ANALYZER SIS-LOW-LIGHT-STATUS ! ;

: SIS-CALL-MOTION-BLUR ( -- )
   SIS-RUN-MOTION-BLUR-ANALYZER SIS-MOTION-BLUR-STATUS ! ;

: SIS-CALL-SYNC ( -- )
   SIS-RUN-SYNC-ANALYZER SIS-SYNC-STATUS ! ;

: SIS-MD-STATUS ( ptr u8 n n -- ) {: label:ptr labelu:n st:n :}
   s" - " RB+ label labelu RB+ s" : " RB+
   st SIS-SKIPPED = if s" skipped" RB+ else st RB# then
   RB-NL ;

: SIS-WRITE-RUNNER-SUMMARY ( -- )
   RB-RESET
   s" # Saved-Image Scenario Runner" RB+ RB-NL RB-NL
   s" scenario" SIS-SCENARIO$ MD-S
   s" output id" SIS-OUTPUT-ID$ MD-S
   s" results root" SIS-RESULTS-ROOT$ MD-S
   s" capture" SIS-CAPTURE-DIR$ MD-S
   s" exposure" SIS-EXPOSURE-DIR$ MD-S
   s" low-light" SIS-LOW-LIGHT-DIR$ MD-S
   s" motion-blur" SIS-MOTION-BLUR-DIR$ MD-S
   s" sync" SIS-SYNC-DIR$ MD-S
   s" resolution" SIS-RESOLUTION$ MD-S
   s" fps" SIS-FPS$ MD-S
   s" duration ms" SIS-DURATION$ MD-S
   s" warmup ms" SIS-WARMUP$ MD-S
   s" metadata every" SIS-METADATA$ MD-S
   s" save every" SIS-SAVE-EVERY$ MD-S
   s" max saved frames" SIS-MAX-SAVED$ MD-S
   s" pre-capture delay s" SIS-PRE-DELAY$ MD-S
   s" pre-capture cue" SIS-PRE-CUE$ SIS-MAYBE$ MD-S
   s" manual exposure us" SIS-MANUAL-EXPOSURE$ SIS-MAYBE$ MD-S
   s" manual gain" SIS-MANUAL-GAIN$ SIS-MAYBE$ MD-S
   s" motion manifest" SIS-MOTION-MANIFEST$ SIS-MAYBE$ MD-S
   s" exposure manifest" SIS-EXPOSURE-MANIFEST$ SIS-MAYBE$ MD-S
   s" low-light manifest" SIS-LOW-LIGHT-MANIFEST$ SIS-MAYBE$ MD-S
   s" sync readiness mode" SIS-REQUIRE-SYNC @ if s" strict" else SIS-REQUIRE-PAIRING @ if s" timestamp-pairing-only" else s" characterization" then then MD-S
   s" capture exit status" SIS-CAPTURE-STATUS @ SIS-MD-STATUS
   s" exposure exit status" SIS-EXPOSURE-STATUS @ SIS-MD-STATUS
   s" low-light exit status" SIS-LOW-LIGHT-STATUS @ SIS-MD-STATUS
   s" motion-blur exit status" SIS-MOTION-BLUR-STATUS @ SIS-MD-STATUS
   s" sync exit status" SIS-SYNC-STATUS @ SIS-MD-STATUS
   s" first nonzero exit status" SIS-FIRST-FAILURE @ MD-N
   RB-NL
   s" Strict readiness or capture failures preserve this runner index plus any capture/analyzer artifacts written before exit." RB+ RB-NL
   SIS-RUNNER-SUMMARY-P0$ RB$ WRITE-ALL ;

: SIS-LIVE-RUN ( -- )
   SIS-STATUS-RESET
   [: SIS-CAPTURE-RUN ;] catch SIS-CAPTURE-STATUS !
   SIS-CAPTURE-STATUS @ SIS-RECORD-STATUS
   SIS-CAPTURE-READY? 0= if
      SIS-CAPTURE-STATUS @ 0= if SIS-FIRST-FAILURE @ 0= if 2 SIS-FIRST-FAILURE ! then then
   else
      SIS-RUN-EXPOSURE @ 0 <> if
         [: SIS-CALL-EXPOSURE ;] catch dup 0 <> if SIS-EXPOSURE-STATUS ! else drop then
         SIS-EXPOSURE-STATUS @ SIS-RECORD-STATUS
      then
      SIS-RUN-LOW-LIGHT @ 0 <> if
         [: SIS-CALL-LOW-LIGHT ;] catch dup 0 <> if SIS-LOW-LIGHT-STATUS ! else drop then
         SIS-LOW-LIGHT-STATUS @ SIS-RECORD-STATUS
      then
      SIS-RUN-MOTION-BLUR @ 0 <> if
         [: SIS-CALL-MOTION-BLUR ;] catch dup 0 <> if SIS-MOTION-BLUR-STATUS ! else drop then
         SIS-MOTION-BLUR-STATUS @ SIS-RECORD-STATUS
      then
      SIS-RUN-SYNC @ 0 <> if
         [: SIS-CALL-SYNC ;] catch dup 0 <> if SIS-SYNC-STATUS ! else drop then
         SIS-SYNC-STATUS @ SIS-RECORD-STATUS
      then
   then
   SIS-WRITE-RUNNER-SUMMARY
   s" done: " type SIS-OUTPUT-ID$ type cr
   SIS-FIRST-FAILURE @ 0 <> if s" saved-image scenario failed" SIS-FIRST-FAILURE @ die then ;

: SIS-RUN ( -- )
   SIS-SUMMARY$ type
   SIS-DRY-RUN @ 0= if SIS-LIVE-RUN then ;

: SIS-MAIN ( -- )
   SIS-PARSE
   SIS-RUN ;

end-package
