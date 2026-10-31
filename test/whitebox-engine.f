\ whitebox-engine.f - the unsealed engine whitebox suites run on, built once per
\ gate and shared. The shipped image marks each record with no checker-known
\ effect DNAME-INT (src/core/internal-mark.f), so interpret and tick fail closed
\ on the bare token, and a TRUSTED: wrapper does not reopen it. A suite asks the
\ engine builder for --engine --whitebox; the product is published as
\ img/whitebox-engine under its content digest and never installed over bin/hb.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/image-grant.f
require test/compiler-images.f
require src/lib/content-key.f
require tools/build-out.f
require tools/keyed-image.f

package WHITEBOX-ENGINE

$10000 constant IO-CAP

create PATH-BUF PATH-CAP allot
create WORK-BUF PATH-CAP allot
create TMP-BUF PATH-CAP allot
create BUNDLE-BUF PATH-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot
create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create ENGINE-KEY KEYED-IMAGE:KEY-HEX-LEN allot   \ the candidate's, naming its img/

variable PATH-U
variable WORK-U
variable TMP-U
variable BUNDLE-U
variable EMIT-RC
variable READY?

\ The exit status of a build that produced no executable image.
75 constant WB-RC

: PATH-BYTES ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: WORK-BYTES ( -- ptr u8 n )
   WORK-BUF WORK-U @ ;

: TMP-BYTES ( -- ptr u8 n )
   TMP-BUF TMP-U @ ;

: BUNDLE-ROOT$ ( -- ptr u8 n )
   COMPILER-IMAGES:ROOT$ ;

: BUNDLE? ( -- bool )
   BUNDLE-ROOT$ nip 0 > ;

: BUNDLE-PATH$ ( -- ptr u8 n )
   BUNDLE-ROOT$ s" hb-whitebox-source" BUNDLE-BUF FS:JOIN-PATH BUNDLE-U !
   BUNDLE-BUF BUNDLE-U @ ;

\ The family name: the image's name under img/, the stem of its work
\ directories, and the name a gate grants the engine under (test/image-grant.f).
: FAMILY$ ( -- ptr u8 n )
   s" whitebox-engine" ;

: COPY-OUT! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u 0 <= if E-FS-PATH throw then
   u PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

\ The builder writes into a work directory of its own (KEYED-IMAGE:WORK-OPEN),
\ so a half-written image is never visible at the family's path: only
\ KEYED-IMAGE:PUBLISH puts it there.
: WORK-OPEN ( -- )
   FAMILY$ ENGINE-KEY KEYED-IMAGE:WORK-OPEN WORK-BUF WORK-U COPY-OUT!
   WORK-BYTES FAMILY$ TMP-BUF FS:JOIN-PATH TMP-U ! ;

: ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: BUILD-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   s" tools/hb-build.f" ARG
   s" --" ARG
   s" --engine" ARG
   s" --whitebox" ARG
   s" -o" ARG
   TMP-BYTES ARG ;

: BUILD-RUN ( -- )
   BUILD-ARGS
   ENGINE-CANDIDATE:PATH$ >LEN s" " >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop
   \ A deadline that expired in the build (tools/native-build-args.f) is named
   \ and thrown again.
   rc PROC-TIMEOUT-RC = if
      s" whitebox-engine: unsealed engine build ran out of time" type cr
      E-PROC-TIMEOUT throw
   then
   rc 0 <> if s" whitebox-engine: unsealed engine build failed" rc die then ;

: PRODUCT-KEY-BODY ( CONTENT-KEY:fold -- )
   s" whitebox-product-v1" CONTENT-KEY:TEXT+
   TMP-BYTES s" product" CONTENT-KEY:FILE-NAMED+
   KEY-HEX CONTENT-KEY:FINAL-HEX ;

: PUBLISH ( -- )
   TMP-BYTES FS:EXECUTABLE? 0= if
      s" whitebox-engine: builder produced no executable image" WB-RC die
   then
   [: PRODUCT-KEY-BODY ;] CONTENT-KEY:WITH-FOLD
   KEY-HEX FAMILY$ ENGINE-KEY TMP-BYTES KEYED-IMAGE:PUBLISH
   FAMILY$ ENGINE-KEY PATH-BUF PATH-U KEYED-IMAGE:PATH! ;

\ A same-key racer keeps the image KEYED-IMAGE:PUBLISH put there first. The
\ work directory goes on every outcome; a die leaves it to the exit registry.
: EMIT ( -- )
   ENGINE-CANDIDATE:PATH$ ENGINE-KEY BUILD-OUT:KEY-OF!
   WORK-OPEN
   ['] BUILD-RUN catch EMIT-RC !
   EMIT-RC @ 0 = if ['] PUBLISH catch EMIT-RC ! then
   WORK-BYTES KEYED-IMAGE:WORK-CLOSE
   EMIT-RC @ 0 <> if EMIT-RC @ throw then ;

public

\ The unified builder validates source inputs before this process adopts a
\ product. Later calls share that immutable result while it remains published; a
\ missing supplier is materialized again. Bundle rows use the gate's supplier.
\ Under a gate the engine must be granted (test/image-grant.f): the gate's
\ whitebox-engine build row settles it and copies it for the WHITEBOX-SUITE rows
\ (test/gate-images.f); a row fetching its own copy starts after that row
\ passed.
: ENSURE ( -- )
   BUNDLE? if
      BUNDLE-PATH$ FS:EXECUTABLE? 0= if
         s" whitebox-engine: suite bundle has no whitebox image" WB-RC die
      then
      exit
   then
   FAMILY$ IMAGE-GRANT:CHECK
   READY? @ if KEY-HEX FAMILY$ ENGINE-KEY KEYED-IMAGE:FRESH? if exit then then
   EMIT
   true READY? ! ;

\ The keyed artifact, for a caller that only wants to name it.
: PATH$ ( -- ptr u8 n )
   ENSURE
   BUNDLE? if BUNDLE-PATH$ exit then
   PATH-BYTES ;

\ A private copy at the caller's path, so no suite runs from or clobbers the
\ shared bytes.
: PROVIDE ( ptr u8 n -- ) {: dst:ptr dstu:n :}
   ENSURE
   dst dstu FS:EXISTS? if dst dstu FS:REMOVE-FILE then
   BUNDLE? if BUNDLE-PATH$ else PATH-BYTES then
   dst dstu FS:COPY-FILE-STREAM
   dst dstu FS:CHMOD-X ;

;package
