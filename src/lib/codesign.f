\ codesign.f - checked executable promotion, target signing and signed-image
\ comparison (package CODESIGN) for build drivers. On macOS the words run
\ /usr/bin/codesign ad hoc: no tool is E-BUILD-COMMAND, a nonzero status
\ E-BUILD-STATUS; ENSURE re-signs only when VERIFY fails. Linux has no signer:
\ FORCE sets the executable bit. A missing artifact or empty path is
\ E-BUILD-PATH, another target E-BUILD-SOURCE.
\ STORAGE CLASS. PROCESS-WIDE: per-run capture buffers; one task signs at a time.
require src/lib/le.f
require src/lib/be.f
require src/lib/errors.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f

package CODESIGN

private

10000 constant CODESIGN-TIMEOUT-MS

DYNAMIC-BUFFER CODESIGN-OUT u8
DYNAMIC-BUFFER CODESIGN-ERR u8

: CODESIGN-OUT-ROOM ( n -- ptr u8 )
   CODESIGN-OUT-RESERVE 0 CODESIGN-OUT ;

: CODESIGN-ERR-ROOM ( n -- ptr u8 )
   CODESIGN-ERR-RESERVE 0 CODESIGN-ERR ;

public

: TOOL ( -- ptr u8 n )
   s" /usr/bin/codesign" ;

private

: CODESIGN-RC0 ( n -- )
   0 <> if E-BUILD-STATUS throw then ;

: CODESIGN-EXPECT-TOOL ( -- )
   TOOL FS:FILE? 0= if E-BUILD-COMMAND throw then ;

: CODESIGN-EXPECT-FILE ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-BUILD-PATH throw then
   a u FS:FILE? 0= if E-BUILD-PATH throw then ;

: CODESIGN-EXPECT-EXECUTABLE ( ptr u8 n -- ) {: a:ptr u :}
   a u CODESIGN-EXPECT-FILE
   a u FS:EXECUTABLE? 0= if E-BUILD-PATH throw then ;

: CODESIGN-CAPTURE ( -- n )
   PROC-ENV-RESET
   TOOL >LEN s" " >LEN
   [: CODESIGN-OUT-ROOM ;] [: CODESIGN-ERR-ROOM ;]
   CODESIGN-TIMEOUT-MS >MS RUN-ARGV-ENV-STDIN-GROWING-BOTH-CAPTURE-OUTCOME
   {: o:len e:len oc :}
   oc PROC-OUTCOME>RC RC>N ;

: CODESIGN-RELEASE ( -- )
   CODESIGN-OUT-RELEASE
   CODESIGN-ERR-RELEASE ;

: CODESIGN-RUN ( -- n )
   CODESIGN-EXPECT-TOOL
   [: CODESIGN-CAPTURE ;] [: CODESIGN-RELEASE ;] finally ;

: CODESIGN-TARGET-UNKNOWN ( -- )
   E-BUILD-SOURCE throw ;

: CODESIGN-MACOS-VERIFY-RC ( ptr u8 n -- n ) {: a:ptr u :}
   a u CODESIGN-EXPECT-EXECUTABLE
   PROC-ARGV-RESET
   s" -v"  >LEN PROC-ARGV+
   a u  >LEN PROC-ARGV+
   CODESIGN-RUN ;

: CODESIGN-MACOS-FORCE ( ptr u8 n -- ) {: a:ptr u :}
   a u CODESIGN-EXPECT-FILE
   PROC-ARGV-RESET
   s" -s"  >LEN PROC-ARGV+
   s" -"  >LEN PROC-ARGV+
   s" --force"  >LEN PROC-ARGV+
   a u  >LEN PROC-ARGV+
   CODESIGN-RUN CODESIGN-RC0 ;

: CODESIGN-MACOS-FORCE-AS ( ptr u8 n ptr u8 n -- ) {: a:ptr u id:ptr idu :}
   a u CODESIGN-EXPECT-FILE
   PROC-ARGV-RESET
   s" -s"  >LEN PROC-ARGV+
   s" -"  >LEN PROC-ARGV+
   s" -i"  >LEN PROC-ARGV+
   id idu  >LEN PROC-ARGV+
   s" --force"  >LEN PROC-ARGV+
   a u  >LEN PROC-ARGV+
   CODESIGN-RUN CODESIGN-RC0 ;

: CODESIGN-LINUX-VERIFY-RC ( ptr u8 n -- n )
   CODESIGN-EXPECT-EXECUTABLE
   0 ;

: CODESIGN-LINUX-FORCE ( ptr u8 n -- )
   2dup CODESIGN-EXPECT-FILE
   FS:CHMOD-X ;

public

: VERIFY-RC ( ptr u8 n -- n ) {: a:ptr u:n :}
   HB-TARGET-LINUX-KERNEL? if a u CODESIGN-LINUX-VERIFY-RC exit then
   HB-TARGET-MACOS? if a u CODESIGN-MACOS-VERIFY-RC exit then
   CODESIGN-TARGET-UNKNOWN ;

: VERIFY ( ptr u8 n -- ) {: a:ptr u:n :}
   HB-TARGET-LINUX-KERNEL? if
      a u CODESIGN-EXPECT-EXECUTABLE
      exit
   then
   HB-TARGET-MACOS? if
      a u VERIFY-RC CODESIGN-RC0
      exit
   then
   CODESIGN-TARGET-UNKNOWN ;

: FORCE ( ptr u8 n -- ) {: a:ptr u:n :}
   HB-TARGET-LINUX-KERNEL? if a u CODESIGN-LINUX-FORCE exit then
   HB-TARGET-MACOS? if a u CODESIGN-MACOS-FORCE exit then
   CODESIGN-TARGET-UNKNOWN ;

: ENSURE ( ptr u8 n -- ) {: a:ptr u:n :}
   HB-TARGET-LINUX-KERNEL? if
      a u FORCE
      a u VERIFY
      exit
   then
   HB-TARGET-MACOS? if
      a u VERIFY-RC 0= if exit then
      a u FORCE
      a u VERIFY
      exit
   then
   CODESIGN-TARGET-UNKNOWN ;

\ Sign under a fixed identifier, whatever the file already carries. Without one
\ codesign names the signature after the file, so the name a writer happened to
\ use would travel inside the image.
: SIGN-AS ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n id:ptr idu:n :}
   HB-TARGET-LINUX-KERNEL? if
      a u FORCE
      a u VERIFY
      exit
   then
   HB-TARGET-MACOS? if
      a u id idu CODESIGN-MACOS-FORCE-AS
      a u VERIFY
      exit
   then
   CODESIGN-TARGET-UNKNOWN ;

: PROMOTE-EXECUTABLE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu CODESIGN-EXPECT-FILE
   dstu 0 <= if E-BUILD-PATH throw then
   src srcu FS:CHMOD-X
   src srcu dst dstu FS:RENAME-FILE
   dst dstu CODESIGN-EXPECT-FILE ;

: PROMOTE-SIGNED-EXECUTABLE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu FORCE
   src srcu dst dstu PROMOTE-EXECUTABLE
   dst dstu VERIFY ;

private

\ ---- signed-image comparison ------------------------------------------------
\ An identity signature stamps its signing time into the CMS blob, so two
\ signings of one image differ there alone. Mach-O integers are little-endian,
\ code-signature integers big-endian (src/os/macos/sign2.f writes the layout).

$FEEDFACF constant MACHO64-MAGIC
32 constant MACHO64-HEADER
8 constant LC-HEAD
$1D constant LC-SIGNATURE
16 constant LC-SIGNATURE-SIZE
$FADE0CC0 constant SUPERBLOB-MAGIC
12 constant SUPERBLOB-HEAD
8 constant SLOT-ENTRY
8 constant BLOB-HEAD
$10000 constant CMS-SLOT

: SPAN-IN? ( n n n -- bool ) {: off:n len:n size:n :}
   off 0 >= len 0 >= and off len + size <= and ;

\ The offset of the image's LC_CODE_SIGNATURE, 0 when it is no 64-bit Mach-O
\ or carries none, -1 when a load command runs past the image or the signature
\ command is not LC-SIGNATURE-SIZE bytes.
: SIG-COMMAND ( ptr u8 n -- n ) {: a:ptr u:n :}
   u MACHO64-HEADER < if 0 exit then
   a LE:U32@ MACHO64-MAGIC <> if 0 exit then
   MACHO64-HEADER
   a 16 + LE:U32@ 0 ?do
      {: at:n :}
      at LC-HEAD u SPAN-IN? 0= if -1 unloop exit then
      a at + 4 + LE:U32@ {: size:n :}
      size LC-HEAD < at size u SPAN-IN? 0= or if -1 unloop exit then
      a at + LE:U32@ LC-SIGNATURE = if
         size LC-SIGNATURE-SIZE = if at else -1 then unloop exit
      then
      at size +
   loop
   drop 0 ;

\ The CMS blob's offset and length in the superblob at SIG, of SIGU bytes,
\ 0 0 when no slot names one; false when the index or blob leaves the region.
: CMS-SLOT-SPAN ( ptr u8 n n -- n n bool ) {: a:ptr sig:n sigu:n :}
   a sig + 8 + BE:U32@ {: count:n :}
   SUPERBLOB-HEAD count SLOT-ENTRY * sigu SPAN-IN? 0= if 0 0 false exit then
   count 0 ?do
      sig SUPERBLOB-HEAD + i SLOT-ENTRY * + {: entry:n :}
      a entry + BE:U32@ CMS-SLOT = if
         a entry + 4 + BE:U32@ {: off:n :}
         off BLOB-HEAD sigu SPAN-IN? 0= if 0 0 false unloop exit then
         a sig + off + 4 + BE:U32@ {: len:n :}
         off len sigu SPAN-IN? 0= if 0 0 false unloop exit then
         sig off + len true unloop exit
      then
   loop
   0 0 true ;

\ The CMS blob's offset and length in the image, 0 0 when it has none; false
\ when a read would leave the image: every load command, the signature and its
\ index are bounds-checked. The first LC_CODE_SIGNATURE and its first CMS slot
\ are used; real signers put the signature command last and the CMS blob after
\ the CodeDirectory.
: CMS-SPAN ( ptr u8 n -- n n bool ) {: a:ptr u:n :}
   a u SIG-COMMAND {: lc:n :}
   lc 0< if 0 0 false exit then
   lc 0= if 0 0 true exit then
   a lc + 8 + LE:U32@ {: sig:n :}
   a lc + 12 + LE:U32@ {: sigu:n :}
   sig sigu u SPAN-IN? 0= sigu SUPERBLOB-HEAD < or if 0 0 false exit then
   a sig + BE:U32@ SUPERBLOB-MAGIC <> if 0 0 false exit then
   a sig sigu CMS-SLOT-SPAN ;

public

\ True when the two images have one length and equal bytes outside the CMS
\ blob of their code signatures; an image with no signature is compared whole.
\ A signature that lies outside its image is never the same image.
: SAME-IMAGE? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr au:n b:ptr bu:n :}
   au bu <> if false exit then
   a au CMS-SPAN {: as:n al:n aok:bool :}
   b bu CMS-SPAN {: bs:n bl:n bok:bool :}
   aok bok and 0= if false exit then
   as bs <> al bl <> or if false exit then
   a as b as STR= 0= if false exit then
   as al + {: tail:n :}
   a tail + au tail - b tail + bu tail - STR= ;

;package
