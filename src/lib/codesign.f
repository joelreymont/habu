\ codesign.f - checked executable promotion and target signing, in `package
\ CODESIGN`, for build drivers that already produced an artifact.
\
\ On macOS VERIFY, FORCE, ENSURE and SIGN-AS run CODESIGN:TOOL
\ (/usr/bin/codesign, ad-hoc `-s -`) through the checked argv process layer;
\ an absent tool throws E-BUILD-COMMAND and a nonzero status E-BUILD-STATUS.
\ On Linux there is no signing tool: FORCE sets the executable bit and VERIFY
\ checks that the file exists and is executable. ENSURE leaves a verified
\ executable; on macOS it forces an ad-hoc signature only when verification
\ fails. PROMOTE-EXECUTABLE marks the source executable and renames it to the
\ destination, so the source path is gone on success;
\ PROMOTE-SIGNED-EXECUTABLE forces the source, promotes it and verifies the
\ result. A missing artifact or empty path throws E-BUILD-PATH; a target that
\ is neither Linux nor macOS E-BUILD-SOURCE.
\
\ STORAGE CLASS. PROCESS-WIDE: the capture buffers grow with the tool's
\ output and are released after each run; one task signs at a time.
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
   TOOL FILE? 0= if E-BUILD-COMMAND throw then ;

: CODESIGN-EXPECT-FILE ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-BUILD-PATH throw then
   a u FILE? 0= if E-BUILD-PATH throw then ;

: CODESIGN-EXPECT-EXECUTABLE ( ptr u8 n -- ) {: a:ptr u :}
   a u CODESIGN-EXPECT-FILE
   a u EXECUTABLE? 0= if E-BUILD-PATH throw then ;

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
   CHMOD-X ;

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
   src srcu CHMOD-X
   src srcu dst dstu RENAME-FILE
   dst dstu CODESIGN-EXPECT-FILE ;

: PROMOTE-SIGNED-EXECUTABLE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu FORCE
   src srcu dst dstu PROMOTE-EXECUTABLE
   dst dstu VERIFY ;

;package
