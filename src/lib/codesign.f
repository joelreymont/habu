\ codesign.f - checked target signing and signed-image
\ comparison (package CODESIGN) for build drivers. On macOS the words run
\ /usr/bin/codesign with the identity of team TEAM$, never ad hoc: no tool is
\ E-BUILD-COMMAND, a nonzero status E-BUILD-STATUS, a host without the
\ identity E-IDENTITY. Linux has no signer, and an ELF file is a Linux
\ executable on any host: FORCE sets the executable bit. A missing artifact or
\ empty path is E-BUILD-PATH, another target E-BUILD-SOURCE.
\ STORAGE CLASS. PROCESS-WIDE: per-run capture buffers and the identity found
\ once per process; one task signs at a time.
require src/lib/le.f
require src/lib/be.f
require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fd.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f

package CODESIGN

private
-9650 constant E-FIRST
-9659 constant E-LAST
public
-9650 constant E-IDENTITY  \ a macOS host without the signing identity of team TEAM$

private

10000 constant CODESIGN-TIMEOUT-MS

DYNAMIC-BUFFER CODESIGN-OUT u8
DYNAMIC-BUFFER CODESIGN-ERR u8
variable CODESIGN-OUT-U
variable CODESIGN-ERR-U
DYNAMIC-BUFFER CODESIGN-IDS u8
variable CODESIGN-IDS-U

\ An identity as codesign -s names it: its certificate's SHA-1 in hex.
40 constant CODESIGN-ID-LEN
create CODESIGN-ID CODESIGN-ID-LEN allot
variable CODESIGN-ID?

: CODESIGN-OUT-ROOM ( n -- ptr u8 )
   CODESIGN-OUT-RESERVE 0 CODESIGN-OUT ;

: CODESIGN-ERR-ROOM ( n -- ptr u8 )
   CODESIGN-ERR-RESERVE 0 CODESIGN-ERR ;

public

: TOOL ( -- ptr u8 n )
   s" /usr/bin/codesign" ;

private

: SECURITY ( -- ptr u8 n )
   s" /usr/bin/security" ;

\ The team ID every macOS build signs for.
: TEAM$ ( -- ptr u8 n )
   s" 8TW82D98K7" ;

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

: CODESIGN-CAPTURE ( ptr u8 n -- n ) {: tool:ptr toolu:n :}
   PROC-ENV-RESET
   tool toolu >LEN s" " >LEN
   [: CODESIGN-OUT-ROOM ;] [: CODESIGN-ERR-ROOM ;]
   CODESIGN-TIMEOUT-MS >MS RUN-ARGV-ENV-STDIN-GROWING-BOTH-CAPTURE-OUTCOME
   {: o:len e:len oc :}
   o LEN>N CODESIGN-OUT-U !
   e LEN>N CODESIGN-ERR-U !
   oc PROCESS:ENDING>RC RC>N ;

\ A failing tool's own reason (a locked keychain, a denied dialog) goes to
\ stderr ahead of the status code.
: CODESIGN-ERR-SHOW ( n -- n ) {: rc:n :}
   rc 0<> if
      CODESIGN-ERR-U @ 0 > if 0 CODESIGN-ERR CODESIGN-ERR-U @ ERROR-TEXT:TYPE then
   then
   rc ;

: CODESIGN-RELEASE ( -- )
   CODESIGN-OUT-RELEASE
   CODESIGN-ERR-RELEASE ;

: CODESIGN-RUN ( -- n )
   CODESIGN-EXPECT-TOOL
   [: TOOL CODESIGN-CAPTURE ;] [: CODESIGN-RELEASE ;] finally ;

\ CODESIGN-RUN that throws on a nonzero status, the tool's stderr printed first.
: CODESIGN-RUN-OK ( -- )
   CODESIGN-EXPECT-TOOL
   [: TOOL CODESIGN-CAPTURE CODESIGN-ERR-SHOW CODESIGN-RC0 ;] [: CODESIGN-RELEASE ;] finally ;

: CODESIGN-NO-IDENTITY ( -- )
   s" codesign: no valid code-signing identity of team " ERROR-TEXT:TYPE
   TEAM$ ERROR-TEXT:TYPE
   s" ; `security find-identity -v -p codesigning` lists the valid ones" ERROR-TEXT:TYPE
   s\" \n" ERROR-TEXT:TYPE
   E-IDENTITY throw ;

\ Runs /usr/bin/security with PROC-ARGV, its stdout to xt. With no identity only
\ find-certificate exits nonzero; find-identity -v prints "0 valid identities found".
: CODESIGN-SECURITY ( [ ptr u8 n -- ] -- ) {: xt :}
   SECURITY CODESIGN-CAPTURE CODESIGN-ERR-SHOW 0<>
   CODESIGN-OUT-U @ 0= or if CODESIGN-NO-IDENTITY then
   0 CODESIGN-OUT CODESIGN-OUT-U @ xt execute ;

: CODESIGN-KEEP-IDS ( ptr u8 n -- ) {: a:ptr u:n :}
   u CODESIGN-IDS-RESERVE
   a 0 CODESIGN-IDS u BYTE-COPY
   u CODESIGN-IDS-U ! ;

: CODESIGN-IDS$ ( -- ptr u8 n )
   0 CODESIGN-IDS CODESIGN-IDS-U @ ;

\ The hashes of the valid signing identities, certificate and private key both
\ in the keychain search list.
: CODESIGN-LIST-IDS ( -- )
   PROC-ARGV-RESET
   s" find-identity" >LEN PROC-ARGV+
   s" -v" >LEN PROC-ARGV+
   s" -p" >LEN PROC-ARGV+
   s" codesigning" >LEN PROC-ARGV+
   [: [: CODESIGN-KEEP-IDS ;] CODESIGN-SECURITY ;] [: CODESIGN-RELEASE ;] finally ;

\ find-certificate -Z prints each certificate from its "SHA-1 hash: " line on,
\ its subject's DER bytes escaped: printable bytes as themselves, others \ooo.
: CODESIGN-CERT-MARK ( -- ptr u8 n )
   s" SHA-1 hash: " ;

\ The subject's line; the issuer's names its own organizational unit.
: CODESIGN-SUBJ-MARK ( -- ptr u8 n )
   s\" \"subj\"<blob>=" ;

\ The subject's organizational unit, OID 2.5.4.11, escaped; the string's tag
\ and its length 10 follow as two \ooo, then the team.
: CODESIGN-OU-MARK ( -- ptr u8 n )
   s" U\004\013" ;

8 constant CODESIGN-OU-SKIP

\ The offset of b in a at or after from, else u.
: CODESIGN-FIND ( n ptr u8 n ptr u8 n -- n ) {: from:n a:ptr u:n b:ptr v:n :}
   a u b v from FIND-SUB-FROM MATCH option
     none OF u ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

\ Whether the OU mark at offset at is followed by TEAM$.
: CODESIGN-TEAM-AT? ( n ptr u8 n -- bool ) {: at:n a:ptr u:n :}
   at CODESIGN-OU-MARK nip + CODESIGN-OU-SKIP + {: team:n :}
   team TEAM$ nip + u > if false exit then
   a team + TEAM$ nip TEAM$ STR= ;

: CODESIGN-TEAM-IN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin
      a u CODESIGN-OU-MARK CODESIGN-FIND
      dup u <
   while
      dup a u CODESIGN-TEAM-AT? if drop true exit then
      1 +
   repeat
   drop false ;

\ Whether the certificate's subject names TEAM$.
: CODESIGN-TEAM-CERT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 a u CODESIGN-SUBJ-MARK CODESIGN-FIND {: subj:n :}
   subj u >= if false exit then
   subj a u s\" \n" CODESIGN-FIND {: eol:n :}
   a subj + eol subj - CODESIGN-TEAM-IN? ;

\ Keeps the hash of the certificate from at to next when it is the team's and
\ a valid identity.
: CODESIGN-PICK-AT ( n n ptr u8 n -- bool ) {: at:n next:n a:ptr u:n :}
   at CODESIGN-CERT-MARK nip + {: hash:n :}
   hash CODESIGN-ID-LEN + next > if false exit then
   a at + next at - CODESIGN-TEAM-CERT? 0= if false exit then
   CODESIGN-IDS$ a hash + CODESIGN-ID-LEN CONTAINS? 0= if false exit then
   a hash + CODESIGN-ID CODESIGN-ID-LEN BYTE-COPY
   -1 CODESIGN-ID? !
   true ;

: CODESIGN-NEXT-CERT ( n ptr u8 n -- n ) {: at:n a:ptr u:n :}
   at CODESIGN-CERT-MARK nip + a u CODESIGN-CERT-MARK CODESIGN-FIND ;

: CODESIGN-PICK ( ptr u8 n -- ) {: a:ptr u:n :}
   0 a u CODESIGN-CERT-MARK CODESIGN-FIND
   begin dup u < while
      dup a u CODESIGN-NEXT-CERT
      2dup a u CODESIGN-PICK-AT if 2drop exit then
      nip
   repeat
   drop ;

: CODESIGN-FIND-ID ( -- )
   CODESIGN-LIST-IDS
   PROC-ARGV-RESET
   s" find-certificate" >LEN PROC-ARGV+
   s" -a" >LEN PROC-ARGV+
   s" -Z" >LEN PROC-ARGV+
   [: [: CODESIGN-PICK ;] CODESIGN-SECURITY ;] [: CODESIGN-RELEASE ;] finally
   CODESIGN-ID? @ 0= if CODESIGN-NO-IDENTITY then ;

public

\ The one identity every macOS signature names, found on first use.
: IDENTITY$ ( -- ptr u8 n )
   CODESIGN-ID? @ 0= if
      [: CODESIGN-FIND-ID ;] [: CODESIGN-IDS-RELEASE ;] finally
   then
   CODESIGN-ID CODESIGN-ID-LEN ;

private

: CODESIGN-TARGET-UNKNOWN ( -- )
   E-BUILD-SOURCE throw ;

: CODESIGN-MACOS-VERIFY-ARGS ( ptr u8 n -- ) {: a:ptr u :}
   a u CODESIGN-EXPECT-EXECUTABLE
   PROC-ARGV-RESET
   s" -v"  >LEN PROC-ARGV+
   a u  >LEN PROC-ARGV+ ;

: CODESIGN-REQ-HEAD ( -- ptr u8 n )
   s\" =anchor apple generic and certificate leaf[subject.OU] = \"" ;

create CODESIGN-REQ CODESIGN-REQ-HEAD nip TEAM$ nip + 1 + allot

\ The requirement every team signature meets: an Apple-issued certificate of
\ team TEAM$.
: CODESIGN-TEAM-REQ ( -- ptr u8 n )
   CODESIGN-REQ-HEAD {: h:ptr hu:n :}
   TEAM$ {: t:ptr tu:n :}
   h CODESIGN-REQ hu BYTE-COPY
   t CODESIGN-REQ hu + tu BYTE-COPY
   [char] " CODESIGN-REQ hu tu + + c!
   CODESIGN-REQ hu tu + 1 + ;

\ codesign -v -R exits nonzero for any other signer, Apple's and ad hoc
\ included, and for a file it cannot read.
: CODESIGN-MACOS-TEAM? ( ptr u8 n -- bool ) {: a:ptr u :}
   PROC-ARGV-RESET
   s" -v"  >LEN PROC-ARGV+
   s" -R"  >LEN PROC-ARGV+
   CODESIGN-TEAM-REQ  >LEN PROC-ARGV+
   a u  >LEN PROC-ARGV+
   CODESIGN-RUN 0= ;

: CODESIGN-SIGN-ARGS ( -- )
   IDENTITY$ {: id:ptr idu:n :}
   PROC-ARGV-RESET
   s" -s"  >LEN PROC-ARGV+
   id idu  >LEN PROC-ARGV+
   s" --timestamp=none"  >LEN PROC-ARGV+
   s" --force"  >LEN PROC-ARGV+ ;

\ A written image carries its writer's identifier (src/habu/sign-id.f); the
\ keychain's access list names it, so the team signature keeps it.
: CODESIGN-MACOS-FORCE ( ptr u8 n -- ) {: a:ptr u :}
   a u CODESIGN-EXPECT-FILE
   CODESIGN-SIGN-ARGS
   s" --preserve-metadata=identifier"  >LEN PROC-ARGV+
   a u  >LEN PROC-ARGV+
   CODESIGN-RUN-OK ;

: CODESIGN-MACOS-FORCE-AS ( ptr u8 n ptr u8 n -- ) {: a:ptr u id:ptr idu :}
   a u CODESIGN-EXPECT-FILE
   CODESIGN-SIGN-ARGS
   s" -i"  >LEN PROC-ARGV+
   id idu  >LEN PROC-ARGV+
   a u  >LEN PROC-ARGV+
   CODESIGN-RUN-OK ;

: CODESIGN-LINUX-VERIFY-RC ( ptr u8 n -- n )
   CODESIGN-EXPECT-EXECUTABLE
   0 ;

: CODESIGN-LINUX-FORCE ( ptr u8 n -- )
   2dup CODESIGN-EXPECT-FILE
   FS:CHMOD-X ;

4 SPAN-BUFFER: CODESIGN-MAGIC
variable CODESIGN-MAGIC-FD

: CODESIGN-ELF-MAG ( -- ptr u8 n )
   s\" \x7FELF" ;

: CODESIGN-MAGIC-READ ( -- bool )
   CODESIGN-MAGIC-FD @ >FD CODESIGN-MAGIC FD:READ-EXACT MATCH FD:fill
      full OF CODESIGN-MAGIC SPAN:$ CODESIGN-ELF-MAG STR= ENDOF
      eof OF drop false ENDOF
   ;MATCH ;

: CODESIGN-MAGIC-CLOSE ( -- )
   CODESIGN-MAGIC-FD @ close ;

: CODESIGN-ELF? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 <= if false exit then
   a u FS:FILE? 0= if false exit then
   a u FS:OPEN-SIZED {: f:fd size:n found:bool :}
   found 0= if false exit then
   f FD>N CODESIGN-MAGIC-FD !
   [: CODESIGN-MAGIC-READ ;] [: CODESIGN-MAGIC-CLOSE ;] finally ;

\ A Linux host, or an ELF file built for one on a Mac (--target).
: CODESIGN-LINUX? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   HB-TARGET-LINUX-KERNEL? if true exit then
   a u CODESIGN-ELF? ;

public

: VERIFY-RC ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u CODESIGN-LINUX? if a u CODESIGN-LINUX-VERIFY-RC exit then
   HB-TARGET-MACOS? if a u CODESIGN-MACOS-VERIFY-ARGS CODESIGN-RUN exit then
   CODESIGN-TARGET-UNKNOWN ;

: VERIFY ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CODESIGN-LINUX? if
      a u CODESIGN-EXPECT-EXECUTABLE
      exit
   then
   HB-TARGET-MACOS? if
      a u CODESIGN-MACOS-VERIFY-ARGS
      CODESIGN-RUN-OK
      exit
   then
   CODESIGN-TARGET-UNKNOWN ;

: FORCE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CODESIGN-LINUX? if a u CODESIGN-LINUX-FORCE exit then
   HB-TARGET-MACOS? if a u CODESIGN-MACOS-FORCE exit then
   CODESIGN-TARGET-UNKNOWN ;

\ Whether a file carries a valid signature of team TEAM$. An ELF file has no
\ signer: true when it is executable, E-BUILD-PATH when it is not.
: TEAM-SIGNED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u CODESIGN-LINUX? if a u CODESIGN-LINUX-VERIFY-RC 0= exit then
   HB-TARGET-MACOS? if a u CODESIGN-MACOS-TEAM? exit then
   CODESIGN-TARGET-UNKNOWN ;

\ Signs a file for the team unless it already carries a valid team signature,
\ so an install keeps the one its build made; a written image's own is ad hoc.
: ENSURE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CODESIGN-LINUX? 0= HB-TARGET-MACOS? and if
      a u TEAM-SIGNED? if exit then
   then
   a u FORCE
   a u VERIFY ;

\ Sign under a fixed identifier, whatever the file already carries. Without one
\ codesign names the signature after the file, so the name a writer happened to
\ use would travel inside the image.
: SIGN-AS ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n id:ptr idu:n :}
   a u CODESIGN-LINUX? if
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
8 constant SIG-DATAOFF
12 constant SIG-DATASIZE
$19 constant LC-SEGMENT-64
72 constant SEGMENT-SIZE
8 constant SEG-NAME
32 constant SEG-VMSIZE
48 constant SEG-FILESIZE
$FADE0CC0 constant SUPERBLOB-MAGIC
12 constant SUPERBLOB-HEAD
8 constant SLOT-ENTRY
8 constant BLOB-HEAD
$10000 constant CMS-SLOT

: SPAN-IN? ( n n n -- bool ) {: off:n len:n size:n :}
   off 0 >= len 0 >= and off len + size <= and ;

\ The offset of the first load command hit accepts; 0 for no 64-bit Mach-O or no
\ hit, -1 when a command up to that one runs past the image.
: COMMAND ( ptr u8 n [ n -- bool ] -- n ) {: a:ptr u:n hit :}
   u MACHO64-HEADER < if 0 exit then
   a LE:U32@ MACHO64-MAGIC <> if 0 exit then
   MACHO64-HEADER
   a 16 + LE:U32@ 0 ?do
      {: at:n :}
      at LC-HEAD u SPAN-IN? 0= if -1 unloop exit then
      a at + 4 + LE:U32@ {: size:n :}
      size LC-HEAD < at size u SPAN-IN? 0= or if -1 unloop exit then
      at hit execute if at unloop exit then
      at size +
   loop
   drop 0 ;

\ The offset of the image's LC_CODE_SIGNATURE, as COMMAND answers, and -1
\ when the command is not LC-SIGNATURE-SIZE bytes.
: SIG-COMMAND ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u [: a + LE:U32@ LC-SIGNATURE = ;] COMMAND {: at:n :}
   at 0 > if
      a at + 4 + LE:U32@ LC-SIGNATURE-SIZE <> if -1 exit then
   then
   at ;

: LINKEDIT-NAME ( -- ptr u8 n )
   s" __LINKEDIT" ;

\ Whether the load command at offset at is the __LINKEDIT segment's, its
\ 16-byte name zero-padded.
: LINKEDIT? ( ptr u8 n -- bool ) {: a:ptr at:n :}
   a at + LE:U32@ LC-SEGMENT-64 <> if false exit then
   a at + 4 + LE:U32@ SEGMENT-SIZE < if false exit then
   a at + SEG-NAME + LINKEDIT-NAME nip LINKEDIT-NAME STR= 0= if false exit then
   a at + SEG-NAME + LINKEDIT-NAME nip + c@ 0= ;

\ The offset of the image's __LINKEDIT segment command, as COMMAND answers.
: LINKEDIT-COMMAND ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u [: a swap LINKEDIT? ;] COMMAND ;

\ The offset past a field at at of len bytes when the images agree on the
\ bytes from from up to it, else -1; -1 when from is.
: SAME-TO ( n ptr u8 ptr u8 n n -- n ) {: from:n a:ptr b:ptr at:n len:n :}
   from 0< if -1 exit then
   a from + at from - b from + at from - STR= if at len + else -1 then ;

\ Whether the signature command at lc and the segment command at seg end
\ before the data offset doff.
: HEAD-BEFORE? ( n n n -- bool ) {: lc:n seg:n doff:n :}
   lc LC-SIGNATURE-SIZE + doff <=
   seg SEGMENT-SIZE + doff <= and ;

\ Whether the images agree on the bytes before the signature's data offset
\ doff but for its datasize and the vmsize and filesize of __LINKEDIT at seg.
: HEAD-SAME? ( ptr u8 ptr u8 n n n -- bool )
   {: a:ptr b:ptr lc:n seg:n doff:n :}
   lc seg < if
      lc SIG-DATASIZE + 4 seg SEG-VMSIZE + 8 seg SEG-FILESIZE + 8
   else
      seg SEG-VMSIZE + 8 seg SEG-FILESIZE + 8 lc SIG-DATASIZE + 4
   then {: o1:n l1:n o2:n l2:n o3:n l3:n :}
   0 a b o1 l1 SAME-TO a b o2 l2 SAME-TO a b o3 l3 SAME-TO
   a b doff 0 SAME-TO 0 >= ;

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
   a lc + SIG-DATAOFF + LE:U32@ {: sig:n :}
   a lc + SIG-DATASIZE + LE:U32@ {: sigu:n :}
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

\ Whether two images match up to one shared signature offset, outside the sizes
\ signing sets in LC_CODE_SIGNATURE and __LINKEDIT; unsigned ones match whole.
: SAME-CODE? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr au:n b:ptr bu:n :}
   a au SIG-COMMAND {: lc:n :}
   lc b bu SIG-COMMAND <> lc 0< or if false exit then
   lc 0= if a au b bu STR= exit then
   a au LINKEDIT-COMMAND {: seg:n :}
   seg b bu LINKEDIT-COMMAND <> seg 0 <= or if false exit then
   a lc + SIG-DATAOFF + LE:U32@ {: doff:n :}
   doff b lc + SIG-DATAOFF + LE:U32@ <> if false exit then
   doff au <= doff bu <= and 0= if false exit then
   lc seg doff HEAD-BEFORE? 0= if false exit then
   a b lc seg doff HEAD-SAME? ;

;package
