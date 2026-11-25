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
require src/lib/string-option.f
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
-9650 s" this macOS host holds no valid code-signing identity of the team: `security find-identity -v -p codesigning` lists the valid ones"
   ERROR:FAULT E-IDENTITY

private

10000 constant CODESIGN-TIMEOUT-MS

DYNAMIC-BUFFER CODESIGN-OUT u8
DYNAMIC-BUFFER CODESIGN-ERR u8
SCRATCH-VARIABLE CODESIGN-OUT-U
SCRATCH-VARIABLE CODESIGN-ERR-U
DYNAMIC-BUFFER CODESIGN-IDS u8
SCRATCH-VARIABLE CODESIGN-IDS-U

\ An identity as codesign -s names it: its certificate's SHA-1 in hex.
40 constant CODESIGN-ID-LEN
create CODESIGN-ID CODESIGN-ID-LEN SCRATCH-ALLOT
SCRATCH-VARIABLE CODESIGN-ID?

: CODESIGN-OUT-ROOM ( n -- ^u8 )
   CODESIGN-OUT-RESERVE 0 CODESIGN-OUT ;

: CODESIGN-ERR-ROOM ( n -- ^u8 )
   CODESIGN-ERR-RESERVE 0 CODESIGN-ERR ;

public

: TOOL ( -- str )
   s" /usr/bin/codesign" ;

private

: SECURITY ( -- str )
   s" /usr/bin/security" ;

\ The team ID every macOS build signs for.
: TEAM$ ( -- str )
   s" 8TW82D98K7" ;

: CODESIGN-RC0 ( n -- )
   0 <> if E-BUILD-STATUS throw then ;

: CODESIGN-EXPECT-TOOL ( -- )
   TOOL FS:FILE? 0= if E-BUILD-COMMAND throw then ;

: CODESIGN-EXPECT-FILE ( str -- ) {: s:str :}
   s STR:LEN 0 <= if E-BUILD-PATH throw then
   s FS:FILE? 0= if E-BUILD-PATH throw then ;

: CODESIGN-EXPECT-EXECUTABLE ( str -- ) {: s:str :}
   s CODESIGN-EXPECT-FILE
   s FS:EXECUTABLE? 0= if E-BUILD-PATH throw then ;

: CODESIGN-CAPTURE ( str -- n ) {: tool:str :}
   PROC-ENV-RESET
   tool s" "
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
      CODESIGN-ERR-U @ 0 > if 0 CODESIGN-ERR CODESIGN-ERR-U @ >str ERROR-TEXT:TYPE then
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
: CODESIGN-SECURITY ( [ str -- ] -- ) {: xt :}
   SECURITY CODESIGN-CAPTURE CODESIGN-ERR-SHOW 0<>
   CODESIGN-OUT-U @ 0= or if CODESIGN-NO-IDENTITY then
   0 CODESIGN-OUT CODESIGN-OUT-U @ >str xt execute ;

: CODESIGN-KEEP-IDS ( str -- ) {: s:str :}
   s STR:LEN CODESIGN-IDS-RESERVE
   s STR:DATA 0 CODESIGN-IDS s STR:LEN BYTE-COPY
   s STR:LEN CODESIGN-IDS-U ! ;

: CODESIGN-IDS$ ( -- str )
   0 CODESIGN-IDS CODESIGN-IDS-U @ >str ;

\ The hashes of the valid signing identities, certificate and private key both
\ in the keychain search list.
: CODESIGN-LIST-IDS ( -- )
   PROC-ARGV-RESET
   s" find-identity" PROC-ARGV+
   s" -v" PROC-ARGV+
   s" -p" PROC-ARGV+
   s" codesigning" PROC-ARGV+
   [: [: CODESIGN-KEEP-IDS ;] CODESIGN-SECURITY ;] [: CODESIGN-RELEASE ;] finally ;

\ find-certificate -Z prints each certificate from its "SHA-1 hash: " line on,
\ its subject's DER bytes escaped: printable bytes as themselves, others \ooo.
: CODESIGN-CERT-MARK ( -- str )
   s" SHA-1 hash: " ;

\ The subject's line; the issuer's names its own organizational unit.
: CODESIGN-SUBJ-MARK ( -- str )
   s\" \"subj\"<blob>=" ;

\ The subject's organizational unit, OID 2.5.4.11, escaped; the string's tag
\ and its length 10 follow as two \ooo, then the team.
: CODESIGN-OU-MARK ( -- str )
   s" U\004\013" ;

8 constant CODESIGN-OU-SKIP

\ The offset of pat in s at or after from, else s's length.
: CODESIGN-FIND ( n str str -- n ) {: from:n s:str pat:str :}
   s pat from STR:FIND-SUB-FROM MATCH option
     none OF s STR:LEN ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

\ Whether the OU mark at offset at is followed by TEAM$.
: CODESIGN-TEAM-AT? ( n str -- bool ) {: at:n s:str :}
   at CODESIGN-OU-MARK STR:LEN + CODESIGN-OU-SKIP + {: team:n :}
   team TEAM$ STR:LEN + s STR:LEN > if false exit then
   s STR:DATA team + TEAM$ STR:LEN >str TEAM$ STR:= ;

: CODESIGN-TEAM-IN? ( str -- bool ) {: s:str :}
   0 begin
      s CODESIGN-OU-MARK CODESIGN-FIND
      dup s STR:LEN <
   while
      dup s CODESIGN-TEAM-AT? if drop true exit then
      1 +
   repeat
   drop false ;

\ Whether the certificate's subject names TEAM$.
: CODESIGN-TEAM-CERT? ( str -- bool ) {: s:str :}
   0 s CODESIGN-SUBJ-MARK CODESIGN-FIND {: subj:n :}
   subj s STR:LEN >= if false exit then
   subj s s\" \n" CODESIGN-FIND {: eol:n :}
   s STR:DATA subj + eol subj - >str CODESIGN-TEAM-IN? ;

\ Keeps the hash of the certificate from at to next when it is the team's and
\ a valid identity.
: CODESIGN-PICK-AT ( n n str -- bool ) {: at:n next:n s:str :}
   at CODESIGN-CERT-MARK STR:LEN + {: hash:n :}
   hash CODESIGN-ID-LEN + next > if false exit then
   s STR:DATA at + next at - >str CODESIGN-TEAM-CERT? 0= if false exit then
   CODESIGN-IDS$ s STR:DATA hash + CODESIGN-ID-LEN >str STR:CONTAINS? 0= if false exit then
   s STR:DATA hash + CODESIGN-ID CODESIGN-ID-LEN BYTE-COPY
   -1 CODESIGN-ID? !
   true ;

: CODESIGN-NEXT-CERT ( n str -- n ) {: at:n s:str :}
   at CODESIGN-CERT-MARK STR:LEN + s CODESIGN-CERT-MARK CODESIGN-FIND ;

: CODESIGN-PICK ( str -- ) {: s:str :}
   0 s CODESIGN-CERT-MARK CODESIGN-FIND
   begin dup s STR:LEN < while
      dup s CODESIGN-NEXT-CERT
      2dup s CODESIGN-PICK-AT if 2drop exit then
      nip
   repeat
   drop ;

: CODESIGN-FIND-ID ( -- )
   CODESIGN-LIST-IDS
   PROC-ARGV-RESET
   s" find-certificate" PROC-ARGV+
   s" -a" PROC-ARGV+
   s" -Z" PROC-ARGV+
   [: [: CODESIGN-PICK ;] CODESIGN-SECURITY ;] [: CODESIGN-RELEASE ;] finally
   CODESIGN-ID? @ 0= if CODESIGN-NO-IDENTITY then ;

public

\ The one identity every macOS signature names, found on first use.
: IDENTITY$ ( -- str )
   CODESIGN-ID? @ 0= if
      [: CODESIGN-FIND-ID ;] [: CODESIGN-IDS-RELEASE ;] finally
   then
   CODESIGN-ID CODESIGN-ID-LEN >str ;

private

: CODESIGN-TARGET-UNKNOWN ( -- )
   E-BUILD-SOURCE throw ;

: CODESIGN-MACOS-VERIFY-ARGS ( str -- ) {: s:str :}
   s CODESIGN-EXPECT-EXECUTABLE
   PROC-ARGV-RESET
   s" -v"  PROC-ARGV+
   s  PROC-ARGV+ ;

: CODESIGN-REQ-HEAD ( -- str )
   s\" =anchor apple generic and certificate leaf[subject.OU] = \"" ;

create CODESIGN-REQ CODESIGN-REQ-HEAD STR:LEN TEAM$ STR:LEN + 1 + allot

\ The requirement every team signature meets: an Apple-issued certificate of
\ team TEAM$.
: CODESIGN-TEAM-REQ ( -- str )
   CODESIGN-REQ-HEAD {: h:str :}
   TEAM$ {: t:str :}
   h STR:DATA CODESIGN-REQ h STR:LEN BYTE-COPY
   t STR:DATA CODESIGN-REQ h STR:LEN + t STR:LEN BYTE-COPY
   [char] " CODESIGN-REQ h STR:LEN t STR:LEN + + c!
   CODESIGN-REQ h STR:LEN t STR:LEN + 1 + >str ;

\ codesign -v -R exits nonzero for any other signer, Apple's and ad hoc
\ included, and for a file it cannot read.
: CODESIGN-MACOS-TEAM? ( str -- bool ) {: s:str :}
   PROC-ARGV-RESET
   s" -v"  PROC-ARGV+
   s" -R"  PROC-ARGV+
   CODESIGN-TEAM-REQ  PROC-ARGV+
   s  PROC-ARGV+
   CODESIGN-RUN 0= ;

: CODESIGN-SIGN-ARGS ( -- )
   IDENTITY$ {: id:str :}
   PROC-ARGV-RESET
   s" -s"  PROC-ARGV+
   id  PROC-ARGV+
   s" --timestamp=none"  PROC-ARGV+
   s" --force"  PROC-ARGV+ ;

\ A written image carries its writer's identifier (src/habu/sign-id.f); the
\ keychain's access list names it, so the team signature keeps it.
: CODESIGN-MACOS-FORCE ( str -- ) {: s:str :}
   s CODESIGN-EXPECT-FILE
   CODESIGN-SIGN-ARGS
   s" --preserve-metadata=identifier"  PROC-ARGV+
   s  PROC-ARGV+
   CODESIGN-RUN-OK ;

: CODESIGN-MACOS-FORCE-AS ( str str -- ) {: s:str id:str :}
   s CODESIGN-EXPECT-FILE
   CODESIGN-SIGN-ARGS
   s" -i"  PROC-ARGV+
   id  PROC-ARGV+
   s  PROC-ARGV+
   CODESIGN-RUN-OK ;

: CODESIGN-LINUX-VERIFY-RC ( str -- n )
   CODESIGN-EXPECT-EXECUTABLE
   0 ;

: CODESIGN-LINUX-FORCE ( str -- )
   dup CODESIGN-EXPECT-FILE
   FS:CHMOD-X ;

4 constant CODESIGN-MAGIC-BYTES
create CODESIGN-MAGIC-ROW CODESIGN-MAGIC-BYTES SCRATCH-ALLOT
: CODESIGN-MAGIC ( -- SPAN:span<u8> ) CODESIGN-MAGIC-ROW CODESIGN-MAGIC-BYTES SPAN:MAKE ;
SCRATCH-VARIABLE CODESIGN-MAGIC-FD

: CODESIGN-ELF-MAG ( -- str )
   s\" \x7FELF" ;

: CODESIGN-MAGIC-READ ( -- bool )
   CODESIGN-MAGIC-FD @ >FD CODESIGN-MAGIC FD:READ-EXACT MATCH FD:fill
      full OF CODESIGN-MAGIC SPAN:$ CODESIGN-ELF-MAG STR:= ENDOF
      eof OF drop false ENDOF
   ;MATCH ;

: CODESIGN-MAGIC-CLOSE ( -- )
   CODESIGN-MAGIC-FD @ close ;

: CODESIGN-ELF? ( str -- bool ) {: s:str :}
   s STR:LEN 0 <= if false exit then
   s FS:FILE? 0= if false exit then
   s FS:OPEN-SIZED {: f:fd size:n found:bool :}
   found 0= if false exit then
   f FD>N CODESIGN-MAGIC-FD !
   [: CODESIGN-MAGIC-READ ;] [: CODESIGN-MAGIC-CLOSE ;] finally ;

\ A Linux host, or an ELF file built for one on a Mac (--target).
: CODESIGN-LINUX? ( str -- bool ) {: s:str :}
   HB-TARGET-LINUX? if true exit then
   s CODESIGN-ELF? ;

public

: VERIFY-RC ( str -- n ) {: s:str :}
   s CODESIGN-LINUX? if s CODESIGN-LINUX-VERIFY-RC exit then
   HB-TARGET-MACOS? if s CODESIGN-MACOS-VERIFY-ARGS CODESIGN-RUN exit then
   CODESIGN-TARGET-UNKNOWN ;

: VERIFY ( str -- ) {: s:str :}
   s CODESIGN-LINUX? if
      s CODESIGN-EXPECT-EXECUTABLE
      exit
   then
   HB-TARGET-MACOS? if
      s CODESIGN-MACOS-VERIFY-ARGS
      CODESIGN-RUN-OK
      exit
   then
   CODESIGN-TARGET-UNKNOWN ;

: FORCE ( str -- ) {: s:str :}
   s CODESIGN-LINUX? if s CODESIGN-LINUX-FORCE exit then
   HB-TARGET-MACOS? if s CODESIGN-MACOS-FORCE exit then
   CODESIGN-TARGET-UNKNOWN ;

\ Whether a file carries a valid signature of team TEAM$. An ELF file has no
\ signer: true when it is executable, E-BUILD-PATH when it is not.
: TEAM-SIGNED? ( str -- bool ) {: s:str :}
   s CODESIGN-LINUX? if s CODESIGN-LINUX-VERIFY-RC 0= exit then
   HB-TARGET-MACOS? if s CODESIGN-MACOS-TEAM? exit then
   CODESIGN-TARGET-UNKNOWN ;

\ Signs a file for the team unless it already carries a valid team signature,
\ so an install keeps the one its build made; a written image's own is ad hoc.
: ENSURE ( str -- ) {: s:str :}
   s CODESIGN-LINUX? 0= HB-TARGET-MACOS? and if
      s TEAM-SIGNED? if exit then
   then
   s FORCE
   s VERIFY ;

\ Sign under a fixed identifier, whatever the file already carries. Without one
\ codesign names the signature after the file, so the name a writer happened to
\ use would travel inside the image.
: SIGN-AS ( str str -- ) {: s:str id:str :}
   s CODESIGN-LINUX? if
      s FORCE
      s VERIFY
      exit
   then
   HB-TARGET-MACOS? if
      s id CODESIGN-MACOS-FORCE-AS
      s VERIFY
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
: COMMAND ( str [ n -- bool ] -- n ) {: s:str hit :}
   s STR:LEN MACHO64-HEADER < if 0 exit then
   s STR:DATA LE:U32@ MACHO64-MAGIC <> if 0 exit then
   MACHO64-HEADER
   s STR:DATA 16 + LE:U32@ 0 ?do
      {: at:n :}
      at LC-HEAD s STR:LEN SPAN-IN? 0= if -1 unloop exit then
      s STR:DATA at + 4 + LE:U32@ {: size:n :}
      size LC-HEAD < at size s STR:LEN SPAN-IN? 0= or if -1 unloop exit then
      at hit execute if at unloop exit then
      at size +
   loop
   drop 0 ;

\ The offset of the image's LC_CODE_SIGNATURE, as COMMAND answers, and -1
\ when the command is not LC-SIGNATURE-SIZE bytes.
: SIG-COMMAND ( str -- n ) {: s:str :}
   s [: s STR:DATA + LE:U32@ LC-SIGNATURE = ;] COMMAND {: at:n :}
   at 0 > if
      s STR:DATA at + 4 + LE:U32@ LC-SIGNATURE-SIZE <> if -1 exit then
   then
   at ;

: LINKEDIT-NAME ( -- str )
   s" __LINKEDIT" ;

\ Whether the load command at offset at is the __LINKEDIT segment's, its
\ 16-byte name zero-padded.
: LINKEDIT? ( ^u8 n -- bool ) {: a:^ at:n :}
   a at + LE:U32@ LC-SEGMENT-64 <> if false exit then
   a at + 4 + LE:U32@ SEGMENT-SIZE < if false exit then
   a at + SEG-NAME + LINKEDIT-NAME STR:LEN >str LINKEDIT-NAME STR:= 0= if false exit then
   a at + SEG-NAME + LINKEDIT-NAME STR:LEN + c@ 0= ;

\ The offset of the image's __LINKEDIT segment command, as COMMAND answers.
: LINKEDIT-COMMAND ( str -- n ) {: s:str :}
   s [: s STR:DATA swap LINKEDIT? ;] COMMAND ;

\ The offset past a field at at of len bytes when the images agree on the
\ bytes from from up to it, else -1; -1 when from is.
: SAME-TO ( n ^u8 ^u8 n n -- n ) {: from:n a:^ b:^ at:n len:n :}
   from 0< if -1 exit then
   a from + at from - >str b from + at from - >str STR:= if at len + else -1 then ;

\ Whether the signature command at lc and the segment command at seg end
\ before the data offset doff.
: HEAD-BEFORE? ( n n n -- bool ) {: lc:n seg:n doff:n :}
   lc LC-SIGNATURE-SIZE + doff <=
   seg SEGMENT-SIZE + doff <= and ;

\ Whether the images agree on the bytes before the signature's data offset
\ doff but for its datasize and the vmsize and filesize of __LINKEDIT at seg.
: HEAD-SAME? ( ^u8 ^u8 n n n -- bool )
   {: a:^ b:^ lc:n seg:n doff:n :}
   lc seg < if
      lc SIG-DATASIZE + 4 seg SEG-VMSIZE + 8 seg SEG-FILESIZE + 8
   else
      seg SEG-VMSIZE + 8 seg SEG-FILESIZE + 8 lc SIG-DATASIZE + 4
   then {: o1:n l1:n o2:n l2:n o3:n l3:n :}
   0 a b o1 l1 SAME-TO a b o2 l2 SAME-TO a b o3 l3 SAME-TO
   a b doff 0 SAME-TO 0 >= ;

\ The CMS blob's offset and length in the superblob at SIG, of SIGU bytes,
\ 0 0 when no slot names one; false when the index or blob leaves the region.
: CMS-SLOT-SPAN ( ^u8 n n -- n n bool ) {: a:^ sig:n sigu:n :}
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
: CMS-SPAN ( str -- n n bool ) {: s:str :}
   s SIG-COMMAND {: lc:n :}
   lc 0< if 0 0 false exit then
   lc 0= if 0 0 true exit then
   s STR:DATA lc + SIG-DATAOFF + LE:U32@ {: sig:n :}
   s STR:DATA lc + SIG-DATASIZE + LE:U32@ {: sigu:n :}
   sig sigu s STR:LEN SPAN-IN? 0= sigu SUPERBLOB-HEAD < or if 0 0 false exit then
   s STR:DATA sig + BE:U32@ SUPERBLOB-MAGIC <> if 0 0 false exit then
   s STR:DATA sig sigu CMS-SLOT-SPAN ;

public

\ True when the two images have one length and equal bytes outside the CMS
\ blob of their code signatures; an image with no signature is compared whole.
\ A signature that lies outside its image is never the same image.
: SAME-IMAGE? ( str str -- bool ) {: a:str b:str :}
   a STR:LEN b STR:LEN <> if false exit then
   a CMS-SPAN {: as:n al:n aok:bool :}
   b CMS-SPAN {: bs:n bl:n bok:bool :}
   aok bok and 0= if false exit then
   as bs <> al bl <> or if false exit then
   a STR:DATA as >str b STR:DATA as >str STR:= 0= if false exit then
   as al + {: tail:n :}
   a STR:DATA tail + a STR:LEN tail - >str b STR:DATA tail + b STR:LEN tail - >str STR:= ;

\ Whether two images match up to one shared signature offset, outside the sizes
\ signing sets in LC_CODE_SIGNATURE and __LINKEDIT; unsigned ones match whole.
: SAME-CODE? ( str str -- bool ) {: a:str b:str :}
   a SIG-COMMAND {: lc:n :}
   lc b SIG-COMMAND <> lc 0< or if false exit then
   lc 0= if a b STR:= exit then
   a LINKEDIT-COMMAND {: seg:n :}
   seg b LINKEDIT-COMMAND <> seg 0 <= or if false exit then
   a STR:DATA lc + SIG-DATAOFF + LE:U32@ {: doff:n :}
   doff b STR:DATA lc + SIG-DATAOFF + LE:U32@ <> if false exit then
   doff a STR:LEN <= doff b STR:LEN <= and 0= if false exit then
   lc seg doff HEAD-BEFORE? 0= if false exit then
   a STR:DATA b STR:DATA lc seg doff HEAD-SAME? ;

;package
