\ sign2.f - the ad-hoc signing post-pass for src/os/macos/macho.f's images:
\ CODESIG2 inserts LC_CODE_SIGNATURE into the header slack, grows __LINKEDIT
\ and appends a CSMAGIC_EMBEDDED_SIGNATURE SuperBlob with one CodeDirectory
\ (version $20400, adhoc, one SHA-256 hash per $1000-byte page, a partial last
\ page hashed at its length). It works in place on MBUF, MLEN and LE-OFF once
\ SHA256 and macho.f, which sizes the signature, are loaded. Signature
\ integers are big-endian; the header patches little-endian.

require src/os/image-bytes.f
require src/os/macos/macho.f

package MACHO
variable  SIG-DOFF
create CS-SHA-CTX SHA256-CTX-BYTES allot

: ALN ( n n -- n ) {: n a :}  n a 1 - +  a 1 - invert and ;
: CS-REM ( -- n )
   SIG-DOFF @ CS-PAGE mod ;

: CS-SLOT-SIZE ( n -- n ) {: idx:n :}
   idx SIG-DOFF @ NCSLOTS 1 - = if
      CS-REM dup 0 <> if exit then drop
   then
   CS-PAGE ;
$FADE0CC0 constant CSMAGIC-EMBEDDED
$FADE0C02 constant CSMAGIC-CODEDIR
$00020400 constant CD-VERSION
2         constant CD-ADHOC
2         constant HT-SHA256
12        constant CS-PAGE-LOG
1         constant EXECSEG-MAIN
variable EXECSEG-LIM   0 EXECSEG-LIM !

: ADD-CODESIG-LC ( -- )
   MH-HDR-SZ  20 IMAGE-BYTES:M-OFF IMAGE-BYTES:M-LE32@ +  {: at :}
   LC-CODE-SIG  at IMAGE-BYTES:M-OFF IMAGE-BYTES:M-LE32!   16  at 4 + IMAGE-BYTES:M-OFF IMAGE-BYTES:M-LE32!
   SIG-DOFF @   at 8 + IMAGE-BYTES:M-OFF IMAGE-BYTES:M-LE32!   SIG-DOFF @ SB-SIZE  at 12 + IMAGE-BYTES:M-OFF IMAGE-BYTES:M-LE32!
   16 IMAGE-BYTES:M-OFF IMAGE-BYTES:M-LE32@ 1 +  16 IMAGE-BYTES:M-OFF IMAGE-BYTES:M-LE32!
   20 IMAGE-BYTES:M-OFF IMAGE-BYTES:M-LE32@ 16 +  20 IMAGE-BYTES:M-OFF IMAGE-BYTES:M-LE32! ;

: PATCH-LINKEDIT ( -- )
   LE-OFF @ {: le :}
   LE-BASE-SIZE @ SIG-DOFF @ SB-SIZE + {: size:n :}
   size $4000 ALN  le 32 + IMAGE-BYTES:M-OFF IMAGE-BYTES:M-LE64!
   size            le 48 + IMAGE-BYTES:M-OFF IMAGE-BYTES:M-LE64! ;

: CD-HDR, ( -- )
   CSMAGIC-CODEDIR IMAGE-BYTES:M-BE32   SIG-DOFF @ CD-SIZE IMAGE-BYTES:M-BE32   CD-VERSION IMAGE-BYTES:M-BE32   CD-ADHOC IMAGE-BYTES:M-BE32
   HASH-OFF IMAGE-BYTES:M-BE32          CD-HDR IMAGE-BYTES:M-BE32
   0 IMAGE-BYTES:M-BE32                 SIG-DOFF @ NCSLOTS IMAGE-BYTES:M-BE32
   SIG-DOFF @ IMAGE-BYTES:M-BE32
   CS-HASH IMAGE-BYTES:M-BE8  HT-SHA256 IMAGE-BYTES:M-BE8  0 IMAGE-BYTES:M-BE8  CS-PAGE-LOG IMAGE-BYTES:M-BE8
   0 IMAGE-BYTES:M-BE32
   0 IMAGE-BYTES:M-BE32  0 IMAGE-BYTES:M-BE32
   0 IMAGE-BYTES:M-BE32  0 IMAGE-BYTES:M-BE64
   0 IMAGE-BYTES:M-BE64  EXECSEG-LIM @ dup 0 = if drop TEXTSZ then IMAGE-BYTES:M-BE64  EXECSEG-MAIN IMAGE-BYTES:M-BE64 ;
variable CSI

: CODESIG2-BODY ( -- )
   IMAGE-BYTES:MLEN@ SIG-DOFF !
   ADD-CODESIG-LC  PATCH-LINKEDIT
   SIG-DOFF @ IMAGE-BYTES:M-OFF IMAGE-BYTES:M-BE-RESET
   CSMAGIC-EMBEDDED IMAGE-BYTES:M-BE32   SIG-DOFF @ SB-SIZE IMAGE-BYTES:M-BE32   1 IMAGE-BYTES:M-BE32
   0 IMAGE-BYTES:M-BE32   20 IMAGE-BYTES:M-BE32
   CD-HDR,
   SIGA@ SIGU @ >str IMAGE-BYTES:M-BE-BYTES  0 IMAGE-BYTES:M-BE8
   0 CSI ! begin CSI @ SIG-DOFF @ NCSLOTS < while
     CS-SHA-CTX  IMAGE-BYTES:MBUF CSI @ CS-PAGE * +  CSI @ CS-SLOT-SIZE >str  IMAGE-BYTES:M-BE-PTR  SHA256-IN  CS-HASH IMAGE-BYTES:M-LEN IMAGE-BYTES:M-BE-SKIP
     CSI @ 1 + CSI ! repeat
   IMAGE-BYTES:M-BE-HERE IMAGE-BYTES:MLEN! ;
public
: CODESIG2 ( img -- img )
   CODESIG2-BODY ;

;package
