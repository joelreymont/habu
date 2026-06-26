\ sign2.fs — ad-hoc self-signing post-pass for the FULL Mach-O builder (macho.fs):
\ insert LC_CODE_SIGNATURE into header slack, grow
\ __LINKEDIT, append a CSMAGIC_EMBEDDED_SIGNATURE SuperBlob with one CodeDirectory
\ (v0x20400, adhoc, SHA-256 page hashes). Operates on MBUF/MLEN/LE-OFF in place.
\ Needs sha256.fs + macho.fs. Signature ints BIG-endian; header patches LE.
variable SIGA  variable SIGU
: SIGA@ ( -- ptr u8 ) SIGA @ ;
s" SIGA@" s" -- ptr u8" TRUST

: SET-SIGID ( ptr u8 n -- ) {: a:ptr u :}  a SIGA !  u SIGU ! ;
s" SET-SIGID" s" ptr u8 n --" TRUST

: SIG-IDLEN ( -- n )  SIGU @ 1 + ;
$1D       constant LC-CODE-SIG
88        constant CD-HDR
$1000     constant CS-PAGE
32        constant CS-HASH
variable  SIG-DOFF

: NCSLOTS ( -- n )   SIG-DOFF @ CS-PAGE 1 - +  CS-PAGE / ;

: HASH-OFF ( -- n )  CD-HDR SIG-IDLEN + ;

: CD-SIZE ( -- n )   HASH-OFF  NCSLOTS CS-HASH * + ;

: SB-SIZE ( -- n )   20 CD-SIZE + ;

: ALN ( n n -- n ) {: n a :}  n a 1 - +  a 1 - invert and ;
$FADE0CC0 constant CSMAGIC-EMBEDDED
$FADE0C02 constant CSMAGIC-CODEDIR
$00020400 constant CD-VERSION
2         constant CD-ADHOC
2         constant HT-SHA256
12        constant CS-PAGE-LOG
1         constant EXECSEG-MAIN
variable EXECSEG-LIM   0 EXECSEG-LIM !   \ 0 = use TEXTSZ (snapshots override: bigger __TEXT)

: ADD-CODESIG-LC ( -- )
   MH-HDR-SZ  20 M-OFF M-LE32@ +  {: at :}
   LC-CODE-SIG  at M-OFF M-LE32!   16  at 4 + M-OFF M-LE32!
   SIG-DOFF @   at 8 + M-OFF M-LE32!   SB-SIZE  at 12 + M-OFF M-LE32!
   16 M-OFF M-LE32@ 1 +  16 M-OFF M-LE32!
   20 M-OFF M-LE32@ 16 +  20 M-OFF M-LE32! ;

: PATCH-LINKEDIT ( -- )
   LE-OFF @ {: le :}
   SB-SIZE $4000 ALN  le 32 + M-OFF M-LE64!
   SB-SIZE            le 48 + M-OFF M-LE64! ;

: CD-HDR, ( -- )
   CSMAGIC-CODEDIR M-BE32   CD-SIZE M-BE32   CD-VERSION M-BE32   CD-ADHOC M-BE32
   HASH-OFF M-BE32          CD-HDR M-BE32
   0 M-BE32                 NCSLOTS M-BE32
   SIG-DOFF @ M-BE32
   CS-HASH M-BE8  HT-SHA256 M-BE8  0 M-BE8  CS-PAGE-LOG M-BE8
   0 M-BE32
   0 M-BE32  0 M-BE32
   0 M-BE32  0 M-BE64
   0 M-BE64  EXECSEG-LIM @ dup 0 = if drop TEXTSZ then M-BE64  EXECSEG-MAIN M-BE64 ;
variable CSI

: CODESIG2 ( -- )
   MLEN @ SIG-DOFF !
   ADD-CODESIG-LC  PATCH-LINKEDIT
   SIG-DOFF @ M-OFF M-BE-RESET
   CSMAGIC-EMBEDDED M-BE32   SB-SIZE M-BE32   1 M-BE32
   0 M-BE32   20 M-BE32
   CD-HDR,
   SIGA@ SIGU @ M-LEN M-BE-BYTES-LEN  0 M-BE8
   0 CSI ! begin CSI @ NCSLOTS < while
     MBUF CSI @ CS-PAGE * +  CS-PAGE  M-BE-PTR  SHA256  CS-HASH M-LEN M-BE-SKIP
     CSI @ 1 + CSI ! repeat
   M-BE-HERE MLEN ! ;
s" CODESIG2" s" --" TRUST
