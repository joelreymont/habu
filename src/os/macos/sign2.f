\ sign2.fs — ad-hoc self-signing post-pass for the FULL Mach-O builder (macho.fs),
\ transcribed from bootstrap/cg/sign.fs: insert LC_CODE_SIGNATURE into header slack, grow
\ __LINKEDIT, append a CSMAGIC_EMBEDDED_SIGNATURE SuperBlob with one CodeDirectory
\ (v0x20400, adhoc, SHA-256 page hashes). Operates on MBUF/MLEN/LE-OFF in place.
\ Needs sha256.fs + macho.fs. Signature ints BIG-endian; header patches LE.
variable SIGA  variable SIGU
: SIGA@ SIGA @ ;
s" SIGA@" s" -- ptr u8" TRUST

: SET-SIGID {: a:ptr u :}  a SIGA !  u SIGU ! ;

: SIG-IDLEN  SIGU @ 1 + ;
$1D       constant LC-CODE-SIG
88        constant CD-HDR
$1000     constant CS-PAGE
32        constant CS-HASH
variable  SIG-DOFF

: NCSLOTS   SIG-DOFF @ CS-PAGE 1 - +  CS-PAGE / ;

: HASH-OFF  CD-HDR SIG-IDLEN + ;

: CD-SIZE   HASH-OFF  NCSLOTS CS-HASH * + ;

: SB-SIZE   20 CD-SIZE + ;

: ALN {: n a :}  n a 1 - +  a 1 - invert and ;
\ absolute LE access into MBUF (header patching)
variable HLP
: HLP@ HLP @ ;
s" HLP@" s" -- ptr u8" TRUST

: HL@ {: off :}  MBUF off +  dup c@  over 1 + c@ 8 lshift or
   over 2 + c@ 16 lshift or  swap 3 + c@ 24 lshift or ;

: HL! {: w off :}  MBUF off + HLP !
   w $FF and HLP@ c!  w 8 rshift $FF and HLP@ 1 + c!
   w 16 rshift $FF and HLP@ 2 + c!  w 24 rshift $FF and HLP@ 3 + c! ;

: HX! {: x off :}  x off HL!  x 32 rshift off 4 + HL! ;
\ big-endian cursor into MBUF (the signature blob)
variable SC

: B8  {: c :}  c MBUF SC @ + c!  SC @ 1 + SC ! ;

: B32 {: w :}  w 24 rshift B8  w 16 rshift $FF and B8  w 8 rshift $FF and B8  w $FF and B8 ;

: B64 {: x :}  x 32 rshift B32  x $FFFFFFFF and B32 ;

: BSTR {: a:ptr u :}  0 BEGIN dup u < WHILE  dup a + c@ B8  1 + REPEAT drop ;
$FADE0CC0 constant CSMAGIC-EMBEDDED
$FADE0C02 constant CSMAGIC-CODEDIR
$00020400 constant CD-VERSION
2         constant CD-ADHOC
2         constant HT-SHA256
12        constant CS-PAGE-LOG
1         constant EXECSEG-MAIN
variable EXECSEG-LIM   0 EXECSEG-LIM !   \ 0 = use TEXTSZ (snapshots override: bigger __TEXT)

: ADD-CODESIG-LC
   MH-HDR-SZ  20 HL@ +  {: at :}
   LC-CODE-SIG  at HL!   16  at 4 + HL!
   SIG-DOFF @   at 8 + HL!   SB-SIZE  at 12 + HL!
   16 HL@ 1 +  16 HL!
   20 HL@ 16 +  20 HL! ;

: PATCH-LINKEDIT
   LE-OFF @ {: le :}
   SB-SIZE $4000 ALN  le 32 + HX!
   SB-SIZE            le 48 + HX! ;

: CD-HDR,
   CSMAGIC-CODEDIR B32   CD-SIZE B32   CD-VERSION B32   CD-ADHOC B32
   HASH-OFF B32          CD-HDR B32
   0 B32                 NCSLOTS B32
   SIG-DOFF @ B32
   CS-HASH B8  HT-SHA256 B8  0 B8  CS-PAGE-LOG B8
   0 B32
   0 B32  0 B32
   0 B32  0 B64
   0 B64  EXECSEG-LIM @ dup 0 = IF drop TEXTSZ THEN B64  EXECSEG-MAIN B64 ;
variable CSI

: CODESIG2
   MLEN @ SIG-DOFF !
   ADD-CODESIG-LC  PATCH-LINKEDIT
   SIG-DOFF @ SC !
   CSMAGIC-EMBEDDED B32   SB-SIZE B32   1 B32
   0 B32   20 B32
   CD-HDR,
   SIGA@ SIGU @ BSTR  0 B8
   0 CSI ! BEGIN CSI @ NCSLOTS < WHILE
     MBUF CSI @ CS-PAGE * +  CS-PAGE  MBUF SC @ +  SHA256  SC @ CS-HASH + SC !
     CSI @ 1 + CSI ! REPEAT
   SC @ MLEN ! ;
