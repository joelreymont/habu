\ sign2.fs — ad-hoc self-signing post-pass for the FULL Mach-O builder (macho.fs),
\ transcribed from src/cg/sign.fs: insert LC_CODE_SIGNATURE into header slack, grow
\ __LINKEDIT, append a CSMAGIC_EMBEDDED_SIGNATURE SuperBlob with one CodeDirectory
\ (v0x20400, adhoc, SHA-256 page hashes). Operates on MBUF/MLEN/LE-OFF in place.
\ Needs sha256.fs + macho.fs. Signature ints BIG-endian; header patches LE.
variable SIGA  variable SIGU
: SET-SIGID {: a u :}  a SIGA !  u SIGU ! ;
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
: hl@ {: off :}  MBUF off +  dup c@  over 1 + c@ 8 lshift or
   over 2 + c@ 16 lshift or  swap 3 + c@ 24 lshift or ;
: hl! {: w off :}  MBUF off + HLP !
   w $FF and HLP @ c!  w 8 rshift $FF and HLP @ 1 + c!
   w 16 rshift $FF and HLP @ 2 + c!  w 24 rshift $FF and HLP @ 3 + c! ;
: hx! {: x off :}  x off hl!  x 32 rshift off 4 + hl! ;
\ big-endian cursor into MBUF (the signature blob)
variable SC
: b8  {: c :}  c MBUF SC @ + c!  SC @ 1 + SC ! ;
: b32 {: w :}  w 24 rshift b8  w 16 rshift $FF and b8  w 8 rshift $FF and b8  w $FF and b8 ;
: b64 {: x :}  x 32 rshift b32  x $FFFFFFFF and b32 ;
: bstr {: a u :}  0 BEGIN dup u < WHILE  dup a + c@ b8  1 + REPEAT drop ;
$FADE0CC0 constant CSMAGIC-EMBEDDED
$FADE0C02 constant CSMAGIC-CODEDIR
$00020400 constant CD-VERSION
2         constant CD-ADHOC
2         constant HT-SHA256
12        constant CS-PAGE-LOG
1         constant EXECSEG-MAIN
: add-codesig-lc
   MH-HDR-SZ  20 hl@ +  {: at :}
   LC-CODE-SIG  at hl!   16  at 4 + hl!
   SIG-DOFF @   at 8 + hl!   SB-SIZE  at 12 + hl!
   16 hl@ 1 +  16 hl!
   20 hl@ 16 +  20 hl! ;
: patch-linkedit
   LE-OFF @ {: le :}
   SB-SIZE $4000 ALN  le 32 + hx!
   SB-SIZE            le 48 + hx! ;
: cd-hdr,
   CSMAGIC-CODEDIR b32   CD-SIZE b32   CD-VERSION b32   CD-ADHOC b32
   HASH-OFF b32          CD-HDR b32
   0 b32                 NCSLOTS b32
   SIG-DOFF @ b32
   CS-HASH b8  HT-SHA256 b8  0 b8  CS-PAGE-LOG b8
   0 b32
   0 b32  0 b32
   0 b32  0 b64
   0 b64  MPAGE b64  EXECSEG-MAIN b64 ;
variable CSI
: CODESIG2
   MLEN @ SIG-DOFF !
   add-codesig-lc  patch-linkedit
   SIG-DOFF @ SC !
   CSMAGIC-EMBEDDED b32   SB-SIZE b32   1 b32
   0 b32   20 b32
   cd-hdr,
   SIGA @ SIGU @ bstr  0 b8
   0 CSI ! BEGIN CSI @ NCSLOTS < WHILE
     MBUF CSI @ CS-PAGE * +  CS-PAGE  MBUF SC @ +  SHA256  SC @ CS-HASH + SC !
     CSI @ 1 + CSI ! REPEAT
   SC @ MLEN ! ;
