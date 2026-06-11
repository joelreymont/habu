\ sign.fs — ad-hoc self-signing POST-PASS for habu's Mach-O. Operates on the
\ finished unsigned binary in MBUF (built by macho.fs): rewrites the header to
\ add an LC_CODE_SIGNATURE load command (into the header slack), grows __LINKEDIT,
\ and appends an embedded signature — a CSMAGIC_EMBEDDED_SIGNATURE SuperBlob
\ holding one CSMAGIC_CODEDIRECTORY blob (version 0x20400, flags=adhoc, SHA-256
\ page hashes). Exactly what `codesign -s -` does; zero external tools. All
\ signature integers are BIG-ENDIAN (unlike the little-endian load commands).

require macho.fs
require sha256.fs

2variable SIG-ID    s" a.out" SIG-ID 2!   \ binary identifier (basename), set by EMIT-EXE
: SIG-IDLEN ( -- n )  SIG-ID 2@ nip 1+ ;  \ identifier string + NUL
$1D       constant LC-CODE-SIG         \ LC_CODE_SIGNATURE
88        constant CD-HDR              \ CodeDirectory header, version 0x20400
$1000     constant CS-PAGE             \ code-signing page (4 KiB, independent of VM page)
32        constant CS-HASH             \ SHA-256 digest size
variable  SIG-DOFF                     \ signature data offset = codeLimit (file end pre-sign)
: NCSLOTS  ( -- n )  SIG-DOFF @ CS-PAGE 1- +  CS-PAGE / ;   \ ceil(codeLimit/page)
: HASH-OFF ( -- n )  CD-HDR SIG-IDLEN + ;
: CD-SIZE  ( -- n )  HASH-OFF  NCSLOTS CS-HASH * + ;
: SB-SIZE  ( -- n )  20 CD-SIZE + ;     \ SuperBlob hdr(12)+1 index(8)+CodeDirectory
: ALN ( n a -- n )  1- dup >r +  r> invert and ;

\ --- absolute access into MBUF (header patching); LE, not the BE blob cursor ---
: hl@ ( off -- w )  MBUF + l@ ;        \ 32-bit fetch
: hl! ( w off -- )  MBUF + l! ;        \ 32-bit store
: hx! ( x off -- )  MBUF + ! ;         \ 64-bit store

\ --- big-endian cursor into MBUF (the signature blob) ---
variable SC
: b8   ( c -- )    MBUF SC @ + c!  1 SC +! ;
: b32  ( w -- )    dup 24 rshift b8  dup 16 rshift b8  dup 8 rshift b8  b8 ;
: b64  ( x -- )    dup 32 rshift b32  b32 ;
: bstr ( a u -- )  bounds ?do i c@ b8 loop ;

$FADE0CC0 constant CSMAGIC-EMBEDDED
$FADE0C02 constant CSMAGIC-CODEDIR
$00020400 constant CD-VERSION                \ supports execSeg fields
2         constant CD-ADHOC                  \ CS_ADHOC
2         constant HT-SHA256                 \ hashType
12        constant CS-PAGE-LOG               \ log2(4096)
1         constant EXECSEG-MAIN              \ CS_EXECSEG_MAIN_BINARY

\ Insert LC_CODE_SIGNATURE into the header slack, just past the existing LCs.
: add-codesig-lc ( -- )
   MH-HDR-SZ  20 hl@ +  {: at :}              \ end of existing load commands
   LC-CODE-SIG  at hl!   16  at 4 + hl!
   SIG-DOFF @   at 8 + hl!   SB-SIZE  at 12 + hl!
   16 hl@ 1+  16 hl!                          \ mach_header.ncmds++
   20 hl@ 16 +  20 hl! ;                      \ mach_header.sizeofcmds += 16

\ Grow __LINKEDIT (LC at LE-OFF) to cover the appended signature.
: patch-linkedit ( -- )
   LE-OFF @ {: le :}
   SB-SIZE $4000 ALN  le 32 + hx!             \ vmsize (page-aligned)
   SB-SIZE            le 48 + hx! ;            \ filesize

\ Write the CodeDirectory header (88 bytes, version 0x20400) at the cursor.
: cd-hdr, ( -- )
   CSMAGIC-CODEDIR b32   CD-SIZE b32   CD-VERSION b32   CD-ADHOC b32
   HASH-OFF b32          CD-HDR b32                      \ hashOffset, identOffset
   0 b32                 NCSLOTS b32                     \ nSpecialSlots, nCodeSlots
   SIG-DOFF @ b32                                        \ codeLimit
   CS-HASH b8  HT-SHA256 b8  0 b8  CS-PAGE-LOG b8        \ hashSize,hashType,platform,pageSize
   0 b32                                                 \ spare2
   0 b32  0 b32                                          \ scatterOffset, teamOffset
   0 b32  0 b64                                          \ spare3, codeLimit64
   0 b64  MPAGE b64  EXECSEG-MAIN b64 ;                  \ execSegBase/Limit/Flags

\ CODESIG ( -- ) : self-sign the finished unsigned MBUF in place. Hashes the file
\ pages [0,codeLimit) — all strictly below the signature, so no self-reference.
: CODESIG ( -- )
   MLEN @ SIG-DOFF !                          \ codeLimit = current file end (= MPAGE)
   add-codesig-lc  patch-linkedit
   SIG-DOFF @ SC !
   CSMAGIC-EMBEDDED b32   SB-SIZE b32   1 b32  \ SuperBlob: magic,length,count
   0 b32   20 b32                             \ index: slot CODEDIRECTORY @ offset 20
   cd-hdr,
   SIG-ID 2@ bstr  0 b8                       \ identifier + NUL
   NCSLOTS 0 ?do  MBUF i CS-PAGE * +  CS-PAGE  MBUF SC @ +  sha256  CS-HASH SC +!  loop
   SC @ MLEN ! ;
