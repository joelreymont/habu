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
: HL@ ( off -- w )  MBUF + l@ ;        \ 32-bit fetch

: HL! ( w off -- )  MBUF + l! ;        \ 32-bit store

: HX! ( x off -- )  MBUF + ! ;         \ 64-bit store

\ --- big-endian cursor into MBUF (the signature blob) ---
variable SC

: B8   ( c -- )    MBUF SC @ + c!  1 SC +! ;

: B32  ( w -- )    dup 24 rshift B8  dup 16 rshift B8  dup 8 rshift B8  B8 ;

: B64  ( x -- )    dup 32 rshift B32  B32 ;

: BSTR ( a u -- )  bounds ?do i c@ B8 loop ;

$FADE0CC0 constant CSMAGIC-EMBEDDED
$FADE0C02 constant CSMAGIC-CODEDIR
$00020400 constant CD-VERSION                \ supports execSeg fields
2         constant CD-ADHOC                  \ CS_ADHOC
2         constant HT-SHA256                 \ hashType
12        constant CS-PAGE-LOG               \ log2(4096)
1         constant EXECSEG-MAIN              \ CS_EXECSEG_MAIN_BINARY

\ Insert LC_CODE_SIGNATURE into the header slack, just past the existing LCs.
: ADD-CODESIG-LC ( -- )
   MH-HDR-SZ  20 HL@ +  {: at :}              \ end of existing load commands
   LC-CODE-SIG  at HL!   16  at 4 + HL!
   SIG-DOFF @   at 8 + HL!   SB-SIZE  at 12 + HL!
   16 HL@ 1+  16 HL!                          \ mach_header.ncmds++
   20 HL@ 16 +  20 HL! ;                      \ mach_header.sizeofcmds += 16

\ Grow __LINKEDIT (LC at LE-OFF) to cover the appended signature.
: PATCH-LINKEDIT ( -- )
   LE-OFF @ {: le :}
   SB-SIZE $4000 ALN  le 32 + HX!             \ vmsize (page-aligned)
   SB-SIZE            le 48 + HX! ;            \ filesize

\ Write the CodeDirectory header (88 bytes, version 0x20400) at the cursor.
: CD-HDR, ( -- )
   CSMAGIC-CODEDIR B32   CD-SIZE B32   CD-VERSION B32   CD-ADHOC B32
   HASH-OFF B32          CD-HDR B32                      \ hashOffset, identOffset
   0 B32                 NCSLOTS B32                     \ nSpecialSlots, nCodeSlots
   SIG-DOFF @ B32                                        \ codeLimit
   CS-HASH B8  HT-SHA256 B8  0 B8  CS-PAGE-LOG B8        \ hashSize,hashType,platform,pageSize
   0 B32                                                 \ spare2
   0 B32  0 B32                                          \ scatterOffset, teamOffset
   0 B32  0 B64                                          \ spare3, codeLimit64
   0 B64  MPAGE B64  EXECSEG-MAIN B64 ;                  \ execSegBase/Limit/Flags

\ CODESIG ( -- ) : self-sign the finished unsigned MBUF in place. Hashes the file
\ pages [0,codeLimit) — all strictly below the signature, so no self-reference.
: CODESIG ( -- )
   MLEN @ SIG-DOFF !                          \ codeLimit = current file end (= MPAGE)
   ADD-CODESIG-LC  PATCH-LINKEDIT
   SIG-DOFF @ SC !
   CSMAGIC-EMBEDDED B32   SB-SIZE B32   1 B32  \ SuperBlob: magic,length,count
   0 B32   20 B32                             \ index: slot CODEDIRECTORY @ offset 20
   CD-HDR,
   SIG-ID 2@ BSTR  0 B8                       \ identifier + NUL
   NCSLOTS 0 ?do  MBUF i CS-PAGE * +  CS-PAGE  MBUF SC @ +  SHA256  CS-HASH SC +!  loop
   SC @ MLEN ! ;
