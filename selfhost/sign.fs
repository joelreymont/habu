\ sign.fs — ad-hoc self-signing POST-PASS in the STANDALONE's Forth. Mirrors
\ src/cg/sign.fs: rewrites the unsigned image at MSTART (built by macho-min.fs) to
\ add LC_CODE_SIGNATURE into the header slack, grow __LINKEDIT, and append an embedded
\ CSMAGIC_EMBEDDED_SIGNATURE SuperBlob with one version-0x20400 CodeDirectory (adhoc,
\ SHA-256 of each 4 KiB page). Needs sha256.fs (SHA256) loaded first. Zero codesign.
\ Signature integers are BIG-ENDIAN (B8/B32/B64); load commands little-endian (M/P).
29 constant LCSIG   88 constant CDHDR   4096 constant CSPAGE   32 constant CSHASH
16 constant NCSLOT                       \ MPAGE / CSPAGE
create SIGID 115 c, 101 c, 45 c, 111 c, 117 c, 116 c,   \ "se-out"
6 constant SIGIDN
: SIGIDL SIGIDN 1 + ;                    \ identifier + NUL
: HASHOFF CDHDR SIGIDL + ;
: CDSIZE HASHOFF NCSLOT CSHASH * + ;
: SBSIZE 20 CDSIZE + ;                    \ SuperBlob hdr(12)+1 index(8)+CodeDirectory
\ absolute access into the image (little-endian)
: G32 {: a :} a c@  a 1 + c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;
: P64 {: x a :}  x 255 and a c!  x 8 rshift 255 and a 1 + c!  x 16 rshift 255 and a 2 + c!
   x 24 rshift 255 and a 3 + c!  x 32 rshift 255 and a 4 + c!  x 40 rshift 255 and a 5 + c!
   x 48 rshift 255 and a 6 + c!  x 56 rshift 255 and a 7 + c! ;
\ big-endian appenders (the signature blob)
: B8 c, ;
: B32 {: w :} w 24 rshift 255 and B8  w 16 rshift 255 and B8  w 8 rshift 255 and B8  w 255 and B8 ;
: B64 {: x :} x 32 rshift B32  x B32 ;
create SDG 32 allot
: CODESIG
   \ insert LC_CODE_SIGNATURE into the header slack (at MSTART + 32 + sizeofcmds)
   MSTART @ 20 + G32 {: scmds :}
   MSTART @ 32 + scmds + {: at :}
   LCSIG at P32   16 at 4 + P32   MPAGE at 8 + P32   SBSIZE at 12 + P32
   MSTART @ 16 + G32 1 + MSTART @ 16 + P32           \ ncmds++
   scmds 16 + MSTART @ 20 + P32                      \ sizeofcmds += 16
   \ grow __LINKEDIT (its LC is the 4th, at MSTART+256): vmsize @+32, filesize @+48
   MSTART @ 256 + {: le :}
   16384 le 32 + P64   SBSIZE le 48 + P64
   \ append the embedded signature (here = MSTART+MPAGE after BUILD's final MPAGE MPAD)
   4208856256 B32  SBSIZE B32  1 B32                 \ SuperBlob: magic, length, count
   0 B32  20 B32                                     \ index: CodeDirectory @ offset 20
   4208856066 B32  CDSIZE B32  132096 B32  2 B32     \ CD: magic, length, version, adhoc
   HASHOFF B32  CDHDR B32                            \ hashOffset, identOffset
   0 B32  NCSLOT B32                                 \ nSpecialSlots, nCodeSlots
   MPAGE B32                                         \ codeLimit
   32 B8  2 B8  0 B8  12 B8                          \ hashSize, hashType, platform, pageSize
   0 B32  0 B32  0 B32  0 B32  0 B64                 \ spare2, scatter, team, spare3, codeLimit64
   0 B64  MPAGE B64  1 B64                           \ execSegBase, execSegLimit, execSegFlags(MAIN)
   SIGIDN 0 DO SIGID i + c@ B8 LOOP  0 B8            \ identifier + NUL
   NCSLOT 0 DO
      MSTART @ i CSPAGE * +  CSPAGE  SDG SHA256
      32 0 DO SDG i + c@ B8 LOOP
   LOOP ;
