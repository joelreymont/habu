\ engine-id.f - the running engine's own resolved executable path + content key.
\
\ One concern: engine self-identity. The path is an ENGINE-SIDE fact taken from
\ the kernel-provided process image, not a script guess and not the
\ caller-controlled argv[0]:
\   macOS  - the apple[] array (contiguous after envp on the entry stack) carries
\            `executable_path=<exec-path>`, the same source _NSGetExecutablePath
\            reads. ENVP-CELL is captured at engine startup (src/habu EM-DATA-INIT),
\            so ENVP-BASE reaches apple[] with no extra syscall.
\   Linux  - /proc/self/exe is the absolute canonical binary path (readlink).
\ The content key is SHA-256 (hex) of that binary, computed ONCE on first request
\ and cached, so the durable-only field never weighs on the interactive key path.
\ Fail closed with a named throw if the path cannot be resolved or the binary
\ cannot be hashed; never a placeholder (a sometimes-real key would fragment any
\ engine-keyed store).
\
\ The raw self-path read (apple[] pointer walk / readlink syscall) is the one
\ boundary the checker cannot express (ptr-NULL tests, indexing the startup image
\ past envp); it lives in the two TRUSTED helpers below, each with a TRUSTED.md
\ row. Everything public is checked.

require lib/errors.f
require lib/string.f

\ ENVP-BASE / ENVP / ZLEN (src/os/env-base.f), HB-TARGET-* (src/os/<t>/target.f),
\ readlink + SHA256-FILE-HEX + BYTE-COPY are engine-provided (startup prefix / baked).

1024 constant EID-PATH-CAP      \ max self-exe path bytes (matches lib/fs.f FS-PATH-CAP)
64 constant EID-KEY-LEN         \ SHA-256 hex digest length

create EID-PATH EID-PATH-CAP allot   variable EID-PATH-U   variable EID-PATH-DONE
create EID-KEY  EID-KEY-LEN  allot   variable EID-KEY-DONE
variable EID-I                        \ apple[] scan cursor

\ NUL-terminated "/proc/self/exe" for readlink (Linux)
create EID-PROC-EXE
   char / c, char p c, char r c, char o c, char c c, char / c,
   char s c, char e c, char l c, char f c, char / c,
   char e c, char x c, char e c, 0 c,

: EID-EXE-PREFIX$ ( -- ptr u8 n )  s" executable_path=" ;

\ ---- raw self-path read (TRUSTED: syscall / startup-image pointer boundary) ----
TRUSTED: ENGINE-SELF-MACOS ( -- n )      \ apple[] executable_path -> EID-PATH; bytes or 0
   ENVP-BASE 0= if 0 exit then
   0 begin dup ENVP 0= 0= while 1+ repeat 1+ EID-I !   \ EID-I = first apple[] index
   begin EID-I @ ENVP dup 0= 0= while                  \ ( entry )
      dup ZLEN                                         \ ( entry u )
      2dup EID-EXE-PREFIX$ STARTS-WITH? if             \ ( entry u )
         16 - dup 0 <= if 2drop 0 exit then            \ ( entry pu )  pu>0
         dup EID-PATH-CAP > if 2drop 0 exit then       \ ( entry pu )  pu fits
         >r 16 + EID-PATH r@ BYTE-COPY r>              \ copy entry+16 -> EID-PATH, keep pu
         exit
      then
      2drop  EID-I @ 1+ EID-I !
   repeat drop 0 ;

TRUSTED: ENGINE-SELF-LINUX ( -- n )      \ /proc/self/exe -> EID-PATH; bytes or 0
   EID-PROC-EXE EID-PATH EID-PATH-CAP readlink
   dup 0 < if drop 0 then ;

: ENGINE-SELF-PATH ( -- n )              \ resolve self-exe into EID-PATH; bytes or 0
   HB-TARGET-MACOS? if ENGINE-SELF-MACOS exit then
   HB-TARGET-LINUX? if ENGINE-SELF-LINUX exit then
   0 ;

\ ---- checked public surface ------------------------------------------------
: ENGINE-PATH$ ( -- ptr u8 n )           \ absolute/exec path of the running bin/hb
   EID-PATH-DONE @ 0= if
      ENGINE-SELF-PATH dup 0 <= if drop E-ENGINE-PATH throw then
      EID-PATH-U !  -1 EID-PATH-DONE !
   then
   EID-PATH EID-PATH-U @ ;

: ENGINE-KEY$ ( -- ptr u8 n )            \ SHA-256 hex content key over the binary
   EID-KEY-DONE @ 0= if
      ENGINE-PATH$ EID-KEY SHA256-FILE-HEX 0 <> if E-ENGINE-KEY throw then
      -1 EID-KEY-DONE !
   then
   EID-KEY EID-KEY-LEN ;
