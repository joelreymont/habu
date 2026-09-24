\ engine-id.f - the running engine's own resolved executable path + content key.
\
\ One concern: engine self-identity. The module lives in `package ENGINE-ID`.
\ External callers use the qualified public API `ENGINE-ID:PATH$` (the running
\ engine's own executable path) and `ENGINE-ID:KEY$` (the SHA-256 hex content key
\ over that binary); the raw self-path readers and the path buffers are
\ package-private.
\
\ The path is an ENGINE-SIDE fact taken from the kernel-provided process image,
\ not a script guess and not the caller-controlled argv[0]:
\   macOS  - proc_pidpath asks the kernel for the executable's absolute path.
\   Linux  - /proc/self/exe is the absolute canonical binary path (readlink).
\ The content key is SHA-256 (hex) of that binary, computed ONCE on first request
\ and cached, so the durable-only field never weighs on the interactive key path.
\ Fail closed with a named throw if the path cannot be resolved or the binary
\ cannot be hashed; never a placeholder (a sometimes-real key would fragment any
\ engine-keyed store).
\
\ The Linux readlink primitive is the raw boundary. Darwin uses a bounded
\ process-symbol binding; neither target depends on the caller's CWD.

require lib/errors.f
require lib/string.f
require lib/ffi-abi.f

\ ENVP-BASE / ENVP / ZLEN (src/os/env-base.f), HB-TARGET-* (src/os/<t>/target.f),
\ readlink + SHA256-FILE-HEX-IN + BYTE-COPY are engine-provided (startup prefix / baked).

package ENGINE-ID

1024 constant EID-PATH-CAP      \ max self-exe path bytes (matches lib/fs.f FS-PATH-CAP)
64 constant EID-KEY-LEN         \ SHA-256 hex digest length

create EID-PATH EID-PATH-CAP allot   variable EID-PATH-U   variable EID-PATH-DONE
create EID-KEY  EID-KEY-LEN  allot   variable EID-KEY-DONE
create EID-FSHA-CTX SHA256-FILE-CTX-BYTES allot   \ this package's file-digest context

\ NUL-terminated "/proc/self/exe" for readlink (Linux)
create EID-PROC-EXE
   char / c, char p c, char r c, char o c, char c c, char / c,
   char s c, char e c, char l c, char f c, char / c,
   char e c, char x c, char e c, 0 c,

PROCESS-SYMBOLS
FUNCTION: SELF-PATH proc_pidpath ( n ptr u8 n -- n )
   1 2 WRITES-ARG
;FUNCTION

: ENGINE-SELF-MACOS ( -- n )
   getpid EID-PATH EID-PATH-CAP SELF-PATH
   dup 0 <= over EID-PATH-CAP >= or if drop 0 then ;

TRUSTED: ENGINE-SELF-LINUX ( -- n )      \ /proc/self/exe -> EID-PATH; bytes or 0
   EID-PROC-EXE EID-PATH EID-PATH-CAP readlink
   dup 0 < if drop 0 then ;

\ Both Linux targets read /proc/self/exe; the caller turns a zero into
\ E-ENGINE-PATH, which is also what an unknown target gets.
: ENGINE-SELF-PATH ( -- n )              \ resolve self-exe into EID-PATH; bytes or 0
   HB-TARGET-MACOS? if ENGINE-SELF-MACOS exit then
   HB-TARGET-LINUX? if ENGINE-SELF-LINUX exit then
   HB-TARGET-LINUX-X86-64? if ENGINE-SELF-LINUX exit then
   0 ;

\ ---- checked public surface ------------------------------------------------
public

: PATH$ ( -- ptr u8 n )                  \ absolute/exec path of the running bin/hb
   EID-PATH-DONE @ 0= if
      ENGINE-SELF-PATH dup 0 <= if drop E-ENGINE-PATH throw then
      EID-PATH-U !  -1 EID-PATH-DONE !
   then
   EID-PATH EID-PATH-U @ ;

: KEY$ ( -- ptr u8 n )                   \ SHA-256 hex content key over the binary
   EID-KEY-DONE @ 0= if
      EID-FSHA-CTX PATH$ EID-KEY SHA256-FILE-HEX-IN 0 <> if E-ENGINE-KEY throw then
      -1 EID-KEY-DONE !
   then
   EID-KEY EID-KEY-LEN ;

;package
