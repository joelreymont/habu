\ codex-home.f - checked Codex benchmark home isolation.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, and lib/process-env.f.

-3224 constant E-MRUN-CODEX-HOME

create CX-SRC-BUF FS-PATH-CAP allot
create CX-HOME-BUF FS-PATH-CAP allot
create CX-SRC-FILE-BUF FS-PATH-CAP allot
create CX-HOME-FILE-BUF FS-PATH-CAP allot

variable CX-SRC-U
variable CX-HOME-U
variable CX-SRC-FILE-U
variable CX-HOME-FILE-U

: CX-SRC$ ( -- ptr u8 n )
   CX-SRC-BUF CX-SRC-U @ ;

: CX-HOME$ ( -- ptr u8 n )
   CX-HOME-BUF CX-HOME-U @ ;

: CX-SRC-FILE$ ( -- ptr u8 n )
   CX-SRC-FILE-BUF CX-SRC-FILE-U @ ;

: CX-HOME-FILE$ ( -- ptr u8 n )
   CX-HOME-FILE-BUF CX-HOME-FILE-U @ ;

: CX-COPY-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u 0 <= if E-MRUN-CODEX-HOME throw then
   u FS-PATH-CAP > if E-MRUN-CODEX-HOME throw then
   a dst u BYTE-COPY
   u lenp ! ;

: CX-SRC! ( ptr u8 n -- )
   CX-SRC-BUF CX-SRC-U CX-COPY-PATH! ;

: CX-HOME! ( ptr u8 n -- )
   CX-HOME-BUF CX-HOME-U CX-COPY-PATH! ;

: CX-SRC-FILE! ( ptr u8 n -- ) {: name:ptr nameu :}
   CX-SRC$ name nameu CX-SRC-FILE-BUF JOIN-PATH CX-SRC-FILE-U ! ;

: CX-HOME-FILE! ( ptr u8 n -- ) {: name:ptr nameu :}
   CX-HOME$ name nameu CX-HOME-FILE-BUF JOIN-PATH CX-HOME-FILE-U ! ;

: CX-LINK-REQUIRED ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu CX-SRC-FILE!
   CX-SRC-FILE$ EXISTS? 0= if E-MRUN-CODEX-HOME throw then
   name nameu CX-HOME-FILE!
   CX-HOME-FILE$ EXISTS? 0= if CX-SRC-FILE$ CX-HOME-FILE$ MAKE-SYMLINK then ;

: CX-LINK-OPTIONAL ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu CX-SRC-FILE!
   CX-SRC-FILE$ EXISTS? 0= if exit then
   name nameu CX-HOME-FILE!
   CX-HOME-FILE$ EXISTS? 0= if CX-SRC-FILE$ CX-HOME-FILE$ MAKE-SYMLINK then ;

: CODEX-HOME-PREPARE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu home:ptr homeu :}
   src srcu CX-SRC!
   home homeu CX-HOME!
   CX-HOME$ MAKE-DIRS
   CX-HOME$ FS-MUT-MODE-PRIVATE-DIR CHMOD-MODE
   s" auth.json" CX-LINK-REQUIRED
   s" models_cache.json" CX-LINK-OPTIONAL
   s" version.json" CX-LINK-OPTIONAL
   s" CODEX_HOME" CX-HOME$ PROC-ENV+ ;

: CX-NONEMPTY$ ( ptr u8 n -- ptr u8 n )
   dup 0= if E-MRUN-CODEX-HOME throw then ;

: CX-REQUIRE-HOME$ ( -- ptr u8 n )
   s" HOME" GETENV CX-NONEMPTY$ ;

: CX-USER$ ( -- ptr u8 n )
   s" USER" GETENV dup 0= if 2drop s" user" then ;

: CX-TMPDIR$ ( -- ptr u8 n )
   s" TMPDIR" GETENV dup 0= if 2drop s" /tmp" then ;

: CX-BENCH-NAME$ ( -- ptr u8 n )
   SB-RESET
   s" habu-codex-bench-home-" SB-APPEND
   CX-USER$ SB-APPEND
   SB$ ;

: CX-SOURCE-ENV$ ( -- ptr u8 n )
   s" CODEX_SOURCE_HOME" GETENV dup 0= if
      2drop
      CX-REQUIRE-HOME$ s" .codex" CX-SRC-BUF JOIN-PATH CX-SRC-U !
      CX-SRC$ exit
   then
   CX-SRC!
   CX-SRC$ ;

: CX-BENCH-HOME-ENV$ ( -- ptr u8 n )
   s" CODEX_BENCH_HOME" GETENV dup 0= if
      2drop
      CX-TMPDIR$ CX-BENCH-NAME$ CX-HOME-BUF JOIN-PATH CX-HOME-U !
      CX-HOME$ exit
   then
   CX-HOME!
   CX-HOME$ ;

: CODEX-HOME-PREPARE-ENV ( -- )
   CX-SOURCE-ENV$ CX-BENCH-HOME-ENV$ CODEX-HOME-PREPARE ;
