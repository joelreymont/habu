\ codex-home-test.f - focused tests for bench/llm/codex-home.f.

8192 constant CHT-CAP
1000 constant CHT-TIMEOUT-MS

create CHT-ROOT-BUF FS-PATH-CAP allot
create CHT-SRC-BUF FS-PATH-CAP allot
create CHT-HOME-BUF FS-PATH-CAP allot
create CHT-HOME2-BUF FS-PATH-CAP allot
create CHT-BAD-SRC-BUF FS-PATH-CAP allot
create CHT-AUTH-SRC-BUF FS-PATH-CAP allot
create CHT-AUTH-LINK-BUF FS-PATH-CAP allot
create CHT-MODELS-SRC-BUF FS-PATH-CAP allot
create CHT-MODELS-LINK-BUF FS-PATH-CAP allot
create CHT-VERSION-LINK-BUF FS-PATH-CAP allot
create CHT-READLINK-BUF FS-PATH-CAP allot
create CHT-OUT-BUF CHT-CAP allot
create CHT-ERR-BUF CHT-CAP allot

variable CHT-ROOT-U
variable CHT-SRC-U
variable CHT-HOME-U
variable CHT-HOME2-U
variable CHT-BAD-SRC-U
variable CHT-AUTH-SRC-U
variable CHT-AUTH-LINK-U
variable CHT-MODELS-SRC-U
variable CHT-MODELS-LINK-U
variable CHT-VERSION-LINK-U
variable CHT-OUT-U
variable CHT-ERR-U
variable CHT-RC

: CHT-ROOT$ ( -- ptr u8 n )
   CHT-ROOT-BUF CHT-ROOT-U @ ;

: CHT-SRC$ ( -- ptr u8 n )
   CHT-SRC-BUF CHT-SRC-U @ ;

: CHT-HOME$ ( -- ptr u8 n )
   CHT-HOME-BUF CHT-HOME-U @ ;

: CHT-HOME2$ ( -- ptr u8 n )
   CHT-HOME2-BUF CHT-HOME2-U @ ;

: CHT-BAD-SRC$ ( -- ptr u8 n )
   CHT-BAD-SRC-BUF CHT-BAD-SRC-U @ ;

: CHT-AUTH-SRC$ ( -- ptr u8 n )
   CHT-AUTH-SRC-BUF CHT-AUTH-SRC-U @ ;

: CHT-AUTH-LINK$ ( -- ptr u8 n )
   CHT-AUTH-LINK-BUF CHT-AUTH-LINK-U @ ;

: CHT-MODELS-SRC$ ( -- ptr u8 n )
   CHT-MODELS-SRC-BUF CHT-MODELS-SRC-U @ ;

: CHT-MODELS-LINK$ ( -- ptr u8 n )
   CHT-MODELS-LINK-BUF CHT-MODELS-LINK-U @ ;

: CHT-VERSION-LINK$ ( -- ptr u8 n )
   CHT-VERSION-LINK-BUF CHT-VERSION-LINK-U @ ;

: CHT-OUT$ ( -- ptr u8 n )
   CHT-OUT-BUF CHT-OUT-U @ ;

: CHT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CHT-JOIN-ROOT! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu dst:ptr lenp:ptr :}
   CHT-ROOT$ name nameu dst JOIN-PATH lenp ! ;

: CHT-JOIN-SRC! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu dst:ptr lenp:ptr :}
   CHT-SRC$ name nameu dst JOIN-PATH lenp ! ;

: CHT-JOIN-HOME! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu dst:ptr lenp:ptr :}
   CHT-HOME$ name nameu dst JOIN-PATH lenp ! ;

: CHT-PATHS! ( -- )
   s" src" CHT-SRC-BUF CHT-SRC-U CHT-JOIN-ROOT!
   s" home" CHT-HOME-BUF CHT-HOME-U CHT-JOIN-ROOT!
   s" home2" CHT-HOME2-BUF CHT-HOME2-U CHT-JOIN-ROOT!
   s" bad-src" CHT-BAD-SRC-BUF CHT-BAD-SRC-U CHT-JOIN-ROOT!
   s" auth.json" CHT-AUTH-SRC-BUF CHT-AUTH-SRC-U CHT-JOIN-SRC!
   s" auth.json" CHT-AUTH-LINK-BUF CHT-AUTH-LINK-U CHT-JOIN-HOME!
   s" models_cache.json" CHT-MODELS-SRC-BUF CHT-MODELS-SRC-U CHT-JOIN-SRC!
   s" models_cache.json" CHT-MODELS-LINK-BUF CHT-MODELS-LINK-U CHT-JOIN-HOME!
   s" version.json" CHT-VERSION-LINK-BUF CHT-VERSION-LINK-U CHT-JOIN-HOME! ;

: CHT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-codex-home" TMPDIR-MKDIR CHT-ROOT-BUF CHT-ROOT-U CHT-COPY!
   CHT-ROOT$ CLEANUP-TREE+
   CHT-PATHS!
   CHT-SRC$ MAKE-DIR
   CHT-BAD-SRC$ MAKE-DIR
   CHT-AUTH-SRC$ s" auth" WRITE-ALL
   CHT-MODELS-SRC$ s" models" WRITE-ALL ;

: CHT-READ-LINK= ( ptr u8 n ptr u8 n -- ) {: link:ptr linku want:ptr wantu :}
   link linku CHT-READLINK-BUF FS-PATH-CAP READ-LINK wantu T=
   CHT-READLINK-BUF wantu want wantu T$= ;

: CHT-CODEX-HOME-ENTRY$ ( -- ptr u8 n )
   SB-RESET
   s" CODEX_HOME=" SB-APPEND
   CHT-HOME$ SB-APPEND
   SB$ ;

: CHT-RUN-ENV ( -- )
   PROC-ENV-INHERIT-MISSING
   s" /usr/bin/env" >LEN CHT-OUT-BUF CHT-CAP >LEN
   CHT-ERR-BUF CHT-CAP >LEN CHT-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE {: outu erru rc :}
   rc RC>N CHT-RC !
   erru LEN>N CHT-ERR-U !
   outu LEN>N CHT-OUT-U ! ;

: CHT-TEST-PREPARE ( -- )
   PROC-ARGV-ENV-RESET
   CHT-SRC$ CHT-HOME$ CODEX-HOME-PREPARE
   CHT-HOME$ DIR? TTRUE
   CHT-HOME$ STAT-MODE FS-MUT-MODE-PERM and FS-MUT-MODE-PRIVATE-DIR T=
   CHT-AUTH-LINK$ SYMLINK? TTRUE
   CHT-AUTH-LINK$ CHT-AUTH-SRC$ CHT-READ-LINK=
   CHT-MODELS-LINK$ SYMLINK? TTRUE
   CHT-MODELS-LINK$ CHT-MODELS-SRC$ CHT-READ-LINK=
   CHT-VERSION-LINK$ EXISTS? TFALSE
   CHT-SRC$ CHT-HOME$ CODEX-HOME-PREPARE
   CHT-RUN-ENV
   CHT-RC @ 0 T=
   CHT-OUT$ CHT-CODEX-HOME-ENTRY$ CONTAINS? TTRUE ;

: CHT-MISSING-AUTH ( -- )
   PROC-ARGV-ENV-RESET
   CHT-BAD-SRC$ CHT-HOME2$ CODEX-HOME-PREPARE ;

: CODEX-HOME-TEST-MAIN ( -- )
   T-RESET
   CHT-PREPARE
   CHT-TEST-PREPARE
   [: CHT-MISSING-AUTH ;] E-MRUN-CODEX-HOME TTHROWSQ
   CLEANUP-RUN
   CHT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" codex-home-test: ok" type cr ;

CODEX-HOME-TEST-MAIN
