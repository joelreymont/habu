\ Fixed build target and action values. CTARGET remains the wire and digest owner.
require lib/string.f
require src/compiler/target.f
require src/compiler/digest.f

package RTARGET
public

ENUM profile-id DERIVE eq
   aarch64-apple-darwin
   aarch64-unknown-linux-gnu
   x86-64-unknown-linux-gnu
   aarch64-pc-windows-msvc
   x86-64-pc-windows-msvc
   wasm32-unknown-unknown
   wasm64-unknown-unknown
;ENUM

ENUM native-profile-id DERIVE eq
   darwin-arm64
   linux-arm64
   linux-x64
;ENUM

ENUM foreign-abi DERIVE eq
   darwin-arm64
   linux-arm64
   linux-x64
   windows-arm64
   windows-x64
   wasm
;ENUM

ENUM runtime DERIVE eq
   native-v1
   wasm-v1
;ENUM

ENUM image DERIVE eq
   macho
   elf
   pe
   wasm
;ENUM

ENUM habu-abi DERIVE eq
   cell64-v1
;ENUM

STRUCTURE resolved-target 0
   FIELD profile profile-id
   FIELD features CTARGET:features
;STRUCTURE

STRUCTURE execution-platform 0
   FIELD native native-profile-id
;STRUCTURE

STRUCTURE emitter-set 0 DERIVE eq
   FIELD bits n
;STRUCTURE

STRUCTURE compiler-product 0
   FIELD executable-target resolved-target
   FIELD enabled-emitters emitter-set
   FIELD default-output-profile profile-id
;STRUCTURE

STRUCTURE build-action 0
   FIELD execution-platform execution-platform
   FIELD body-target resolved-target
;STRUCTURE

STRUCTURE build-identity 0
   FIELD target resolved-target
   FIELD action-inputs CDIGEST:digest
;STRUCTURE

ENUM core-result 0
   VARIANT supported FIELD contract CTARGET:contract ;VARIANT
   VARIANT unsupported ;VARIANT
;ENUM

\ RTARGET owns recoverable selection/action errors in an unallocated block.
-9420 constant E-FIRST
-9429 constant E-LAST
-9420 constant E-UNKNOWN
-9421 constant E-ACTIVE
-9422 constant E-UNSET
-9423 constant E-UNSUPPORTED

private

: PROFILE-ARCH ( profile-id -- CTARGET:arch )
   MATCH profile-id
      aarch64-apple-darwin OF CTARGET-ARCH:AARCH64 ENDOF
      aarch64-unknown-linux-gnu OF CTARGET-ARCH:AARCH64 ENDOF
      x86-64-unknown-linux-gnu OF CTARGET-ARCH:X86-64 ENDOF
      aarch64-pc-windows-msvc OF CTARGET-ARCH:AARCH64 ENDOF
      x86-64-pc-windows-msvc OF CTARGET-ARCH:X86-64 ENDOF
      wasm32-unknown-unknown OF CTARGET-ARCH:WASM ENDOF
      wasm64-unknown-unknown OF CTARGET-ARCH:WASM ENDOF
   ;MATCH ;

: PROFILE-PTR ( profile-id -- CTARGET:ptr-width )
   MATCH profile-id
      wasm32-unknown-unknown OF CTARGET-PTR--WIDTH:BITS32 ENDOF
      wasm64-unknown-unknown OF CTARGET-PTR--WIDTH:BITS64 ENDOF
      aarch64-apple-darwin OF CTARGET-PTR--WIDTH:BITS64 ENDOF
      aarch64-unknown-linux-gnu OF CTARGET-PTR--WIDTH:BITS64 ENDOF
      x86-64-unknown-linux-gnu OF CTARGET-PTR--WIDTH:BITS64 ENDOF
      aarch64-pc-windows-msvc OF CTARGET-PTR--WIDTH:BITS64 ENDOF
      x86-64-pc-windows-msvc OF CTARGET-PTR--WIDTH:BITS64 ENDOF
   ;MATCH ;

: PROFILE-FOREIGN ( profile-id -- foreign-abi )
   MATCH profile-id
      aarch64-apple-darwin OF RTARGET-FOREIGN--ABI:DARWIN-ARM64 ENDOF
      aarch64-unknown-linux-gnu OF RTARGET-FOREIGN--ABI:LINUX-ARM64 ENDOF
      x86-64-unknown-linux-gnu OF RTARGET-FOREIGN--ABI:LINUX-X64 ENDOF
      aarch64-pc-windows-msvc OF RTARGET-FOREIGN--ABI:WINDOWS-ARM64 ENDOF
      x86-64-pc-windows-msvc OF RTARGET-FOREIGN--ABI:WINDOWS-X64 ENDOF
      wasm32-unknown-unknown OF RTARGET-FOREIGN--ABI:WASM ENDOF
      wasm64-unknown-unknown OF RTARGET-FOREIGN--ABI:WASM ENDOF
   ;MATCH ;

\ Windows profiles are declarable compiler products, but their process and
\ foreign calling conventions have no CORE implementation yet.
: FOREIGN-KNOWN? ( profile-id -- bool )
   {: p:profile-id :}
   p RTARGET-PROFILE--ID:AARCH64-PC-WINDOWS-MSVC RTARGET-PROFILE--ID:EQ if false exit then
   p RTARGET-PROFILE--ID:X86-64-PC-WINDOWS-MSVC RTARGET-PROFILE--ID:EQ if false exit then
   true ;

: PROFILE-RUNTIME ( profile-id -- runtime )
   MATCH profile-id
      wasm32-unknown-unknown OF RTARGET-RUNTIME:WASM-V1 ENDOF
      wasm64-unknown-unknown OF RTARGET-RUNTIME:WASM-V1 ENDOF
      aarch64-apple-darwin OF RTARGET-RUNTIME:NATIVE-V1 ENDOF
      aarch64-unknown-linux-gnu OF RTARGET-RUNTIME:NATIVE-V1 ENDOF
      x86-64-unknown-linux-gnu OF RTARGET-RUNTIME:NATIVE-V1 ENDOF
      aarch64-pc-windows-msvc OF RTARGET-RUNTIME:NATIVE-V1 ENDOF
      x86-64-pc-windows-msvc OF RTARGET-RUNTIME:NATIVE-V1 ENDOF
   ;MATCH ;

: PROFILE-IMAGE ( profile-id -- image )
   MATCH profile-id
      aarch64-apple-darwin OF RTARGET-IMAGE:MACHO ENDOF
      aarch64-unknown-linux-gnu OF RTARGET-IMAGE:ELF ENDOF
      x86-64-unknown-linux-gnu OF RTARGET-IMAGE:ELF ENDOF
      aarch64-pc-windows-msvc OF RTARGET-IMAGE:PE ENDOF
      x86-64-pc-windows-msvc OF RTARGET-IMAGE:PE ENDOF
      wasm32-unknown-unknown OF RTARGET-IMAGE:WASM ENDOF
      wasm64-unknown-unknown OF RTARGET-IMAGE:WASM ENDOF
   ;MATCH ;

: PROFILE-HABU ( profile-id -- habu-abi )
   drop RTARGET-HABU--ABI:CELL64-V1 ;

: PROFILE-ENDIAN ( profile-id -- CTARGET:endian )
   drop CTARGET-ENDIAN:LITTLE ;

: DEFAULT-FEATURES ( profile-id -- CTARGET:features )
   MATCH profile-id
      wasm32-unknown-unknown OF CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH ENDOF
      wasm64-unknown-unknown OF CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH ENDOF
      aarch64-apple-darwin OF CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH ENDOF
      aarch64-unknown-linux-gnu OF CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH ENDOF
      x86-64-unknown-linux-gnu OF CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH ENDOF
      aarch64-pc-windows-msvc OF CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH ENDOF
      x86-64-pc-windows-msvc OF CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH ENDOF
   ;MATCH ;

: LABEL ( profile-id -- ptr u8 n )
   MATCH profile-id
      aarch64-apple-darwin OF s" aarch64-apple-darwin" ENDOF
      aarch64-unknown-linux-gnu OF s" aarch64-unknown-linux-gnu" ENDOF
      x86-64-unknown-linux-gnu OF s" x86_64-unknown-linux-gnu" ENDOF
      aarch64-pc-windows-msvc OF s" aarch64-pc-windows-msvc" ENDOF
      x86-64-pc-windows-msvc OF s" x86_64-pc-windows-msvc" ENDOF
      wasm32-unknown-unknown OF s" wasm32-unknown-unknown" ENDOF
      wasm64-unknown-unknown OF s" wasm64-unknown-unknown" ENDOF
   ;MATCH ;

: NAME? ( ptr u8 n ptr u8 n -- bool ) STR= ;

: PROFILE ( ptr u8 n -- profile-id )
   {: name:ptr size:n :}
   name size s" macos-aarch64" NAME? if RTARGET-PROFILE--ID:AARCH64-APPLE-DARWIN exit then
   name size s" linux-aarch64" NAME? if RTARGET-PROFILE--ID:AARCH64-UNKNOWN-LINUX-GNU exit then
   name size s" linux-x86-64" NAME? if RTARGET-PROFILE--ID:X86-64-UNKNOWN-LINUX-GNU exit then
   name size s" aarch64-apple-darwin" NAME? if RTARGET-PROFILE--ID:AARCH64-APPLE-DARWIN exit then
   name size s" aarch64-unknown-linux-gnu" NAME? if RTARGET-PROFILE--ID:AARCH64-UNKNOWN-LINUX-GNU exit then
   name size s" x86_64-unknown-linux-gnu" NAME? if RTARGET-PROFILE--ID:X86-64-UNKNOWN-LINUX-GNU exit then
   name size s" aarch64-pc-windows-msvc" NAME? if RTARGET-PROFILE--ID:AARCH64-PC-WINDOWS-MSVC exit then
   name size s" x86_64-pc-windows-msvc" NAME? if RTARGET-PROFILE--ID:X86-64-PC-WINDOWS-MSVC exit then
   name size s" wasm32-unknown-unknown" NAME? if RTARGET-PROFILE--ID:WASM32-UNKNOWN-UNKNOWN exit then
   name size s" wasm64-unknown-unknown" NAME? if RTARGET-PROFILE--ID:WASM64-UNKNOWN-UNKNOWN exit then
   E-UNKNOWN throw ;

: LOOKUP-KEEP ( ptr u8 n -- ptr u8 n )
   2dup PROFILE drop ;

: DETECT-NATIVE-PROFILE ( -- native-profile-id )
   HB-TARGET-LINUX? if RTARGET-NATIVE--PROFILE--ID:LINUX-ARM64 exit then
   HB-TARGET-MACOS? if RTARGET-NATIVE--PROFILE--ID:DARWIN-ARM64 exit then
   HB-TARGET-LINUX-X86-64? if RTARGET-NATIVE--PROFILE--ID:LINUX-X64 exit then
   E-UNSET throw ;

\ The action owner calls HOST before opening the target source window. A
\ product engine later calls this same mapping against its own baked flags.
: HOST-PROFILE ( -- native-profile-id )
   DETECT-NATIVE-PROFILE ;

: NATIVE-PROFILE ( native-profile-id -- profile-id )
   MATCH native-profile-id
      darwin-arm64 OF RTARGET-PROFILE--ID:AARCH64-APPLE-DARWIN ENDOF
      linux-arm64 OF RTARGET-PROFILE--ID:AARCH64-UNKNOWN-LINUX-GNU ENDOF
      linux-x64 OF RTARGET-PROFILE--ID:X86-64-UNKNOWN-LINUX-GNU ENDOF
   ;MATCH ;

: EMITTER-BIT ( CTARGET:arch -- n )
   MATCH CTARGET:arch
      aarch64 OF 1 ENDOF
      ptx OF 2 ENDOF
      a32 OF 4 ENDOF
      thumb2 OF 8 ENDOF
      c66x OF 16 ENDOF
      x86-64 OF 32 ENDOF
      wasm OF 64 ENDOF
   ;MATCH ;

public

: KNOWN? ( ptr u8 n -- bool )
   [: LOOKUP-KEEP ;] catch >r 2drop r>
   dup 0= if drop true exit then
   dup E-UNKNOWN = if drop false exit then
   throw ;

: RESOLVE ( ptr u8 n -- resolved-target )
   PROFILE dup DEFAULT-FEATURES RTARGET-RESOLVED--TARGET:MAKE ;

: PROFILE@ ( resolved-target -- profile-id )
   RTARGET-RESOLVED--TARGET:UNMAKE drop ;

: FEATURES@ ( resolved-target -- CTARGET:features )
   RTARGET-RESOLVED--TARGET:UNMAKE nip ;

: ARCH@ ( resolved-target -- CTARGET:arch )
   PROFILE@ PROFILE-ARCH ;

: PROFILE$ ( resolved-target -- ptr u8 n )
   PROFILE@ LABEL ;

: SAME? ( resolved-target resolved-target -- bool )
   {: x:resolved-target y:resolved-target :}
   x PROFILE@ y PROFILE@ RTARGET-PROFILE--ID:EQ
   x FEATURES@ y FEATURES@ CTARGET-FEATURES:EQ and ;

: WITH-FEATURES ( resolved-target CTARGET:features -- resolved-target )
   {: target:resolved-target extra:CTARGET:features :}
   target ARCH@ CTARGET:ARCH-MASK extra CTARGET:HAS? 0= if E-CTGT-FEATURE throw then
   target PROFILE@ target FEATURES@ extra CTARGET:WITH RTARGET-RESOLVED--TARGET:MAKE ;

: HOST ( -- execution-platform )
   HOST-PROFILE RTARGET-EXECUTION--PLATFORM:MAKE ;

: HOST-TARGET ( -- resolved-target )
   HOST-PROFILE NATIVE-PROFILE dup DEFAULT-FEATURES RTARGET-RESOLVED--TARGET:MAKE ;

: EXECUTION-TARGET ( execution-platform -- resolved-target )
   RTARGET-EXECUTION--PLATFORM:UNMAKE NATIVE-PROFILE dup DEFAULT-FEATURES RTARGET-RESOLVED--TARGET:MAKE ;

: CORE ( resolved-target -- core-result )
   {: target:resolved-target :}
   target PROFILE@ {: p:profile-id :}
   p RTARGET-PROFILE--ID:AARCH64-PC-WINDOWS-MSVC RTARGET-PROFILE--ID:EQ if
      RTARGET-CORE--RESULT:UNSUPPORTED exit then
   p RTARGET-PROFILE--ID:X86-64-PC-WINDOWS-MSVC RTARGET-PROFILE--ID:EQ if
      RTARGET-CORE--RESULT:UNSUPPORTED exit then
   p PROFILE-ARCH
   p MATCH profile-id
      aarch64-apple-darwin OF CTARGET-ABI:AAPCS64-DARWIN ENDOF
      aarch64-unknown-linux-gnu OF CTARGET-ABI:AAPCS64-LINUX ENDOF
      x86-64-unknown-linux-gnu OF CTARGET-ABI:SYSV-AMD64 ENDOF
      wasm32-unknown-unknown OF CTARGET-ABI:HABU-WASM-CELL64-V1 ENDOF
      wasm64-unknown-unknown OF CTARGET-ABI:HABU-WASM-CELL64-V1 ENDOF
      aarch64-pc-windows-msvc OF E-UNSUPPORTED throw ENDOF
      x86-64-pc-windows-msvc OF E-UNSUPPORTED throw ENDOF
   ;MATCH
   p PROFILE-ENDIAN p PROFILE-PTR target FEATURES@ CTARGET:CONTRACT
   RTARGET-CORE--RESULT:SUPPORTED ;

: NO-EMITTERS ( -- emitter-set )
   0 RTARGET-EMITTER--SET:MAKE ;

: EMITTER ( CTARGET:arch -- emitter-set )
   EMITTER-BIT RTARGET-EMITTER--SET:MAKE ;

: EMITTER-WITH ( emitter-set emitter-set -- emitter-set )
   RTARGET-EMITTER--SET:UNMAKE swap RTARGET-EMITTER--SET:UNMAKE or
   RTARGET-EMITTER--SET:MAKE ;

: PRODUCT ( resolved-target emitter-set profile-id -- compiler-product )
   RTARGET-COMPILER--PRODUCT:MAKE ;

: DEFAULT ( compiler-product -- resolved-target )
   RTARGET-COMPILER--PRODUCT:UNMAKE nip nip
   dup DEFAULT-FEATURES RTARGET-RESOLVED--TARGET:MAKE ;

: FOR-COMPILER ( execution-platform compiler-product -- build-action )
   RTARGET-COMPILER--PRODUCT:UNMAKE drop drop
   RTARGET-BUILD--ACTION:MAKE ;

: FOR-OUTPUT ( execution-platform resolved-target -- build-action )
   RTARGET-BUILD--ACTION:MAKE ;

: BODY-TARGET@ ( build-action -- resolved-target )
   RTARGET-BUILD--ACTION:UNMAKE nip ;

: EXECUTION@ ( build-action -- execution-platform )
   RTARGET-BUILD--ACTION:UNMAKE drop ;

: BUILD-IDENTITY ( resolved-target CDIGEST:digest -- build-identity )
   RTARGET-BUILD--IDENTITY:MAKE ;

: SAME-BUILD-IDENTITY? ( build-identity build-identity -- bool )
   RTARGET-BUILD--IDENTITY:UNMAKE {: yt:resolved-target yd:CDIGEST:digest :}
   RTARGET-BUILD--IDENTITY:UNMAKE {: xt:resolved-target xd:CDIGEST:digest :}
   xt yt SAME? xd yd CDIGEST-DIGEST:EQ and ;

: LINK-COMPATIBLE? ( resolved-target resolved-target -- bool )
   {: x:resolved-target y:resolved-target :}
   x PROFILE@ {: xp:profile-id :}
   y PROFILE@ {: yp:profile-id :}
   xp FOREIGN-KNOWN? yp FOREIGN-KNOWN? and 0= if false exit then
   xp PROFILE-ARCH yp PROFILE-ARCH CTARGET-ARCH:EQ
   xp PROFILE-ENDIAN yp PROFILE-ENDIAN CTARGET-ENDIAN:EQ and
   xp PROFILE-PTR yp PROFILE-PTR CTARGET-PTR--WIDTH:EQ and
   xp PROFILE-HABU yp PROFILE-HABU RTARGET-HABU--ABI:EQ and
   xp PROFILE-FOREIGN yp PROFILE-FOREIGN RTARGET-FOREIGN--ABI:EQ and
   xp PROFILE-RUNTIME yp PROFILE-RUNTIME RTARGET-RUNTIME:EQ and ;

: RUNTIME-ADMISSIBLE? ( resolved-target resolved-target -- bool )
   {: selected:resolved-target required:resolved-target :}
   selected required LINK-COMPATIBLE? 0= if false exit then
   selected FEATURES@ required FEATURES@ CTARGET:HAS? ;

: EXECUTABLE-HERE? ( execution-platform resolved-target -- bool )
   {: execution:execution-platform target:resolved-target :}
   execution EXECUTION-TARGET {: host:resolved-target :}
   host target LINK-COMPATIBLE?
   host PROFILE@ PROFILE-IMAGE target PROFILE@ PROFILE-IMAGE RTARGET-IMAGE:EQ and
   target PROFILE@ PROFILE-RUNTIME RTARGET-RUNTIME:NATIVE-V1 RTARGET-RUNTIME:EQ and ;

;package
