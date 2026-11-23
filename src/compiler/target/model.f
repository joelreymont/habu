split-str
\ Fixed build target and action values. CTARGET remains the wire and digest owner.
require src/core/exit-status.f
require src/lib/string.f
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

STRUCTURE resolved-target
   FIELD profile profile-id
   FIELD features CTARGET:features
;STRUCTURE

STRUCTURE execution-platform
   FIELD native native-profile-id
;STRUCTURE

STRUCTURE emitter-set DERIVE eq
   FIELD bits n
;STRUCTURE

STRUCTURE compiler-product
   FIELD executable-target resolved-target
   FIELD enabled-emitters emitter-set
   FIELD default-output-profile profile-id
;STRUCTURE

STRUCTURE build-action
   FIELD execution-platform execution-platform
   FIELD body-target resolved-target
;STRUCTURE

STRUCTURE build-identity
   FIELD target resolved-target
   FIELD action-inputs CDIGEST:digest
;STRUCTURE

ENUM core-result
   VARIANT supported FIELD contract CTARGET:contract ;VARIANT
   VARIANT unsupported ;VARIANT
;ENUM

\ A target's platform name, one per target; empty for a profile with none.
: PLATFORM-NAME ( profile-id -- str )
   MATCH profile-id
      aarch64-apple-darwin OF s" macos-aarch64" ENDOF
      aarch64-unknown-linux-gnu OF s" linux-aarch64" ENDOF
      x86-64-unknown-linux-gnu OF s" linux-x86-64" ENDOF
      wasm32-unknown-unknown OF s" wasm" ENDOF
      aarch64-pc-windows-msvc OF s" " ENDOF
      x86-64-pc-windows-msvc OF s" " ENDOF
      wasm64-unknown-unknown OF s" " ENDOF
   ;MATCH ;

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

\ Windows profiles are declarable targets, but CORE implements neither their
\ process nor their foreign calling convention.
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

: LABEL ( profile-id -- str )
   MATCH profile-id
      aarch64-apple-darwin OF s" aarch64-apple-darwin" ENDOF
      aarch64-unknown-linux-gnu OF s" aarch64-unknown-linux-gnu" ENDOF
      x86-64-unknown-linux-gnu OF s" x86_64-unknown-linux-gnu" ENDOF
      aarch64-pc-windows-msvc OF s" aarch64-pc-windows-msvc" ENDOF
      x86-64-pc-windows-msvc OF s" x86_64-pc-windows-msvc" ENDOF
      wasm32-unknown-unknown OF s" wasm32-unknown-unknown" ENDOF
      wasm64-unknown-unknown OF s" wasm64-unknown-unknown" ENDOF
   ;MATCH ;

: NAME? ( str str -- bool ) STR= ;

\ Q runs on each profile, in declaration order.
: EACH-PROFILE ( R [ R profile-id -- R ] -- R )
   {: q :}
   RTARGET-PROFILE--ID:AARCH64-APPLE-DARWIN q execute
   RTARGET-PROFILE--ID:AARCH64-UNKNOWN-LINUX-GNU q execute
   RTARGET-PROFILE--ID:X86-64-UNKNOWN-LINUX-GNU q execute
   RTARGET-PROFILE--ID:AARCH64-PC-WINDOWS-MSVC q execute
   RTARGET-PROFILE--ID:X86-64-PC-WINDOWS-MSVC q execute
   RTARGET-PROFILE--ID:WASM32-UNKNOWN-UNKNOWN q execute
   RTARGET-PROFILE--ID:WASM64-UNKNOWN-UNKNOWN q execute ;

\ The profile found and true; else p and true when NAMES spells p as the name.
: FIND-STEP ( profile-id bool profile-id str [ profile-id -- str ] -- profile-id bool )
   {: found:profile-id hit:bool p:profile-id name:^ size:n names :}
   hit if found true exit then
   p names execute {: a:^ u:n :}
   u 0<> a u name size NAME? and if p true exit then
   found false ;

\ The profile NAMES spells as the name and true; else false over a filler.
: FIND ( str [ profile-id -- str ] -- profile-id bool )
   {: name:^ size:n names :}
   RTARGET-PROFILE--ID:AARCH64-APPLE-DARWIN false
   [: name size names FIND-STEP ;] EACH-PROFILE ;

\ A platform name's profile and true; else false over a filler profile the caller drops.
: PLATFORM ( str -- profile-id bool )
   ['] PLATFORM-NAME FIND ;

\ A platform name's or a triple's profile and true; else false over a filler.
: PROFILE ( str -- profile-id bool )
   {: name:^ size:n :}
   name size PLATFORM if true exit then drop
   name size ['] LABEL FIND ;

\ Writes a comma before the name unless it is the first; an empty name is skipped.
: SAY-ITEM ( bool str -- bool )
   {: more:bool a:^ u:n :}
   u 0= if more exit then
   more if s" , " REJECTION:EMIT-STR then
   a u REJECTION:EMIT-STR true ;

: SAY-NAMES ( -- )
   false [: PLATFORM-NAME SAY-ITEM ;] EACH-PROFILE
   [: LABEL SAY-ITEM ;] EACH-PROFILE drop ;

\ The name came from a caller at run time, so the rejection has no source token.
: UNKNOWN ( str -- )
   {: name:^ size:n :}
   s" E-UNKNOWN-TARGET" REJECTION:OPEN
   96 REJECTION:EMIT1  name size REJECTION:EMIT-STR  96 REJECTION:EMIT1
   s"  is not a target Habu knows" REJECTION:EMIT-STR
   s" targets" REJECTION:NOTE  SAY-NAMES
   REJECTION:HELP
   s" name one of the targets listed, as `aarch64-unknown-linux-gnu`"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: DETECT-NATIVE-PROFILE ( -- native-profile-id )
   HB-TARGET-LINUX? if RTARGET-NATIVE--PROFILE--ID:LINUX-ARM64 exit then
   HB-TARGET-MACOS? if RTARGET-NATIVE--PROFILE--ID:DARWIN-ARM64 exit then
   HB-TARGET-LINUX-X86-64? if RTARGET-NATIVE--PROFILE--ID:LINUX-X64 exit then
   s" rtarget: the engine was built for no host it knows" EXIT-STATUS:INTERNAL die ;

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

: KNOWN? ( str -- bool ) PROFILE nip ;

\ True for a platform name, one per target; false for a triple.
: PLATFORM? ( str -- bool ) PLATFORM nip ;

: RESOLVE ( str -- resolved-target )
   {: name:^ size:n :}
   name size PROFILE 0= if drop name size UNKNOWN then
   dup DEFAULT-FEATURES RTARGET-RESOLVED--TARGET:MAKE ;

: PROFILE@ ( resolved-target -- profile-id )
   RTARGET-RESOLVED--TARGET:UNMAKE drop ;

: FEATURES@ ( resolved-target -- CTARGET:features )
   RTARGET-RESOLVED--TARGET:UNMAKE nip ;

: ARCH@ ( resolved-target -- CTARGET:arch )
   PROFILE@ PROFILE-ARCH ;

\ The file format the target's programs are written in.
: IMAGE@ ( resolved-target -- image )
   PROFILE@ PROFILE-IMAGE ;

: PROFILE$ ( resolved-target -- str )
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
   p PROFILE-ARCH
   p MATCH profile-id
      aarch64-apple-darwin OF CTARGET-ABI:AAPCS64-DARWIN ENDOF
      aarch64-unknown-linux-gnu OF CTARGET-ABI:AAPCS64-LINUX ENDOF
      x86-64-unknown-linux-gnu OF CTARGET-ABI:SYSV-AMD64 ENDOF
      wasm32-unknown-unknown OF CTARGET-ABI:HABU-WASM-CELL64-V1 ENDOF
      wasm64-unknown-unknown OF CTARGET-ABI:HABU-WASM-CELL64-V1 ENDOF
      aarch64-pc-windows-msvc OF drop RTARGET-CORE--RESULT:UNSUPPORTED exit ENDOF
      x86-64-pc-windows-msvc OF drop RTARGET-CORE--RESULT:UNSUPPORTED exit ENDOF
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
