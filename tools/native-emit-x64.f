\ The linux-x86-64 writer: links a captured window to the x86-64 kernel and
\ writes the image (docs/x86-64.md "The write-time link").
\
\ WHY ITS OWN FILE, LOADED BEFORE THE WINDOW. tools/native-emit.f loads after
\ the capture, so the window's compiler compiles it: CHECKER-REG:SEAL
\ (src/habu/native-runtime.f) installs that compiler in the dispatch cell this
\ process shares, and an x86-64 window's compiler lowers for x86-64, which the
\ AArch64 engine running the build cannot run. tools/native-build-core.f
\ therefore loads this file for a linux-x86-64 target before it resets for the
\ window, and ahead of the AArch64 code layer, whose globals (CODE, LBL,
\ ASM-LEN) would refuse X64CODE's tails; it holds WRITE and OPEN-SHADOW by xt.
\ The image's routines are the x86-64 shadow the build host's compiler files
\ beside each definition while the window loads (OPEN-SHADOW), never the
\ window's payload, which is AArch64 code.
\
\ THE STREAM. Byte 0, the ELF entry, jumps over the kernel to the boot. The
\ kernel's rows go first, so X64LINK:LAYOUT can enter each of its bodies in the
\ record that names it; then the boot of a linked image
\ (X64BOOT:LINKED-START,), the kernel text marked native, and the entry
\ (X64BOOT:ENTRY,).
\
\ THE FILE. The ELF src/os/linux-x86-64/elf.f builds, then the region's records
\ (X64LINK:DICT$) at ELF-REGION-AT and its code band (X64LINK:CODE$) DICT-SIZE
\ past them, then the DATA (X64LINK:DATA$) at ELF-DATA-AT up to its last nonzero
\ byte, every gap zero. Each fixed segment's memsz keeps its whole span, which
\ the kernel maps zero past the bytes the file carries.
require lib/byte-buffer.f
require lib/fs.f
require lib/le.f
require src/habu/image-x64.f
require src/compiler/native/shadow.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/passes.f
require src/habu/primitive-registry.f
require src/habu/boot-x64.f
require src/habu/kernel-x64.f
require src/habu/code-origin-x64.f
require src/habu/link-x64.f
require src/habu/aot-decl.f
require src/habu/aot-runtime.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-owned.f
require tools/native-layout.f

package NATIVE-EMIT

private

74 constant REFUSE-RC
variable TEXT-CODE-END

\ A window captured with no x86-64 shadow open has no routine to link.
: ?SHADOW ( -- )
   AOT-SHADOW:REC-N @ 0= if
      s" native-emit: the window carries no x86-64 routines; NATIVE-EMIT:OPEN-SHADOW opens their shadow before the window loads" REFUSE-RC die
   then ;

: LINKED-PROVIDED? ( n -- bool ) {: cell:n :}
   X64LINK:DATA$ cell X64KERNEL:PROVIDED-FILLED? ;

: STREAM, ( bool -- ) {: scoped:bool :}
   X64CODE:ASM-RESET
   X64CODE:LBL X64CODE:LBL {: start:label text:label :}
   start X64CODE:JMP,
   ENGINE-PRIMS:RESET
   AOT-RUNTIME:COMPLETE? ENGINE-PRIMS:SEEDED!
   text X64CODE:LBL,
   X64KERNEL:KERNEL,
   X64LINK:LAYOUT
   [: LINKED-PROVIDED? ;] ENGINE-PRIMS:COMPLETE
   scoped if X64LINK:C2-ENTRIES else 0 0 0 0 0 0 0 then
   X64KERNEL:SCOPE-KIND-HELPER,
   start X64CODE:LBL,
   X64LINK:RECORDS X64LINK:CP-VA X64KERNEL:FLOORREC-LBL
      [: X64KERNEL:HIDX-BUILD, ;] X64BOOT:SNAP-START,
   text start X64PROV:TEXT-NATIVE,
   X64BOOT:ENTRY,
   X64CODE:TEXT-SITES, TEXT-CODE-END !
   X64BOOT:TEXT-END, ;

: FILE! ( ptr u8 n -- )
   {: path:ptr size:n :}
   path size FS-PATHZ FS-O-WRONLY FS-O-CREAT or FS-O-TRUNC or
   FS-MODE-0644 open {: fd:n :}
   fd 0 < if s" native-emit: cannot open image" REFUSE-RC die then
   X64LINK:TEXT-VA X64CODE:ASM-LINK
   X64LINK:REGION$ {: region:ptr region-len:n :}
   X64LINK:DATA$ {: data:ptr data-len:n :}
   X64LINK:TEXT-VA TEXT-CODE-END @ + data CODE-END-CELL + LE:U64!
   X64CODE:CODE X64CODE:ASM-LEN region region-len data data-len fd
   X64IMAGE:WRITE-FD
   fd close ;

: WRITE-CORE ( AOT-OWNED:capture ptr n n ptr u8 n bool -- )
   {: host:ptr count:n path:ptr size:n scoped:bool :}
   AOT-FILE:IMPORT
   scoped if AOT-RUNTIME:COMPLETE? 0= if
      s" native-emit: C2 entries require complete runtime" REFUSE-RC die
   then then
   host count NATIVE-LAYOUT:TRANSLATE-ROWS
   ?SHADOW
   X64CODE:ASM-SINK X64CODE:CODE-CAP-BYTES BUF:N>BLEN BUF:INIT
   scoped STREAM,
   path size FILE!
   X64CODE:ASM-SINK BUF:DISPOSE ;

public

\ Open the shadow the window's x86-64 routines are filed in: the build host's
\ compiler lowers each definition the window loads for x86-64 as well.
: OPEN-SHADOW ( bool -- )
   if X64ABI:BINDING NSHADOW:OPEN-NATIVE
   else X64ABI:BINDING NSHADOW:OPEN then ;

\ Ordinary captured fragments grant no C2 scope authority.
: WRITE ( AOT-OWNED:capture ptr n n ptr u8 n -- )
   false WRITE-CORE ;

\ The complete native runtime grants scope only to its seven fixed entries.
: WRITE-C2 ( AOT-OWNED:capture ptr n n ptr u8 n -- )
   true WRITE-CORE ;

;package
