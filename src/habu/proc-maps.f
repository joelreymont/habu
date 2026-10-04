\ proc-maps.f - the memory areas THIS process holds, enumerated by its kernel.
\
\ Linux reads /proc/self/maps; macOS walks mach_vm_region_recurse, descending
\ into submaps and omitting permanently inaccessible reservations. Neither
\ target invents an address range when the query fails.
\ Its caller is src/habu/aot-closure.f CELL-MAPPED?, which asks whether a
\ persistent cell holds an address the linking process mapped.
\
\ WHY THE KERNEL AND NOT A REGISTRY. `mmap` is reached from lib/memory.f
\ (MEM-MMAP-RC, MEM-ALLOC-GUARDED), lib/vector.f, lib/aio.f,
\ src/compiler/native/string.f, src/os/image-bytes.f, src/arch/arm64/icode.f and
\ more, and a foreign allocator linked into the process calls the primitive
\ without passing any of them. A registry kept by one library would answer for
\ its own allocations alone. The kernel answers for every area the process
\ holds, whoever made it. NO VALUE-RANGE GUESS LIVES HERE: an address is mapped
\ only because the kernel listed the area it falls in.
\
\ ON LINUX EACH AREA IS TAGGED WITH WHETHER OUR OWN EXECUTABLE BACKS IT, comparing the
\ line's pathname field with `readlink /proc/self/exe`. That is the one area
\ class whose ADDRESS is not the kernel's choice: bin/hb and the images it links
\ are both EXEC-type ELFs loaded at the same fixed base (0x400000, measured on
\ both), so those addresses mean as much in the image as in the build, and small
\ integers land in the band - a cell holding the last three bytes of a string
\ has the value s0 + s1<<8 + s2<<16, which falls inside [0x400000,0x7a4000)
\ whenever the third byte is a letter. Every other area - anonymous or another
\ file - sits where the kernel put it for this run alone. The caller decides
\ what to do with the tag; this file only reports it.
\ macOS Mach-O images use ASLR: their mappings have neither this exemption nor
\ Linux's brk exemption below, so both tags are false for Darwin regions.
\
\ THE BRK AREA IS TAGGED TOO, by the kernel's own name for it, `[heap]`. Its
\ ADDRESS is not the kernel's choice in a second way: arm64 randomizes the break
\ over a gigabyte above the executable's end, so the band lands anywhere in
\ [0x7a4000,0x407a4000) and an ordinary 32-bit-shaped integer - a hash, a packed
\ pair of fields, four bytes of a string - lands inside whatever part of it the
\ process has taken. No Habu word allocates from the break (lib/memory.f maps),
\ so the tag is how a caller tells that band apart from an allocation.
\
\ WHAT IT CANNOT SEE is an area that was mapped and then unmapped: a pointer
\ into freed memory is as dead as one into a live area, and this reader calls it
\ unmapped. A pointer into such an area is invisible here; a plain integer whose
\ value lands inside a live area is not told apart from a pointer into it, and
\ nothing in a cell's value can tell them apart.
\
\ ITS OWN STATE IS NOT WINDOW-SAFE and an application must not require it. The
\ table heads are mmap addresses and the parser's pending pair holds the last
\ line's start until that line ends, which is the very thing a stripped build
\ refuses: a program requiring this file is refused at word=PEND-LO, rc 70
\ (measured). The linker loads it ABOVE the capture window (tools/aot-build-core.f),
\ where none of its cells are the image's to carry.
\
\ A CAPTURE DROPS THE MAP WHOLE. The rows, the Linux text snapshot and the one
\ cell that says a read completed and how many areas it found are DYNAMIC-BUFFER
\ storage. Every image capture releases them (src/core/dynamic-storage.f
\ RELEASE-ALL, run by the
\ snapshot writer's CANON-DATA), so an image restored from any process starts
\ with no map and reads its own at the first question. No DATA cell says
\ whether the map was read, because DATA is copied: the linker reads the map
\ inside every IMAGE-LIFECYCLE:PREPARE (tools/aot-build-core.f), and while a
\ DATA flag and count said so, an image saved from a process that loaded the
\ linker started marked loaded, with its builder's count, over released rows -
\ its first question threw E-BOUNDS (7122) and EXTENTS answered the builder's
\ count (measured on the gate's keyed linker image, test/preloaded-engine.f).
\
\ THE PARSE IS A STATE MACHINE OVER THE BYTES, not a line reader. Every line is
\ `<start>-<end> <perms> <offset> <dev> <inode> <path>`, the path field may be
\ absent, and a deleted file adds a further ` (deleted)` field - which is why the
\ path is taken as field four after the address pair and not as the last field.
\ A path can reach PATH_MAX, and the file can span any number of reads.
\
\ THE TABLE IS ASCENDING BECAUSE THE KERNEL WRITES IT THAT WAY, area by area in
\ address order, which is what makes the lookup a binary search. A file that
\ broke that order would silently break the search, so the reader refuses it
\ rather than sorting bytes whose shape it has already stopped trusting.
\ Capture the file before appending rows: table growth maps and unmaps memory,
\ which can change a live /proc/self/maps stream between its read calls. If the
\ byte buffer fills, close the file, grow the buffer, and read again from the
\ beginning; no mapping changes from this reader occur during a complete pass.

require lib/ffi-abi.f
require lib/le.f

package PROC-MAPS

74 constant FAIL-RC              \ the internal-driver exit status, as fdio.f uses
4096 constant SNAP-INIT-CAP      \ initial read budget, doubled on a full pass
4096 constant MAPS-PATH-MAX      \ PATH_MAX, the longest pathname a line can carry
4 constant PATH-FIELD            \ fields after the address pair: perms dev offset inode PATH

0 constant ST-LO                 \ reading the start address
1 constant ST-HI                 \ ... the end address
2 constant ST-REST               \ ... walking the fields after it

create PATH-BUF MAPS-PATH-MAX allot
create EXE-BUF MAPS-PATH-MAX allot
DYNAMIC-BUFFER SNAP-BYTES u8       \ complete Linux maps text, released on capture
DYNAMIC-BUFFER EXT-LO n
DYNAMIC-BUFFER EXT-HI n
DYNAMIC-BUFFER EXT-SELF n        \ 1 when our own executable backs the area, else 0
DYNAMIC-BUFFER EXT-HEAP n        \ 1 when the kernel named the area [heap], else 0
DYNAMIC-BUFFER EXT-READ n        \ one cell once a read completes: the areas it found
variable EXT-N                   \ areas the read in progress has listed
variable ST
variable ACC
variable PEND-LO                 \ the line's pair, held until its path is known
variable PEND-HI
variable FLD                     \ which field after the address pair
variable INFLD
variable PATH-U
variable PATH-OVER               \ a pathname longer than MAPS-PATH-MAX: not ours, on purpose
variable EXE-U
variable FD
variable GOT
variable SNAP-CAP
variable SNAP-U
\ The search cursor is storage rather than locals because a local binds once and
\ this loop moves its bounds; the linker is single-threaded, as the tables in
\ src/habu/aot-closure.f beside it are.
variable BS-LO  variable BS-HI  variable BS-MID  variable BS-AT

: ROW+ ( n n n n -- ) {: lo:n hi:n self:n heap:n :}
   hi lo <= IF s" proc-maps: area has no ascending extent" FAIL-RC die THEN
   EXT-N @ 0 > IF
      lo EXT-N @ 1- EXT-HI @ < IF
         s" proc-maps: areas are not in ascending order" FAIL-RC die THEN
   THEN
   EXT-N @ 1+ EXT-LO-RESERVE
   EXT-N @ 1+ EXT-HI-RESERVE
   EXT-N @ 1+ EXT-SELF-RESERVE
   EXT-N @ 1+ EXT-HEAP-RESERVE
   lo EXT-N @ EXT-LO !
   hi EXT-N @ EXT-HI !
   self EXT-N @ EXT-SELF !
   heap EXT-N @ EXT-HEAP !
   EXT-N @ 1+ EXT-N ! ;

\ Is the pathname this line carried the given one? A truncated one is answered
\ no, which records the area rather than excusing it.
: PATH-IS? ( ptr u8 n -- bool ) {: name:ptr nameu:n :}
   PATH-OVER @ IF false EXIT THEN
   PATH-U @ nameu <> IF false EXIT THEN
   nameu 0 ?do
      PATH-BUF i + c@  name i + c@ <> IF false unloop EXIT THEN
   loop true ;

\ Our own executable's, or the kernel's own name for the brk area - that one is
\ not a path, so the bytes are compared against the literal.
: EXE-PATH? ( -- bool )  EXE-BUF EXE-U @ PATH-IS? ;
: HEAP-PATH? ( -- bool )  s" [heap]" PATH-IS? ;

: LINE-END ( -- )
   PEND-LO @ PEND-HI @
   EXE-PATH? IF 1 ELSE 0 THEN
   HEAP-PATH? IF 1 ELSE 0 THEN ROW+
   0 ACC !  ST-LO ST ! ;

: HEXVAL ( n -- n ) {: c:n :}     \ the hex digit's value, or -1 for anything else
   c 48 >= c 58 < and IF c 48 - EXIT THEN
   c 97 >= c 103 < and IF c 87 - EXIT THEN
   c 65 >= c 71 < and IF c 55 - EXIT THEN
   -1 ;

: PATH-C+ ( n -- ) {: c:n :}
   PATH-U @ MAPS-PATH-MAX >= IF true PATH-OVER !  EXIT THEN
   c PATH-BUF PATH-U @ + c!
   PATH-U @ 1+ PATH-U ! ;

\ The fields after the address pair, one byte at a time: a run of non-spaces is
\ one field, and the PATH-FIELD'th of them is the pathname.
: REST-BYTE ( n -- ) {: c:n :}
   c 10 = IF LINE-END EXIT THEN
   c 32 = IF
      INFLD @ IF FLD @ 1+ FLD !  false INFLD ! THEN
      EXIT THEN
   true INFLD !
   FLD @ PATH-FIELD = IF c PATH-C+ THEN ;

: BYTE ( n -- ) {: c:n :}
   ST @ ST-REST = IF c REST-BYTE EXIT THEN
   c HEXVAL {: d:n :}
   d 0 >= IF ACC @ 4 lshift d + ACC ! EXIT THEN
   ST @ ST-LO = IF
      c 45 <> IF s" proc-maps: /proc/self/maps line has no start-end pair" FAIL-RC die THEN
      ACC @ PEND-LO !  0 ACC !  ST-HI ST !
      EXIT THEN
   c 32 <> IF s" proc-maps: /proc/self/maps line does not close its address pair" FAIL-RC die THEN
   ACC @ PEND-HI !  0 ACC !
   0 FLD !  false INFLD !  0 PATH-U !  false PATH-OVER !
   ST-REST ST ! ;

\ True means the buffer filled before EOF, so the caller reopens after growth.
: READ-SNAPSHOT ( -- bool )
   0 SNAP-U !
   BEGIN
      SNAP-U @ SNAP-CAP @ = IF true EXIT THEN
      FD @ SNAP-U @ SNAP-BYTES SNAP-CAP @ SNAP-U @ - read GOT !
      GOT @ 0 < IF s" proc-maps: cannot read /proc/self/maps" FAIL-RC die THEN
      GOT @ 0= IF false EXIT THEN
      SNAP-U @ GOT @ + SNAP-U !
   AGAIN ;

: CAPTURE-MAPS ( -- )
   SNAP-INIT-CAP SNAP-CAP !
   BEGIN
      SNAP-CAP @ SNAP-BYTES-RESERVE
      s\" /proc/self/maps\z" drop open-rd FD !
      FD @ 0 < IF s" proc-maps: cannot open /proc/self/maps" FAIL-RC die THEN
      READ-SNAPSHOT {: full:bool :}
      FD @ close
      full 0= IF EXIT THEN
      SNAP-CAP @ 2 * SNAP-CAP !
   AGAIN ;

: FEED-SNAPSHOT ( -- )
   SNAP-U @ 0= IF EXIT THEN
   0 SNAP-BYTES {: bytes:ptr :}
   SNAP-U @ 0 ?do bytes i + c@ BYTE loop ;

\ Our own executable's path, which the tag above compares against. A link that
\ does not resolve, or one longer than a pathname can be, leaves every area
\ untagged, so the reader stops instead.
: EXE-PATH! ( -- )
   s\" /proc/self/exe\z" drop EXE-BUF MAPS-PATH-MAX readlink EXE-U !
   EXE-U @ 0 <= IF s" proc-maps: cannot read /proc/self/exe" FAIL-RC die THEN
   EXE-U @ MAPS-PATH-MAX >= IF s" proc-maps: /proc/self/exe is longer than PATH_MAX" FAIL-RC die THEN ;

\ Darwin's v0 vm_region_submap_info_64 is 64 bytes, packed to four bytes;
\ is_submap is the four-byte boolean at offset 48. Asking for v0 avoids a
\ dependency on newer accounting fields. See mach/vm_region.h in the SDK and
\ https://github.com/apple-oss-distributions/xnu/blob/main/osfmk/mach/vm_region.h
64 constant MACH-INFO-BYTES
16 constant MACH-INFO-COUNT
0 constant MACH-PROT-OFF
4 constant MACH-MAX-PROT-OFF
48 constant MACH-SUBMAP-OFF
1 constant MACH-END                    \ KERN_INVALID_ADDRESS: no later region
8 BUFFER: MACH-ADDR
8 BUFFER: MACH-SIZE
4 BUFFER: MACH-DEPTH
4 BUFFER: MACH-COUNT
MACH-INFO-BYTES BUFFER: MACH-INFO

PROCESS-SYMBOLS
FUNCTION: MACH-SELF task_self_trap ( -- u32 ) ;FUNCTION
FUNCTION: MACH-REGION mach_vm_region_recurse ( n ptr u8 ptr u8 ptr u8 ptr u8 ptr u8 -- i32 )
   1 8 WRITES-BYTES
   2 8 WRITES-BYTES
   3 4 WRITES-BYTES
   4 MACH-INFO-BYTES WRITES-BYTES
   5 4 WRITES-BYTES
;FUNCTION

\ Darwin reserves large address bands with both current and maximum protection
\ VM_PROT_NONE. They cannot contain accessible memory, even after vm_protect;
\ treating one as an allocation refused the FFI name bytes "close" in a
\ stripped image. Check both permissions rather than the reservation's address
\ or tag: an ordinary PROT_NONE allocation retains nonzero maximum permissions
\ and must remain in the map. test/proc-maps.f creates and checks both kinds.
: MACH-ACCESSIBLE? ( -- bool )
   MACH-INFO MACH-PROT-OFF + LE:U32@
   MACH-INFO MACH-MAX-PROT-OFF + LE:U32@ or 0<> ;

\ ROW+ can mmap an adjacent page and extend the last Mach region mid-walk.
\ A reply containing the old end contributes only its unseen suffix.
: MACH-ROW-LO ( n n n -- n ) {: cursor:n lo:n hi:n :}
   hi lo <= IF s" proc-maps: Mach region has no ascending extent" FAIL-RC die THEN
   hi cursor <= IF s" proc-maps: Mach region does not advance" FAIL-RC die THEN
   lo cursor max ;

: MACOS-RELOAD ( -- )
   MACH-SELF {: task:n :}
   0 MACH-ADDR LE:U64!  0 MACH-DEPTH LE:U32!
   BEGIN
      MACH-ADDR LE:U64@ {: cursor:n :}
      MACH-INFO-COUNT MACH-COUNT LE:U32!
      task MACH-ADDR MACH-SIZE MACH-DEPTH MACH-INFO MACH-COUNT MACH-REGION
      {: code:n :}
      code MACH-END = IF EXIT THEN
      code 0<> IF s" proc-maps: mach_vm_region_recurse failed" FAIL-RC die THEN
      MACH-COUNT LE:U32@ MACH-INFO-COUNT < IF
         s" proc-maps: truncated Mach region information" FAIL-RC die THEN
      MACH-INFO MACH-SUBMAP-OFF + LE:U32@ 0<> IF
         MACH-DEPTH LE:U32@ 1+ MACH-DEPTH LE:U32!
      ELSE
         MACH-ADDR LE:U64@ {: lo:n :}
         lo MACH-SIZE LE:U64@ + {: hi:n :}
         cursor lo hi MACH-ROW-LO {: row-lo:n :}
         MACH-ACCESSIBLE? IF row-lo hi 0 0 ROW+ THEN
         hi MACH-ADDR LE:U64!
      THEN
   AGAIN ;

: LINUX-RELOAD ( -- )
   EXE-PATH!
   CAPTURE-MAPS
   ST-LO ST !  0 ACC !
   FEED-SNAPSHOT
   ST @ ST-LO <> IF s" proc-maps: /proc/self/maps ended inside a line" FAIL-RC die THEN ;

\ The area this value is interior to, or -1, among the map's first count rows.
\ [start, end): the end is the next area's start, or a hole, and belongs to
\ neither.
: ROW-AT ( n n -- n ) {: v:n count:n :}
   0 BS-LO !  count BS-HI !  -1 BS-AT !
   BEGIN BS-LO @ BS-HI @ < BS-AT @ 0 < and WHILE
      BS-LO @ BS-HI @ + 2 / BS-MID !
      v BS-MID @ EXT-HI @ >= IF
         BS-MID @ 1+ BS-LO !
      ELSE
         v BS-MID @ EXT-LO @ < IF
            BS-MID @ BS-HI !
         ELSE
            BS-MID @ BS-AT !
         THEN
      THEN
   REPEAT
   BS-AT @ ;

\ Has this process no map? The count cell holds a mapping only once a read has
\ completed, and the mapping lives in the head DYNAMIC-STORAGE nulls when the
\ buffer is released - by RELOAD before it reads, or by a capture.
: UNREAD? ( -- bool ) EXT-READ#base @ 0= ;

public

\ READ THE MAP NOW. Every earlier row is dropped: the answer is the process's
\ areas at the moment of the call and nothing older. The count cell is released
\ first and reserved last, so a read that throws part way leaves no map behind.
: RELOAD ( -- )
   EXT-READ-RELEASE  0 EXT-N !
   HB-TARGET-MACOS? IF MACOS-RELOAD ELSE LINUX-RELOAD THEN
   EXT-N @ 0 = IF s" proc-maps: kernel listed no area" FAIL-RC die THEN
   1 EXT-READ-RESERVE  EXT-N @ 0 EXT-READ ! ;

\ Areas the last read found, and none before this process has read its map.
: EXTENTS ( -- n )
   UNREAD? IF 0 EXIT THEN
   0 EXT-READ @ ;

private

\ The areas of this process's map, which is read now if it has none.
: AREAS ( -- n )
   UNREAD? IF RELOAD THEN
   0 EXT-READ @ ;

public

\ IS THIS VALUE INSIDE ONE OF THEM? The map is read once, on the first question:
\ a stripped link asks this for every cell of the window it carries, and the
\ process's areas do not change while it does.
: MAPPED? ( n -- bool ) {: v:n :}
   v AREAS ROW-AT 0 >= ;

\ ... and is that area one our own executable backs?
: SELF-IMAGE? ( n -- bool ) {: v:n :}
   v AREAS ROW-AT {: at:n :}
   at 0 < IF false EXIT THEN
   at EXT-SELF @ 0<> ;

\ ... or the brk area, which the kernel names [heap] and no Habu word allocates
\ from: every allocation lib/memory.f makes is an mmap. The caller's reason for
\ asking is src/habu/aot-closure.f CELL-MAPPED?.
: HEAP? ( n -- bool ) {: v:n :}
   v AREAS ROW-AT {: at:n :}
   at 0 < IF false EXIT THEN
   at EXT-HEAP @ 0<> ;

\ WHERE THE BREAK BEGAN, an address the heap holds for as long as the process
\ does: brk moves its end and never its start. The name is a range test in the
\ kernel (a vma that starts at or below brk and ends at or above start_brk), so
\ a brk area split by an mprotect is listed under it twice and the lowest row
\ is where the break began. A process whose map lists no [heap] has no answer
\ here, so the reader stops rather than naming an address outside it.
: HEAP-START ( -- n )
   AREAS 0 ?do
      i EXT-HEAP @ 0<> IF i EXT-LO @ unloop EXIT THEN
   loop
   s" proc-maps: /proc/self/maps lists no [heap] area" FAIL-RC die ;

;package
