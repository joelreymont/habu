\ proc-maps.f - the memory areas this process holds, as its kernel lists them:
\ Linux /proc/self/maps, macOS mach_vm_region_recurse into submaps. Only the
\ kernel sees every area (mmap is reached from many libraries and foreign
\ allocators); a failed query invents no range. A question is answered for the
\ areas the process holds when it is asked. A freed area is invisible, and no
\ cell value tells an integer from a pointer into a live area. The caller is
\ aot-data-cell.f MAPPED?. An application must not require this file (PEND-LO).

require src/lib/ffi-abi.f
require src/lib/le.f
require src/lib/os-memory.f

package PROC-MAPS

74 constant FAIL-RC              \ the internal-driver exit status, as fdio.f uses
4096 constant SNAP-INIT-CAP      \ initial read budget, doubled on a full pass
4096 constant MAPS-PATH-MAX      \ PATH_MAX, the longest pathname a line can carry
4 constant PATH-FIELD            \ fields after the address pair: perms offset dev inode PATH

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
DYNAMIC-BUFFER EXT-READ n        \ READ-CELLS once a read completes, indexed below
DYNAMIC-BUFFER TASK-PORT n       \ one cell: this process's Mach task port, once asked
0 constant READ-AREAS            \ the areas the read found
1 constant READ-ALONE            \ 1 when every question answers from this read alone
2 constant READ-MIN              \ macOS: the map's lowest address, from the kernel
3 constant READ-MAX              \ macOS: the address the map ends below
4 constant READ-CELLS
variable EXT-N                   \ areas the read in progress has listed
variable ST
variable ACC
\ NOT WINDOW-SAFE: the table heads are mmap addresses and PEND-LO holds a line's
\ start until it ends, so a stripped build requiring this file is refused at
\ word=PEND-LO, rc 70 (measured). The FILE producer loads it before the capture
\ window to decline caching a mapped pointer (strict export names and refuses
\ the cell) and retires it before the window opens; the linker loads its own
\ copy above the captured span.
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

\ Kernel VMA addresses are ordered as unsigned cells, including x86-64's
\ high vsyscall mapping. Flipping the sign bit makes signed order equivalent.
$8000000000000000 constant VA-SIGN
: VA< ( n n -- bool ) {: a:n b:n :}
   a VA-SIGN xor b VA-SIGN xor < ;

\ The kernel writes areas in address order, which makes the lookup a binary
\ search; a file that breaks the order is refused, not sorted.
: ROW+ ( n n n n -- ) {: lo:n hi:n self:n heap:n :}
   lo hi VA< 0= IF s" proc-maps: area has no ascending extent" FAIL-RC die THEN
   EXT-N @ 0 > IF
      lo EXT-N @ 1- EXT-HI @ VA< IF
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

\ Each line is `<start>-<end> <perms> <offset> <dev> <inode> <path>`: the path
\ may be absent and a deleted file adds ` (deleted)`, so the path is field
\ PATH-FIELD after the address pair, not the last field. A path can reach
\ PATH_MAX and the file can span any number of reads, so the parse is a byte
\ state machine. A run of non-spaces is one field.
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

\ The whole file is read before any row is appended: table growth maps and
\ unmaps memory, which can change /proc/self/maps between read calls. A full
\ buffer closes the file, grows, and rereads from the beginning.
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

\ task_vm_info through rev2 is 42 four-byte words; min_address and max_address
\ are its last two cells. See mach/task_info.h in the SDK.
22 constant TASK-VM-INFO               \ the task_info flavor
168 constant VM-INFO-BYTES
42 constant VM-INFO-COUNT
152 constant VM-MIN-OFF
160 constant VM-MAX-OFF
VM-INFO-BYTES BUFFER: VM-INFO

PROCESS-SYMBOLS
FUNCTION: MACH-SELF task_self_trap ( -- u32 ) ;FUNCTION
FUNCTION: MACH-REGION mach_vm_region_recurse ( n ptr u8 ptr u8 ptr u8 ptr u8 ptr u8 -- i32 )
   1 8 WRITES-BYTES
   2 8 WRITES-BYTES
   3 4 WRITES-BYTES
   4 MACH-INFO-BYTES WRITES-BYTES
   5 4 WRITES-BYTES
;FUNCTION
FUNCTION: TASK-INFO task_info ( n n ptr u8 ptr u8 -- i32 )
   2 VM-INFO-BYTES WRITES-BYTES
   3 4 WRITES-BYTES
;FUNCTION
FUNCTION: SYNC-RANGE msync ( n n n -- i32 ) ;FUNCTION

\ Darwin reserves large address bands with both current and maximum protection
\ VM_PROT_NONE. They cannot contain accessible memory, even after vm_protect;
\ treating one as an allocation refused the FFI name bytes "close" in a
\ stripped image. Check both permissions rather than the reservation's address
\ or tag: an ordinary PROT_NONE allocation retains nonzero maximum permissions
\ and must remain in the map. test/proc-maps.f creates and checks both kinds.
: MACH-ACCESSIBLE? ( -- bool )
   MACH-INFO MACH-PROT-OFF + LE:U32@
   MACH-INFO MACH-MAX-PROT-OFF + LE:U32@ or 0<> ;

\ The task port names this task for the whole process run, and every
\ task_self_trap adds a user reference to it, so it is asked once. It is
\ DYNAMIC-BUFFER storage because an image restores into another process.
: TASK ( -- n )
   TASK-PORT#base @ 0= IF 1 TASK-PORT-RESERVE  MACH-SELF 0 TASK-PORT ! THEN
   0 TASK-PORT @ ;

\ The map's bounds, into VM-INFO, where READ-MAP takes them.
: MACOS-BOUNDS ( n -- ) {: task:n :}
   VM-INFO-COUNT MACH-COUNT LE:U32!
   task TASK-VM-INFO VM-INFO MACH-COUNT TASK-INFO
   0<> IF s" proc-maps: task_info failed" FAIL-RC die THEN
   MACH-COUNT LE:U32@ VM-INFO-COUNT < IF
      s" proc-maps: truncated task VM information" FAIL-RC die THEN ;

\ ROW+ can mmap an adjacent page and extend the last Mach region mid-walk.
\ A reply containing the old end contributes only its unseen suffix.
: MACH-ROW-LO ( n n n -- n ) {: cursor:n lo:n hi:n :}
   hi lo <= IF s" proc-maps: Mach region has no ascending extent" FAIL-RC die THEN
   hi cursor <= IF s" proc-maps: Mach region does not advance" FAIL-RC die THEN
   lo cursor max ;

: MACOS-RELOAD ( -- )
   TASK {: task:n :}
   task MACOS-BOUNDS
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

: CLEAR-BYTES ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do 0 a i + c! loop ;

\ The Mach query scratch, after a walk or a single-address query.
: CLEAR-MACH ( -- )
   MACH-ADDR 8 CLEAR-BYTES
   MACH-SIZE 8 CLEAR-BYTES
   MACH-DEPTH 4 CLEAR-BYTES
   MACH-COUNT 4 CLEAR-BYTES
   MACH-INFO MACH-INFO-BYTES CLEAR-BYTES
   VM-INFO VM-INFO-BYTES CLEAR-BYTES ;

\ The completed map lives in EXT-LO/HI/SELF/HEAP and EXT-READ. None of the
\ parser or Mach query scratch belongs to it or to an image saved after it.
: CLEAR-SCAN ( -- )
   SNAP-BYTES-RELEASE  0 SNAP-CAP !  0 SNAP-U !
   PATH-BUF MAPS-PATH-MAX CLEAR-BYTES
   EXE-BUF MAPS-PATH-MAX CLEAR-BYTES
   0 EXT-N !  0 ST !  0 ACC !
   0 PEND-LO !  0 PEND-HI !  0 FLD !  0 INFLD !
   0 PATH-U !  0 PATH-OVER !  0 EXE-U !
   0 FD !  0 GOT !
   CLEAR-MACH ;

\ Does the kernel hold an area at v that MACOS-RELOAD would list? One query per
\ submap level answers with the region holding v or the next one above it; a
\ submap holding v is entered one level deeper at v, as the walk enters it.
: MACOS-HOLDS? ( n -- bool ) {: v:n :}
   TASK {: task:n :}
   0 MACH-DEPTH LE:U32!
   BEGIN
      v MACH-ADDR LE:U64!
      MACH-INFO-COUNT MACH-COUNT LE:U32!
      task MACH-ADDR MACH-SIZE MACH-DEPTH MACH-INFO MACH-COUNT MACH-REGION
      {: code:n :}
      code MACH-END = IF false EXIT THEN
      code 0<> IF s" proc-maps: mach_vm_region_recurse failed" FAIL-RC die THEN
      MACH-COUNT LE:U32@ MACH-INFO-COUNT < IF
         s" proc-maps: truncated Mach region information" FAIL-RC die THEN
      MACH-ADDR LE:U64@ {: lo:n :}
      lo MACH-SIZE LE:U64@ + {: hi:n :}
      v lo VA< v hi VA< 0= or IF false EXIT THEN
      MACH-INFO MACH-SUBMAP-OFF + LE:U32@ 0= IF MACH-ACCESSIBLE? EXIT THEN
      MACH-DEPTH LE:U32@ 1+ MACH-DEPTH LE:U32!
   AGAIN ;

1 constant MS-ASYNC                    \ the same flag on macOS and Linux
12 constant ENOMEM                     \ the same number on macOS and Linux

\ Does the kernel hold an area at v that LINUX-RELOAD would list? msync fails
\ with ENOMEM exactly when part of its range lies in no area, whatever the
\ area's protection, and MS_ASYNC asks it for no work.
: LINUX-HOLDS? ( n -- bool ) {: v:n :}
   v OS-MEMORY:PAGE-SIZE negate and 1 MS-ASYNC SYNC-RANGE 0= IF true EXIT THEN
   FFI:ERRNO ENOMEM <> IF s" proc-maps: msync failed" FAIL-RC die THEN
   false ;

\ Does the kernel hold an area at v now? It asks about v alone, whatever the
\ map's size, and leaves no query scratch behind. Linux arm64's msync strips
\ the top byte, so a tagged value whose untagged address is mapped is held here,
\ costs a full read, and answers from that read, which lists no area at it.
: KERNEL-HOLDS? ( n -- bool ) {: v:n :}
   HB-TARGET-MACOS? IF v MACOS-HOLDS? ELSE v LINUX-HOLDS? THEN
   CLEAR-MACH ;

\ The area this value is interior to, or -1, among the map's first count rows.
\ [start, end): the end is the next area's start, or a hole, and belongs to
\ neither.
: ROW-AT ( n n -- n )
   {: v:n count:n :}
   0 BS-LO !  0 BS-HI !  0 BS-MID !  0 BS-AT !
   count BS-HI !  -1 BS-AT !
   BEGIN BS-LO @ BS-HI @ < BS-AT @ 0 < and WHILE
      BS-LO @ BS-HI @ + 2 / BS-MID !
      v BS-MID @ EXT-HI @ VA< 0= IF
         BS-MID @ 1+ BS-LO !
      ELSE
         v BS-MID @ EXT-LO @ VA< IF
            BS-MID @ BS-HI !
         ELSE
            BS-MID @ BS-AT !
         THEN
      THEN
   REPEAT
   BS-AT @ {: found:n :}
   \ The completed answer is the index, not this query's search cursor.
   0 BS-LO !  0 BS-HI !  0 BS-MID !  0 BS-AT !
   found ;

\ A capture drops the map whole: the rows, the Linux text snapshot, the Mach
\ task port and the EXT-READ record (a read completed, its area count, the map's
\ bounds, whether it answers alone) are DYNAMIC-BUFFER storage, nulled by every
\ image capture (src/core/dynamic-storage.f RELEASE-ALL, from the snapshot
\ writer's CANON-DATA) and by a read before it reads, so a restored image reads
\ its own map at the first question. No DATA cell says whether the map was read,
\ because DATA is copied: a DATA flag would start an image saved after the
\ linker loaded as read, with its builder's count, over released rows
\ (measured: E-BOUNDS 7122 on the keyed linker image, test/preloaded-engine.f).
: UNREAD? ( -- bool ) EXT-READ#base @ 0= ;

\ Does the read's map hold no area at v by its bounds alone? Only macOS reads
\ bounds (task_info TASK_VM_INFO): a 64-bit task's map starts above __PAGEZERO,
\ so 0 and the small integers dependency walks ask about never reach the
\ kernel. On Linux every value is put to the kernel.
: OUTSIDE? ( n -- bool ) {: v:n :}
   HB-TARGET-MACOS? 0= IF false EXIT THEN
   v READ-MIN EXT-READ @ VA< IF true EXIT THEN
   v READ-MAX EXT-READ @ VA< 0= ;

\ Read the map now; alone says whether every question answers from this read
\ without asking the kernel. Every earlier row is dropped: the answer is the
\ process's areas at the moment of the call and nothing older. The record is
\ released first and reserved last, so a read that throws part way leaves no
\ map behind.
: READ-MAP ( bool -- ) {: alone:bool :}
   EXT-READ-RELEASE  0 EXT-N !
   HB-TARGET-MACOS? IF MACOS-RELOAD ELSE LINUX-RELOAD THEN
   EXT-N @ 0 = IF s" proc-maps: kernel listed no area" FAIL-RC die THEN
   READ-CELLS EXT-READ-RESERVE
   EXT-N @ READ-AREAS EXT-READ !
   alone IF 1 ELSE 0 THEN READ-ALONE EXT-READ !
   VM-INFO VM-MIN-OFF + LE:U64@ READ-MIN EXT-READ !
   VM-INFO VM-MAX-OFF + LE:U64@ READ-MAX EXT-READ !
   CLEAR-SCAN ;

public

\ Read the map now, and answer every question from this read alone until a
\ capture drops it. The linker's persistent PREPARE hook takes this read
\ (tools/aot-build-core.f) and PREPARE runs FFI's FORGET-SYMBOLS after it. A
\ kernel query calls Mach (on Linux, getpagesize, msync and errno) through
\ FFI's shared table, so a miss in LINK would resolve a C library address into
\ that table, which an application's FUNCTION: rows can place in the capture
\ window, and the link would refuse the address it just stored
\ (test/stripped-image.f).
: RELOAD ( -- )  true READ-MAP ;

\ Areas the last read found, and none before this process has read its map.
: EXTENTS ( -- n )
   UNREAD? IF 0 EXIT THEN
   READ-AREAS EXT-READ @ ;

private

\ The areas of this process's map, which is read now if it has none.
: AREAS ( -- n )
   UNREAD? IF false READ-MAP THEN
   READ-AREAS EXT-READ @ ;

\ The row of the area holding v, or -1, for the areas the process holds now.
\ A row of the last read answers at once; an area unmapped since that read
\ still answers, which makes MAPPED? refuse and SELF-IMAGE? or HEAP? exempt the
\ value by the stale row's tag. A value outside the map's bounds is held by no
\ area. Any other value is put to the kernel alone, unless RELOAD took the
\ read, and an area the kernel holds that the read lacks
\ costs a full read, so the row carries its tags and the next question about
\ that area is answered from the read.
: ROW ( n -- n ) {: v:n :}
   v AREAS ROW-AT {: at:n :}
   at 0 >= IF at EXIT THEN
   READ-ALONE EXT-READ @ 0<> IF -1 EXIT THEN
   v OUTSIDE? IF -1 EXIT THEN
   v KERNEL-HOLDS? 0= IF -1 EXIT THEN
   false READ-MAP
   v AREAS ROW-AT ;

public

\ IS THIS VALUE INSIDE ONE OF THEM, the areas the process holds when it asks?
\ The map is read on the first question and again, in full, only when the
\ kernel holds an area the read lacks; a stripped link asks this for every cell
\ of the window it carries, and answers each from RELOAD's read with one search.
\ Elsewhere a question costs one search and, inside the map's bounds, one
\ kernel query about a value the read places in no area; only a value the
\ kernel holds there adds the full read.
: MAPPED? ( n -- bool ) {: v:n :}
   v ROW 0 >= ;

\ Linux tags an area our own executable backs (its path equals readlink
\ /proc/self/exe), the one class whose address is not the kernel's choice:
\ bin/hb and the images it links are EXEC ELFs at the fixed base 0x400000
\ (measured), so small integers land there; the last three bytes of a string,
\ s0 + s1<<8 + s2<<16, fall in [0x400000,0x7a4000) whenever the third byte is
\ a letter. The caller decides what the tag means. Darwin images use ASLR, so
\ both tags are false there.
: SELF-IMAGE? ( n -- bool ) {: v:n :}
   v ROW {: at:n :}
   at 0 < IF false EXIT THEN
   at EXT-SELF @ 0<> ;

\ The brk area, which the kernel names [heap]: arm64 randomizes the break over a
\ gigabyte above the executable's end, so a 32-bit-shaped integer (a hash, a
\ packed pair, four string bytes) can land in [0x7a4000,0x407a4000). No Habu
\ word allocates from the break (src/lib/memory.f maps), so the tag tells that
\ band apart from an allocation.
: HEAP? ( n -- bool ) {: v:n :}
   v ROW {: at:n :}
   at 0 < IF false EXIT THEN
   at EXT-HEAP @ 0<> ;

;package
