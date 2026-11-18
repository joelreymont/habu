split-str
\ xattr.f - extended attributes of an open file, through libc's fgetxattr,
\ fsetxattr and fremovexattr. A name is the whole attribute name, namespace
\ included (`user.origin`); a value is bytes. An empty name, or one over 255
\ bytes, throws E-XATTR with ERANGE as FS:ERRNO before any call; a name
\ holding a NUL throws E-FFI-NUL. Any other refusal a word does not answer is
\ E-XATTR with the call's errno as FS:ERRNO, logged with its text.
\ STORAGE CLASS. TASK-LOCAL: the name's C copy is one TASK:+USERS row.
\ CALLER-OWNED: every value and GET's buffer.
HB-TARGET-LINUX-KERNEL? ?TARGET
require src/lib/error.f
require src/lib/span.f
require src/lib/ffi-abi.f
require src/lib/task.f
require src/lib/fs.f
require src/lib/log.f

package XATTR

private
-9900 constant E-FIRST
-9909 constant E-LAST

public
-9900 s" an extended attribute call failed" ERROR:CODE E-XATTR

\ How GET ended: no attribute of the name, a value of `size` bytes at the
\ buffer's start, or a value longer than the buffer.
ENUM got
   VARIANT absent ;VARIANT
   VARIANT value FIELD size n ;VARIANT
   VARIANT oversize ;VARIANT
;ENUM

private
\ XATTR_NAME_MAX, the NUL not counted.
255 constant NAME-MAX
1 constant XATTR-CREATE

NAME-MAX FFI:CSTR-ROOM TASK:+USERS NAME-ROW u8

PROCESS-SYMBOLS
FUNCTION: GET-CALL fgetxattr ( n ^u8 ^u8 n -- n ) ;FUNCTION
FUNCTION: SET-CALL fsetxattr ( n ^u8 ^u8 n n -- i32 ) ;FUNCTION
FUNCTION: REMOVE-CALL fremovexattr ( n ^u8 -- i32 ) ;FUNCTION

\ The kernel answers ERANGE for these names too, which GET would read as a
\ value too long for its buffer.
: NAMEZ ( str -- ^u8 )
   {: name:^ size:n :}
   size 0=  size NAME-MAX >  or if FS:ERANGE FS:CAUSE! E-XATTR throw then
   name size 0 NAME-ROW NAME-MAX FFI:CSTR-ROOM SPAN:MAKE FFI:CSTR ;

: FAIL ( n ^u8 n ^u8 n -- )
   {: e:n call:^ cu:n name:^ nu:n :}
   e FS:ERRNO$ {: text:^ tu:n :}
   LOG:ERROR s" xattr" LOG:LINE
   call cu LOG:TEXT+ s"  " LOG:TEXT+ name nu LOG:TEXT+
   s" : " LOG:TEXT+ text tu LOG:TEXT+ LOG:;LINE
   e FS:CAUSE! E-XATTR throw ;

\ fgetxattr with no room answers the value's size instead of ERANGE.
: GOT ( n n -- got )
   {: k:n cap:n :}
   k cap > if XATTR-GOT:OVERSIZE exit then
   k XATTR-GOT:VALUE ;

: GET-ERROR ( str -- got )
   {: name:^ nu:n :}
   FFI:ERRNO {: e:n :}
   e FS:ENODATA = if XATTR-GOT:ABSENT exit then
   e FS:ERANGE = if XATTR-GOT:OVERSIZE exit then
   e s" fgetxattr" name nu FAIL ;

\ fsetxattr's answer: 0, or -1 with errno set.
: SETX ( fd str str n -- n )
   {: f:fd name:^ nu:n val:^ vu:n flags:n :}
   f FD>N name nu NAMEZ val vu flags SET-CALL ;

public
: GET ( fd ^u8 n SPAN:span<u8> -- got )
   {: f:fd name:^ nu:n buf :}
   buf SPAN:$ {: dst:^ cap:n :}
   f FD>N name nu NAMEZ dst cap GET-CALL {: k:n :}
   k 0 < if name nu GET-ERROR exit then
   k cap GOT ;

\ Creates the attribute; one that exists is E-XATTR with EEXIST.
: ADD ( fd ^u8 n ^u8 n -- )
   {: f:fd name:^ nu:n val:^ vu:n :}
   f name nu val vu XATTR-CREATE SETX 0= if exit then
   FFI:ERRNO s" fsetxattr" name nu FAIL ;

\ Creates the attribute or replaces its value.
: SET ( fd ^u8 n ^u8 n -- )
   {: f:fd name:^ nu:n val:^ vu:n :}
   f name nu val vu 0 SETX 0= if exit then
   FFI:ERRNO s" fsetxattr" name nu FAIL ;

\ An absent attribute is already erased.
: ERASE ( fd ^u8 n -- )
   {: f:fd name:^ nu:n :}
   f FD>N name nu NAMEZ REMOVE-CALL 0= if exit then
   FFI:ERRNO {: e:n :}
   e FS:ENODATA = if exit then
   e s" fremovexattr" name nu FAIL ;

;package
