# SwiftForth reference, distilled

Source: *SwiftForth Development System for macOS and Linux, Reference Manual*,
FORTH, Inc., copyright 1998-2019, latest revision August 2020 (ISBN
9781075209963). The manual is proprietary to FORTH, Inc.; this page restates its
facts in new words so that a Forth implementer can compare and cite SwiftForth
without the PDF. Section numbers (§5.5.3) are the manual's. Stack effects use
the manual's notation; `<name>` marks a word that parses a name from the input.
This page describes SwiftForth only.

## Welcome

SwiftForth is FORTH, Inc.'s interactive Forth development system for Linux,
macOS and Windows; this manual covers the Linux and macOS versions, which share
one code base and target the i386 (IA-32) processor family. The manual explains
the compiler, multitasker, libraries and tools; it does not teach Forth, and
points instead to *Forth Programmer's Handbook* (shipped as a PDF) and *Forth
Application Techniques*. Forth words appear in bold in the original; a path
component written `<os>` stands for `osx` or `linux`.

## 1 Getting Started

**§1.1 Components.** A distribution has the executable development image (IDE,
OS and library interfaces, tools such as the disassembler), Forth source for the
whole system, a cross compiler that rebuilds the kernel, and PDF documentation,
all under one `SwiftForth` directory tree.

**§1.2-1.3 Requirements and installation.** An Intel PC with Linux or an Intel
Mac, about 12 MB of disk, and any programmer's editor. Installation unpacks a
tarball and puts `bin/<os>/sf` (the full system) and `sfk` (the bare kernel,
absent from the evaluation version) on `PATH`, usually as symlinks in
`$HOME/bin`. `sf` greets with its release and build date; `BYE` exits.
**§1.3.2:** `EDIT` and `G` run the shell script `bin/editor` with the file name
and line number; it defers to an executable `$HOME/.SwiftForth-editor` if
present, else to `$VISUAL` or `$EDITOR`, which it assumes accept `+line`.

**§1.4 Development procedures.** The command line runs whatever is typed when
Enter is pressed, with line editing, history (up and down) and Tab completion
from history. Name lookup is case-insensitive by default, though names keep the
case they were defined with; `CASE-SENSITIVE` and `CASE-INSENSITIVE` switch the
mode. Samples live in `lib/samples` and load with `INCLUDE`. New code enters
four ways: typed, pasted, `INCLUDE <file>`, or loaded from blocks when block
support is present. The manual recommends short definitions tested bottom-up.

**§1.5 Licensing.** Because the system can save images that contain the
development environment itself, the license separates the two: a shipped turnkey
program may embed the run-time kernel only with the compiler, assembler and
other development aids unavailable to its users (§1.5.1); the development system
is licensed per person, not for concurrent use, and may not be modified,
reverse-engineered or have derivative works made except as §1.5.1 allows
(§1.5.2).

## 2 Using SwiftForth

**§2.1-2.2 Organization.** One integrated package: Forth compiler, i386
assembler, dynamic-library interface, libraries and interactive aids. It claims
ANS Forth compliance for Core, most Core Extensions and most optional word sets
(§4.8, Appendix B). It has two layers: a precompiled kernel and configurable
options loaded on top, plus the application; the turnkey utility (§4.1.2) saves
the result as a distributable binary. Purchased copies include source for the
extensions, most of the kernel and the kernel cross compiler.

**§2.3 Command line.** The IDE is a terminal command line with a history buffer,
line editing, insert or overwrite mode, Esc to abandon a line, and Tab
completion that cycles through matching history entries. Keys follow the
terminal's own erase and kill characters; Home and End (or Ctrl-A and Ctrl-E)
move to the line ends.

**§2.4.1 Source navigation.** Every compiled definition records its source file
and line, which drives these tools. They cannot show source for code typed or
pasted at the console, for files from another version, or for words of the
metacompiler, and they show stale lines once the file changes after compiling.

| Word | Effect | Meaning |
|---|---|---|
| `LOCATE <name>` | ( -- ) | Show the source lines around the definition of name, with path and line; name must be in scope. |
| `N`, `B` | ( -- ) | Show the next, or previous, screenful of that file. |
| `L` | ( -- ) | After a compile error, show the failing line. |
| `G` | ( -- ) | After a compile error, open the editor at the failing line. |
| `EDIT <name>` | ( -- ) | Open the editor at name's definition; aborts if name is not found. Bare `EDIT` after `LOCATE` opens at the shown text. |

**§2.4.2 Listing.** `WORDS` lists the current search order; a vocabulary name
before it (`FORTH WORDS`) narrows it to that vocabulary, `ALL WORDS` widens it
to all vocabularies, and a string after it filters by substring.

**§2.4.3 Cross-references.** `WHERE <name>` (synonym `WH`) shows each definition
of name and every source line in the compiled program that refers to it,
separately for each redefinition. It works from compiled references, not text
search, and searches every wordlist whatever the search order, so hidden
definitions show up. Output lines are prefixed `file line|`; `LOCATE 21 53|` and
`EDIT 21 53|` accept that form. `UNCALLED` ( -- ) lists words never called from
a colon definition.

**§2.4.4 Disassembler.** Since SwiftForth compiles machine code, it cannot
decompile to high-level source. `SEE <name>` ( -- ) disassembles a word, showing
address, instruction and bytes; calls show the callee's name. `DASM` ( addr -- )
disassembles from an absolute address, such as one from a `LABEL` or an
exception report. Use `LOCATE` for source.

**§2.4.5 Memory dumps.** All take ( addr u -- ) with u in bytes: `DUMP` shows
hex bytes with ASCII, `IDUMP` signed 32-bit cells in the current base, `UDUMP`
unsigned cells in the current base, `HDUMP` unsigned cells in hex. `'
<name> >BODY` gives a word's data field for dumping.

**§2.4.6 Single-step debugger.** `REQUIRES singlestep` loads it. `DEBUG` before
a word arms it; for code compiled from a file, each step between Forth words
shows the data stack and offers Nest (step into a call), Step (step over),
Return (run to the end of the current definition) and Finish (run the rest
without stopping).

## 3 Source Management

SwiftForth supports both the ANS block and file word sets, but its own source
lives in text files; blocks survive as a historical portable medium (Appendix
A).

**§3.1 Interpreting source files.** `INCLUDE <file>` interprets a whole file and
may nest; a missing extension means `.f`. A path may be absolute (`/...`),
relative to the working directory, climb with `../`, or start with `%`, which
means the directory that holds the SwiftForth tree (found from the location of
`SwiftForth/bin`). `INCLUDE` never changes the working directory. The manual
advises gathering a component's `INCLUDE`s in one load file rather than
scattering them.

| Word | Effect | Meaning |
|---|---|---|
| `INCLUDE <file>` | ( -- ) | Interpret file, searching only the given or current path. |
| `INCLUDING` | ( -- c-addr u ) | Name of the file `INCLUDE` is processing now. |
| `REQUIRES <file>` | ( -- ) | Include the first match from a fixed list: current directory, `%SwiftForth/lib`, `lib/options`, `lib/options/<os>`, `lib/samples`, `lib/samples/<os>`. |
| `OPTIONAL <name> <text>` | ( -- ) | Only while including: if name is already in the `LOADED-OPTIONS` wordlist, skip the rest of the file; else add name there and continue. The rest of the line is a comment. This is the load-once guard. |
| `CD <path>`, `PWD` | ( -- ) | Change or show the working directory as a shell does; bare `CD` goes home, `CD -` toggles to the previous one; quote paths with spaces. |
| `PUSHPATH`, `POPPATH` | ( -- ) | Save the current directory on a directory stack, and restore it. |

**§3.2 Extended comments.** `{` ( -- ) starts a comment that runs, across lines,
to the next `}`, so it may contain parentheses and stack comments; `{` is a word
and needs a following space, `}` is only a delimiter. By convention such blocks
start and end with a line of dashes. `\\` ( -- ) treats the rest of the file
being included as comment.

**§3.3 Load monitoring.** `VERBOSE` ( -- ) makes `INCLUDE` echo each file name
and each line as it is interpreted; `SILENT` ( -- ), the default, turns that
off. Neither is immediate.

## 4 Programming in SwiftForth

**§4.1.1 Dictionary management.** Development reloads a component repeatedly, so
the code under test is treated as an overlay that discards its previous version.

- *Single level (§4.1.1.1).* `EMPTY` ( -- ) cuts the dictionary back to the
  "golden" state, discarding every later definition and its data space; the
  golden state starts as the state after launch. `GILD` ( -- ) makes the current
  state the new golden state; nothing can ever be discarded below it. A load
  file that begins with `EMPTY` is a one-level overlay. `PROGRAM` then saves a
  gilded system permanently.
- *Multiple levels (§4.1.1.2).* Plain `EMPTY` or `FORGET` cannot know every
  structure a program links into the system (chains, vectors patched into system
  words), so SwiftForth uses extensible dictionary markers. A marker records
  enough state to restore the system to the moment it was made. `MARKER <name>`
  defines one that removes itself when run (suited to one-shot code); `REMEMBER
  <name>` ( -- ) defines one that survives its own execution, so it can serve
  again as an overlay boundary. Load file 1 ends with `REMEMBER OVERLAY`; each
  alternative level-2 file starts with `OVERLAY`, so loading either discards the
  other. Any number of levels can be stacked with distinct marker names, all
  above the `GILD` point. `EMPTY` and `GILD` are themselves built on markers.
- *Extending markers.* Creating a marker runs a chain of save routines that
  compile context into the marker's body; running it runs a matching chain of
  restore ("prune") routines. Both chains run oldest first, starting at the
  `GILD` point. Code that alters one of the system's pointers, or a value
  created elsewhere, must add a pair:

| Word | Effect | Meaning |
|---|---|---|
| `:REMEMBER` | ( -- ) | Start an unnamed definition added to the save chain; it records state with `,` or `C,`. |
| `:PRUNE` | ( addr1 -- addr2 ) | Start an unnamed definition added to the restore chain; it receives the address of the data its `:REMEMBER` partner saved and returns the address just past it. |
| `?PRUNE` | ( -- flag ) | True if the definition containing it is itself being discarded. |
| `?PRUNED` | ( addr -- flag ) | True if addr is no longer inside the live dictionary. |

The two are always written as a pair in the same overlay layer, except that
`:PRUNE` alone suffices when the restored value is a constant. A typical
restore: if `?PRUNE` (this extension itself is going) restore the value captured
at compile time with `[ ... ] LITERAL`, else restore the value saved in the
marker. When several layers guard the same pointer, the restore also checks
`?PRUNED` on the compile-time value, leaving the location alone when an older
extension that set it is also being removed. Markers made before an extension
existed carry no saved data for it, so the extension must distinguish them.

At the keyboard, `MARKER <name>` before experimenting, or just `EMPTY`, clears
the experiment away.

**§4.1.2 Turnkey images.** `PROGRAM <file>` ( -- ) saves an executable snapshot
of the running system, including everything compiled so far, in the current or
given path. A standalone application first stores its entry word's xt in the
vector `'MAIN` ( -- addr ): `' GO 'MAIN !` then `PROGRAM foo`. Without that, the
saved image is simply a customized SwiftForth (for example with floating point
loaded).

**§4.2 Compiler control.** Console input and source text are treated the same;
anything source can do can be typed.

- *§4.2.1 Case.* Lookup ignores case unless `CASE-SENSITIVE` is set
  (`CASE-INSENSITIVE` restores it); standard words and most SwiftForth words are
  upper case. The manual offers case sensitivity for legacy code.
- *§4.2.2 Name conflicts.* The shipped system has more than 3,500 named words;
  those not for general use sit in separate vocabularies, leaving about 1,600 in
  `FORTH`. Redefinition is legal and leaves earlier compiled references bound to
  the old word; the manual endorses it for clear high-level names and for
  transparently wrapping a word. By default the compiler warns ("isn't unique")
  when a new name matches one in the same vocabulary. `WARNING` ( -- addr ) is
  the flag controlling that warning, set with `ON` ( addr -- ) and `OFF` ( addr
  -- ); `-?` ( -- ) placed before a definition silences the warning for that one
  definition.
- *§4.2.3 Conditional compilation.* `[IF]`, `[ELSE]`, `[THEN]` skip the text of
  the unselected branch and nest; `[DEFINED] <word>` and `[UNDEFINED] <word>`
  give a flag. A common idiom guards a portable high-level fallback with
  `[UNDEFINED] name [IF] ... [THEN]` so a code version loaded earlier wins,
  which makes load order significant.

**§4.3 Input number conversion.** The text interpreter converts numbers using
the user variable `BASE`; compiling, it compiles a literal. `DECIMAL` (the
default), `HEX`, `OCTAL` and `BINARY` set `BASE` until changed. A one-number
prefix overrides the base without changing `BASE`: `%` binary, `&` octal, `#`
decimal, `$` hex; a minus sign goes after the prefix. Any of the punctuation
characters `, . + - / :` inside a number (anywhere but a leading minus) makes it
a double; all are equivalent, can repeat, and only mark that punctuation
occurred and count the digits after it, so `1234.56` and `1,23.456` give the
same double. Conversion stops at a blank or at the first character that is not a
digit or one of the separators. Floating point (§10) changes these rules for its
own numbers.

| Word | Effect | Meaning |
|---|---|---|
| `>NUMBER` | ( ud1 addr1 u1 -- ud2 addr2 u2 ) | ANS primitive: accumulate digits in `BASE` into ud1 until a non-digit (signs included) or the end. |
| `NUMBER?` | ( addr u -- 0 \| n 1 \| d 2 ) | Convert a whole string: 0 on failure, single with 1 if unpunctuated, double with 2 if punctuated. |
| `NUMBER` | ( addr u -- n \| d ) | Same rules, returning only the value; an illegal character throws (the glossary says aborts). |
| `DPL` | ( -- addr ) | Punctuation state of the last conversion: negative for a single; otherwise the count of digits after the last punctuation, usable to scale decimals whatever the separator. It starts very negative, counts up per digit and resets to 0 at each punctuation. |
| `NH` | ( -- addr ) | High cell of the last single conversion, to recover it as a double. |

**§4.4.1 Date and time.** Time of day is an unsigned double of seconds since
midnight. Dates are a modified Julian day number: days since 31 December 1899 (a
Sunday), so `7 MOD` gives the weekday with 0 for Sunday. The arithmetic assumes
every fourth year is a leap year, so it is valid only from 1 March 1900 to 28
February 2100. Entry relies on §4.3: `8/03/1940` reads as the double 8031940
(day needs two digits, year four), which `M/D/Y` turns into a day number;
`10:25:30 HOURS` sets the clock the same way.

| Word | Effect | Meaning |
|---|---|---|
| `@NOW` | ( -- ud u ) | Seconds since midnight and day count. |
| `!NOW` | ( ud u -- ) | Set time and date from those. |
| `TIME&DATE` | ( -- u1 u2 u3 u4 u5 u6 ) | Seconds, minutes, hours, day, month, year (year range 1900-2079). |
| `@TIME` | ( -- ud ) | Seconds since midnight. |
| `(TIME)`, `.TIME`, `TIME` | ( ud -- addr u ), ( ud -- ), ( -- ) | Format as hh:mm:ss, display it, display the current time. |
| `HOURS` | ( ud -- ) | Set the clock from an hh:mm:ss double. |
| `D/M/Y` | ( u1 u2 u3 -- u4 ) | Day, month, year to day number. |
| `M/D/Y` | ( ud -- u ) | mm/dd/yyyy double to day number. |
| `@DATE` | ( -- u ) | Today's day number. |
| `(DATE)` | ( u1 -- addr u2 ) | Deferred date formatter; default `(MM/DD/YYYY)`, alternative `(DD-MM-YYYY)` (day, month abbreviation, year); change with `IS`. |
| `.DATE`, `DATE` | ( u -- ), ( -- ) | Display a date, display today. |
| `NOW` | ( ud -- ) | Set the date from an mm/dd/yyyy double. |

**§4.4.2 Interval timing.** `MS` ( n -- ) suspends the calling task for n
milliseconds while others run (it pauses); its error is about one clock tick,
more if the OS preempts the process. `COUNTER` ( -- u ) reads the millisecond
timer; `TIMER` ( u -- ) reads it again and prints the elapsed milliseconds;
`EXPIRED` ( u -- flag ) is true once the timer has passed u, so `COUNTER n +`
gives a deadline to poll. `uCOUNTER` ( -- d ) and `uTIMER` ( d -- ) do the same
with a 64-bit microsecond count from the high-resolution (about 1 MHz) clock. To
time one operation, run it many times between `COUNTER` and `TIMER`, and
subtract the cost of an empty loop.

**§4.5.1 String buffers.** `PAD` ( -- addr ) lies 512 bytes above `HERE` and
reaches into all unused dictionary space, so its address and size (`UNUSED 512
-`) move whenever the dictionary grows. It is per task (it sits in the user
area) and suits only short-lived scratch strings; the system itself uses it only
in tools such as `WORDS` and the cross-referencer. For lasting storage use
`BUFFER: <name>` ( n -- ), a static, global buffer of n characters (`50 CELLS
BUFFER: x` for cells) whose name returns its address.

**§4.5.2 String structures.** OS and library calls mostly take zero-terminated
("ASCIIZ") strings; a few take "Unicode" strings, which here means ASCII in the
low byte of 16-bit characters. The string words are meant for use inside
definitions or structures such as switches; used interpretively they return an
address in a transient buffer that must not be kept. Words with `\` in their
name interpret C `printf`-style escapes, case-sensitively: `\\` and `\"` (also
`\q`) for themselves, `\a` bell, `\b` backspace, `\e` escape, `\f` form feed,
`\l` line feed, `\n` the platform's end of line, `\r` carriage return, `\t` and
`\v` tabs, `\xhh` any byte in hex, `\z` a zero byte.

| Word | Effect | Meaning |
|---|---|---|
| `Z" ..."`, `Z\" ..."` | ( -- addr ) | Compiled zero-terminated string; the second takes escapes. |
| `,Z" ..."`, `,Z\" ..."` | ( -- ) | Lay down a zero-terminated string, uncounted, at `HERE`; the interpretive form of `Z"`. |
| `,U" ..."`, `,U\" ..."` | ( -- ) | Lay down a 16-bit-character string; interpretive form of `U"`. |
| `S\" ..."` | ( -- addr n ) | `S"` with escapes. |
| `C\" ..."` | ( -- addr ) | Counted string with escapes. |
| `.\" ..."` | ( -- ) | Print a string with escapes at run time. |
| `," ..."`, `,\" ..."` | ( -- ) | Lay down a string at `HERE` and allot it; the second takes escapes. |
| `STRING,` | ( addr u -- ) | Lay down a given string at `HERE` and allot it. |
| `PLACE`, `APPEND` | ( addr1 u addr2 -- ) | Store as a counted string (length in byte 0), or append to one. No bounds check. |
| `ZPLACE`, `ZAPPEND` | ( addr1 u addr2 -- ) | The same for zero-terminated strings. No bounds check. |

**§4.5.3 Linked lists.** Lists chain items that may differ in length and be
scattered among other code and data; switches and many system chains use them. A
`VARIABLE` holds the head; each link holds the address of the next as a
*relative* pointer, and zero ends the list. Because data words return absolute
addresses (§5.1.4) but only relative ones survive in a relocatable image, links
use these conversions. Lists are built at compile time.

| Word | Effect | Meaning |
|---|---|---|
| `@REL` | ( addr1 -- addr2 ) | Fetch a relative pointer as an absolute address. |
| `!REL` | ( addr1 addr2 -- ) | Store absolute addr1 at addr2 as a relative pointer. |
| `,REL` | ( addr -- ) | Compile an absolute address as a relative pointer. |
| `>LINK` | ( addr -- ) | Compile a new link at `HERE` at the head of the list whose head cell is addr. |
| `<LINK` | ( addr -- ) | Compile a new link at `HERE` at the tail of that list. |
| `CALLS` | ( addr -- ) | Walk a list and execute the high-level code that follows each link. |

The data an entry carries is compiled right after its link; a walker loops on
`@REL ?DUP WHILE` and reads `CELL+`.

**§4.5.4 Switches.** A switch maps single-cell keys to actions, for OS event
codes, `THROW` codes and the like. `[SWITCH <name> <default>` opens one; each
case is `n RUNS <word>` (an existing word) or `n RUN: ... ;` (a nameless
definition for this case only); `SWITCH]` closes it. Each entry is a linked-list
node holding link, key and xt; nodes may lie anywhere in memory, which is what
lets a switch grow later. Executing the switch with a key searches the list in
order and runs the matching xt without the key; with no match it runs the
default with the key on the stack. Order is free, but frequent keys first search
faster. `[+SWITCH <name>` reopens an existing switch to append cases, ended by
`SWITCH]`, and cannot change the default.

| Word | Effect | Meaning |
|---|---|---|
| `[SWITCH <name>` | ( -- switch-sys addr ) | Begin a switch; its default word follows the name. |
| `[+SWITCH <name>` | ( -- switch-sys addr ) | Reopen a switch to add cases. |
| `SWITCH]` | ( switch-sys addr -- ) | Terminate the list. |
| `RUNS <word>` | ( switch-sys addr n -- switch-sys addr ) | Case n runs word. |
| `RUN: ... ;` | ( switch-sys addr n -- switch-sys addr ) | Case n runs the following nameless code. |

**§4.5.5 Execution vectors.** `DEFER <name>` ( -- ) makes a word that runs the
xt stored in it and aborts if none has been set; `IS <name>` ( xt -- ) sets it.
Setting a deferred word with `TO` is not supported. `@EXECUTE` ( addr -- )
executes the xt in a cell and does nothing if the cell holds zero, so tables of
vectors may safely start zeroed.

**§4.5.6 Local variables.** ANS-compatible `LOCALS| a b c |` ( xn ... x1 -- )
inside a colon definition takes initial values from the stack, the first name
getting the top item, so names read in the reverse of the stack comment. A local
returns its value; `TO <name>` ( x -- ) stores, `+TO <name>` ( n -- ) adds, `&OF
<name>` ( -- addr ) gives its address; all three also work on `VALUE`s. Locals
are visible only inside their definition and are found first while it compiles.
Limits: at most 16 locals per definition, names up to 254 characters. Locals
live on the return stack, so nothing may be pushed there (`>R`, `DO`) before the
declaration. Because locals cannot be used interpretively, the manual advises
using them sparingly.

**§4.6 Convenient extensions.** `++` ( addr -- ) increments a cell. `@+` ( addr
-- addr+4 x ) fetches and advances; `!+` ( addr x -- addr+4 ) and `~!+` ( x addr
-- addr+4 ) store and advance. `3DUP`, `3DROP` act on three items. `ZERO` ( x --
0 ). `ENUM <name>` ( n1 -- n2 ) defines a constant n1 and leaves n1+1; `ENUM4`
steps by 4 for cell offsets. `NEXT-WORD` ( -- addr u ) parses the next word,
crossing line ends, with length 0 at end of file. `GET-XY` ( -- nx ny ) reads
the cursor position, the inverse of `AT-XY`. `/ALLOT` ( n -- ) allots n zeroed
bytes.

**§4.7 Exceptions and errors.** Errors use Standard `CATCH` and `THROW`. The
text interpreter wraps each line it interprets in a `CATCH`; its handler prints
a message and aborts interpretation. Applications catch where they want control
and dispatch on the code with `CASE` or `IF`. Optional warnings flag likely
mistakes such as redefinitions and compiling or storing absolute addresses
(§5.1.4), and can be limited to some classes or turned off.

- `>THROW` ( n addr u -- n ) registers a message for throw code n, which the
  standard handler prints when it catches n, and returns n so it can be named:
  `n S" text" >THROW CONSTANT name`.
- The system's own codes are named `IOR_<name>` (`WORDS IOR_` lists them). The
  value `THROW#` holds the next free code; an application allocates its codes by
  starting from `THROW#`, chaining `S" text" >THROW ENUM IOR_X` for each, and
  storing the final count back with `TO THROW#`.
- OS signals become throws: a signal (for example SIGSEGV from `0 @`) is
  converted to a `THROW` that any `CATCH` can handle, and the report names the
  signal, address and word. Each thread has its own chain of catch frames,
  nested independently.

**§4.8 Standard Forth compatibility.** SwiftForth targets ANSI X3.215-1994 /
ISO/IEC 15145:1997 and ships the Hayes compliance suite (`unsupported/anstest`).
It provides every word of every ANS word set (Core, Block, Double, Exception,
Facility, File, Floating, Locals, Memory, Tools, Search, String and their
extensions) except the obsolescent `EXPECT` and `SPAN` (Core Ext) and `FORGET`
(Tools Ext); Block Ext is an option, with an extended block editor (Appendix A).
Appendix B gives the implementation-defined details.

## 5 SwiftForth Implementation

**§5.1.1 Execution model.** A 32-bit, subroutine-threaded, native-code Forth:
each reference inside a colon definition compiles as a machine `CALL`, so there
is no address interpreter. Colon and code definitions have no separate code
field (the body is the code); data objects start with a call into the run-time
code of their type. A colon definition ends in a machine return rather than a
compiled `EXIT`. A header byte marks a word for inline expansion instead of a
call; kernel primitives defined with `ICODE` instead of `CODE` (§6.2) get it,
and the compiler inlines any word whose inline field is set.

**§5.1.2 Optimizer.** A rule-based optimizer, on by default, rewrites common
high-level phrases into short machine sequences: it folds literals and
arithmetic on literals into immediate operands, fuses comparisons with the
following conditional branch (`DUP 9 > IF` becomes a compare and a conditional
jump), and removes the stack traffic between primitives; the manual's example
shrinks a word from about twenty instructions to five. `-OPTIMIZER` and
`+OPTIMIZER` turn it off and on, for debugging or for comparing with `SEE`. When
a colon definition ends, the optimizer turns a final `CALL` plus `RET` into a
`JMP` (tail call); `NO-TAIL-RECURSION` after a definition prevents that, for
words that manipulate the return stack.

**§5.1.3 Registers.** EBX caches the top of the data stack; ESI points at the
current task's user area; EDI holds the origin of SwiftForth's memory; EBP
points at the data stack; ESP, the hardware stack, is the return stack. All
other registers are free scratch, not preserved. Threading by subroutine frees
the registers an address interpreter would need, which is what pays for the
top-of-stack cache.

**§5.1.4 Memory model.** The dictionary is one contiguous flat 32-bit space and
the image is position-independent: compiled references are relative, so it runs
wherever the OS loads it. Words for data objects (`CREATE`, `VARIABLE` and the
like) nevertheless return absolute addresses, which pass straight to OS and
library calls; every "addr" in the manual is absolute. Execution tokens are
offsets from the start of the run-time memory, used only internally. Rule: never
compile an absolute address into code meant for a turnkey image; execute the
object's name at run time instead. Assembler references to data follow §6.5.5.

**§5.1.5 Stacks.** Two stacks of 32-bit cells, both in the OS-assigned stack
region, growing downward; the return stack is the CPU's call stack. Return-stack
rules: read only what you pushed (`R@ R> 2R@ 2R>` after `>R 2>R`); inside a `DO`
loop do not reach values pushed before the loop, and pop everything you pushed
in the loop before `I`, `J`, `LOOP`, `+LOOP` or `LEAVE`; push nothing before a
locals declaration; pop everything before the definition ends or `EXIT`s.

**§5.1.6 Dictionary.** Names up to 254 characters; no address alignment
requirement.

**§5.2 Memory organization.** The dictionary occupies a virtual region whose
size is fixed at launch, 8 MB by default. It starts at `ORIGIN` and grows
upward; the stacks grow downward. The OS allocates stack space at load;
SwiftForth takes the top part for the data stack and leaves the rest to the
return stack. At boot there is one task, `OPERATOR`, whose user area sits above
the kernel (§7).

| Word | Effect | Meaning |
|---|---|---|
| `ORIGIN` | ( -- addr ) | Start of SwiftForth's committed memory. |
| `MEMTOP` | ( -- addr ) | Top of committed memory. |
| `+ORIGIN` | ( xt -- addr ) | xt to absolute address. |
| `-ORIGIN` | ( addr -- xt ) | Absolute address to relative. |
| `UNUSED` | ( -- n ) | Free dictionary space; must stay at least 32K. |
| `SET-MEMSIZE` | ( n -- ) | Request a dictionary of the larger of n and current size plus 32K; takes effect in images saved with `PROGRAM` and relaunched. |
| `FYI` | ( -- ) | Show origin, `HERE`, committed memory and free space. |

**§5.3 Control-structure balance.** When a colon definition ends, the compiler
checks that every control structure was closed and aborts with an "unbalanced"
error otherwise. `-BALANCE` and `+BALANCE` turn the check off and on (default
on). Turning it off permits deliberately split structures, such as a `BEGIN` or
`IF` opened in one definition and resolved in the next, which then share code.

**§5.4 Dynamic memory.** The ANS memory word set; `ALLOCATE` takes memory from
the C heap via `malloc`, outside the dictionary. Memory not freed is released
when the process ends.

**§5.5.1 Dictionary structure.** Code, headers and data are interleaved in one
space, with no code/data separation. A header has a marker byte of FFh that
marks where the name field begins (no other name-field byte may be FFh), a flags
byte (immediate and smudge bits among them), a count byte (0-254) followed by
the name, and an inline byte: zero means callers compile a `CALL`, otherwise it
gives the inline expansion size, 1-254 bytes, which excludes any final `RET`.

| Word | Effect | Meaning |
|---|---|---|
| `IMMEDIATE` | ( -- ) | Set the immediate bit of the latest header. |
| `+SMUDGE`, `-SMUDGE` | ( -- ) | Set or clear the smudge bit, which hides the name from search; set while a colon definition is compiled so it cannot accidentally find itself. |
| `>BODY`, `BODY>` | ( xt -- addr ), ( addr -- xt ) | Between xt and parameter field. |
| `>CODE`, `CODE>` | ( xt -- addr ), ( addr -- xt ) | Between xt and code address. |
| `>NAME`, `NAME>` | ( xt -- addr ), ( addr -- xt ) | Between xt and name field. |

**§5.5.2 Wordlists and vocabularies.** Definitions are kept in many linked
lists, called wordlists, to shorten searches, let one name mean different things
in different contexts, shield internal words from misuse or redefinition, and
let the programmer order the search. The ANS search-order words are complete. A
vocabulary is a named wordlist; executing its name puts its wordlist first in
the search order (`VOCABULARY` defines one). New definitions go to the current
wordlist, normally a vocabulary. Each wordlist is split into strands chosen by a
hash of the name, to speed search; any count is allowed, the default being 31,
set system-wide by `#STRANDS` and saved with a new image.

| Word | Effect | Meaning |
|---|---|---|
| `WORDLIST` | ( -- wid ) | New, empty, unnamed wordlist; name it with `CONSTANT` if wanted. |
| `(WID-CREATE)` | ( addr u wid -- ) | Add a definition named by the string addr u to wordlist wid. |
| `SEARCH-WORDLIST` | ( addr u wid -- 0 \| xt 1 \| xt -1 ) | Look a name up in one wordlist; 1 means immediate. |
| `#STRANDS` | ( -- addr ) | Strands per new wordlist, default 31. |

**§5.5.3 Packages.** A package encapsulates a group of words: a small public API
stays visible, and its supporting words stay private so they can change without
breaking outside users. Packages are built from wordlists.

- `PACKAGE <name>` creates a named wordlist and leaves a *tag* on the data
  stack, which `PUBLIC`, `PRIVATE` and `END-PACKAGE` consume and pass along; the
  tag must stay on the stack for the whole package.
- Definitions start out private: `CURRENT` is set to the package's own wordlist.
  Private words are usable while the package compiles, and later only by putting
  that wordlist in the search order explicitly.
- `PUBLIC` sends later definitions to the wordlist current at the time the
  package opened; public words can call private ones and anything else in the
  search order. `PRIVATE` switches back. The two can alternate any number of
  times.
- `END-PACKAGE` drops the package wordlist from the search order, so only the
  public words remain visible.
- Naming an existing package with `PACKAGE` reopens it instead of creating a
  second one, after which `PUBLIC`, `PRIVATE` and `END-PACKAGE` apply as before;
  this is how a package is extended later.

| Word | Effect | Meaning |
|---|---|---|
| `PACKAGE <name>` | ( -- tag ) | Open the package, creating it if it does not exist. |
| `PRIVATE` | ( tag -- tag ) | Later definitions are hidden outside the package (the default). |
| `PUBLIC` | ( tag -- tag ) | Later definitions are visible outside the package. |
| `END-PACKAGE` | ( tag -- ) | End the package, hiding its private words. |

**§5.5.4 Header constants.** The many named C header constants of the OS API
(such as `O_RDWR`) are not Forth definitions, which would bloat the dictionary
and slow searches. The compiler loads an external table of them and consults it
when a token is neither a word nor a number; a hit acts as a numeric literal.

**§5.5.5 Search order of the interpreter.** For each token, in order: the active
local variables, the dictionary by the search order, number conversion (floating
point too if loaded), the header-constant table, and finally an abort with an
error message.

**§5.6 Terminal-type devices.** The standard character I/O words (`KEY`, `EMIT`,
`TYPE` and the rest) are vectored through a *personality*, the set of device
functions behind the terminal API. Each task has its own current personality,
pointed to by the user variable `'PERSONALITY`; the console and buffered output
are two personalities.

- *Layout (§5.6.1).* A personality is a data section then a vector section. The
  data section's cells are: data size in bytes, number of vectors, an I/O handle
  and the saved previous personality (the last two start at 0). The vectors are
  xts in a fixed order: `INVOKE` and `REVOKE` (open and close the device),
  `/INPUT` (reset input), `EMIT`, `TYPE`, `?TYPE` (type honoring margins), `CR`,
  `PAGE`, `ATTRIBUTE` ( n -- ), `KEY`, `KEY?`, `EKEY`, `EKEY?`, `AKEY` (the key
  routine `ACCEPT` uses, handling editing keys), `PUSHTEXT` ( addr len -- )
  (feed text to the interpreter), `AT-XY`, `GET-XY`, `GET-SIZE` ( -- nx ny ) and
  `ACCEPT`. A device may stop the table early but may not leave gaps; any vector
  present must have the right stack effect, using `NOOP`, `DROP`, `2DROP` or a
  word returning zeros for unsupported operations.
- `OPEN-PERSONALITY` ( addr -- ) saves the current personality in the new one
  and runs its `INVOKE`; `CLOSE-PERSONALITY` ( -- ) closes the current one and
  restores the saved one. The buffered personality in `src/ide/buffio.f` uses
  this to capture a word's output between `[BUF` and `BUF]`, with `@BUF` ( --
  addr len ) returning the captured text.

**§5.6.2 Keyboard events.** `EKEY` and `EKEY?` report extended keys: escape
sequences from the terminal are mapped to constants `K-UP`, `K-DOWN`, `K-LEFT`,
`K-RIGHT`, `K-INSERT`, `K-DELETE`, `K-HOME`, `K-END`, `K-PRIOR`, `K-NEXT`,
`K-F1` to `K-F12`, the Ctrl-arrow variants, and the mask `K-CTRL-MASK`. Keys the
terminal or desktop consumes never arrive.

**§5.7 Timers.** `COUNTER` and `TIMER` read the system clock (`gettimeofday`,
granularity down to a microsecond depending on the system); `MS` calls
`nanosleep`. Neither is exact; both depend on configuration and load.

## 6 i386 Assembler

SwiftForth runs in 32-bit protected mode on IA-32 (Intel and AMD); the chapter
supplements Intel's manuals and notes only departures from Intel usage.

**§6.1 Principles.** Assembly implements the kernel, CPU-specific operations and
time-critical code; the kernel itself is written with this assembler. Each
mnemonic is a Forth word that assembles its instruction at the next code
location. Names follow MASM, but operands come first, in postfix order on the
data stack, and the mnemonic last. Conditional jumps are built from a condition
code followed by `IF`, `UNTIL` or `WHILE` (§6.8).

**§6.2 Code definitions.** `CODE <name> ... RET END-CODE` or `ICODE <name> ...
RET END-CODE`; every code definition ends with `END-CODE`.

| Word | Effect | Meaning |
|---|---|---|
| `CODE <name>` | ( -- ) | A machine-code word that colon definitions *call*; it may call other words and exit at several points. Executed when interpreted. |
| `ICODE <name>` | ( -- ) | A machine-code word that colon definitions *expand inline*; it may contain no calls or branches outside itself and must leave only through its final `RET`. Executed when interpreted. |
| `LABEL <name>` | ( -- ) | Names a code address: the name always returns the address (as a branch or call target), never executes it. |
| `END-CODE` | ( -- ) | End a `CODE`, `ICODE` or `LABEL` sequence. |

None of these may define an entry point anywhere but the start of a routine.

**§6.3 Registers.** The eight 32-bit general registers EAX, EBX, ECX, EDX, EBP,
ESI, EDI, ESP, with 16-bit forms (AX and so on) and byte halves AH-DH and AL-DL;
ESP cannot be an index. Some instructions fix their registers (string ops use
ECX, ESI, EDI; also wide multiply and divide, I/O, loop, variable shifts, stack
ops). The Forth machine reserves EBX (top of stack), ESI (user area), EDI
(memory origin), EBP (data stack) and ESP (return stack); code must save and
restore them before other use.

**§6.4 Instruction components.** An instruction may have prefixes, register
specifiers, an addressing-mode byte, a SIB (scale-index-base) byte, a
displacement (signed 8, 16 or 32 bits; 8-bit is used when it fits and is
sign-extended) and the opcode, which alone is required. The assembler takes the
other components as arguments before the opcode word; apart from that order it
follows Intel notation.

**§6.5 Operands.** Everything but the opcode is an operand; with two, the order
is source then destination.

- *§6.5.1 Implicit:* named by the instruction (`CLD`, `RET`).
- *§6.5.2 Register:* register names before the opcode, as in `EBX ECX MOV` (copy
  top of stack to ECX).
- *§6.5.3 Immediate:* a number followed by `#`, as in `8 # EBP ADD`; any base or
  base prefix works.
- *§6.5.4 I/O:* `IN` and `OUT` take an 8-bit immediate port or DX; user-mode
  programs normally may not use them.
- *§6.5.5.1 Direct and offset addressing.* `JMP` and `CALL` targets assemble as
  offsets from the instruction; a word's target address is `' <name> >CODE`.
  Data addresses are absolute, and since a shared library may load at different
  addresses, code that could end up in one must not compile them. `ADDR` ( addr
  reg -- ) is a macro that loads reg with a data object's address computed
  relative to SwiftForth's memory base (cheapest for EAX). To abort from code,
  put the throw code in EBX and jump to `' THROW >CODE`. Offset addressing is
  written `disp [base] [index*scale] opcode`: the displacement is always
  written, even 0 (the assembler picks the shortest encoding and omits a zero
  one); either register may be omitted; scale 1 is written like a base register,
  others as `[reg*2]`, `[reg*4]`, `[reg*8]`. Example: `0 [EDI] [EDX*4] EAX CMP`
  compares EAX with the cell EDX cells past EDI.
- *§6.5.5.2 Stacks.* ESP is the hardware stack, used implicitly by `CALL` and
  `RET` and explicitly by `PUSH` and `POP`, handy for saving registers. The data
  stack in EBP has no push instruction: decrement EBP by 4 and store to push,
  increment to pop, remembering the top item lives in EBX. Macros `PUSH(EBX)`
  and `POP(EBX)` push and pop the cached top item. `@` is a single `0 [EBX] EBX
  MOV`; `2DROP` loads EBX from `4 [EBP]` and adds 8 to EBP.
- *§6.5.5.3 User variables.* A user variable returns an absolute address
  (current user area plus offset), so indexing ESI with it would add the base
  twice. `[U]` ( addr -- +addr ) converts it into an ESI-relative operand: `BASE
  [U] EBX MOV`.

**§6.6 Mode specifiers.** *Size (§6.6.1),* written after a memory operand:
`BYTE` (8-bit), `WORD` (16), `DWORD` (32-bit integer or real), `QWORD` (64-bit
integer or real), `TBYTE` (80-bit real or BCD), as in `0 [EAX] TBYTE FLD`.
*Repeat (§6.6.2):* the string instructions use ESI as source, EDI as destination
and ECX as count, step one element by default, and repeat under `REP` (until ECX
is 0), `REPE`/`REPZ` (also stop when ZF clears) and `REPNE`/`REPNZ` (also stop
when ZF sets). Segment overrides exist but user-mode programs may not use them.

**§6.7 Direct branches.** Forth style prefers short definitions and structured
flow, but `JMP`, `CALL` and every conditional jump are available for compactness
or speed. `CALL` needs the code address of a routine that ends in `RET`: a
`LABEL` or `' <name> >CODE`. Local labels inside one routine: `n L:` ( n -- )
defines label n (0-19) and resolves forward branches to it; `n L#` ( n -- addr )
references it for a forward or backward branch. The manual discourages them in
favor of the structures of §6.8.

**§6.8 Assembler structures.** Structured macros replace labels and work like
their high-level namesakes, except that they test the CPU flags set by the
previous instruction instead of the stack: `BEGIN ... AGAIN`, `BEGIN ... cc
UNTIL`, `BEGIN ... cc WHILE ... REPEAT`, `cc IF ... ELSE ... THEN`. A condition
code followed by `UNTIL`, `WHILE` or `IF` assembles one conditional jump;
`BEGIN`, `REPEAT`, `ELSE` and `THEN` supply its targets. Intel's `LOOP`,
`LOOPE`, `LOOPNE` also close a `BEGIN`. `EBX EAX CMP < IF` runs the true part
when EAX is less than EBX. Unlike high-level Forth, a structure may span
routines (the assembler picks short or long jump forms itself), though the
manual advises against it; the structure words keep addresses on the data stack,
so mixing structures needs care.

| Word | Effect | Meaning |
|---|---|---|
| `BEGIN` | ( -- addr ) | Mark a branch target; assembles nothing. |
| `AGAIN` | ( addr -- ) | Unconditional jump back. |
| `UNTIL` | ( addr cc -- ) | Conditional jump back. |
| `WHILE` | ( addr1 cc -- addr2 addr1 ) | Conditional forward jump, target pending. |
| `REPEAT` | ( addr2 addr1 -- ) | Resolve the `WHILE` exit and jump back to `BEGIN`. |
| `IF` | ( cc -- addr ) | Conditional forward jump, target pending. |
| `ELSE` | ( addr1 -- addr2 ) | Resolve the `IF` and start an unconditional forward jump. |
| `THEN` | ( addr -- ) | Resolve a pending forward jump; assembles nothing. |

Condition codes ( -- cc ) are named for the case in which the following code
runs, so the jump assembled is on the opposite condition, as in high-level `0<
IF`. Signed: `0<`, `0=`, `0>`, `0<>`, `0>=`, `<`, `>`, `>=`, `<=`. Unsigned:
`U<`, `U>`, `U>=`, `U<=`. Flags: `CS` and `CC` (carry), `PE` and `PO` (parity),
`OV` (overflow), `ECXNZ` (proceed while ECX is not zero, jumping on zero via
`JECXZ`), and `NEVER`, which makes the jump unconditional. `NOT` ( cc1 -- cc2 )
inverts a condition code; since `NOT` is also an x86 mnemonic, it inverts when a
valid condition code is on the stack and otherwise assembles the `NOT`
instruction.

**§6.9 Assembler macros.** Assembler words are ordinary Forth words that
assemble when executed, so a macro is simply a colon definition whose body runs
assembler words. The assembler vocabulary is normally off the search order,
because its `IF`, `WHILE` and so on mean something else in high-level Forth;
`CODE`, `ICODE` and `LABEL` select it and `END-CODE` restores the previous
order, but a macro must switch itself. `PUSH(EBX)` and `POP(EBX)` are macros
built this way, and the kernel builds its comparison primitives from one macro
that takes a condition code and emits the compare and flag-setting sequence.
Search-order words: `ASSEMBLER` ( -- ) puts the assembler vocabulary on top of
the search order; the immediate words `[+ASSEMBLER]` and `[FORTH]` do the same
for the assembler and the main Forth vocabulary inside a definition, and
`[PREVIOUS]` drops the top of the order.

## 7 Multitasking

**§7.1.1 Definitions.** The OS runs separate processes, each with its own
resources, and threads within a process, which run code in the process's memory
asynchronously under OS scheduling; the parent starts the thread, sets its
priority and can stop or kill it. A SwiftForth task is a thread with its own
stacks and user variables, running code in the shared dictionary. Because the OS
preempts and schedules, there is no cooperative round-robin loop like FORTH,
Inc.'s embedded multitaskers. Separate SwiftForth processes are fully
independent.

**§7.1.2 Re-entrancy.** Code shared by tasks must be re-entrant; global
variables break that. Forth code is easily re-entrant because intermediate
results live on the stacks, per-task buffers hold strings and arrays, and
per-task copies of variables (user variables) give each task private state
behind shared names. So the whole dictionary is shared by all tasks.

**§7.2 Tasks.** A task is an OS thread to which SwiftForth adds Forth
facilities; the manual suggests naming a task with a permanent job after that
job. The OS gives each task its own stack frame, holding its data and return
stacks and its user variables. Any task may inspect or change another task's
user variables, but no task may alter another's stacks.

**§7.2.1 User variables.** Per-task copies of variables that shared code needs
privately, such as `BASE`, so tasks run the same code without conflict. A user
variable's name returns its address in the user area of the task that executes
it (the user-area base register plus a fixed offset). A task's name followed by
`@` gives the address of the first user variable of its user area,
conventionally `STATUS`. The system's user variables are in `src/kernel/data.f`.
Every user variable exists in every task whose user area is big enough. The
manual advises defining them together rather than scattered.

| Word | Effect | Meaning |
|---|---|---|
| `+USER <name>` | ( n1 n2 -- n3 ) | Name an n2-byte user variable placed at offset n1; n3 is where the next one starts, so successive definitions chain on the stack. |
| `#USER` | ( -- n ) | A `VALUE`: bytes of user area allocated so far, the start offset for more `+USER`s; store the final offset back with `TO #USER`. |
| `HIS` | ( addr1 n -- addr2 ) | The same user variable in another task: `MONITOR BASE HIS` converts the executing task's address of `BASE` into an offset and applies it to `MONITOR`'s user area. |

**§7.2.2 Sharing resources.** Devices, non-re-entrant routines and shared data
are guarded one task at a time with `GET` and `RELEASE`, modeled on Dijkstra's
semaphore operations. A *facility variable* is an ordinary `VARIABLE`: zero when
free, else the address of the owner's `STATUS`. `GET` ( addr -- ) pauses first,
then waits, pausing repeatedly, until the facility is free or already its own,
and takes it. `RELEASE` ( addr -- ) frees it if the caller owns it and does
nothing otherwise, so neither can take a facility from another task. There is no
deadlock protection: two tasks taking two facilities in opposite orders can
hang. The manual's remedy is to hold one facility at a time, or else always take
several in the same order.

**§7.2.3 Task definition and control.** Definition and instantiation are
separate. `n TASK <name>` compiles a permanent dictionary entry holding a task
control block (TCB) with the requested size of user area plus data stack; the
name returns the TCB address. Instantiation asks the OS for the thread's stack
frame, sets up stacks and user area there, and assigns the work, inside a colon
definition: `: START  MONITOR ACTIVATE  ... ;`, so that the words after
`ACTIVATE` run in the new task. That work is either transient (it ends with `;`
or `EXIT`, and the task can be activated again later) or persistent (an endless
loop, which must call `PAUSE` or a blocking OS call, or it eats its time
slices). The activating task is the owner. Communication is through shared
memory or the task's user variables.

| Word | Effect | Meaning |
|---|---|---|
| `TASK <name>` | ( u -- ) | Define a task of u bytes; name returns its TCB. |
| `CONSTRUCT` | ( addr -- ) | Instantiate the task if its first TCB cell is still zero, storing a pointer to its memory there; optional, as `ACTIVATE` does it. |
| `ACTIVATE` | ( addr -- ) | Instantiate if needed and start the task running the rest of the current definition. Compile only. |
| `HALT` | ( addr -- ) | Stop the task permanently at its next `PAUSE`, keeping it instantiated; it can be activated again. |
| `KILL` | ( addr -- ) | Stop the task and free its memory; it can be activated again. |
| `PAUSE` | ( -- ) | Yield the CPU. |

## 8 System Functions

**§8.1 Dynamically loaded libraries.** Libraries opened at run time through the
OS's open, look-up, error and close API (`dlopen` and friends), which SwiftForth
wraps in an import syntax modeled on C prototypes. The import words are in
§8.1.1.

- `LIBRARY <file>` ( -- ) opens a library for the imports that follow.
- `FUNCTION: <name> ( params -- return )` ( -- ) defines a word that calls the
  named C function. The parameters are listed left to right in the C prototype's
  order, one cell each, strings usually zero-terminated; the right side is one
  result or none (at most one value is returned). `( ... -- return )` declares a
  varargs function, which takes its argument count on top of the stack at run
  time. Unlike the Windows version, which searches every open library, the Linux
  and macOS versions resolve a function only in the most recent `LIBRARY`.
- `[OPTIONAL]` ( -- ) before `LIBRARY` or `FUNCTION:` suppresses the error when
  the library or symbol is missing at compile time or at turnkey start; calling
  an unresolved optional import aborts with a message on stderr.
- `GLOBAL: <name>` ( -- ) imports a data symbol from the current library: no
  stack picture, no parameters, and the word returns the data's address rather
  than calling anything.
- `.LIBS` ( -- ) lists opened libraries; `.IMPORTS` ( -- ) lists imported
  functions with resolved address, library and name.

**§8.2 Callbacks.** A callback is an entry point the OS (or a library) calls,
for example a signal handler passed to `sigaction`; it is asynchronous to the
application, like an interrupt. For each call SwiftForth makes a temporary task,
which has private stacks and a private user area and exists only until the call
returns. `xt n CALLBACK: <name>` ( xt n -- ) wraps xt to take n parameters; n
drives the stack cleanup code, so it must be exact. The callback must return
exactly one integer, the function's result. On entry both stacks are empty,
`BASE` is decimal, and only `S0`, `R0`, `H` and `BASE` are initialized; the
callback may use the stacks, `HERE`, `PAD` and the number-formatting buffer,
owns neither a dictionary nor a data area, may keep scratch values in other
user variables until it returns, may run any re-entrant word, and talks to
the program only through shared data, which needs care. Parameters are read
with `_PARAM_0` to `_PARAM_7` ( -- x ), the first through eighth arguments,
which are defined by `NTH_PARAM <name>` ( n -- ) (position n from the top of
the frame, 0 the top).

**§8.3 Command-line arguments.** The C `main(argc, argv)` arguments. `ARGC` ( --
n ) is at least 1, counting the program name. `ARGV` ( i -- addr len ) returns
argument i, or an empty string if i is out of range. `CMDLINE` ( -- addr len )
joins the arguments into one counted-string buffer, so at most 255 characters.

**§8.4 Environment.** `FIND-ENV` ( addr1 len1 -- addr2 len2 flag ) looks up a
variable by name (without `=`, which it adds) and returns its value and true, or
the original string and false. `.ENV` (synonym `PRINTENV`) ( -- ) prints the
environment. Helpers: `ATOI` ( addr len -- n ) converts decimal, or hex after
`0x`, giving 0 on failure; `GETCH` ( addr1 len1 -- addr2 len2 char ) splits off
the first character.

## 9 SwiftForth Object-Oriented Programming (SWOOP)

SWOOP is SwiftForth's object package. It provides encapsulation (data and
methods in one unit that answers messages), information hiding, inheritance (a
class built from a parent, adding, replacing, or filling in deferred members)
and polymorphism (subclasses answering one message differently). Parts of the
chapter derive from articles in *Forth Dimensions*.

**§9.1.1 Classes and members.** `CLASS <name>` ... `END-CLASS` ( -- ) defines a
class; everything between is a *member*. Executing a class name leaves its
handle, *hclass*, for the constructors. The definition allocates no storage; it
only records how much each instance needs. A small class might hold `VARIABLE
X`, `VARIABLE Y` and colon members that print them. The five member kinds are:

1. *Data members*: any data definition, namely `CREATE` (with `,` or `C,`),
   `BUFFER:` (size in address units), `VARIABLE`, `CVARIABLE` (one character)
   and `CONSTANT`. They behave as the ordinary words do, but at an offset in the
   instance.
2. *Colon members*: definitions that work on the data members, valid only in the
   context of an instance.
3. *Embedded objects* of other classes (§9.1.4).
4. *Deferred members*: colon-like members with a default behavior that
   subclasses may replace (§9.1.6).
5. *Messages*: unnamed responses to numeric message values (§9.1.7).

**§9.1.2 Static instances.** `BUILDS <name>` ( hclass -- ) is the defining word
for a static instance: `POINT BUILDS ORIGIN`. Executing an instance name does
two things: it adds the class's member names to the interpreter's context, and
it pushes the instance address. A following member acts on that address: a data
member adds its offset; a colon or deferred member saves `'SELF`, sets it to the
instance for the duration of the call, and restores it afterward. Each instance
has its own data, and members have no existence apart from an instance.
`BUILDS[] <name>` ( n hclass -- ) builds an array of n static instances; the
name takes an index and returns that instance's address (the `[]` suffix is a
naming convention).

**§9.1.3 Dynamic objects.**

- `NEW` ( hclass -- addr ) allocates an instance from memory, like `ALLOCATE`;
  it is not a defining word. `DESTROY` ( addr -- ) frees it, and must be called
  explicitly because such objects are not in the dictionary.
- `USING <classname>` ( addr -- addr ) applies a class's members to the object
  at addr; it is early bound, since the class is known when compiling. With a
  `VALUE` holding the address, `FOO USING POINT X !` stores into the object.
- Local objects live only while one definition runs and are visible only inside
  it. They are declared between `[OBJECTS` and `OBJECTS]` ( -- ), at most one
  such region per definition. Inside it, `MAKES <name>` ( hclass -- addr )
  instantiates a local object for the duration of the call, and `NAMES <name>` (
  addr hclass -- ) gives a local name to an existing instance whose address was
  passed in, allocating nothing. With these, one word can create an object and
  pass its address to words that name it locally. `MAKES` and `NAMES` may share
  one region.

**§9.1.4 Embedded objects.** `BUILDS` inside a class embeds an instance of
another class as a member (such as two points as the corners of a rectangle).
The embedded data is simply added to the container's data and constructed with
it. Members of the outer class reach the inner object's members by name; a
member of the inner class keeps its own meaning even when the outer class has a
member of the same name. The container exposes the public members of its
embedded objects as well as its own.

**§9.1.5 Information hiding.** Member names are already out of scope without an
instance, but classes also control visibility. Members are `PUBLIC` by default;
`PROTECTED` members are visible only in the class and its subclasses; `PRIVATE`
members only within the defining class. The three words ( -- ) are valid only
inside a class definition and set the visibility of the members that follow. A
typical class keeps its storage and helpers private and exposes accessor
members.

**§9.1.6 Inheritance and polymorphism.** `SUBCLASS <name>` ( hclass -- ) starts
a class that inherits every member of the parent and is otherwise like `CLASS`.
A redefined member is used from then on in the subclass, but references compiled
earlier stay bound to the old member. For polymorphism a member is declared with
`DEFER: <name>` ( -- ), followed by its default body. A subclass that defines a
colon member of that name overrides it everywhere it is called in the subclass's
context, including from members compiled in the parent. For example, a parent's
`DOT` that calls a deferred `SHOW` prints the subclass's version for a subclass
instance. `SUPREME` ( -- hclass ) is the root class of every class made with
`CLASS`.

**§9.1.7 Numeric messages.** A member name resolves to a *member ID*, which is
matched against the class's member chains. `n MESSAGE: ... ;` ( n -- ) defines
an unnamed member answering message n, which is the same as member ID n.
`SENDMSG` ( addr n -- ) runs member n in the context of the object at addr, the
way to send an arbitrary message.

**§9.1.8 Early and late binding.** SWOOP binds early by default: the compiler
resolves a member reference to a known member, which is simplest and fastest.
Late binding, looking the member up at run time, is available in three ways:
deferred members (name known, behavior chosen later), `SENDMSG` (any ID to any
object), and `CALLING` and `->`, which bind dynamic objects whose class is known
only at run time, in contrast to `USING`. The manual does not give those last
two a glossary entry.

**§9.2.1 Class structure.** A class is a ten-cell record, and every class is
linked into one list headed by `CLASSES` (typing `CLASSES` shows the hierarchy).
The cells hold: the link, the handle, the superclass handle, the heads of the
public, protected and private member chains, the anonymous (numeric message)
chain head (really three chains), the instance size, the head of the
embedded-object list, and a tag constant that marks the record as a class.

- The handle is the class name's xt: `' POINT` and `POINT` give the same value.
- Every class but `SUPREME` has a superclass, `SUPREME` by default. The member
  chains continue into the superclass's chains, back to `SUPREME`, which is how
  inheritance works.
- The instance size is the class's own data plus its superclass's size.
- The embedded-object list lets constructing an object also construct the
  objects embedded in it.
- *MEMBERS.* A member name is resolved in two steps. First the name must exist
  in the wordlist `MEMBERS`, which holds each member name once however many
  classes define it, and which is in the search order after an object or class
  name. Second, that entry's xt is the key searched for in the three named
  chains of the object's class; the chains, the class handle and `MEMBERS`
  together make up the class namespace. A hit binds the member, and for a data
  member that means the instance address plus the member's offset. A miss
  hands the name back to the normal search through the other wordlists.
- During a class definition the ordinary interpreter and compiler run, with the
  class namespace and `CC-WORDS` added to the search order.
- All links are relative and all handles are xts, so compiled classes and
  objects work at any load address.

**§9.2.2 Member structure.** Each member has a name in `MEMBERS`, whose xt is
its *member ID* or *message ID*, and a data record with five fields: the
compiler xt (early-binding behavior), the link to the next member, the message
ID, the run-time xt (late-binding behavior), and type-specific data. Each member
type has its own compiler and run-time xts, and both take the address of the
data field. The data field holds the offset in the object for a data member, the
xt to run for a colon member, the default xt for a deferred member (used only
when no subclass extends it), and the offset, class handle and construct link
for an embedded-object member.

**§9.2.3 Instance structure.** Members can be applied to any address, but formal
instances carry a header. A static instance (`BUILDS`) starts with two cells,
the object's own handle (used at compile time) and its class handle. A dynamic
one (`NEW`, `MAKES`) has only the class handle. At run time the class handle
lets late-bound access check that the member's ID is in a visible chain of the
object's class. For that reason the manual prefers formal instances accessed
with `CALLING` or `->` over `USING`, which applies any class's members to any
address (`PAD`, `HERE`) without a check.

**§9.3.1 Global state.** SWOOP merges instance data and methods into one
concept, the member, and each member's identifier doubles as its message. A name
corresponds to one identifier in every class and context. A class's member
chains are switches (§4.5.4) keyed by member ID, so a class knows its members'
IDs but not their names.

| Word | Effect | Meaning |
|---|---|---|
| `MEMBERS` | ( -- ) | Select the wordlist of all member names; added to the search order automatically when a class or object name executes. |
| `CC-WORDS` | ( -- ) | Select the wordlist of member-defining compiler words; in the search order only between `CLASS`/`SUBCLASS` and `END-CLASS`. |
| `'THIS` | ( -- addr ) | Holds the handle of the active class. |
| `'SELF` | ( -- addr ) | Holds the data address of the active object. |
| `CSTATE` | ( -- addr ) | The handle of the class being defined, or zero outside class definitions. |

`'THIS` and `'SELF` live in the user area, so each task has its own and object
code is safe to share.

**§9.3.2 Compilation strategy.** A class's namespace is every `MEMBERS` entry
whose ID matches a key in its chains. `MEMBERS` entries are immediate. When
found, an entry looks its own ID up in the chains of the class in `'THIS` and,
on a match, runs that member's compile or execute xt according to `STATE`. On no
match it puts the name back into the input and lets the ordinary interpreter
search the other wordlists. The rules for building a class:

- Every member definition gets its ID from `MEMBERS`: an existing name's ID is
  reused, and a new name is added to `MEMBERS`, never among the class's own
  definitions. The member's entry goes on the class's current chain, which is
  the public one unless changed.
- Member-defining words are the versions of `VARIABLE`, `:` and the rest found
  in `CC-WORDS`. A member colon definition is compiled as a `:NONAME` body; the
  matching `;` stores its xt with the ID as the chain entry.
- Using a member inside another member's body runs the used member's compiler
  method, which lays down the reference that suits its kind, such as
  `COLON-METHOD` or `DATA-METHOD`.
- `CSTATE` holds the handle of the class under construction from `CLASS` until
  `END-CLASS`, and zero otherwise; the member compilers read it. `CLASS` also
  makes the class record, chains it onto `CLASSES`, sets `'THIS` to the new
  handle and puts both `CC-WORDS` and `MEMBERS` into the search order.

**§9.3.3 Self.** Inside a class definition a bare member reference such as `X`
is unambiguous, so SWOOP inserts the implicit current object itself. No explicit
`SELF` is written, and member code reads like ordinary Forth, whereas outside
the class a member must follow an object.

**§9.4 Tools.** `SUPREME` provides `DUMP-OBJECT`, which dumps an object's whole
data area (`<object> DUMP-OBJECT`), and `ADDR`, a zero-length data member that
returns the start of an object's data, for partial dumps such as `<object> ADDR
n DUMP`.

## 10 Floating-Point Math Library

**§10.1 The FPU.** Floating point is an optional library for the x87 FPU, which
implements IEEE 754/854 real, integer and BCD types with an eight-deep, 80-bit
hardware register stack. SwiftForth uses that hardware stack only inside
primitives. Each task has its own *numeric stack* (N-stack) of `#NS` bytes, room
for 32 80-bit items, at the bottom of its data space below `HERE`. A primitive
moves its operands to the hardware stack and its result back. Text is converted
to floating point directly on the hardware stack, keeping full 80-bit precision;
integers still convert on the data stack. The design goals were throughput,
precision and compatibility with integer SwiftForth.

**§10.2 Using the option.** `REQUIRES fpmath` (or `INCLUDE`) loads it, and the
FPU is then initialized at startup. The user words comply fully with ANS Forth.
Memory operations (`F@`, `F!`) default to 64-bit IEEE doubles.

- *§10.2.1 Configuration.* Defaults live in `lib/options/<os>/fpconfig.f`, which
  `fpmath` loads with `REQUIRES`. A project overrides them by loading its own
  `fpconfig` from the working directory first; the load-once rule then skips the
  default.
- *§10.2.2 Input.* Per ANS, a float literal needs an `E` or `e` and must begin
  with a digit, optionally signed: `-0.5e0` is valid, `.2e0` is not. A decimal
  point and the exponent digits are optional (a missing exponent means 0). No
  punctuation other than the decimal point is allowed. Floats are tried only
  when `BASE` is decimal; otherwise, or if the text is not a valid float, §4.3
  applies. `(REAL)` does the conversion on the hardware stack; `>FLOAT` accepts
  a wider range of forms.
- *§10.2.3 Output.* `F.R` and `FS.R` ( n -- ) ( F: r -- ) print in `F.` or `FS.`
  form right-justified in n columns. A value whose significant digits all fall
  too far right prints as zeros, and integer digits that do not fit are printed
  anyway. `N.` ( F: r -- ) prints in a run-time-selected format: `FIX`, `SCI`,
  `ENG` ( n -- ) choose `F.`, `FS.` or `FE.` style with n significant digits.
  `SET-PRECISION` ( n -- ) sets the default precision.
- *§10.2.4 Literals* ( F: r -- ), compiling a float from the numeric stack:
  `DFLITERAL` and `FLITERAL` as 64-bit, `SFLITERAL` as 32-bit, and `FILITERAL`
  as a rounded two's-complement integer (64-bit if it needs more than 32).
- *§10.2.5 Constants and variables.* `FCONSTANT` and `FVARIABLE` are 64-bit,
  identical to `DFCONSTANT` and `DFVARIABLE`. `SFCONSTANT` and `SFVARIABLE` are
  32-bit. 80-bit real and packed BCD memory formats are not supported and would
  need `CODE` words.
- *§10.2.6 Memory access* (address on the data stack, value on the numeric
  stack). `F+!`, `DF+!` (64-bit) and `SF+!` (32-bit) add r to memory. `F,`,
  `FL,` (64-bit) and `FS,` (32-bit) compile r. Integer transfers: `FI@` and
  `DFI@` load a 64-bit integer and `SFI@` a 32-bit one as a float; `FI!`, `DFI!`
  and `SFI!` store r rounded to a 64- or 32-bit integer; `FI,`, `DFI,` and
  `SFI,` compile r so rounded.
- *§10.2.6.1 Stack operators.* `MAKE-FLOOR` makes float-to-integer transfers
  truncate (the ANS rule and the default, set at load) and `MAKE-ROUND` makes
  them round (the FPU's own rule). Also: `F2DUP` ( F: r1 r2 -- r1 r2 r1 r2 );
  `F?DUP` ( -- flag ) ( F: r -- r | ), which keeps r if nonzero; `FWITHIN` ( --
  flag ) ( F: r l h -- ); `/FSTACK`, which empties the numeric stack; `?FSTACK`,
  which aborts if it is empty; `S>F` and `D>F` (32- and 64-bit integer to
  float); `F>S` and `F>D` (back, rounding per the glossary); `F2*`, `F2/` and
  `1/N` (reciprocal); and `SIN`, `COS`, `TAN`, `COT`, `SEC`, `CSC`, whose
  arguments are in **degrees**.
- *§10.2.6.2 Matrices.* `SMATRIX <name>` and `LMATRIX <name>` ( nr nc -- )
  define a two-dimensional array of 32- or 64-bit floats. The first parameter
  cell holds the bytes per row and the elements follow, row-major. `row col
  name` returns an element address with zero-based subscripts and no bounds
  check. `SMD` and `LMD <name>` ( nr nc -- ) display a matrix, and the sizes
  must match its definition.

**§10.3 FPU assembler.** Floating-point mnemonics extend the i386 assembler in
the same postfix syntax; most FPU instructions exist. Memory operands are a
subset of the CPU forms, since the CPU computes FPU addresses.

- *§10.3.1 Hardware stack.* FPU instructions see only the hardware stack, so
  primitives begin by moving operands from the numeric stack and end by moving
  results back, with macros `>f` and `f>` (one item) and `>fs` and `fs>` ( n --
  ) (n items, order kept).
- *§10.3.2 Synchronization.* FPU instructions are not synchronized with the CPU
  unless coded so. Put a `WAIT` before a CPU instruction that uses memory the
  FPU is transferring, and end every `CODE` word that uses the FPU with `FNEXT`.
- *§10.3.3 Addressing.* Memory operands may be register indirect, register plus
  displacement, or direct (absolute). A format specifier before the operand
  selects size and type: `WORD` for a 16-bit integer, `DWORD` for a 32-bit
  integer or float, and `QWORD` for a 64-bit float. Register operands are
  relative to the hardware stack top, from `ST(0)` (top) and `ST(1)` on, so
  `ST(1) FXCH` swaps, `ST(0) FLD` duplicates, `ST(1) FLD` copies the second
  item, and `ST(0) FSTP` drops. Intel's IA-32 manuals are the reference for
  accuracy and operation.

## 11 Recompiling SwiftForth

The distribution has two binaries in `bin/<os>`. `sfk` is the bare kernel, a
minimal console program. `sf` is the full environment (debug tools, library
interface, SWOOP, the turnkey generator), and is itself a turnkey built from
`sfk`. Full source for both ships with the full product.

- *§11.1 Rebuilding the turnkey.* Start `sfk` and load the optional extensions
  with `HI`, which loads `hi.f` from the working directory if present, else from
  `src/ide/<os>`. Then save with `PROGRAM` (§4.1.2). Customizations belong in a
  local `hi.f` outside the installed tree so upgrades do not overwrite them.
- *§11.2 Rebuilding the kernel.* Kernel source is in `src/kernel` and the target
  compiler in `xcomp`. Run the full `sf` (the bare kernel lacks what the target
  compiler needs) in `src/kernel/<os>` and `INCLUDE make`. The new kernel is
  written to `bin/<os>/sfk` and `sf` exits; `make.f` sets the output name and
  place. The copyright notice shown by `sf` and `sfk` must not be altered.

## Appendix A: Managing Block Files

Native Forths once owned the disk as 1024-byte blocks with no directory;
OS-hosted Forths kept blocks inside files for portability. SwiftForth itself
works only with files, but offers optional block support for portability:
`REQUIRES blocks` for blocks alone, `REQUIRES blockedit` for blocks plus the
editor and tools.

**§A.1 Disk blocks.** Blocks live in host files, several of which may be open at
once; each file's blocks form a numbered *part*. The *blockmap* `PARTS` (up to
256 files as shipped) records for each part, one cell per field: the starting
block number (`#BLOCK`, -1 when the part is unused), the end block plus one
(`#LIMIT`), the file handle, an updated-but-not-flushed flag, the access mode,
and an index into `'FILENAMES`. `n >PART` points those field names at part n.
Part numbers are arbitrary handles unrelated to block numbers; part 0 is the
first mapped. Files ending in `.src` hold source and shadow blocks, and `.src`
is the default extension wherever a block file is named (a name with a dot
overrides it). A file created by `NEWFILE` has its name compiled in the
dictionary, so a later `EMPTY` unmaps it unless a `GILD` intervened.

| Word | Effect | Meaning |
|---|---|---|
| `PART` | ( n -- ) | Make part n current, setting `OFFSET` to its first block. |
| `MAPS <file>` | ( n -- ) | Set up free part n for an existing file; `MAPS" <file>"` inside definitions. Followed by `READONLY` or `MODIFY`. |
| `READONLY`, `MODIFY` | ( n -- ) | Open the file read-only or read-write with starting block n, completing the entry. |
| `CHART <file>` | ( -- ) | Map a file into the lowest free part (and show the map when interpreted). |
| `USING <file>` | ( -- n ) | Part where the file is mapped, charting it if necessary. |
| `NEWFILE <file>` | ( n -- ) | Create a file of n blocks, open it for modification, map it at the next free blocks, and print where. |
| `UNMAP`, `UNMAPS` | ( n -- ), ( n1 n2 -- ) | Flush, close and free one part or a range of parts. |
| `-LOAD` | ( n -- ) | Load block n, then unmap its file unless it is part 0. |
| `LOADUSING <file>` | ( n -- ) | Open the file read-only and load its block n. |
| `FLUSH` | ( -- ) | Write all updated buffers. |
| `-MAPPED` | ( addr n -- f ) | True if the named file is not mapped; else select its part and return false. |
| `UNMAPPED` | ( -- n ) | Lowest free part. |
| `AFTER` | ( -- n ) | First block beyond all mapped blocks. |
| `?PART` | ( n -- n1 ) | Part that maps absolute block n. |
| `.PARTS`, `.MAP` | ( -- ) | Show the map: start block, access, size and name for each part (`.MAP` also resets the scroll region). |

**§A.2 Block editor.** A resident line and string editor in the `EDITOR`
vocabulary; `T` and `L` live in `FORTH` and select `EDITOR` themselves. All
commands act on the block in the user variable `SCR`, which `LIST` or `LOCATE`
sets.

- *§A.2.1 Display.* `n LIST` shows and selects block n as sixteen numbered
  64-character lines (numbered in the current base; the numbers and the trailing
  `ok` are not part of the text). `L` redisplays it, `N` and `B` step forward
  and back, and `Q` toggles between a block and its documentation *shadow* block
  at a fixed offset. `WIPE` blanks a block. A block whose first line is all
  spaces counts as unused, so that line must never be left blank.
- *§A.2.2 Buffers.* A find buffer (last search string, used by `F`, `S`, `D`,
  `TILL`) and an insert buffer (last inserted, placed or deleted text, used by
  `I`, `R`, `X`, `U`, `P`), each at least 64 characters; `K` swaps them. A
  command followed by text uses that text and stores it in its buffer. A command
  followed by just a return reuses the buffer.
- *§A.2.3-A.2.6 Lines.* `n T` selects line n (0-15) by setting the cursor
  variable `CHR` to its start. `P text` replaces the line, and `P` followed by
  spaces blanks it. `U text` inserts a line under it. `X` deletes it into the
  insert buffer, shifting later lines up and blanking line 15, so `T X` then `T
  U` moves a line (counting the shift when moving down). `blk line M` copies a
  line from another block below the current one and leaves the arguments ready
  for the next line, aborting unless exactly two items are on the stack.
- *§A.2.7 Characters.* Edits stay within the range in the user variable
  `EXTENT`: 64 by default (one line, so text pushed past the end is lost and
  deletions blank-fill), 1024 to let edits flow through the block. `CLIP` and
  `WRAP` set these two values. Strings end at a return or a caret and are
  truncated to the buffer size; `F` and `S` search beyond the extent.


## Appendix B: Standard Forth Documentation Requirements

Standard Forth obliges a system to document its implementation-defined options,
its handling of ambiguous conditions and some other facts, word set by word set.
SwiftForth answers in one table group per word set.

**§B.1 System documentation.** Tables 15-17.

- *Implementation-defined options.* Memory has no alignment requirement, for
  cells or characters. `EMIT` passes every character through to the output
  device. `ACCEPT` edits its line: backspace deletes, Return ends input, the
  arrow keys move the cursor and insert or overwrite mode applies; `EXPECT` is
  not supported. The character set is 7-bit ASCII. While a file is interpreted,
  every control character below `BL` counts as a space except CR, which ends the
  line. The control-flow stack is the data stack, and its entries are addresses.
  Digits past `Z` are refused, and whether a lowercase digit is accepted follows
  the case-sensitivity setting. `ABORT"` performs `-2 THROW` and shows the last
  word typed with the string. Terminal input ends with Enter. A counted string
  and a parsed string hold at most 255 characters, a definition name at most
  254, and the string `ENVIRONMENT?` takes at most 254. The input and output
  devices are chosen through the personality mechanism (§5.6.1). The system is
  subroutine threaded. The address unit is 8 bits, a cell 4 bytes and a
  character 1 byte; arithmetic is two's complement, singles are 32 bits and
  doubles 64. No data region is read-only. The `WORD` buffer is at least 256
  bytes, the terminal input buffer 256 bytes, the pictured-output buffer 68
  bytes and the `PAD` scratch area 256 bytes. The two `PAD` figures conflict:
  this table gives 256 bytes, while §4.5.1 sizes `PAD` as all unused space above
  `HERE`+512 (`UNUSED 512 -`). Names are case-insensitive by default. The prompt
  is `ok` followed by CR LF. Division is symmetric. A true `STATE` is -1.
  Integer overflow wraps. After `DOES>` the definition being built cannot be
  found by name.
- *Ambiguous conditions.* An unrecognised token goes through the lookup sequence
  of §5.5.5 and then `-13 THROW`. An over-long name is truncated. Any address
  the host OS permits may be used, and a type mismatch is not checked. Taking
  the execution token of a compile-only word is allowed, and interpreting such a
  word performs its compilation semantics. Division by zero is `-10 THROW`; a
  full dictionary is `-8 THROW`. Stack overflow and loop-parameter space are not
  checked. The interpreter checks data-stack underflow (`-4 THROW`); the return
  stack is unchecked. A zero-length name at the end of input is `-16 THROW`.
  Setting `>IN` past the buffer aborts, and so does `RESTORE-INPUT` with state
  from a different source. Writing into the input buffer or a string literal,
  overflowing pictured output, `PICK` or `ROLL` on too short a stack, missing
  loop parameters, `RECURSE` after `DOES>`, and releasing data space that holds
  definitions are all ignored. Parsed strings are cut to 255 characters and
  `WORD` truncates an over-long result. Results out of range wrap. Misaligned
  access works. `IMMEDIATE` on a nameless definition does nothing. `TO` on a
  word that is not a `VALUE` is `-32 THROW`. A name that `'`, `POSTPONE` or
  `[']` cannot find is `-13 THROW`. `DO`, `?DO` and `WITHIN` accept mixed signed
  and unsigned arguments. `[COMPILE]` is not supported and `POSTPONE TO` is
  refused. A shift count is taken modulo the cell width in bits. `>BODY` and
  `DOES>` are permitted on words not made by `CREATE`, and `#`, `#S`, `HOLD` and
  `SIGN` work outside `<# #>`.
- *Other facts.* `WH PAD` lists the system words that use `PAD`. Terminal
  behaviour is described in §2.3.1. The data space defaults to 1 MB in this
  table, while §5.2 gives the default dictionary as 8 MB; the manual does not
  reconcile the two. The return stack holds 16,384 cells and the data stack
  4,096. The system dictionary occupies about 300K.

**§B.2 Block word set.** `LIST` shows 16 lines of 64 characters with line
numbers, and `\` in a block comments out the rest of its 64-character line. A
failed or erroneous block transfer and an invalid block number for `BLOCK`,
`BUFFER` or `LOAD` abort. Storing into `BLK` directly is ignored. There is
always a current buffer for `UPDATE`. Under multitasking a buffer address stays
valid only inside a critical section bracketed by `DISK GET` and `DISK RELEASE`;
the manual's cross-reference for this is broken ("see ???"). The number of
blocks is set by the user (§A.1).

**§B.3 Double-number word set.** `D>S` truncates a double outside single range.

**§B.4 Exception word set.** The `THROW` codes the system uses, and the actions
it takes, are defined in the source file `SwiftForth/src/ide/errmessages.f`
rather than tabulated.

**§B.5 Facility word set.** `EKEY` codes are as in §5.6.2 (the manual's table
says §5.5.2). A clock tick lasts from about 1 ms to 55 ms depending on the host,
and `MS` waits at least the requested time. `AT-XY` on a device that cannot
position the cursor is ignored.

**§B.6 File-access word set.**

- *Implementation-defined options.* The access methods `R/O`, `R/W` and `W/O`
  map onto the host's generic read and write access; `BIN` does nothing because
  all access is binary. File exceptions are -36 (invalid position), -37 (I/O
  failure), -38 (file does not exist) and -39 (unexpected end of file).
  `READ-LINE` ends a line at CR (0DH). File names follow the Linux and macOS
  conventions. `FILE-STATUS` returns zero when the file is available, else an
  ior from the list below. A file being included is closed automatically when
  the error that ends it was thrown. The iors are -191 delete, -192 rename, -193
  resize, -194 flush, -195 read (`READ-FILE`, `READ-LINE`), -196 write, -197
  close, -198 create and -199 open (`OPEN-FILE`, `INCLUDE-FILE`) failures;
  `FILE-POSITION`, `REPOSITION-FILE`, `SEEK-FILE` and `FILE-SIZE` always
  return 0. File input nests at most 16 deep and an input line holds at most 256
  characters. Block ranges map to files as in §A.1. `S"` has 8 string buffers of
  128 bytes each.
- *Ambiguous conditions.* Repositioning outside the file is not an error and
  leaves the file at its end. Reading positions never written returns zero bytes
  without error. An invalid fileid given to `INCLUDE-FILE` is used as it stands,
  with whatever result follows. An I/O failure while `INCLUDE-FILE` or
  `INCLUDED` reads or closes is `-37 THROW`, and a file `INCLUDED` cannot open
  is `-38 THROW`. An unmapped block number aborts. When `BLK` is non-zero the
  block source wins over `SOURCE-ID`.

**§B.7 Floating-point word set.** Numbers are IEEE 64-bit (§10.1), rounding is
as in §10.2, and the stack holds 32 items of 80 bits, possibly one stack per
task. `REPRESENT` on an out-of-range value, `F/` overflow or division by zero,
and the domain errors of `FACOSH` (below one), `FLNP1` (at or below minus one),
`FLN` and `FLOG` (at or below zero), `FASINH` and `FSQRT` (below zero) and
`FACOS`, `FASIN` and `FATANH` (magnitude above one) raise an FPU trap. `DF@`,
`DF!`, `F@`, `F!`, `SF@` and `SF!` need no alignment. A non-decimal `BASE` for
`REPRESENT`, `F.`, `FE.` and `FS.`, `FATAN2` with both arguments zero, `FTAN`
where the cosine is zero, a `D>F` that cannot be exact, an exponent too large
for `DF!`, `DF@`, `SF!` or `SF@`, and float output larger than the
pictured-numeric area are ignored. `F>D` on a value whose integer part does not
fit truncates and yields invalid data.

**§B.8 Local-variables word set.** A definition has at most 16 locals. Naming a
local while interpreting fails to find it outside a definition and compiles a
reference inside one (after `[`). `TO` on a name that is neither a `VALUE` nor a
local is `-32 THROW`.

**§B.9 Memory-allocation word set.** An ior of zero is success; `ALLOCATE` fails
with -100 and `FREE` with -102, and `RESIZE` is built from the two.

**§B.10 Programming-tools word set.** `FORGET` is not supported, so its
ambiguous conditions do not arise. `CODE` and `;CODE` assemble the following
text (§6) up to `END-CODE` (§6.2). Both `EDITOR` (the block editor of §A.2) and
`ASSEMBLER` are search-order words. `SEE` output is shown in §2.4.4 (the manual
says §2.4.3). `CS-PICK` and `CS-ROLL` on a short control-flow stack, `POSTPONE
[IF]`, and input that ends before `[ELSE]` or `[THEN]` are ignored; `;CODE` on a
word not made by `CREATE` is allowed.

**§B.11 Search-order word set.** The search order holds at most 16 word lists,
and the minimum order is `FORTH`. Changing the compilation word list and `ALSO`
beyond the limit are ignored; after `PREVIOUS` empties the order no word can be
found.

## Appendix C: Forth Words Index

An alphabetical index of the words in the manual's glossaries, each with its
stack arguments and a page number, plus the table of stack-argument notation
(Table 36: `addr`, `n`, `u`, `d`, `flag`, `c-addr`, `xt` and the like, with
numeric subscripts to tell same-typed arguments apart). It points into the
chapters above and has nothing of its own to distill.
