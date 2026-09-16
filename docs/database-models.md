# Database models in polyFORTH and VFX Forth

How a Forth-native database kit looks, and how a hosted Forth reaches SQL,
restated in our words so Habu's database package can start from a proven
vocabulary. See [tasking-models.md](tasking-models.md) for the sources and
their licences.

Sources, local copies as of 2026-09-16:

- polyFORTH Reference Manual, GreenArrays DB005: chapter 8 "Data Base
  Support" (pp. 201-270), sections 8.1-8.7; 8.8 is the report generator and
  8.9 has four worked designs.
- VFX Forth 64 for Linux: `Lib/SharedLibs/SQLite3/sqlite3h.fth` (6664 lines),
  manual chapter 24 "Supported shared libraries".
- Habu: `lib/fs.f`, `lib/json-read.f`, `lib/json-write.f`, `lib/xml.f`,
  `lib/zip.f`, `lib/hashmap.f`, `lib/ffi-abi.f`.

## 1. polyFORTH Data Base Support: the kit

Premises, in order of importance:

1. A file is a contiguous range of blocks with a fixed record length and a
   maximum record count, declared in a small table in the dictionary. Nothing
   is "opened": naming a file stores its table address in the user variable
   `F#`; naming a record number stores it in `R#`. "Files are pointed to, not
   opened; records are pointed to, not read."
2. Records and fields are fixed length. Variable amounts of data are chains
   of fixed records, not variable fields.
3. Fields are dictionary words holding an offset; a record description is a
   mask that any file may use. Field words return the address of the field
   inside the task's working-storage image; the access operators translate to
   the block buffer, so data is read from the buffer cache and exactly one
   copy of a record exists. Sharing between tasks therefore needs no locking
   for plain access.
4. Each task has a working-storage image of one record: the place to build a
   search key or a record before insertion.

| Word | Effect | Meaning |
| --- | --- | --- |
| `FILE` | `( length limit blocks origin -- )` name | define the file table; executing the name makes it current |
| `ORG`, `LIM`, `B/B`, `B/R` | `( -- a )` | fields of the current file table |
| `INITIALIZE` | `( -- )` | zero the file; write the index stopper |
| `#R`, `#B` | | records per blocks and blocks per records, for layout |
| `READ` | `( n -- )` | make record `n` current after a range check |
| `SLOT` | `( -- n )` | allocate the next free record after `AVAILABLE`, wrapping; mark it used |
| `SCRATCH` | `( n -- )` | free record `n` (zero its first cell) |
| `AVAILABLE` | `( -- a )` | record 0 holds the last allocated record number |
| `RECORDS`, `WHOLE` | `( -- hi lo )` | loop bounds over allocated or all records |
| `SAVE`, `RESTORE` | `( -- )` | push and pop `F#` and `R#` on the return stack |
| `1BYTE`, `NUMERIC`, `DOUBLE`, `n BYTES`, `n FILLER` | offset-threading defining words | a record description is `0 ... DROP` |
| `1@ 1! 1?`, `N@ N! N?`, `D@ D! D?`, `B@ B! B?` | | fetch, store, display per field type; strings travel through `PAD` |
| `PUT`, `ASK` | `( n a -- )` | store the rest of the input line, or prompt, into a `BYTES` field |
| `ENTIRE` | `( -- n a )` | the whole record as one `BYTES` field |
| `ADDRESS` | `( a -- a' )` | the field's real buffer address, for `MOVE`; then `UPDATE`; never held across I/O or `PAUSE` |
| `S@`, `S!`, `WORKING` | | the working-storage image |

Ordered indexes: a dense file of key plus link, kept sorted by moving records
on insert and delete, searched by binary search. The link is the first cell
of every record, predefined as `LINK`.

| Word | Effect | Meaning |
| --- | --- | --- |
| `BINARY` | `( n a -- rec )` | find the key held in working storage; return the linked main record; abort if absent |
| `-BINARY` | `( n a -- t )` | true if absent; leaves `R#` at the insertion point; takes the `ORDERED` facility |
| `+ORDERED` | `( -- )` | insert the working-storage record before `R#`; releases `ORDERED` |
| `-ORDERED` | `( -- )` | delete `R#` and close the gap |
| `ORDERED` | facility variable | held from search to insertion so record numbers do not move under another task |

Guidance from the manual: keep index records small and key-only; split an
index of tens of thousands of keys into sub-indexes selected by a cheap rule
(a hierarchy); never store an ordered index's record numbers elsewhere,
because they move.

Chaining: linkage through `LINK` inside one file or to an auxiliary file, for
a variable number of dependents. Decide up front whether zero dependents is
allowed, and whether the chain is first-in-first-out, last-in-first-out or
keyed. Every chained record carries a pointer back to its owner so chains can
be rebuilt after damage.

| Word | Effect | Meaning |
| --- | --- | --- |
| `HEAD` | `( -- a )` | user variable: first record of the current chain |
| `FIRST` | `( -- )` or `( -- t )` | read the head record; variant reports an empty chain |
| `-NEXT` | `( -- t )` | read the next record; true at the end |
| `-LOCATE` | `( n -- t )` | read the nth record of the chain |
| `CHAIN`, `UNCHAIN` | `( n -- )` | insert or remove at position n; -1 means the end |
| `SNATCH` | `( a r -- r' )` | swap a record number into a link field, returning the old one |

## 2. VFX Forth: SQL through the FFI

VFX has no native database. Its SQLite support is `sqlite3.h` translated
declaration by declaration into `extern:` lines, with the C structs
declared as Forth structs and nothing on top: the program calls
`sqlite3_open`, `sqlite3_prepare_v2`, `sqlite3_step` and `sqlite3_column_*`
exactly as C would. Two lessons: for hosted programs the FFI is the database
strategy, and a tool that generates a binding from a C header is worth
having, because the binding is mechanical and large.

## 3. Habu today

Files, JSON, XML, zip and hash maps exist. There are no records, fields,
indexes or SQL. The FFI resolves `dlopen` and `dlsym` on Linux aarch64.

## 4. What to adopt

The decision is open: `habu-decide-the-db-4bb703ee`. The candidates the
decision must weigh:

1. A typed record kit in the polyFORTH shape over a Habu file or a flash
   region: `FILE`, `READ`, `SLOT`, `SCRATCH`, field defining words that
   return nominal field handles instead of bare addresses (so the checker
   ties `N@` to a numeric field and `B@` to a byte field), ordered index and
   chain words, the `ORDERED` facility around index maintenance. This is what
   a microcontroller can run, and it is small.
2. SQLite through the FFI first, Postgres later, for hosted programs, with a
   binding generator from C headers so the binding stays mechanical.
3. Both, with one field vocabulary on top so application code reads the same
   against either store.
