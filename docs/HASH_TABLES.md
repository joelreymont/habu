# Hash Tables in Habu

## Overview

Hash tables provide efficient key-value storage with O(1) average-case lookup, insertion, and deletion.

## Basic Usage

```lisp
;; Create a hash table
(let ((ht (make-hash-table)))
  ;; Add entries
  (puthash 'key1 42 ht)
  (puthash 'key2 100 ht)

  ;; Lookup
  (gethash 'key1 ht)  ; => 42
  (gethash 'key3 ht)  ; => 0 (nil)

  ;; Remove
  (remhash 'key1 ht)

  ;; Count entries
  (hash-table-count ht))  ; => 1
```

## API Reference

### make-hash-table

Create a new hash table with optional initial capacity.

```lisp
(make-hash-table)          ; Default capacity (16 buckets)
(make-hash-table 32)       ; Custom capacity
```

**Arguments:**
- `capacity` (optional): Initial number of buckets (default: 16)

**Returns:** New hash table object

### gethash

Look up a value by key.

```lisp
(gethash key hash-table)
```

**Arguments:**
- `key`: Key to look up (symbol, fixnum, or string)
- `hash-table`: Hash table to search

**Returns:** Associated value, or 0 (nil) if not found

### puthash

Insert or update a key-value pair.

```lisp
(puthash key value hash-table)
```

**Arguments:**
- `key`: Key to insert/update
- `value`: Value to associate with key
- `hash-table`: Hash table to modify

**Returns:** The value

### remhash

Remove a key-value pair.

```lisp
(remhash key hash-table)
```

**Arguments:**
- `key`: Key to remove
- `hash-table`: Hash table to modify

**Returns:** Non-zero if key was found and removed, 0 otherwise

### hash-table-count

Get the number of entries in the hash table.

```lisp
(hash-table-count hash-table)
```

**Arguments:**
- `hash-table`: Hash table to query

**Returns:** Number of key-value pairs (as fixnum)

## Implementation Details

### Phase 1: Bootstrap Implementation

**Representation:**
- Tag: 0x6
- Heap layout:
  ```
  [Header: 8 bytes]     - Type tag and metadata
  [Capacity: 8 bytes]   - Number of buckets
  [Count: 8 bytes]      - Number of entries
  [Buckets: N*8 bytes]  - Array of pointers to association lists
  ```

**Hash Function:**
- Symbols: `sxhash` of symbol name
- Fixnums: Value directly (already small)
- Strings: `sxhash` of string content
- Bucket index: `hash mod capacity`

**Collision Resolution:**
- Chaining: Each bucket is an association list `((key1 . value1) (key2 . value2) ...)`
- Simple and effective for moderate load factors

**No Automatic Resizing:**
- Phase 1 uses fixed-size tables
- User can specify initial capacity
- Phase 2 will add dynamic resizing

### Supported Key Types

**Phase 1:**
- ✅ Symbols
- ✅ Fixnums (small integers)
- ✅ Strings (ASCII)

**Phase 2:**
- All Habu values
- Custom hash functions
- Equality predicates (eq, equal, etc.)

## Common Patterns

### Building a Hash Table

```lisp
(let ((ht (make-hash-table)))
  (puthash 'name "Alice" ht)
  (puthash 'age 30 ht)
  (puthash 'city "Boston" ht)
  ht)
```

### Counting Occurrences

```lisp
(defun count-elements (list)
  (let ((counts (make-hash-table)))
    (dolist (x list)
      (let ((current (gethash x counts)))
        (puthash x (+ current 1) counts)))
    counts))
```

### Checking Membership

```lisp
(defun member? (key ht)
  (/= (gethash key ht) 0))  ; Assumes 0 means not found
```

### Iterating Over Entries

```lisp
;; Phase 1: No built-in iteration
;; Access buckets directly (internal representation)

;; Phase 2: Will add hash-table-map, hash-table-keys, hash-table-values
```

## Limitations (Phase 1)

1. **No automatic resizing**: Fixed capacity
2. **No custom hash functions**: Uses built-in sxhash
3. **No custom equality**: Uses default equality for key types
4. **No iteration primitives**: Must access internal structure
5. **Simple collision handling**: Linear search in buckets

## Phase 2 Enhancements

Future improvements:
- Dynamic resizing (load factor threshold)
- Weak keys/values for GC
- Custom hash and equality functions
- Iteration: hash-table-map, hash-table-keys, hash-table-values
- Better hash distribution
- Open addressing option
- Thread-safe operations

## Performance

**Expected Complexity:**
- Lookup: O(1) average, O(n) worst case
- Insert: O(1) average, O(n) worst case
- Delete: O(1) average, O(n) worst case
- Space: O(n) where n is number of entries

**Tips for Best Performance:**
- Choose initial capacity ≥ expected entries
- Keep load factor (entries/capacity) < 1.0
- Use fixnum keys when possible (fastest hash)

## Examples

### Caching Function Results

```lisp
(let ((cache (make-hash-table)))
  (defun expensive-compute (n)
    (let ((cached (gethash n cache)))
      (if (/= cached 0)
          cached
          (let ((result (* n n)))  ; Expensive computation
            (puthash n result cache)
            result)))))
```

### Symbol Properties

```lisp
(let ((props (make-hash-table)))
  (puthash 'foo '((type . integer) (doc . "A foo variable")) props)
  (gethash 'foo props))
```

### Reverse Lookup Table

```lisp
(defun build-reverse-map (alist)
  (let ((rev (make-hash-table)))
    (dolist (pair alist)
      (puthash (cdr pair) (car pair) rev))
    rev))
```
