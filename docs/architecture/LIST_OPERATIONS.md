# List Operations in Habu

## Overview

Habu provides a comprehensive set of list manipulation functions for working with linked lists (cons cells).

## Core Operations (Already Implemented)

### cons
Create a new cons cell.
```lisp
(cons 1 2)        ; => (1 . 2)
(cons 1 '(2 3))   ; => (1 2 3)
```

### car, cdr
Access head and tail of list.
```lisp
(car '(1 2 3))    ; => 1
(cdr '(1 2 3))    ; => (2 3)
```

### list
Create a list from elements.
```lisp
(list 1 2 3)      ; => (1 2 3)
```

### length
Get number of elements.
```lisp
(length '(1 2 3)) ; => 3
```

### nth
Get element at index (0-based).
```lisp
(nth 0 '(a b c))  ; => a
(nth 2 '(a b c))  ; => c
```

### append
Concatenate lists.
```lisp
(append '(1 2) '(3 4))  ; => (1 2 3 4)
```

### reverse
Reverse a list.
```lisp
(reverse '(1 2 3))  ; => (3 2 1)
```

---

## Extended Operations (This Session)

### last
Get the last cons cell (or last N elements).

```lisp
(last '(1 2 3))      ; => (3)
(last '(1 2 3 4) 2)  ; => (3 4)
```

**Arguments:**
- `list`: List to process
- `n` (optional): Number of elements to return (default: 1)

**Returns:** Last N elements as a list

### butlast
Get all but the last N elements.

```lisp
(butlast '(1 2 3))     ; => (1 2)
(butlast '(1 2 3 4) 2) ; => (1 2)
```

**Arguments:**
- `list`: List to process
- `n` (optional): Number of elements to exclude (default: 1)

**Returns:** New list without last N elements

### nthcdr
Skip N elements, return rest of list.

```lisp
(nthcdr 0 '(a b c))  ; => (a b c)
(nthcdr 2 '(a b c))  ; => (c)
(nthcdr 5 '(a b c))  ; => nil
```

**Arguments:**
- `n`: Number of elements to skip
- `list`: List to process

**Returns:** List starting at position N

### member
Test if element is in list.

```lisp
(member 2 '(1 2 3))   ; => (2 3)  [rest of list from match]
(member 5 '(1 2 3))   ; => nil
```

**Arguments:**
- `item`: Element to find
- `list`: List to search

**Returns:** Tail of list starting from first match, or nil

**Equality:** Uses tagged pointer equality (works for fixnums, symbols)

### assoc
Find key in association list.

```lisp
(assoc 'b '((a . 1) (b . 2) (c . 3)))  ; => (b . 2)
(assoc 'x '((a . 1) (b . 2)))          ; => nil
```

**Arguments:**
- `key`: Key to find
- `alist`: Association list of (key . value) pairs

**Returns:** First matching pair (key . value), or nil

### position
Find index of element in list.

```lisp
(position 'b '(a b c))  ; => 1
(position 'x '(a b c))  ; => nil
```

**Arguments:**
- `item`: Element to find
- `list`: List to search

**Returns:** 0-based index of first occurrence, or nil

### count
Count occurrences of element.

```lisp
(count 2 '(1 2 3 2 4))  ; => 2
(count 5 '(1 2 3))      ; => 0
```

**Arguments:**
- `item`: Element to count
- `list`: List to search

**Returns:** Number of occurrences (fixnum)

### remove
Remove all occurrences of element.

```lisp
(remove 2 '(1 2 3 2 4))  ; => (1 3 4)
(remove 5 '(1 2 3))      ; => (1 2 3)
```

**Arguments:**
- `item`: Element to remove
- `list`: List to process

**Returns:** New list without matching elements

---

## Implementation Notes

### Phase 1 Limitations

**No Runtime Funcall for Predicates:**
- Operations like `remove-if`, `find-if`, `every`, `some` require runtime function calls
- These will be added in future sessions when runtime funcall is enhanced
- Current focus: Operations using equality testing

**Equality Testing:**
- Uses tagged pointer equality
- Works correctly for:
  - Fixnums (small integers)
  - Symbols (interned)
  - Nil
- String equality requires special handling (future enhancement)

### Performance

All operations are O(n) where n is list length:
- `last`: Single traversal to find end
- `butlast`: Traverse and copy all but last
- `nthcdr`: Skip n elements
- `member`, `position`, `count`, `remove`: Linear search
- `assoc`: Linear search through alist

### Memory Allocation

Operations that create new lists:
- `butlast`: Allocates new cons cells
- `remove`: Allocates new list without matches
- `last`, `nthcdr`: Return pointers to existing structure (no allocation)
- `member`: Returns tail (no allocation)
- `assoc`: Returns existing pair (no allocation)

---

## Common Patterns

### Building Association Lists

```lisp
(let ((alist '((name . "Alice")
               (age . 30)
               (city . "Boston"))))
  (assoc 'name alist))  ; => (name . "Alice")
```

### List Membership Testing

```lisp
(defun contains? (item list)
  (/= (member item list) 0))  ; Check if member returns non-nil
```

### Finding by Position

```lisp
(defun nth-safe (n list)
  (if (< n (length list))
      (nth n list)
      nil))
```

### Removing Duplicates (Simple)

```lisp
(defun remove-first (item list)
  "Remove only first occurrence"
  (let ((pos (position item list)))
    (if pos
        (append (nthcdr 0 list pos)
                (nthcdr (+ pos 1) list))
        list)))
```

### Getting Multiple Tail Elements

```lisp
(last '(1 2 3 4 5) 3)  ; => (3 4 5)
```

---

## Future Enhancements (Phase 2)

When runtime funcall is available:

### Higher-Order Functions

```lisp
;; These will be added later
(remove-if #'oddp '(1 2 3 4))     ; => (2 4)
(find-if #'evenp '(1 3 5 6))      ; => 6
(every #'oddp '(1 3 5))           ; => t
(some #'evenp '(1 3 5 6))         ; => t
(mapcar #'1+ '(1 2 3))            ; => (2 3 4)
```

### Custom Equality

```lisp
(member "hello" list :test #'string-equal)
(assoc key alist :test #'custom-equal?)
```

---

## Examples

### Working with Association Lists

```lisp
(defun get-property (key plist)
  "Get property from plist"
  (let ((pair (assoc key plist)))
    (if pair
        (cdr pair)
        nil)))

(defun update-property (key value plist)
  "Update or add property"
  (cons (cons key value)
        (remove-key key plist)))
```

### List Utilities

```lisp
(defun split-at (n list)
  "Split list at position n"
  (cons (butlast list (- (length list) n))
        (nthcdr n list)))

(defun take (n list)
  "Take first n elements"
  (butlast list (- (length list) n)))

(defun drop (n list)
  "Drop first n elements"
  (nthcdr n list))
```

### Searching

```lisp
(defun find-index-all (item list)
  "Find all indices of item"
  (let ((result nil)
        (idx 0))
    (dolist (x list)
      (when (= x item)
        (push idx result))
      (setq idx (+ idx 1)))
    (reverse result)))
```
