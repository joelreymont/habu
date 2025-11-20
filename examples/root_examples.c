/* Practical examples of root usage in Habu
 *
 * This file demonstrates correct root management patterns
 * for common programming scenarios.
 */

#include "../runtime/habu.h"
#include <stdio.h>
#include <string.h>

/* Example 1: Building a simple list
 *
 * Pattern: Loop that builds up a data structure
 */
habu_value_t build_number_list(int n) {
    HABU_ROOT(result, NIL);

    for (int i = 0; i < n; i++) {
        /* Each iteration might trigger GC */
        result = habu_cons(fixnum_to_value(i), result);
    }

    HABU_UNROOT(result);
    return result;
}

/* Example 2: Building a list of strings
 *
 * Pattern: Nested allocations in loop
 */
habu_value_t build_string_list(const char **strings, int n) {
    HABU_ROOT(result, NIL);

    for (int i = 0; i < n; i++) {
        /* String allocation can trigger GC */
        HABU_ROOT(str, habu_make_string(strings[i], strlen(strings[i])));

        /* This cons can also trigger GC, but str and result are rooted */
        result = habu_cons(str, result);

        HABU_UNROOT(str);
    }

    HABU_UNROOT(result);
    return result;
}

/* Example 3: Function with multiple local variables
 *
 * Pattern: Multiple values that need protection
 */
habu_value_t process_values(habu_value_t input) {
    /* Root the input parameter */
    habu_gc_add_root(&input);

    /* Create and root intermediate values */
    HABU_ROOT(temp1, habu_cons(input, NIL));
    HABU_ROOT(temp2, habu_cons(input, NIL));

    /* Process them - these operations can trigger GC */
    HABU_ROOT(result, habu_cons(temp1, temp2));

    /* Clean up in reverse order */
    HABU_UNROOT(result);
    HABU_UNROOT(temp2);
    HABU_UNROOT(temp1);
    habu_gc_remove_root(&input);

    return result;
}

/* Example 4: Recursive function
 *
 * Pattern: Recursive calls need careful rooting
 */
habu_value_t reverse_list(habu_value_t list, habu_value_t acc) {
    /* Root parameters since we'll allocate */
    habu_gc_add_root(&list);
    habu_gc_add_root(&acc);

    habu_value_t result;
    if (is_nil(list)) {
        result = acc;
    } else {
        /* Extract car and cdr */
        habu_value_t head = habu_car(list);
        habu_value_t tail = habu_cdr(list);

        /* Build new accumulator - can trigger GC */
        HABU_ROOT(new_acc, habu_cons(head, acc));

        /* Recursive call */
        result = reverse_list(tail, new_acc);

        HABU_UNROOT(new_acc);
    }

    habu_gc_remove_root(&acc);
    habu_gc_remove_root(&list);

    return result;
}

/* Example 5: Creating complex data structures
 *
 * Pattern: Building nested structures
 */
habu_value_t create_person(const char *name, int age) {
    /* Create components with rooting */
    HABU_ROOT(name_str, habu_make_string(name, strlen(name)));
    HABU_ROOT(name_sym, habu_make_symbol("name"));
    HABU_ROOT(age_sym, habu_make_symbol("age"));

    habu_value_t age_val = fixnum_to_value(age);

    /* Build property list: (name "..." age 42) */
    HABU_ROOT(result, NIL);
    result = habu_cons(age_val, result);
    result = habu_cons(age_sym, result);
    result = habu_cons(name_str, result);
    result = habu_cons(name_sym, result);

    HABU_UNROOT(result);
    HABU_UNROOT(age_sym);
    HABU_UNROOT(name_sym);
    HABU_UNROOT(name_str);

    return result;
}

/* Example 6: Vector operations
 *
 * Pattern: Filling a vector with heap objects
 */
habu_value_t create_vector_of_lists(int n) {
    HABU_ROOT(vec, habu_make_vector(n));

    for (int i = 0; i < n; i++) {
        /* Each list is a heap object that needs rooting */
        HABU_ROOT(list, NIL);
        for (int j = 0; j < 5; j++) {
            list = habu_cons(fixnum_to_value(j), list);
        }

        /* Store in vector */
        habu_vector_set(vec, i, list);

        HABU_UNROOT(list);
    }

    HABU_UNROOT(vec);
    return vec;
}

/* Example 7: Exception-safe rooting
 *
 * Pattern: Always unroot even on error paths
 */
habu_value_t safe_operation(habu_value_t input) {
    if (get_tag(input) != TAG_CONS) {
        return NIL;  /* Early return - no roots to clean */
    }

    habu_gc_add_root(&input);

    HABU_ROOT(result, NIL);

    /* Do processing */
    habu_value_t head = habu_car(input);
    habu_value_t tail = habu_cdr(input);

    if (is_nil(tail)) {
        /* Error case - but must unroot! */
        HABU_UNROOT(result);
        habu_gc_remove_root(&input);
        return NIL;
    }

    result = habu_cons(head, tail);

    /* Normal exit - unroot */
    HABU_UNROOT(result);
    habu_gc_remove_root(&input);

    return result;
}

/* Example 8: Working with closures
 *
 * Pattern: Creating and using closures
 */
habu_value_t create_counter_closure(int start) {
    /* Create environment with initial count */
    HABU_ROOT(env, habu_cons(fixnum_to_value(start), NIL));

    /* Create closure (code pointer would be real function in practice) */
    HABU_ROOT(closure, habu_make_closure(NULL, env));

    HABU_UNROOT(closure);
    HABU_UNROOT(env);

    return closure;
}

/* Example 9: Efficient loop with reused root
 *
 * Pattern: Root once, update many times
 */
habu_value_t sum_list(habu_value_t list) {
    habu_gc_add_root(&list);

    int64_t sum = 0;

    /* Reuse same root location */
    HABU_ROOT(current, list);

    while (!is_nil(current)) {
        if (get_tag(current) == TAG_CONS) {
            habu_value_t val = habu_car(current);
            if (is_fixnum(val)) {
                sum += value_to_fixnum(val);
            }
            current = habu_cdr(current);  /* Root location updated */
        } else {
            break;
        }
    }

    HABU_UNROOT(current);
    habu_gc_remove_root(&list);

    return fixnum_to_value(sum);
}

/* Example 10: WRONG - Common mistakes
 *
 * This shows what NOT to do
 */
#if 0  /* Don't compile this broken code */

habu_value_t WRONG_unrooted_loop(int n) {
    habu_value_t result = NIL;  /* NOT ROOTED! */

    for (int i = 0; i < n; i++) {
        result = habu_cons(fixnum_to_value(i), result);
        /* BUG: If GC runs here, result becomes dangling pointer */
    }

    return result;  /* Might return garbage! */
}

habu_value_t WRONG_forgot_to_unroot(void) {
    HABU_ROOT(obj, habu_cons(fixnum_to_value(42), NIL));
    return obj;  /* BUG: obj still in root set! Memory leak! */
}

habu_value_t WRONG_rooted_value_not_address(void) {
    habu_value_t obj = habu_cons(fixnum_to_value(42), NIL);
    habu_gc_add_root(obj);  /* BUG: Should be &obj */
    habu_gc_remove_root(obj);  /* BUG: Should be &obj */
    return obj;
}

#endif

/* Main demonstration */
int main(void) {
    printf("=== Habu Root Usage Examples ===\n\n");

    /* Initialize with 1MB heap */
    habu_init(1024 * 1024);

    printf("Example 1: Building number list\n");
    HABU_ROOT(numbers, build_number_list(10));
    printf("  Created list with 10 elements\n");
    HABU_UNROOT(numbers);

    printf("\nExample 2: Building string list\n");
    const char *words[] = {"hello", "world", "from", "habu"};
    HABU_ROOT(strings, build_string_list(words, 4));
    printf("  Created list with 4 strings\n");
    HABU_UNROOT(strings);

    printf("\nExample 3: Processing values\n");
    HABU_ROOT(input, habu_cons(fixnum_to_value(42), NIL));
    HABU_ROOT(processed, process_values(input));
    printf("  Processed value successfully\n");
    HABU_UNROOT(processed);
    HABU_UNROOT(input);

    printf("\nExample 4: Reversing list\n");
    HABU_ROOT(list, build_number_list(5));
    HABU_ROOT(reversed, reverse_list(list, NIL));
    printf("  Reversed list successfully\n");
    HABU_UNROOT(reversed);
    HABU_UNROOT(list);

    printf("\nExample 5: Creating person record\n");
    HABU_ROOT(person, create_person("Alice", 30));
    printf("  Created person record\n");
    HABU_UNROOT(person);

    printf("\nExample 6: Vector of lists\n");
    HABU_ROOT(vec, create_vector_of_lists(5));
    printf("  Created vector with 5 lists\n");
    HABU_UNROOT(vec);

    printf("\nExample 7: Safe operation\n");
    HABU_ROOT(safe_input, habu_cons(fixnum_to_value(1),
                                     habu_cons(fixnum_to_value(2), NIL)));
    HABU_ROOT(safe_result, safe_operation(safe_input));
    printf("  Safe operation completed\n");
    HABU_UNROOT(safe_result);
    HABU_UNROOT(safe_input);

    printf("\nExample 8: Creating closure\n");
    HABU_ROOT(closure, create_counter_closure(0));
    printf("  Created closure successfully\n");
    HABU_UNROOT(closure);

    printf("\nExample 9: Summing list\n");
    HABU_ROOT(sum_input, build_number_list(10));
    habu_value_t sum = sum_list(sum_input);
    printf("  Sum: %lld\n", value_to_fixnum(sum));
    HABU_UNROOT(sum_input);

    printf("\n=== All examples completed successfully ===\n");

    /* Get GC stats */
    habu_gc_stats_t stats;
    habu_gc_get_stats(&stats);
    printf("\nGC Statistics:\n");
    printf("  Young collections: %llu\n", stats.young_collections);
    printf("  Old collections: %llu\n", stats.old_collections);
    printf("  Total allocated: %llu bytes\n", stats.total_allocated);

    habu_shutdown();
    return 0;
}
