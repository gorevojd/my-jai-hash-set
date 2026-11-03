/*
    A key-only hash set with the default hashing and comparison conventions of Hash_Table.
    Import with #import "MyBasic/MyHashSet"; a default-initialized HashSet is ready to use.
    Keys are copied by value; referenced string bytes and other pointed-to data remain caller-owned.
    Equal keys must have equal hashes, and key identity must remain unchanged while stored.
    Custom comparison must be an equivalence relation (raw NaNs are unsuitable keys).
    Do not copy a populated set as an independent owner or mutate its storage directly.
    There is no synchronization. Mutations invalidate iteration; only the loop's remove is supported.

    Example inside a procedure:
        keys: HashSet(string);
        defer deinit(*keys);
        add(*keys, "player");  // true
        add(*keys, "player");  // false; count stays 1
        for keys {
            print("%\n", it);
        }
*/
HashSet :: struct (Key_Type: Type,
                   given_hash_function: (Key_Type) -> u32 = null,
                   given_compare_function: (Key_Type, Key_Type) -> bool = null,
                   LOAD_FACTOR_PERCENT: u32 = 70)
{
    #assert LOAD_FACTOR_PERCENT > 0 && LOAD_FACTOR_PERCENT < 100;

    count: int;
    allocated: int;
    slots_filled: int;
    allocator: Allocator;

    Entry :: struct {
        hash: u32;
        key: Key_Type;
    }

    entries: []Entry;

    #if given_hash_function {
        hash_function :: given_hash_function;
    }
    else {
        hash_function :: (key: Key_Type) -> u32 {
            // Standard float hashing uses bits, but +0.0 and -0.0 compare equal.
            #if Key_Type == float32 || Key_Type == float64 {
                if key == 0.0 {
                    return get_hash(cast(Key_Type) 0.0);
                }
            }
            return get_hash(key);
        }
    }

    #if given_compare_function {
        compare_function :: given_compare_function;
    }
    else {
        compare_function :: (a: Key_Type, b: Key_Type) -> bool {
            return a == b;
        }
    }

    SIZE_MIN :: 32;
    MAX_SLOTS :: #run min(0x7fffffffffffffff / size_of(Entry), 0x7fffffffffffffff / 100);
}

/**
 * @brief Inserts a key only if no equal key is already present.
 * @param[in,out] set Set to modify; allocates lazily with the remembered allocator.
 * @param[in] key Key copied into the set on successful insertion.
 * @return True for a new key, false for an existing key. Existing keys are never replaced.
 */
add :: (set: *HashSet, key: set.Key_Type) -> bool {
    hash := hash_key(set.*, key);
    index, found := locate(set.*, key, hash);
    if found {
        return false;
    }

    // Probe past removed slots before reusing one, so an existing key cannot be duplicated.
    if index < 0 || set.entries[index].hash != REMOVED_HASH {
        old_entries := set.entries.data;
        ensure_space(set, 1);
        if set.entries.data != old_entries {
            index, found = locate(set.*, key, hash);
        }
    }

    entry := *set.entries[index];
    if entry.hash == EMPTY_HASH {
        set.slots_filled += 1;
    }
    entry.key = key;
    entry.hash = hash;
    set.count += 1;
    return true;
}

/** @brief Returns whether an equal key is present, without allocating. */
contains :: (set: HashSet, key: set.Key_Type) -> bool {
    index, found := locate(set, key, hash_key(set, key));
    return found;
}

/**
 * @brief Looks up the stored representative of an equal key without exposing a mutable key pointer.
 * @param[in] set Set to search.
 * @param[in] key Lookup key.
 * @return Found flag and stored key by value; ignore the key when the flag is false.
 */
find :: (set: HashSet, key: set.Key_Type) -> bool, set.Key_Type {
    index, found := locate(set, key, hash_key(set, key));
    if found {
        return true, set.entries[index].key;
    }
    missing: set.Key_Type;
    return false, missing;
}

/** @brief Removes an equal key and returns whether it was present. Does not free referenced key data. */
erase :: (set: *HashSet, key: set.Key_Type) -> bool {
    index, found := locate(set.*, key, hash_key(set.*, key));
    if !found {
        return false;
    }
    remove_at(set, index);
    return true;
}

/**
 * @brief Resizes for at least expected_count keys, preserving existing keys and removing tombstones.
 * @param[in,out] set Set to resize. Allocation failure is an assertion failure.
 * @param[in] expected_count Nonnegative total key count; clamped to the current count.
 * @note Capacity is rounded up to a power of two with at least one empty slot and the configured load limit.
 */
resize :: (set: *HashSet, expected_count: s64) {
    assert(expected_count >= 0, "HashSet expected count must be nonnegative.");
    expected_count = max(expected_count, set.count);
    capacity := set.SIZE_MIN;
    while expected_count > (capacity * set.LOAD_FACTOR_PERCENT - 1) / 100 {
        assert(capacity <= set.MAX_SLOTS / 2, "HashSet capacity exceeds addressable storage.");
        capacity *= 2;
    }
    rehash(set, capacity);
}

/**
 * @brief Reserves room for additional distinct keys without reallocating during those insertions.
 * @param[in,out] set Set to reserve; may compact tombstones or grow storage.
 * @param[in] items Nonnegative number of additional keys.
 */
ensure_space :: (set: *HashSet, items: s64) {
    assert(items >= 0 && items <= set.MAX_SLOTS - set.count, "Invalid HashSet reserve count.");
    if items == 0 {
        return;
    }
    if (set.allocated > 0 &&
        items <= (set.allocated * set.LOAD_FACTOR_PERCENT - 1) / 100 - set.slots_filled) {
        return;
    }
    resize(set, set.count + items);
}

/** @brief Removes every key while retaining allocated storage and the allocator. */
reset_keeping_memory :: (set: *HashSet) {
    if set.entries.data != null {
        memset(set.entries.data, 0, set.allocated * size_of(set.Entry));
    }
    set.count = 0;
    set.slots_filled = 0;
}

/** @brief Frees storage and returns the set to an empty, reusable state, preserving its allocator. */
reset :: (set: *HashSet) {
    if set.entries.data != null {
        free(set.entries.data,, set.allocator);
    }
    allocator := set.allocator;
    set.* = {};
    set.allocator = allocator;
}

/** @brief Releases set storage. Safe to repeat; referenced key data remains caller-owned. */
deinit :: (set: *HashSet) {
    reset(set);
}

/**
 * @brief Iterates keys by value in unspecified order; it_index is the storage slot index.
 * @note Supports break, continue and remove. Pointer and reverse iteration are unsupported.
 * @note Key copies can still reference shared data; never mutate data used for hash or comparison.
 */
for_expansion :: (set: *$T/HashSet, body: Code, flags: For_Flags) #expand {
    #assert !(flags & .POINTER) "HashSet keys cannot be iterated by mutable pointer.";
    #assert !(flags & .REVERSE) "HashSet has no reverse iteration order.";
    for entry, index: set.entries {
        if entry.hash < FIRST_VALID_HASH {
            continue;
        }
        `it := entry.key;
        `it_index := index;
        #insert (remove={
            remove_at(set, index);
        }) body;
    }
}

#scope_module
#import "Basic";
#import "Hash";

EMPTY_HASH :: 0;
REMOVED_HASH :: 1;
FIRST_VALID_HASH :: 2;

hash_key :: (set: HashSet, key: set.Key_Type) -> u32 {
    hash := set.hash_function(key);
    if hash < FIRST_VALID_HASH {
        hash += FIRST_VALID_HASH;
    }
    return hash;
}

locate :: (set: HashSet, key: set.Key_Type, hash: u32) -> int, bool {
    if set.allocated == 0 {
        return -1, false;
    }
    mask := set.allocated - 1;
    index := cast(s64) hash & mask;
    increment: s64 = 1;
    first_removed: s64 = -1;
    while set.entries[index].hash != EMPTY_HASH {
        entry := set.entries[index];
        if entry.hash == REMOVED_HASH {
            if first_removed < 0 {
                first_removed = index;
            }
        }
        else if entry.hash == hash && set.compare_function(entry.key, key) {
            return index, true;
        }
        index = (index + increment) & mask;
        increment += 1;
    }
    if first_removed >= 0 {
        return first_removed, false;
    }
    return index, false;
}

remove_at :: (set: *HashSet, index: int) {
    set.entries[index] = {};
    set.entries[index].hash = REMOVED_HASH;
    set.count -= 1;
}

rehash :: (set: *HashSet, capacity: s64) {
    if set.allocator.proc == null {
        remember_allocators(set);
    }
    bytes := capacity * size_of(set.Entry);
    entries := cast(*set.Entry) alloc(bytes,, set.allocator);
    assert(entries != null, "Could not allocate HashSet storage.");
    memset(entries, 0, bytes);

    old_entries := set.entries;
    set.entries.data = entries;
    set.entries.count = capacity;
    set.allocated = capacity;
    set.slots_filled = set.count;
    for entry: old_entries {
        if entry.hash < FIRST_VALID_HASH {
            continue;
        }
        // Already unique: rebuilding needs only an empty slot, never key comparisons.
        mask := capacity - 1;
        index := cast(s64) entry.hash & mask;
        increment: s64 = 1;
        while set.entries[index].hash != EMPTY_HASH {
            index = (index + increment) & mask;
            increment += 1;
        }
        set.entries[index] = entry;
    }
    if old_entries.data != null {
        free(old_entries.data,, set.allocator);
    }
}
