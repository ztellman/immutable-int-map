# Project Moved to Contrib!

ztellman/immutable-int-map is now [clojure/data.int-map](https://github.com/clojure/data.int-map)

## Immutable Integer Maps

Ths library provides an optimized, sorted map that can only have non-negative 64-bit integers as keys, based on Okasaki and Gill's ["Fast Mergeable Integer Maps"](https://www.lri.fr/~filliatr/ftp/ocaml/ds/ptset.ml).  They can be used to represent sparse vectors, or normal maps whose keys happen to all be integers.

```clj
[immutable-int-map "0.1.0"]
```

---

```clj
> (require '[immutable-int-map :as i])
nil
> (i/int-map)
{}
```

These maps support transient/persistent semantics, and also provide special `merge`, `merge-with`, `update`, and `update!` methods that provide significantly faster performance than their normal Clojure counterparts.

The fact that int-maps are mergeable means that they can be used very effectively with Clojure's [reducer](http://clojure.com/blog/2012/05/08/reducers-a-library-and-model-for-collection-processing.html) mechanism.  For instance, consider populating a data structure.  Typically, we'd use `into`:

```clj
> (into {} [[1 2] [3 4]])
{1 2, 3 4}
```

Under the covers, `into` looks something like this:

```clj
> (persistent!
    (reduce conj!
      (transient {})
      [[1 2] [3 4]]))
```

This makes use of Clojure's transients, but is still inherently sequential.  This is because merging together standard Clojure maps is an O(N) operation, so any parallel work would still result in a linear walk of the map's entries.  However, int-maps merges are typically much faster, which means we can build sub-maps on parallel threads, and then cheaply merge them together.  We can use the `fold` method in `clojure.core.reducers` to easily express this:

```clj
> (require '[clojure.core.reducers :as r])
nil
> (r/fold i/merge conj entries)
...
```

If `entries` is a data structure that `fold` can split, such as a vector or hash-map, the performance benefits of this are significant.  Consider this table, which gives the times on a four-core system for populating a map with a million entries, with all values in milliseconds:

| | unsorted entries | sorted entries |
|----|------------------|-------------|
| `(into {} ...)` | 615 | 500 |
| `(into (sorted-map) ...)` | 2140 | 1200 |
| `(into (i/int-map) ...)` | 1200 | 250 |
| `(fold i/merge conj ...)` | 375 | 65 |

As we can see, populating the int-map with keys in non-sorted order is slower (though always faster than Clojure's sorted map implementation), but using `fold` gives us performance that Clojure's standard data structures can't match.

### license

Copyright © 2014 Zachary Tellman

Distributed under the MIT License
