Ths library provides an optimized map that can only have positive integers as keys, based on Okasaki and Gill's ["Fast Mergeable Integer Maps"](https://www.lri.fr/~filliatr/ftp/ocaml/ds/ptset.ml).

```clj
> (require '[immutable-int-map :as i])
nil
> (i/int-map)
{}
```

### license

Copyright © 2014 Zachary Tellman

Distributed under the MIT License
