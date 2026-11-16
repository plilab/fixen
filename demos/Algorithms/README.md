# Examples of algorithms implementation in Fixen

This folder presents different algorithms which can be implemented using [Fixen](https://fixen-lang.org/).

Two lattices are shared by all files:

- `Dist`: a `Natural` where smaller is better (join is `min`).
- `DistRev`: a `Natural` where larger is better (join is `max`).

## `fix/ShortestPath.fix`

### Breadth-first search

- **Task:** shortest distance (number of edges) from a source vertex to every reachable vertex.
- **Relations:** `Edge: Vertex, Vertex` is a directed edge; `DistToBFS: Vertex, Dist` is the distance from the source.
- **Rules:**
    ```
    rule addDistBFS: DistToBFS a d, Edge a b |- DistToBFS b (d + 1)
    priority: d1 > d2 |- addDistBFS { d = d1 } < addDistBFS { d = d2 }
    ```
    The priority expands vertices in order of increasing distance, as a BFS queue does.
- **Initial values:** `DistToBFS s 0` for the source vertex `s`, and `Edge a b` for every edge of the graph.

The same file also has a multi-source variant. Every vertex `v` given as `Dummy v False` starts at distance 0. A start fires only when no `addDistBFS` is pending, so the search from one start finishes before the next start begins:

```
rel Dummy: Vertex, Bool
rule enableBFS: Dummy a False |- DistToBFS a 0
priority: |- enableBFS {} < addDistBFS {}
```

Its initial values are `Dummy v False` for every start vertex `v`, and `Edge a b` for every edge of the graph.

### Depth-first search

- **Task:** depth of every reachable vertex from a source in a graph without cycles. The stored value is the largest depth, so on a cycle it would grow forever.
- **Relations:** `Edge: Vertex, Vertex`; `DistToDFS: Vertex, DistRev` is the depth.
- **Rules:**
    ```
    rule addDistDFS: DistToDFS a d, Edge a b |- DistToDFS b (d + 1)
    priority: d1 < d2 |- addDistDFS { d = d1 } < addDistDFS { d = d2 }
    ```
    The priority always expands the deepest vertex first.
- **Initial values:** `DistToDFS s 0` for the source vertex `s`, and `Edge a b` for every edge of the graph.

### Topological sort

- **Task:** a level for every vertex reachable from a source, such that vertices of one strongly connected component share a level and the levels order the condensed graph.
- **Relations:** `Edge: Vertex, Vertex`; `DistTopsort: Vertex, Dist` is the level.
- **Rules:**
    ```
    rule addEdgeTopsort: DistTopsort a d, Edge a b |- DistTopsort b (d + 1)
    rule relaxEdgeTopsort: DistTopsort a d1, DistTopsort b d2, Edge b a, if d2 > d1 |- DistTopsort b d1
    ```
    `relaxEdgeTopsort` pulls a vertex down to the level of the vertex it points to, so the vertices on a cycle end up with the same level.
- **Initial values:** `DistTopsort s 0` for the source vertex `s`, and `Edge a b` for every edge of the graph.

## `fix/NumberTheory.fix`

### Eratosthenes sieve

- **Task:** all composite numbers up to `m`.
- **Relations:** `Check: Natural` is the upper bound `m`; `Div: Natural, DistRev` with `Div a b` means that the multiples `a·2, …, a·b` have been generated; `Comp: Natural` is a composite number.
- **Rules:**
    ```
    rule findDiv: Check m, Div a b, if ((a * (b + 1)) <= m) |- Div a (b + 1)
    rule defComp: Div a b |- Comp (a * b)
    ```
- **Initial values:** `Check m` for the upper bound `m`, and `Div a 2` for every `a` from 2 to `m / 2`.
- **Asymptotics:**
    - Commonly known: O(n log log n) for the sieve, which crosses out multiples of primes only.
    - This formulation generates the multiples of every `a`, not only of primes, so it fires about n ln n rules.
    - Measured: close to n * log n * log n, likely due to priority queue order maintained by Fixen.

## `fix/DynamicProgramming.fix`

### Knapsack

- **Task:** unbounded knapsack. For every total weight up to the capacity, the largest total value that reaches exactly that weight, where each item can be used any number of times.
- **Relations:** `MaxW: Natural` is the capacity; `Items: Natural, Natural` is an item's weight and value; `Weights: Natural, DistRev` is a total weight and the best value for it.
- **Rules:**
    ```
    rule addItem: MaxW w, Items a b, Weights x y, if ((a + x) <= w) |- Weights (x + a) (b + y)
    ```
- **Initial values:** `MaxW w` for the capacity `w`, `Weights 0 0` for the empty selection, and `Items a b` for every item of weight `a` and value `b`.

### Longest Common Subsequence

- **Task:** length of the longest common subsequence of two strings A and B.
- **Relations:** `StringA: ByteString`, `StringB: ByteString` are the input strings; `SubseqAns: Natural, Natural, DistRev` with `SubseqAns x y d` means that the first `x` characters of A and the first `y` characters of B have a common subsequence of length `d`.
- **Rules:**
    ```
    rule subseqProgressLeft: StringA s, SubseqAns x y d, if (x < (len s)) |- SubseqAns (x + 1) y d
    rule subseqProgressRight: StringB t, SubseqAns x y d, if (y < (len t)) |- SubseqAns x (y + 1) d
    rule subseqProgressBoth: StringA s, StringB t, SubseqAns x y d,
      if (((x < (len s)) && (y < (len t))) && ((charAt s x) == (charAt t y)))
      |- SubseqAns (x + 1) (y + 1) (d + 1)
    ```
    Nine priorities of the form `priority: (x1 + y1) < (x2 + y2) |- r1 { x = x2, y = y2 } < r2 { x = x1, y = y1 }`, one for each pair of the three rules, make cells with a smaller `x + y` fire first. The table is then filled one anti-diagonal at a time, and each cell has its final value before its own rules fire.
- **Initial values:** `StringA` and `StringB` holding the two strings, and `SubseqAns 0 0 0`.
- **Asymptotics:**
    - Commonly known: O(n*m) with the textbook dynamic programme, O(n²) for two strings of length n.
    - Measured: close to n² * log n, likely due to priority queue maintained by Fixen.

### Previous Smaller Element

- **Task:** for every position of an array, the nearest position to its left with a strictly smaller value.
- **Relations:** `Arr: Array Natural Natural` is the input array; `MinCand: Natural, Natural` with `MinCand idx idx1` means that `idx1` is the next candidate for `idx`; `MinAns: Natural, Natural` with `MinAns idx idx1` is the answer for `idx`.
- **Rules:**
    ```
    rule propagateMinLarge: Arr arr, MinCand idx idx1, MinAns idx1 idx2, if ((arr ! idx) <= (arr ! idx1)) |- MinCand idx idx2
    rule propagateMinSmall: Arr arr, MinCand idx idx1, if ((arr ! idx) > (arr ! idx1)) |- MinAns idx idx1
    ```
    A candidate that is not smaller is skipped by jumping straight to that candidate's own answer.
- **Initial values:** `Arr` holding the array at positions 0 to n − 1, and `MinCand (i + 1) i` for every `i` from 0 to n − 2.
- **Asymptotics:**
    - Commonly known: O(n) with a monotonic stack.
    - The jumps visit the same elements the stack would pop, so at most about 2n `MinCand` facts are derived.
    - Measured: close to n * log n, likely due to priority queue maintained by Fixen.

### Prefix Function

- **Task:** for every position `i` of a string, the length of the longest proper prefix of `s[0..i]` that is also its suffix.
- **Relations:** `Str: ByteString` is the input string; `PrefAns: Natural, Natural` is a position and its prefix-function value; `PrefCand: Natural, Natural` with `PrefCand idx v` means that a border of length `v + 1` is the current candidate for `idx`; `PrefZero: Natural` means that the empty border is the current candidate.
- **Rules:**
    ```
    rule prefNext: Str s, PrefAns idx val, if val > 0, if ((idx + 1) < (len s)) |- PrefCand (idx + 1) (val - 1)
    rule prefNextZero: Str s, PrefAns idx val, if ((idx + 1) < (len s)), if (val == 0) |- PrefZero (idx + 1)
    rule prefZero0: Str s, PrefZero idx, if ((charAt s idx) /= (charAt s 0)) |- PrefAns idx 0
    rule prefZero1: Str s, PrefZero idx, if ((charAt s idx) == (charAt s 0)) |- PrefAns idx 1
    rule propagatePrefMatch: Str s, PrefCand idx val, if ((charAt s idx) == (charAt s (val + 1))) |- PrefAns idx (val + 2)
    rule propagatePrefDec: Str s, PrefCand idx idx1, PrefAns idx1 val, if ((charAt s idx) /= (charAt s (idx1 + 1))), if val > 0 |- PrefCand idx (val - 1)
    rule propagatePrefDecZero: Str s, PrefCand idx idx1, PrefAns idx1 val, if ((charAt s idx) /= (charAt s (idx1 + 1))), if (val == 0) |- PrefZero idx
    ```
- **Initial values:** `Str` holding the string, and `PrefAns 0 0`.
- **Asymptotics:**
    - Commonly known: O(n) (amortised, as in Knuth–Morris–Pratt).
    - Fewer than n `PrefCand` facts are derived on the benchmark inputs.
    - Measured: n * log n, likely due to priority queue maintained by Fixen.

In all four benchmarked algorithms every derived fact goes through Fixen's priority queue, which costs O(log n) per operation. This is the likely source of the extra log n factor compared with the textbook bounds.

## `fix/DistributedProtocol.fix`

### Ring leader election (Chang and Roberts)

- **Task:** elect the node with the largest identifier as leader of a one-directional ring.
- **Relations:** `Node: Natural` is a node identifier; `Next: Natural, Natural` with `Next s p` means that `p` follows `s` on the ring; `Messages: Natural, Natural` with `Messages p t` means that node `p` has received identifier `t`; `Leader: Natural` is the elected node.
- **Rules:**
    ```
    rule sendOwn: Next s p |- Messages p s
    rule send: Messages s t, Next s p, if (s < t) |- Messages p t
    rule elect: Messages s s |- Leader s
    ```
    Every node sends its own identifier to the next node. A node passes on only identifiers larger than its own, and a node that receives its own identifier becomes the leader.
- **Initial values:** `Node v` for every node identifier `v`, and `Next s p` for every pair of neighbouring nodes on the ring.