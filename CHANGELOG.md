# Revision history for timeframes

## 0.0.2.0 -- 2022-02-12

Drop `UpToThree` in favour of a more dependently-typed model of adjacency.

* `Overlap` is changed to `Adjacency` where it occurs
* `split` now returns `SomeAdjacency`, which can be pattern-matched to yield
  the exact (one, two, or three) intervals into which the two have been cleaved
* `Interval` moved into `Data.Interval.Types` to avoid a cyclic dependency
  between `Data.Interval` and `Data.Interval.Adjacency`

## 0.0.1.0 -- 2022-02-08

Types and functions for working with intervals and their monoid under union.

* `Data.Suspension` provides the two-point extension of a type,
  used for marking the endpoints of intervals.
* `Data.Interval` defines the `Interval` type, along with type- and value-level
  structures for working with closed and open endpoints.
* `overlap` calculates the relationship between intervals
  according to Allen`s interval algebra.
* `split` separates two intervals into one, two, or three parts,
  depending on their overlap.
