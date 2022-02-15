# Revision history for interval-patterns

## Upcoming

* switch definitions of `lower` and `lowerBound`, `upper` and `upperBound`

## 0.0.7.1 -- 2022-02-13

* `imapS` and `itraverseS`

## 0.0.7.0 -- 2022-02-13

* introduce strictness annotations for `Suspension`, `Interval`, and `Adjacency`

## 0.0.6.1 -- 2022-02-13

* fix `Num` instance for `Suspension`

## 0.0.6.0 -- 2022-02-13

* remove `AdjacencyRepr` (don't need a type parameter you're just gonna hide)
* amalgamate `split` into `adjacency`
* no need for weird import chain, define everything in `Data.Interval`

## 0.0.5.0 -- 2022-02-12

* `Data.Interval.Layered` provides a monoid like `IntervalSet`, but
  that will keep track of the number of times a particular interval
  has been added to the structure. This is useful for determining, e.g.,
  how many clones you need to attend all today's meetings on time.

## 0.0.4.0 -- 2022-02-12

* Rename project

# Revision history for timeframes

## 0.0.3.0 -- 2022-02-12

* Drop `orient`, putting `Ord` constraints on the constructors of `Interval`
  and leaving the ordering to the smart constructors
* Rewrite `intersection`, `union`, and `difference` using `split`
* Settle on changelog format
* move `withBounds` to `Data.Interval`

## 0.0.2.0 -- 2022-02-12

* Drop `UpToThree` in favour of a more dependently-typed model of adjacency
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
