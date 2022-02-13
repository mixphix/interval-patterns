# interval-patterns

Intervals of ordered types, and their monoids under union.

* `Data.Suspension` provides the two-point extension of a type,
  used for marking the endpoints of intervals.
* `Data.Interval` defines the `Interval` type, along with type- and value-level
  structures for working with closed and open endpoints.
  * `adjacency` calculates the relationship between intervals
    according to Allen`s interval algebra.
* `Data.Interval.Set` for the monoid under union.
* `Data.Interval.Layered` for a different monoid, under adjacency,
  that keeps track of overlap count.
