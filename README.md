# interval-patterns

Intervals of ordered types, and their monoids under union.

* `Data.Interval` defines the `Interval` type, along with type- and value-level
  structures for working with closed and open endpoints.
  * `adjacency` calculates the relationship between intervals
    according to Allen`s interval algebra.
* `Data.Interval.Borel` for the Borel sets over a type.
* `Data.Interval.Layers` for a different monoid, under adjacency,
  that keeps track of overlap count.
