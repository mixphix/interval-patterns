# timeframes

This library is called `timeframes`, but provides types and functions
for working with intervals of any underlying type, and their monoid under union.

* `Data.Suspension` provides the two-point extension of a type,
  used for marking the endpoints of intervals.
* `Data.Interval` defines the `Interval` type, along with type- and value-level
  structures for working with closed and open endpoints.
* `adjacency` calculates the relationship between intervals
  according to Allen`s interval algebra.
* `split` separates two intervals into one, two, or three parts,
  depending on their adjacency.
