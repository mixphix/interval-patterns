# interval-patterns

Intervals of ordered types, and their monoids under union.

## Why this package?

There are many implementations of intervals on Hackage:

* [intervals](https://hackage.haskell.org/package/intervals)
  * Has a nonempty variant
  * Geared toward numerical computation
* [numbers](https://hackage.haskell.org/package/numbers-3000.2.0.2)
  * Self-described as incomplete
* [data-interval](https://hackage.haskell.org/package/data-interval-2.1.1)
  * Sophisticated library with many utilities and boundary handling
  * Implements Allen's interval algebra

This package aims to simplify working with intervals by providing
several *patterns* on which to match, many of which are bidirectional
(and hence can also be used as constructors). The patterns are designed
to be mnemonically significant, so that dealing with `Bound` types is
conveniently swept under the rug.

Another strong use case for this library is its use of Allen's interval algebra.
Unlike in `data-interval`, where the relationship is essentially meaningless,
this library turns it into a proof-relevant structure that demonstrates exactly
how two intervals may relate, by decomposing them into the parts that constitute
the relationship itself. See `Data.Interval.adjacency` for details.

Also like `data-interval`, this package provides a monoid under union (which we
call `Borel` as opposed to `IntervalSet`), as well as a slightly different
version of `IntervalMap` which is named `Layers`. This latter monoid preserves
the overlap of the intervals as they get inserted, as opposed to agglomerating
overlapping intervals as would be the case with `Borel`. This allows complex
calculations, that could otherwise be modelled as integrals of step functions.
