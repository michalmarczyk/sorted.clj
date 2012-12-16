# sorted.clj

An implementation of Clojure's sorted collections written in Clojure.
Based on the ClojureScript port of Clojure's implementation (itself
written in Java).

The included test suite is a port of the relevant part of
ClojureScript's test suite.

The Clojure(Script) source files containing the code the present
library is based on carry the following copyright notice:

    Copyright (c) Rich Hickey. All rights reserved.
    The use and distribution terms for this software are covered by the
    Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
    which can be found in the file epl-v10.html at the root of this distribution.
    By using this software in any fashion, you are agreeing to be bound by
      the terms of this license.
    You must not remove this notice, or any other, from this software.

## Usage

There is only one namespace, `sorted.core`, which exports four
functions: `sorted-map`, `sorted-map-by`, `sorted-set`,
`sorted-set-by`. These produce sorted.clj's variants of the data
structures returned by the originals from `clojure.core`.

## Licence

Copyright © 2012 Michał Marczyk

Distributed under the Eclipse Public License, the same as Clojure.
