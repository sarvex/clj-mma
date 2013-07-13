# clj-mma

is a Clojure library which lets you interface with Mathemactica.

Current version is `[clj-mma "0.1.0"]`

```clj
(ns user
  (require [mma.core :as m :refer [math $math]]))
  (m/import Prime)
```

To start, you'll need to set the path to your Mathematica kernel executable. On Windows this looks like `C:\Program Files\Wolfram Research\Mathematica\9.0\math.exe`, which is the default value. If it's on your path, "math" should do the trick.

```clj
(m/set-mma-path "math")
```

The easiest way to use Mathematica functions is to use `import` - `Prime` was imported after the namespace declaration above.

```clj
(Prime 10)
;=> 29
```

For writing blocks of Mathematica code, use the `math` macro. Forms wrapped in `math` will be translated to Mathematica syntax (see below), evaluated, and the result translated back into Clojure - e.g.

```clj
(math (Map Prime (Range 10)))
;=> [2 3 5 7 11 13 17 19 23 29]

(math (Integrate (* :x 2) :x))
;=> (Power :x 2)
```

Note that `math` supports the unquote (`~`) and unquote-splicing (`~@`) operators.

```clj
(math (Map Prime ~(range 1 11)))
;=> [2 3 5 7 11 13 17 19 23 29]

(math (Plus ~@(range 10)))
;=> 45
```

Finally, the `$math` and `$` macros (the latter of which must be enabled with `use-$`) let you interpolate clojure vars into regular Mathematica syntax.

```clj
(def x [1 2 3])
(def y [4 5 6])

($math "~x.~y")) ;=> 32

(m/use-$)

($ Prime[{1,2,3}.{4,5,6}]) ;=> 131

($ Table[i^2, {i, ~x}]) ;=> [1 4 9]
```

Note that currently only symbols can be interpolated - i.e. `($ ~x)` works but `($ ~[1 2 3])` doesn't. Support for arbitrary forms is planned.

## Session management

Up to now each function call started a new Mathematica session. This takes around 300ms on my system - fine for repl usage, but in code we need some optimisation. To open up a session, which will be used by all calls to `math`/`$math` within a block of code, use `with-session`.

```clj
;; 3 seconds
(mapv Prime (range 1 10))

;; 0.3 seconds
(m/with-session (mapv Prime (range 1 10)))
```

Alternatively, if you do need to speed up your repl calls, you can start a global session with `(m/start!)` - but please only use this from the repl.

## Mathematica Vars

The MathematicaVar, or mvar, type lets you seamlessly use objects stored in Mathematica's memory - a great optimisation if you find yourself often working with large matrices etc. which can slow down calls to `math` et. al. if they are converted every time. The `*return-vars*` variable, which can be globally set with `set-return-vars!` for repl usage, variable dictates whether evaluations return mvars.

```clj
(m/set-return-vars! true)

(def r (math (Range 100)))

@r ;=> [1 2 3 4 5 6 7 8 9 10]

@(Prime ~r) ;=> [2 3 5 7 11 13 17 19 23 29]
```

The best part about this is that you don't need to worry about mvars filling up Mathematica's memory - when an mvar is garbage collected, its data in Mathematica will be cleared too.

Note, however, that mvars cannot be used across Mathematica sessions, so use them carefully within the scope of a `with-session` block.

```clj
(m/start!)

(def r (math 5))

@r ;=> 5

(m/start!)

@r ;=> :$3

```

## Converting Clojure to Mathematica

Despite its syntax, Mathematica is actually very lisp-like - everything, including function calls and data structures, can be written as a tree of nested "expressions", which are effectively lists beginning a symbol. So, translating Clojure to Mathematica is very straightforward - lists beginning with symbols become expressions, while other lists and vectors become Mathematica lists. This is handled by the `Mathematica` protocol in the `mma.conversion` namespace.

```clj
(->mma '(1 2 3)) ;=> "{1,2,3}"
(->mma '(Head 1 2 3)) ;=> "Head[1,2,3]"
(->mma :a) ;=> "a"
(->mma 'a) ;=> "a"
```

Note that, since symbols identify themselves in Mathematica, clojure keywords become mathematica symbols (and vice versa). Be careful to respect Mathematica's naming rules - symbols and keywords evaluated in Mathematica must be alphanumeric only.

Expressions are actually passed through the multimethod `->expr` rather than being converted directly, dispatching on the head of the expression (the symbol that starts the list). This means that it is possible to alias Mathematica functions, and several such aliases are provided by default: `+`, `-`, `/`, `*`, `**` (for powers), `do` and `.` (dot product).

```clj
(->mma '(do (* x y) (. x y))) ;=> "Times[x,y];Dot[x,y]"
```

## Converting Mathematica to Clojure

Conversion to Clojure is handled on the Mathematica side, by the `EdnForm` function (the definition of which is supplied in the `init.nb` notebook). List are converted to vectors, expressions from `Head[a,b,c]` to `(Head a b c)`. Symbols are converted to keywords by default. It's easy to add your own types by evaluating a definition of the form `EdnForm[x_Head] := (* clojure-readable string *)`.

## Extending

Extending is easy through the `Mathematica` protocol and the `EdnForm` function. For example, Clojure's maps are not supported by default, but an implementation of a dictionary type is available in `mma.dictionary` which is easy to enable, and provides a model for implementing your own types. The mathematica code is in the `init.nb` notebook.

```clj
(m/start!)
(use 'mma.dictionary)
(enable-maps) ; Note that this must be called with every session

(def m {:a 1 :b 2})

(->mma m) ;=> "Dictionary[a->1,b->2]"

($ a + b /. ~m) ;=> 3
```
If you feel like using Dictionaries in Mathematica, this implementation has some useful features.

```
m = Dictionary[a->1, b->2]

m.a + m.b == m[a] + m[b] == (a + b) /. m == 3

m.c == m[c] == c /. m == c

Assoc[m, c -> 4, d -> 5] == Dictionary[a -> 1, b -> 2, c -> 4, d -> 5]

Dissoc[m, a] == Dictionary[b->2]
```

## Possible Issues

clj-mma will currently wait indefinitely for Mathematica to return a result - so syntax errors, for example, will cause it to hang. I hope to fix this within a configurable timeout soon.
