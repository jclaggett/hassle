;; # Transducer Nets
;; *Lonocloud Online Meetup

;; *Wednesday, March 2, 2022*

;; ## Outline
;; 1. Intro *(you are here)*
;; 2. Walk through the API offered to define transducer nets
;; 3. Review the various transducers used under the hood
;; 4. Q & A

;; ## Intro
;; * **What**: library that allows composition of transducers in Directed
;; Acyclic Graph.
;; * **Why**: Represent Process Flow Programs with pure functions.
;; * **Where**: https://github.com/jclaggett/hassle
;; * **Status**: Experimental (nothing released). Transducers in a DAG works!

;; ### The Origin Story...
;; **Problem**: Reactive Programming has interesting properties but the
;; existing libraries are invariably tied to specific contexts and
;; languages.

;; **Goal**: A functional 'reactive' library that is usable in a wide range
;; of contexts across lanuages.

;; **Approach**: Transducers already provide the abstraction of
;; operating on streams of values. Currently, they are composed using
;; `comp` as a simple pipeline. Can we compose transducers into a complex
;; graph?

;; ### Related links
;; * [Kahn Process Networks](https://en.wikipedia.org/wiki/Kahn_process_networks)
;; * github.com/cgrand/xforms - transducer library hassle borrows ideas from.
;; * cycle.js.org - Reactive programming library
;; * rxmarbles.com - Interactive 'marble diagrams'
