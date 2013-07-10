(ns mma.core
  (:refer-clojure :exclude [read-line flush eval import])
  (use mma.conversion
       me.raynes.conch.low-level
       me.raynes.conch
       [clarity core syntax utils]))

(use-clarity)
(clarity

;; ----------------
;; Stream utilities
;; ----------------

inner-namespace stream

  defn chars-seq [stream]
    ->> #(.read stream) repeatedly (take-while #(> % 0)) (map char)

  defn readline [stream]
    ->> stream chars-seq (drop-while #(#{\newline \return} %)) (take-while #(not (#{\newline \return} %))) (apply str)

  defn read-rest [stream]
    ->> stream chars-seq (take (.available stream)) (apply str)

declare ^:dynamic *mma*

defn ^:private print-rest! []
  println : read-rest : *mma* :out

;; ------------------
;; Session management
;; ------------------

def ^:dynamic *mma* nil

declare eval send-input!

def ^:private init """
WordJoin[strs_] := StringJoin[Riffle[strs, " "]];
PrintResult[expr_: Null] := Print["r_" <> EdnForm[expr]];

EdnForm[expr_] := ToString[expr, InputForm];
EdnForm[x_ /; x == Null] := "nil";
EdnForm[x_ /; x == True] := "true";
EdnForm[x_ /; x == False] := "false";
EdnForm[list_List] := "[" <> WordJoin[EdnForm /@ list] <> "]";
EdnForm[Rational[n_, d_]] := ToString[n] <> "/" <> ToString[d];
EdnForm[x_Real] := StringReplace[ToString[x, InputForm], "*^" -> "E"];
EdnForm[h_[c___]] := "(" <> WordJoin[EdnForm /@ {h, c}] <> ")";

SetOptions["stdout",FormatType->OutputForm];
"""

defn start [& _]
  binding [*mma* (-> (proc """C:\Program Files\Wolfram Research\Mathematica\9.0\math.exe""" "-noprompt")
                     (assoc :queue (queue)))]
    send-input! init
    *mma*

defn stop [session]
  if session (done session)

defn stop! []
  alter-var-root #'*mma* stop

defn start! []
  (stop!)
  alter-var-root #'*mma* start

(defmacro with-session* [& forms]
 `(binding [*mma* (start)]
    (let [result# (do ~@forms)]
      (stop *mma*)
      result#)))

(defmacro with-session [& forms]
 `(if *mma*
    (do ~@forms)
    (with-session* ~@forms)))

;; ---------------------
;; Evaluation of strings
;; ---------------------

defn send-input! [input]
  feed-from-string *mma* : str input \newline
defn ^:private result? [s]
  .startsWith s "r_"
defn ^:private parse-result [s]
  read-string : .substring s 2

defn ^:private get-result! []
  loop [s ""]
    if (result? s)
      parse-result s
      do
        when-not (empty? s) (println s)
        recur (readline : *mma* :out)

defn eval
  "Eval a string in Mathematica and return the
  parsed result. Pipes stdout (i.e. printed
  messages, errors)."
  [s]
  with-session
    queued (*mma* :queue)
      send-input! : str "PrintResult[" s "]"
      (get-result!)

;; --------------------
;; High-level interface
;; --------------------

(defmacro math*
  "Evaluate expressions within Mathematica.
  Supports unquoting (~ and ~@) to access
  local vars."
  [& exprs]
 `(-> (quote* (~'CompoundExpression ~@exprs)) ->mma eval))

;; Vars

(deftype MathematicaVar [symbol session]
  ;Mathematica
  ;  (->mma [this] (str symbol))
  clojure.lang.IDeref
    (deref [this] (math* ~this))
  Object
    (finalize [this]
      (binding [*mma* session]
        (math* (Clear ~this)))))

(extend-type MathematicaVar
  Mathematica
    (->mma [this] (str (.symbol this))))

defn create-var [s]
  let [sym (math* (Unique))]
    eval : i-str "~sym = ~s;"
    MathematicaVar. sym *mma*

(defmacro mvar [& exprs]
 `(-> (quote* (~'CompoundExpression ~@exprs)) ->mma create-var))

def ^:dynamic *return-vars* false

defn set-return-vars! [val]
  alter-var-root #'*return-vars* : λ val

(defmacro math [& exprs]
 `(if *return-vars*
    (mvar  ~@exprs)
    (math* ~@exprs)))

(defmacro $math
  "Evaluate a mathematica string, with
  support for unquoting (~)."
  [& ss]
  `(-> (str ~@(map #(if-not (string? %) `(->mma ~%) %)
                   (symbol-extract (apply str ss))))
       ((if *return-vars* create-var eval))))

(defsyntax $ [s]
 `($math ~s))

defn use-$ [] (use-syntax $)

defn mma-fn
  "Create a pure function which calls the
  Mathematica function given by `sym`.
  ((mma-fn 'Prime) 1) => 2"
  [sym]
  λ math : ~sym ~@%s

(defmacro import
  "Import Mathematica functions.
  (import Prime)
  (Prime 1) => 2"
  [& syms]
 `(do ~@(map (fn [sym]
              `(def ~(with-meta sym) (mma-fn '~sym)))
             syms)))

)

