(ns mma.dictionary
  (use mma.conversion)
  (require [mma.core :as m]
           [clojure.string :as str]))

(m/use-$)

;; Dictionary Functions:
;; m = Dictionary[a->1, b->2]
;; m.a + m.b == m[a] + m[b] == (a + b) /. m == 3
;; m.c == m[c] == c /. m == c
;; Assoc[m, c -> 4, d -> 5] == Dictionary[a -> 1, b -> 2, c -> 4, d -> 5]
;; Dissoc[m, a, b] == Dictionary[]

(extend-protocol Mathematica
  clojure.lang.IPersistentMap
    (->mma [this]
      (str "Dictionary[" (str/join "," (map (fn [[key val]] (str (->mma key) "->" (->mma val))) this)) "]")))

(defn enable-maps
  "Allows interop for hash maps with Mathematica.
  Clojure keywords are converted into MMA symbols,
  and vice versa."
  []
  ($
    SetAttributes[Dictionary, Orderless];
    Dictionary[key_ -> val_, ___][key_] := val;
    Dictionary[key_ -> val_, ___].key_ ^:= val;
    x_ /. Dictionary[rs___] ^:= x /. {rs};
    Dictionary[{rs___}] := Dictionary[rs];

    Assoc[Dictionary[key_ -> _, rs___], key_ -> val_] := Dictionary[key -> val, rs];
    Assoc[Dictionary[rs___], key_ -> val_] := Dictionary[key -> val, rs];
    Assoc[map_Dictionary, rs : (_ -> _) ..] := Fold[Assoc, map, {rs}];
    map_Dictionary /. ((Dictionary | List)[rs___] | r_) ^:= Assoc[map, r, rs];

    Dissoc[Dictionary[key_ -> _, rs___], key_] := Dictionary[rs];
    Dissoc[map_Dictionary, key_] := map;

    Dictionary::dupkey = "Map contains duplicate key `1`.";
    Dictionary[key_ -> _, key_ -> _, ___] := Message[Dictionary::dupkey, key];

    (* Edn form conversion *)

    Keywordise[key_Symbol] := ":" <> EdnForm[key];
    Keywordise[key_] := EdnForm[key];
    RuleStr[key_ -> val_] := Keywordise[key] <> " " <> EdnForm[val];
    EdnForm[Dictionary[rs___]] ^:= "{" <> WordJoin[RuleStr /@ {rs}] <> "}";
  ))
