(ns irregular.core
  (:refer-clojure :exclude [or and peek])
  (:require [clojure.core.async :as async]))

;; a (chr A-Za-z0-9)
;; (exp|exp)
;; * (0+)
                                        ; (a|(bc|cd))


;; Breaking case (runner "abbc" (and (or "ab" "abb") "c"))

;; either use a timeout or multiple out chanels


(defrecord Lexer [input start pos])
;; TODO: maybe this object thing is superfluous

(defn new-lexer
  "Wraps a string in a Lexer"
  [strn]
  (Lexer. strn 0 0))

(defn peek [lexer]
  (get (:input lexer) (:pos lexer) nil))

(defn advance [lexer]
  (update-in lexer [:pos] inc))

(defn accept-chr
  ;; TODO maybe this shouldn't be called an acceptor
  "An acceptor that accepts a char chr
   Returns a new lexer if accepts or nil if it doesn't accept"
  [chr]
  (fn [lexer]
    (if (= (peek lexer) chr)
      (advance lexer)
      nil)))

;; "abc" (and (or "abb" "ab) "c)
;; >>! lexer after abb
;; >>! lexer after "ab"

(defn wrap-acceptor
  "Wraps an acceptor with input and output channels"
  ;; Ask zach: should this be done with partials
  [acceptor]
  (fn [in-chan out-chan]
    (async/go
      (println "inside fn inside wrap acceptor")
      (loop []
        (when-let [lexer (async/<! in-chan)]
          (when-let [result (acceptor lexer)]
            (async/>! out-chan result))
          (recur)))

      ;; TODO: close channel here?
)))

(defn and-chain
  "Takes a seq of acceptors and returns an acceptor that chains them in sequence"
  ;; TODO is acceptor the right name?
  [acceptors]
  (fn [in-chan out-chan]
    ;; TODO should this be a loop or a recurisve
    (loop [[acceptor & remaining] acceptors
           in-chan in-chan
           out-chan out-chan]
      (if (nil? acceptor)
        (async/pipe in-chan out-chan)
        (let [new-chan (async/chan)]
          (acceptor in-chan new-chan)
          (recur remaining new-chan out-chan))))))


(defn or-chain
  "Takes a seq of acceptors and returns an acceptor that runs them in parallel"
  [acceptors]
  (fn [in-chan out-chan]
    (loop [acceptors acceptors]
      (if (empty? acceptors)
        (println "empty or") ;; Whats the base case
        (do
          ((first acceptors) in-chan out-chan)
          (recur (rest acceptors) ))))))


;; TODO implement kleene *



;; a (chr A-Za-z0-9)
;; (exp|exp)
;; * (0+)
                                        ; (a|(bc|cd))


(defn regex->acceptors
  [regex]
  (and-chain (map #(wrap-acceptor (accept-chr %)) regex)))

(defn parse-regex [expr]
  (if (string? expr)
    (regex->acceptors expr)
    expr))

(defn parse-regexes [exprs]
  (map parse-regex exprs))


;; Helpers to build s-expressions for regexes

(defn and [& exprs]
  (and-chain (parse-regexes exprs)))

(defn or [& exprs]
  (or-chain (parse-regexes exprs)))


(defn runner
  "Takes a string and an acceptor, returns whether string is accepted"
  [strn acceptor]
  (let [in-chan (async/chan)
        out-chan (async/chan)
        _ (acceptor in-chan out-chan)]
    (async/go
      (async/>! in-chan (new-lexer strn))
      (async/close! in-chan))
    (let [[val port]  (async/alts!! [out-chan (async/timeout 1000)])]

      (async/close! out-chan) ;wtf
    ;; TODO: ask zach
      (print val)
      (if val
        true
        false))))
