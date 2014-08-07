(ns irregular.core
  (:refer-clojure :exclude [or and peek])
  (:require [clojure.core.async :as async])
  (:use [midje.sweet]))

;; ## Core Async Utils
;;; __multiplex__ and __demultiplex__ are two utility functions for core/async
;; channels. The `core.async.lab` namespace has these, but they are experimental,
;; and we thought it best to write our own.

(defn multiplex
  "__multiplex__ returns a vector of `n` channels. Any write on `in` will be written
   to all of them. Closing `in` closes all returned channels."
  [in n]
  (let [outs (repeatedly n async/chan)]
    (async/go-loop []
      (if-let [val (async/<! in)]
        (do (doseq [out outs] (async/>! out val))
            (recur))
        (doseq [out outs] (async/close! out))))
    outs))

(defn demultiplex
  "__demultiplex__ takes a (finite) seq of `chans` and `out`. Reads from any of
  the `chans` are written to `out`. When all the `chans` are closed, `out` is closed."
  [chans out]
  {:pre [(not (empty? chans))]}
  (async/go-loop
    (loop [chanset (set chans)]
      (when (not (empty? chanset))
        (let [[v c] (async/alts! (vec chanset))]
          (if (nil? v)
            (recur (disj chanset c))
            (do
              (async/>! out v)
              (recur chanset))))))
    (async/close! out)))

;; A __ParseState__ is the object expected by a parser. It tracks cursor position.
(defrecord ParseState
  [input pos])

(defn new-parse-state
  "__new-parse-state__ wraps a string in a ParseState."
  [strn]
  (parse-state. strn 0))

(defn peek
  "__peek__ looks ahead without changing the parse state."
  [parse-state]
  (get (:input parse-state) (:pos parse-state) nil))

(facts "About peek."
  (fact "Peek returns the next char."
    (peek (new-parse-state "a")) => \a))

(defn advance
  "__advance__ moves the cursor forward one character."
  [parse-state]
  (update-in parse-state [:pos] inc))

(facts "About advance."
  (fact "Advance moves the cursor forward one character."
    (-> (new-parse-state "abc")
         advance
         :pos) => 1))

(defn accept-chr
  "__accept-chr__ -- an acceptor that accepts a char chr
   Returns a new parse-state if accepts or nil if it doesn't accept."
  [chr]
  (fn [parse-state]
    (if (= (peek parse-state) chr)
      (advance parse-state)
      nil)))

;; ## Parsers
;; Parsers are functions that take an input channel and an output channel.
;; If a parser successfully parses a ParseState received on its input channel,
;; it puts a new ParseState onto its output channel.

;; Parsers are responsible for closing their output channel when their
;; input channel is closed.

(defn acceptor->parser
  "__acceptor->parser__ turns an Acceptor into a Parser."
  ;; Ask zach: should this be done with partials
  [acceptor]
  (fn [in-chan out-chan]
    (async/go-loop []
      (if-let [parse-state (async/<! in-chan)]
        (do
          (when-let [result (acceptor parse-state)]
            (async/>! out-chan result))
          (recur))
        (async/close! out-chan)))))

(defn parse-empty
  "Parser that advances the parse state regardless of input."
  []
  (acceptor->parser advance))

(defn parse-chr
  [chr]
  (acceptor->parser (accept-chr chr)))

(defn parse-string
  [strn]
  (and-chain (map acceptor->parser strn)))

(defn and-chain
  "Takes a seq of parsers and returns an acceptor that chains them in sequence."
  ;; TODO is acceptor the right name?
  [parsers]
  (fn [in-chan out-chan]
    ;; TODO should this be a loop or a recurisve
    (loop [[parser & remaining] parsers
           in-chan in-chan
           out-chan out-chan]
      (if (nil? parser)
        (async/pipe in-chan out-chan)
        (let [new-chan (async/chan)]
          (parser in-chan new-chan)
          (recur remaining new-chan out-chan))))))


(defn or-chain
  "Takes a seq of parsers and returns an acceptor that runs them in parallel."
  [parsers]
  {:pre [(not (empty? parsers))]}
  (fn [in-chan out-chan]
    (async/go
      (let [ins (multiplex in-chan (count parsers))
            connect-parser (fn [parser in]
                           (let [out (async/chan)]
                             (parser in out)
                             out))
            outs (mapv connect-parser parsers ins)]
        (demultiplex outs out-chan)))))


(defn many
  [parser]
  (or-chain [parse-empty]))

(defn regex->parsers
  [regex]
  (and-chain (map #(acceptor->parser (accept-chr %)) regex)))

;; rename these
(defn parse-regex [expr]
  (if (string? expr)
    (regex->parsers expr)
    expr))

(defn parse-regexes [exprs]
  (map parse-regex exprs))


;; Helpers to build s-expressions for regexes

(defn and [& exprs]
  (and-chain (parse-regexes exprs)))

(defn or [& exprs]
  (or-chain (parse-regexes exprs)))


(defn runner
  "Takes a string and an acceptor, returns whether string is accepted."
  [strn acceptor]
  (let [in-chan (async/chan)
        out-chan (async/chan)]
    (acceptor in-chan out-chan)
    (async/go
      (async/>! in-chan (new-parse-state strn))
      (async/close! in-chan))
    (boolean (async/<!! out-chan))))


(facts "about the parser."
  (fact "can parse empty."
    (runner "a" (parse-empty)) => true)
  (fact "can parse a character."
    (runner "a" (parse-chr \a)) => true
    (runner "a" (parse-chr \b)) => false)
  (fact "can parse an and."
    (runner "ab" (and-chain [(parse-chr \a) (parse-chr \b)])) => true
    (runner "aa" (and-chain [(parse-chr \a) (parse-chr \b)])) => false)
  #_(fact "can parse an or."
    (runner "a" (or-chain [(parse-chr \a) (parse-chr \b)])) => true
    (runner "b" (or-chain [(parse-chr \a) (parse-chr \b)])) => true
    (runner "c" (or-chain [(parse-chr \a) (parse-chr \b)])) => false)
  #_(fact "can parse a many."
    (runner "a" (many (parse-chr \a))) => true
    (runner "aa" (many (parse-chr \a))) => true
    (runner "aab" (and-chain [(many (parse-chr \a)) (parse-chr \b)])) => true
    (runner "aac" (and-chain [(many (parse-chr \a)) (parse-chr \b)])) => true
    )
  )
