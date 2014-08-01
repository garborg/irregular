(ns irregular.core
  (:refer-clojure :exclude [or and peek])
  (:require [clojure.core.async :as async])
  (:use [midje.sweet]))


;; core/async utils
;;; core.async.lab has these, but they are experimental, best to write our own
(defn multiplex
  "Retuns a vector of n channels. Any write on in will be written to all of them. Closing n closes each channel"
  [in n]
  (let [outs (repeatedly n async/chan)]
    (async/go-loop []
      (if-let [val (async/<! in)]
        (do (doseq [out outs] (async/>! out val))
            (recur))
        (doseq [out outs] (async/close! out))))
    outs))

(defn demultiplex
  "Takes a (finite) seq of chans and out. Reads from any of the chans are written to out. When all the chans are closed, out is closed"
  [chans out]
  {:pre [(not (empty? chans))]}
  (async/go
    (loop [chanset (set chans)]
      (when (not (empty? chanset))
        (let [[v c] (async/alts! (vec chanset))]
          (if (nil? v)
            (recur (disj chanset c))
            (do
              (async/>! out v)
              (recur chanset))))))
    (async/close! out)))

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

(facts "about peek"
  (fact "peek returns the next char"
    (peek (new-lexer "a")) => \a))

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
    (async/go-loop []
      (if-let [lexer (async/<! in-chan)]
        (do
          (when-let [result (acceptor lexer)]
            (async/>! out-chan result))
          (recur))
        (async/close! out-chan)))))

(defn parse-empty
  []
  (wrap-acceptor advance))

(defn parse-chr
  [chr]
  (wrap-acceptor (accept-chr chr)))

(defn parse-string
  [strn]
  (and-chain (map wrap-acceptor strn)))

(defn and-chain
  "Takes a seq of parsers and returns an acceptor that chains them in sequence"
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
  "Takes a seq of parsers and returns an acceptor that runs them in parallel"
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
  (and-chain (map #(wrap-acceptor (accept-chr %)) regex)))

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
  "Takes a string and an acceptor, returns whether string is accepted"
  [strn acceptor]
  (let [in-chan (async/chan)
        out-chan (async/chan)]
    (acceptor in-chan out-chan)
    (async/go
      (async/>! in-chan (new-lexer strn))
      (async/close! in-chan))
    (boolean (async/<!! out-chan))))


(facts "about the parser"
  (fact "can parse empty"
    (runner "a" (parse-empty)) => true)
  (fact "can parse a character"
    (runner "a" (parse-chr \a)) => true
    (runner "a" (parse-chr \b)) => false)
  (fact "can parse an and"
    (runner "ab" (and-chain [(parse-chr \a) (parse-chr \b)])) => true
    (runner "aa" (and-chain [(parse-chr \a) (parse-chr \b)])) => false)
  #_(fact "can parse an or"
    (runner "a" (or-chain [(parse-chr \a) (parse-chr \b)])) => true
    (runner "b" (or-chain [(parse-chr \a) (parse-chr \b)])) => true
    (runner "c" (or-chain [(parse-chr \a) (parse-chr \b)])) => false)
  #_(fact "can parse a many"
    (runner "a" (many (parse-chr \a))) => true
    (runner "aa" (many (parse-chr \a))) => true
    (runner "aab" (and-chain [(many (parse-chr \a)) (parse-chr \b)])) => true
    (runner "aac" (and-chain [(many (parse-chr \a)) (parse-chr \b)])) => true
    )
  )
