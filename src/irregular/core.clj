(ns irregular.core
  (:refer-clojure :exclude [or and peek]))

;; a (chr A-Za-z0-9)
;; (exp|exp)
;; * (0+)
                                        ; (a|(bc|cd))

(defrecord Lexer [input start pos])

(defn new-lexer
  "Wraps a string in a Lexer"
  [strn]
  (Lexer. strn 0 0))

(defn peek [lexer]
  (get (:input lexer) (:pos lexer) nil))

(defn advance [lexer]
  (assoc lexer :pos (inc (:pos lexer))))

(defn accept-chr [chr]
  (fn [lexer]
    (if (= (peek lexer) chr)
      (advance lexer)
      nil)))1

(defn or-combinator [acceptors]
  (fn [lexer]
    (loop [lexer lexer
           acceptors acceptors]
      (if (empty? acceptors) nil
          (if-let [result ((first acceptors) lexer)]
            result
            (recur lexer (rest acceptors)))))))

(defn and-combinator [acceptors]
  (fn [lexer]
    (loop [lexer lexer
           acceptors acceptors]
      (if (empty? acceptors)
        lexer
        (if-let [result ((first acceptors) lexer)]
          (recur result (rest acceptors))
          nil)))))

(defn star-combinator [acceptor]
  (fn [lexer]
    (loop [lexer lexer]
      (if-let [result (acceptor lexer)]
        (recur result)
        lexer))))



;; a (chr A-Za-z0-9)
;; (exp|exp)
;; * (0+)
                                        ; (a|(bc|cd))

(defn regex->acceptors
  [regex]
  (and-combinator (map accept-chr regex)))

(defn parse-regex [expr]
  (if (string? expr)
    (regex->acceptors expr)
    expr))

(defn parse-regexes [exprs]
  (map parse-regex exprs))

(defn and [& exprs]
  (and-combinator (parse-regexes exprs)))

(defn or [& exprs]
  (or-combinator (parse-regexes exprs)))

(defn star [expr]
  (star-combinator (parse-regex expr)))

#_(defn regex-builder
    "Returns a function that takes a string and
    returns true if regex accepts the string, false otherwise"
    [regex]
    (let [acceptors (regex->acceptors regex)] ; right now, regex is "a" or "b" ...
      (fn [strn]
        (loop [lexer (new-lexer strn)
               acceptors acceptors]
          (if (empty? acceptors)
            true
            (if-let [result ((first acceptors) lexer)]
              (recur result (rest acceptors))
              false))))))

#_(defn ror
    "Regular expression or helper"
    [strn]
    (or-combinator (map accept-chr strn)))

#_(defn rand
    "Regular expression and helper"
    [strn]
    (and-combinator (map accept-chr strn)))


(defn runner
  "Regegular expression runner"
  [strn acceptor]
  (if (acceptor (new-lexer strn))
    true
    false))

#_(def rg
                                        ; a|(bc|cd)
    (or-combinator [(accept-chr \a) (or-combinator [(rand "bc") (rand "cd")])]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
