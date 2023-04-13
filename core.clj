(ns hw6.core
  (:require [clojure.template :refer [apply-template]]))

;
; To run this program, you can do one of the following:
;
; 1. Load it into the Calva REPL and run (main). If you do this, you
;    will enter input into VS Code's entry at the top of the
;    window, and the displayed output format will be wrong. But,
;    this will be faster for testing than option 2, and should otherwise
;    be fine.
;
; 2. Run it from the command line, from the root of the project directory,
;    with: 
;       clj -X hw6.core/main
;    This option will give you correct formatting, but will require you to
;    install clj, which you can learn about here: https://clojure.org/guides/install_clojure
;



(defn my-eval
  "Evaluates expression, changes environment, and returns resulting state
  with :return set to returned value.
    - state should be a map with a :environment key that stores all bindings, and a
      :return key that stores the result of evaluation
    - expression is the Clojure expression given to the REPL"
  ([state expression] (my-eval state expression nil))
  ([state expression w] (if-not (seq? expression)
                          (if (symbol? expression)
                            (assoc state :return
                                   (if w
                                     (get-in state [:environment w :bindings expression] (get (:environment state) expression))
                                     (get (:environment state) expression)))

                            (assoc state :return expression))

                          (let [env (:environment state)
                                memory (atom {})
                                f (first expression)
                                others (rest expression)
                                _maths (fn [op]
                                         (case op
                                           + true
                                           - true
                                           * true
                                           / true
                                           inc  true
                                           dec true
                                           quot true
                                           min true
                                           max true
                                           rem true
                                           mod true
                                           false))
                                _compare (fn [op]
                                           (case op
                                             =  true
                                             == true
                                             > true
                                             < true
                                             not= true
                                             <= true
                                             >= true
                                             compare true
                                             false))
                                _cast (fn [op]
                                        (case op
                                          byte  true
                                          short true
                                          long true
                                          int true
                                          float true
                                          double true
                                          bigdec true
                                          bigint true
                                          biginteger true
                                          num true
                                          rationalize true
                                          false))
                                _test (fn [op]
                                        (case op
                                          nil?  true
                                          some? true
                                          indentical? true
                                          zero? true
                                          pos? true
                                          neg? true
                                          even? true
                                          odd? true
                                          false))
                                _ratio (fn [op]
                                         (case op
                                           numerator  true
                                           denominator true
                                           ratio? true
                                           false))]

                            ;; (println (:environment (my-eval state (def q 93) 'k)))
                            (cond

                              (= f '?) {:environment (assoc-in env
                                                               [w :bindings]
                                                               (:return (my-eval state
                                                                                 (nth expression 2) w)))
                                        :return (second expression)}
                              (= f 'quote) {:return (second expression)}

                              (= f 'def) {:environment (assoc env
                                                              (second expression)
                                                              (:return (my-eval state
                                                                                (nth expression 2))))
                                          :return (second expression)}

                              ;; (= f 'defn) {:environment (assoc env
                              ;;                                  (second expression)
                              ;;                                  {:func (fn [& args]
                              ;;                                           (for [[m n]
                              ;;                                                 (mapv vector (remove nil? (map-indexed #(when (even? %) %2) args))
                              ;;                                                       (remove nil? (map-indexed #(when (odd? %) %2) (nth expression 2))))]
                              ;;                                             (swap! bindings m (:result (my-eval state n w))))
                              ;;                                           (:return (my-eval state (nth expression 3) (second expression))))
                              ;;                                   :bindings @bindings})


                              ;;              :return (second expression)}

                              (= f 'let) {:return (do
                                                    (swap! memory assoc :id (rand-int 4000))

                                                    (reset! memory (into {} (for [[m n] (mapv vector (remove nil? (map-indexed #(when (even? %) %2) (second expression))) (remove nil? (map-indexed #(when (odd? %) %2) (second expression))))] {m n})))
                                                    
                                                    (reset! memory (conj (dissoc state :return) @memory))
                                                    (:return (my-eval @memory (drop 2 expression)))
                                                    )}

                              (get env f) {:return ((get-in env [f :func]) (rest expression))}

                              (= f 'cons) (if (= (first (last expression)) 'quote)

                                            {:return (conj (:return (my-eval state (second expression))) (:return (my-eval state (nth expression 2))))}

                                            {:return (conj (into '() (:return (my-eval state (nth expression 2))))
                                                           (:return (my-eval state (second expression))))})
                              (= f 'if) {:return ((fn []
                                                    (when (boolean (:return (my-eval state (second expression))))
                                                      (:return (my-eval state (nth expression 2))))

                                                    (when-not (boolean (:return (my-eval state (second expression))))
                                                      (:return (my-eval state (nth expression 3))))))}

                              :else (try (assoc state :return (apply (resolve (symbol f)) (map #(if w (:return (my-eval state % w)) (:return (my-eval state %))) others)))
                                         (catch Exception e
                                           (println "Error:" (.getMessage e)))))))))


(defn repl
  "Implements a REPL without using eval"
  ([] (repl {:environment {}}))
  ([state]
   (print "H]=> ")
   (flush)
   (let [line (read-line)]
     (when-not (empty? line)
       (let [new-state (my-eval state
                                (read-string line))]
         (println "Environment:" (:environment new-state))
         (println (:return new-state))
         (recur new-state))))))













(defn main
  [& args]
  (repl))



