(ns data-trace)

(defn- untrace [v]
  (alter-var-root v (fn [new]
                      (if-let [untraced (:trace/untraced (meta new))]
                        untraced
                        (throw (Exception. "Wasn't traced."))))))

(defn- maybe-disable [state n]
  (if (< (:calls state) n)
    (update state :calls inc)
    (assoc state :done true)))

(defn- data-tracer [v state n old]
  (when-not (fn? old)
    (throw (Exception. "Can't trace non-functions.")))
  (with-meta
    (fn [& args]
      (swap! state maybe-disable n)
      (if (:done @state)
        (do (untrace v)
            (apply old args))
        (do (swap! state update :args conj args)
            (let [ret (apply old args)]
              (swap! state update :returns conj ret)
              ret))))
    {:trace/untraced old}))

(defn data-trace
  "Enable trace on any var containing a function for up to n calls
   E.g. (data-trace #'my-app.functions/run 10)"
  [v n]
  (let [state (atom {:args [] :returns [] :calls 0 :var v})]
    (alter-var-root v (partial data-tracer v state n))
    state))
