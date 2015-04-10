(ns ssa-baby-names-analysis.core
  (:require [clj-http.client :as client]
            [hickory.core :as hick]
            [hickory.select :as hs]))

(set! *print-length* nil)

(def ppr clojure.pprint/pprint)
(defn dbg [l v]
  (println)
  (println l)
  (println v)
  (println)
  v)

(def state (atom {:html {} :names {}}))
(def paths {:html "./data/html.edn"
            :names "./data/names.edn"})

(defn read-file-if-exists
  [filename]
  (when (-> filename
          clojure.java.io/as-file
          .exists)
    (-> filename slurp read-string)))

(defn act-on-state
  [f ks]
  (let [s @state]
    (doseq [k (or (not-empty ks)
                  (keys s))]
     (f s k))))

(defn write-state
  [& ks]
  (act-on-state #(->> % %2 pr-str (spit (%2 paths)))
                ks))

(defn read-state [& keys]
  (act-on-state
   (fn [_ k]
     (when-let [contents (-> (k paths) read-file-if-exists)]
       (swap! state assoc k contents)))
   keys))

(defn save-year-html
  [y h]
  (swap! state assoc-in [:html y] h)
  h)

(defn get-html-for-year
  [y]
  (:body (client/post "http://www.ssa.gov/cgi-bin/popularnames.cgi"
                       {:form-params { :year y :top 1000 :number "p"}})))

(defn fetch-html-for-year
  [y]
  (or (-> @state :html (get y))
      (->> y get-html-for-year (save-year-html y))))

(defn html->data-table
  [html]
  (->> html
       hick/parse
       hick/as-hickory
       (hs/select (hs/descendant (hs/tag :tbody)
                                 (hs/tag :tbody)
                                 (hs/tag :td)))
       (map #(-> % :content first))
       (filter string?)
       (partition 5)
       (map #(let [[rank male male-p female female-p] %]
               [(read-string rank)
                male
                (read-string male-p)
                female
                (read-string female-p)]))))

#_ (-> @state :html (get 2001) html->data-table (data-table->names {} 2001))

(defn data-table->names
  [data init year]
  (reduce (fn [agg r]
            (let [[rank male male-p female female-p] r]
              (-> agg
                  (assoc-in [[male :m] :yrs year] {:rank rank :pct male-p :yr year})
                  (assoc-in [[female :f] :yrs year] {:rank rank :pct female-p :yr year}))))
          init
          data))

(defn make-sort-fn
  [op gi]
  (fn [a b]
    (op (get-in a gi)
        (get-in b gi))))

(defn process-htmls
  [htmls]
  (let [x (->> htmls
               (map (fn [[year html]]
                      (-> html
                          html->data-table
                          (data-table->names {} year))))
               (apply merge-with
                      (partial merge-with merge)))]
    (reduce (fn [agg k]
              (let [yrs (get-in x [k :yrs])
                    sort-yrs-fn #(->> yrs
                                      (map second)
                                      (sort (make-sort-fn % [%2])))]

                (-> agg
                    (assoc-in [k :yrs-v] (sort-yrs-fn < :yr))
                    (assoc-in [k :rank] (sort-yrs-fn < :rank))
                    (assoc-in [k :pct] (sort-yrs-fn > :pct)))))
            x
            (keys x))))


#_ (clojure.pprint/pprint (process-htmls (:html @state) ))


#_ (do
     (println 1)
     (read-state)
     (println 2)
     (doseq [year (range 1880 2013)]
       (fetch-html-for-year year))
     (println 3)
     (swap! state assoc :names (process-htmls (:html @state)))
     (write-state)
     (println "done!"))




;; ffff

#_ @state

#_ (-> @state :html keys)

#_ (-> @state :names keys)

#_ (read-state)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (map first (filter (fn [x] (when-let [v (-> x second :yrs (get 2001) :rank)] (< v 10))) n))
  (set 5)
  (select [val first]
          (w-yrs 2001 :rank #(< % 10))))

(defn nil->
  [v f]
  (fn [& args]
    (apply f (map #(if (nil? %) v %) args))))

(defn ->vec [v] (if (sequential? v) (vec v) [v]))

(defn q
  [names & {:keys [s f o d l ->str] :or {s [:name :gender :o]
                                         f (constantly true)
                                         o nil
                                         d (nil-> 1001 <)
                                         l 10
                                         ->str true}}]
  (let [o+ (or o (constantly nil))
        o-fn #(d (o+ %) (o+ %2))
        view-fn (fn [v]
                  {:o (o+ v)
                   :v v
                   :name (-> v first first)
                   :gender (-> v first second)})]
    (let [f-results (filter
                     (apply every-pred (->vec f))
                     names)
          m-results (map #((apply juxt s) (view-fn %))
                         (if o
                           (sort o-fn f-results)
                           f-results))
          l-results (take l m-results)
          results (if ->str
                    (ppr l-results)
                    l-results)]
      results)))

(defn f-
  [& args]
  (fn [v]
    (loop [v+ v
           a args]
      (if (not-empty a)
        (let [a1 (first a)]
          (recur (cond
                  (fn? a1) (a1 v+)
                  (vector? a1) (apply (first a1) v+ (rest a1))
                  true (get v+ a1))
                 (rest a)))
        v+))))

(defn o-
  [& args]
  (fn [n]
    (get-in n args)))

(def p- partial)

(defn m-
  [f]
  (fn [args]
    (map f args)))

(defn a-
  [f & args]
  (fn [v]
    (apply f (into v args))))

(def n (:names @state))

(def n1 (first n))

(def n1-3 (take 3 n))

(def fst-yr (f- 1 :yrs-v first :yr))

#_(q n
     :s [:name :gender :o (f- :v fst-yr)]
     :f (every-pred (f- fst-yr < 1900) (f- 1 :yrs-v last :yr > 2000))
     :o (f- 1 :pct (m- :pct) (a- max))
     :d (nil-> 0 <)
     :l 30 :->str false)

#_(q n :o #(-> % second :yrs-v first :yr) :l 30 :->str false)

#_ (q n)

(defn get-yrs [n-rec yr1 & [yr2]]
  (filter #((set (range yr1 (inc (or yr2 yr1)))) (first %))
          (-> n-rec second :yrs)))

(get-yrs n1 2000 2002)



(vals {:a 2})
(q n :s [identity] :o (fn [x] (println (-> x second :yrs (get 2001) :rank)) 4))

(q n :s [identity] :o (fn [x] (-> x second :yrs (get 2001) :rank)))
