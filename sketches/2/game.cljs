(ns o8v.game
  (:require [cljs.test :refer-macros [deftest is]]))

(defonce beep
  (let [e (js/document.createElement "audio")]
    (.setAttribute e "src" "http://www.soundjay.com/button/beep-07.wav")
    (fn [_] (.play e))))

(defmulti move (fn [model msg] [(:node model) (:tag msg)]))
(defmethod move :default [model msg] model)

(defn next-pair [model]
  (let [p (:players model) n (count p)
        [a b] (:pair model)
        i (mod (inc (count (take-while #(not= a %1) p))) n)
        j (mod (inc (count (take-while #(not= b %1) p))) n)]
    (if (and (:personal? model) (= 0 i))
      (let [j+ (mod (inc j) n)]
        (if (= j+ i)
          [(nth p i) (nth p (mod (inc j+) n))]
          [(nth p i) (nth p j+)]))
      [(nth p i) (nth p j)])))

(deftest next-pair-tests
  (let [g1 {:players [0 1 2 3 4] :personal? true}
        g2 {:players [0 1 2 3] :personal? false}]
    (is (= [1 2] (next-pair (assoc g1 :pair [0 1]))))
    (is (= [1 0] (next-pair (assoc g1 :pair [0 4]))))
    (is (= [0 2] (next-pair (assoc g1 :pair [4 0]))))
    (is (= [1 3] (next-pair (assoc g2 :pair [0 2]))))
    (is (= [2 0] (next-pair (assoc g2 :pair [1 3]))))))

(defn complete-round [model]
  (let [r? (:return-words? model)
        hands (:hands (:round model))
        return-words (fn [m] (if r? (update m :hat into hands) m))]
    (-> model
      (assoc :node "transition")
      (assoc :pair (next-pair model))
      (return-words)
      (update :history assoc (:step model)
        {:pair (:pair model) :ac (get-in model [:round :ac])
          :rj (if r? '() hands) :hat (if r? hands '())})
      (dissoc :round))))

; {:tag "ac" :word "apple"}
(defmethod move ["explanation" "ac"] [model msg]
  (if-not (some #{(:word msg)} (:hands (:round model)))
    ; ignore word if it is not in the hands
    model
    ; word is correct and can be accepted
    (let [r (->
              (:round model)
              (assoc :hands (vec (remove #{(:word msg)} (:hands (:round model)))))
              (update :ac conj (:word msg)))]
      (cond
        ; take new word from the hat
        (seq (:hat model))
        [(assoc model :round r)
          {:cmd "choose" :coll (:hat model)
            :k (min (count (:hat model))
                  (- (:cards-in-hands model) (count (:hands r))))}]
        ; the hat is empty but we have words in the hands
        (seq (:hands r))
        (assoc model :round r)
        ; no more words
        :else
        (complete-round (assoc model :round r))))))

(deftest explanation*ac-1
  (let [x {:node "explanation" :hat #{"a" "b" "c"} :cards-in-hands 3
          :round {:hands ["pear" "apple" "orange"] :ac '("carrot")}}
        y+ {:tag "ac" :word "apple"}
        y- {:tag "ac" :word "tomato"}
        [z+ cmd] (move x y+)
        z- (move x y-)]
    (is (= '("apple" "carrot") (get-in z+ [:round :ac])))
    (is (= ["pear" "orange"] (get-in z+ [:round :hands])))
    (is (= {:cmd "choose" :coll (:hat x) :k 1} cmd))
    (is (= x z-))))

(deftest explanation*ac-2
  (let [x {:node "explanation" :hat #{"a"} :cards-in-hands 3
            :round {:hands ["pear" "apple"] :ac '("carrot")}}
        y {:tag "ac" :word "apple"}
        [z cmd] (move x y)]
    (is (= {:cmd "choose" :coll (:hat x) :k 1} cmd))))

(deftest explanation*ac-3
  (let [x {:node "explanation"
            :players ["A" "B" "C"] :pair ["C" "B"] :personal? true
            :step 10 :hat #{} :cards-in-hands 3 
            :round {:hands ["apple"] :ac '("carrot")}}
        y {:tag "ac" :word "apple"}
        z (move x y)]
    (is (= (:node z) "transition"))
    (is (= (:pair z) ["A" "B"]))
    (is (= (:history z) {10 {:pair ["C" "B"] :ac '("apple" "carrot")
                              :rj '() :hat '()}}))))

; {:tag "choice" :coll #("car" "map" "bug") :selection '("car" "bug")}
(defmethod move ["explanation" "choice"] [model msg]
  (let [waiting (min (count (:hat model))
        (- (:cards-in-hands model) (count (get-in model [:round :hands]))))]
    (if
      ; we need the specific number of words from the specific collection
      (and (= waiting (count (:selection msg))) (= (:coll msg) (:hat model)))
      (-> model
        (assoc :hat (apply disj (:hat model) (:selection msg)))
        (update-in [:round :hands] into (:selection msg)))
      ; ignore the message for another state
      model)))

(deftest explanation*choice-1
  (let [x {:node "explanation" :hat #{"a" "b" "c"} :cards-in-hands 3
          :round {:hands ["apple"]}}
        y {:tag "choice" :coll (:hat x) :selection '("a" "c")}
        z (move x y)]
    (is (= #{"b"} (:hat z)))
    (is (= #{"apple" "a" "c"} (set (get-in z [:round :hands]))))))

(deftest explanation*choice-2
  (let [x {:node "explanation" :hat #{"a"} :cards-in-hands 3
          :round {:hands ["apple"]}}
        y {:tag "choice" :coll (:hat x) :selection '("a")}
        z (move x y)]
    (is (= #{} (:hat z)))
    (is (= #{"apple" "a"} (set (get-in z [:round :hands]))))))

; {:tag "tick"}
(defmethod move ["explanation" "tick"] [model msg]
  (cond
    ; continue
    (< (inc (get-in model [:round :ticks])) (:explanation-tl model))
    (update-in model [:round :ticks] inc)
    ; move to the guess state
    (pos? (:guess-tl model))
    [(-> model (assoc :node "guess") (assoc-in [:round :ticks] 0))
      {:cmd "beep"}]
    ; complete round
    :else
    [(complete-round model) {:cmd "beep"}]))

(deftest explanation*tick-1
  (let [x {:node "explanation"
            :players ["A" "B" "C"] :pair ["C" "B"] :personal? true
            :step 10 :hat #{} :cards-in-hands 3
            :explanation-tl 20 :guess-tl 5
            :round {:hands ["apple"] :ac '("carrot") :ticks 18}}
        y {:tag "tick"}
        z (move x y)]
    (is (= z (update-in x [:round :ticks] inc)))))

(deftest explanation*tick-2
  (let [x {:node "explanation"
            :players ["A" "B" "C"] :pair ["C" "B"] :personal? true
            :step 10 :hat #{} :cards-in-hands 3
            :explanation-tl 20 :guess-tl 5
            :round {:hands ["apple"] :ac '("carrot") :ticks 19}}
        y {:tag "tick"}
        z (move x y)]
    (is (= z (assoc (assoc-in x [:round :ticks] 0) :node "guess")))))

(deftest explanation*tick-3
  (let [x {:node "explanation"
            :players ["A" "B" "C"] :pair ["C" "B"] :personal? true
            :step 10 :hat #{} :cards-in-hands 3
            :explanation-tl 20 :guess-tl 0
            :round {:hands ["apple"] :ac '("carrot") :ticks 19}}
        y {:tag "tick"}
        z (move x y)]
    (is (= "transition" (:node z)))))

; {:tag "giveup"}
(defmethod move ["explanation" "giveup"] [model msg]
  (complete-round model))

(deftest explanation*giveup-1
  (let [x {:node "explanation"
            :players ["A" "B" "C"] :pair ["C" "B"] :personal? true
            :step 10 :hat #{"orange"} :cards-in-hands 3
            :explanation-tl 20 :guess-tl 0 :return-words? true
            :round {:hands ["apple"] :ac '("carrot") :ticks 19}}
        y {:tag "tick"}
        z (move x y)]
    (is (= #{"apple" "orange"} (:hat z)))
    (is (= "transition" (:node z)))))

; {:tag "ac" :word "apple"}
(defmethod move ["guess" "ac"] [model msg]
  (if-not (some #{(:word msg)} (:hands (:round model)))
    ; ignore word if it is not in the hands
    model
    ; word is correct and can be accepted
    (let [r (->
              (:round model)
              (assoc :hands (vec (remove #{(:word msg)} (:hands (:round model)))))
              (update :ac conj (:word msg)))]
      (complete-round (assoc model :round r)))))

; {:tag "tick"}
(defmethod move ["guess" "tick"] [model msg]
  (if (< (inc (get-in model [:round :ticks])) (:guess-tl model))
    (update-in model [:round :ticks] inc)
    [(complete-round model) {:cmd "beep"}]))

(deftest guess*tick-1
  (let [x {:node "guess"
            :players ["A" "B" "C"] :pair ["C" "B"] :personal? true
            :step 10 :hat #{"a" "b"} :cards-in-hands 3 :return-words? false
            :explanation-tl 20 :guess-tl 5
            :round {:hands ["apple"] :ac '("carrot") :ticks 4}}
        y {:tag "tick"}
        z (move x y)]
    (is (= "transition" (:node z)))
    (is (= #{"a" "b"} (:hat z)))))

(def new-game
  {:node "transition"
  :step 0
  :personal? true :cards-in-hands 2 :return-words? true
  :explanation-tl 20 :guess-tl 3
  :hat #{}
  :players []
  :history {}})

(defmethod move ["transition" "settings"] [model msg]
  (assoc model :node "settings"))
(defmethod move ["settings" "back-from-settings"] [model msg]
  (assoc model :node "transition"))

(defmethod move ["transition" "lastround"] [model msg]
  (assoc model :node "lastround"))
(defmethod move ["lastround" "back-from-lastround"] [model msg]
  (assoc model :node "transition"))

(defmethod move ["settings" "options"] [model msg]
  (assoc model :node "options"))
(defmethod move ["options" "back-from-options"] [model msg]
  (assoc model :node "settings"))

(defmethod move ["settings" "players"] [model msg]
  (assoc model :node "players"))
(defmethod move ["players" "back-from-players"] [model msg]
  (assoc model :node "settings"))

(defmethod move ["settings" "words"] [model msg]
  (assoc model :node "words"))
(defmethod move ["words" "back-from-words"] [model msg]
  (assoc model :node "settings"))

; {:tag "go"}
(defmethod move ["transition" "go"] [model msg]
  (if (and (seq (:hat model)) (:pair model))
    ; to start round we need players and words
    [(-> model
        (assoc :node "explanation")
        (update :step inc)
        (assoc :round {:hands [] :ticks 0}))
      {:cmd "choose" :coll (:hat model)
        :k (min (count (:hat model)) (:cards-in-hands model))}]
    ; otherwise ignore message
    model))

(deftest transition*go-1
  (let [x {:cards-in-hands 3
            :pair ["A" "B"]
            :node "transition"
            :hat #{"a" "b"}
            :players ["A" "B" "C"]
            :step 10}
        x1- (dissoc x :pair)
        x2- (assoc x :hat #{})
        y {:tag "go"}
        [z cmd] (move x y)]
    (is (= "explanation" (:node z)))
    (is (= 11 (:step z)))
    (is (= cmd {:cmd "choose" :coll (:hat x) :k 2}))
    (is (= x1- (move x1- y)))
    (is (= x2- (move x2- y)))))

; {:tag "add-words" :coll ["apple" "orange"]}
(defmethod move ["words" "add-words"] [model msg]
  (update model :hat into (:coll msg)))

; {:tag "remove-words" :coll ["apple" "orange"]}
(defmethod move ["words" "remove-words"] [model msg]
  (assoc model :hat (apply disj (:hat model) (:coll msg))))


; {:tag "add-player" :name "Bob"}
(defmethod move ["players" "add-player"] [model msg]
  (if (some #{(:name msg)} (:players model))
    ; if player with this name already exists then ignore the message
    model
    (let [m (assoc (update model :players conj (:name msg)) :personal? true)]
      (if (= 2 (count (:players m)))
        (assoc m :pair (vec (:players m)))
        m))))

; {:tag "remove-player" :name "Bob"}
(defmethod move ["players" "remove-player"] [model msg]
  (if (some #{(:name msg)} (:players model))
    (let [[a b] (:pair model)
          m (assoc model :personal? true :players
              (filterv #(not= (:name msg) %1) (:players model)))]
      (cond
        ; not enough players
        (<= (count (:players model)) 2)
        (dissoc m :pair)

        (= a (:name msg))
        (assoc-in m [:pair 0] (first (remove #(= b %1) (:players m))))
        (= b (:name msg))
        (assoc-in m [:pair 1] (first (remove #(= a %1) (:players m))))

        :else
        m))
    ; if player with this name does not exist then ignore the message
    model))

; {:tag "change-speaker" :name "Bob"}
(defmethod move ["players" "change-speaker"] [model msg]
  (if (some #{(:name msg)} (:players model))
    (let [[a b] (:pair model)
          n (count (:players model))
          i (count (take-while #(not= (:name msg) %1) (:players model)))
          j (rem (+ i (quot n 2)) n)]
      (if (:personal? model)
        (if (= b (:name msg))
          (assoc model :pair [b a])
          (assoc-in model [:pair 0] (:name msg)))
        (assoc model :pair [(:name msg) (nth (:players model) j)])))

    ; if player with this name does not exist then ignore the message
    model))

; {:tag "change-listener" :name "Bob"}
(defmethod move ["players" "change-listener"] [model msg]
  (if (and (:personal? model) (some #{(:name msg)} (:players model)))
    (let [[a b] (:pair model)]
      (if (= a (:name msg))
        (assoc model :pair [b a])
        (assoc-in model [:pair 1] (:name msg))))
    ; new listener must exist and can be changed in pair game only
    model))

; {:tag "lift-player" :name "Bob"}
(defmethod move ["players" "lift-player"] [model msg]
  (if (some #{(:name msg)} (:players model))
    (let [v (:players model)
          i (count (take-while #(not= (:name msg) %1) v))
          v' (if (> i 0) (assoc  v (dec i) (get v i) i (get v (dec i))) v)
          n (count v')
          j (count (take-while #(not= (first (:pair model)) %1) v'))
          k (rem (+ j (quot n 2)) n)]
      (if (:personal? model)
        (assoc model :players v')
        (assoc model :players v' :pair [(nth v' j) (nth v' k)])))
    ; wrong message
    model))

; {:tag "set-explanation-tl" :value 20}
(defmethod move ["options" "set-explanation-tl"] [model msg]
  (assoc model :explanation-tl (:value msg)))

; {:tag "set-guess-tl" :value 5}
(defmethod move ["options" "set-guess-tl"] [model msg]
  (assoc model :guess-tl (:value msg)))

; {:tag "set-return-flag" :value true}
(defmethod move ["options" "set-return-flag"] [model msg]
  (assoc model :return-words? (:value msg)))

; {:tag "set-number-of-cards" :value 2}
(defmethod move ["options" "set-number-of-cards"] [model msg]
  (assoc model :cards-in-hands (:value msg)))

; {:tag "set-personal" :value true}
(defmethod move ["players" "set-personal"] [model msg]
  (if (:value msg)
    (assoc model :personal? true)
    (if (even? (count (:player model)))
      (let [a (first (:pair model))
            n (count (:players model))
            i (count (take-while #(not= a %1) (:players model)))
            j (rem (+ i (quot n 2)) n)]
        (assoc model :personal? false :pair [a (nth (:players model) j)]))
      model)))

; {:tag "fix" :word "pear" :status :ac})
(defmethod move ["lastround" "fix"] [model msg]
  (let [i (:step model) w (:word msg) y (:status msg)]
    (if-let [x (first (filter (fn [k] (some #{w} (((model :history) i) k)))
                        [:ac :rj :hat]))]
      (if (= x y) model
        (let [m (-> model
                  (update-in [:history i x] (partial remove #{w}))
                  (update-in [:history i y] conj w))]
          (cond
            (= x :hat) (update m :hat disj w)
            (= y :hat) (update m :hat conj w)
            :else m)))
      model)))

; {:tag "gameover"}
(defmethod move ["transition" "gameover"] [model msg]
  (assoc model :node "confirm-gameover"))
; {:tag "yes"}
(defmethod move ["confirm-gameover" "complete-gameover"] [model msg]
  {:node "results" :history (:history model) :hat (:hat model)}) 
; {:tag "no"}
(defmethod move ["confirm-gameover" "cancel-gameover"] [model msg]
  (assoc model :node "transition"))

; {:tag "reset"}
(defmethod move ["transition" "reset"] [model msg]
  (assoc model :node "confirm-reset"))
; {:tag "yes"}
(defmethod move ["confirm-reset" "complete-reset"] [model msg]
  new-game)
; {:tag "no"}
(defmethod move ["confirm-reset" "cancel-reset"] [model msg]
  (assoc model :node "transition"))

; {:tag "reset"}
(defmethod move ["transition" "reset"] [model msg]
  new-game)

; {:tag "reset"}
(defmethod move ["results" "reset"] [model msg]
  new-game)

(deftest foo1
  (-> new-game
    (move {:tag "settings"})
    (move {:tag "words"})
    (move {:tag "add-words" :coll ["apple" "orange" "pear"]})
    (move {:tag "back-from-words"})
    (move {:tag "players"})
    (move {:tag "add-player" :name "Alice"})
    (move {:tag "add-player" :name "Bob"})
    (move {:tag "add-player" :name "Petya"})
    (move {:tag "add-player" :name "Vasya"})
    (move {:tag "set-type" :personal? false})))

(deftest foo2
  (-> new-game
    (move {:tag "settings"})
    (move {:tag "words"})
    (move {:tag "add-words" :coll ["apple" "orange" "pear"]})
    (move {:tag "back-from-words"})
    (move {:tag "players"})
    (move {:tag "add-player" :name "Alice"})
    (move {:tag "add-player" :name "Bob"})
    (move {:tag "add-player" :name "Petya"})
    (move {:tag "add-player" :name "Vasya"})
    (move {:tag "back-from-players"})
    (move {:tag "options"})
    (move {:tag "set-explanation-tl" :explanation-tl 3})
    (move {:tag "set-guess-tl" :guess-tl 0})
    (move {:tag "set-number-of-cards" :cards-in-hands 1})
    (move {:tag "set-return-flag" :return-words? false})
    (move {:tag "back-from-options"})
    (move {:tag "back-from-settings"})
    (move {:tag "go"})))

(deftest foo3
  (-> new-game
    (move {:tag "settings"})
    (move {:tag "words"})
    (move {:tag "add-words" :coll ["apple" "orange" "pear"]})
    (move {:tag "back-from-words"})
    (move {:tag "players"})
    (move {:tag "add-player" :name "Alice"})
    (move {:tag "add-player" :name "Bob"})
    (move {:tag "add-player" :name "Petya"})
    (move {:tag "add-player" :name "Vasya"})
    (move {:tag "back-from-players"})
    (move {:tag "options"})
    (move {:tag "set-explanation-tl" :explanation-tl 3})
    (move {:tag "set-guess-tl" :guess-tl 0})
    (move {:tag "set-number-of-cards" :cards-in-hands 1})
    (move {:tag "set-return-flag" :return-words? false})
    (move {:tag "back-from-options"})
    (move {:tag "back-from-settings"})
    (move {:tag "go"}) (first)
    (move {:tag "choice" :coll #{"apple" "pear" "orange"} :selection ["apple"]})
    (move {:tag "ac" :word "apple"}) (first)
    (move {:tag "choice" :coll #{"pear" "orange"} :selection ["orange"]})
    (move {:tag "tick"})
    (move {:tag "tick"})
    (move {:tag "tick"})
    (move {:tag "lastround"})
    (move {:tag "fix" :word "orange" :status :hat})
    (move {:tag "back-from-lastround"})
    (move {:tag "gameover"})))


(defmulti view :node)

(defn- prev-round-info [model]
  (let [step (:step model)
        [a b] (:pair ((:history model) step))
        words (sort (:ac ((:history model) step)))]
    ["In the last round " a " explained "
      (cond
        (= 0 (count words)) ["zero words to " b "."]
        (= 1 (count words)) ["word \"" (first words) \"" to " b "."]
        :else (concat ["to " b " words \""]
                (interpose "\", \"" (drop-last words))
                ["\" and \"" (last words) "\"."]))]))

(defn- who-is-next [model]
  (let [n (count (:hat model))
        [a b] (:pair model)]
    ["The next players are " [:strong a] " and " [:strong b] ". "
    "The hat contains " n " word" (if (= n 1) "" "s") "."]))

(defmethod view "transition" [model]
  (if (= 0 (:step model))
    [[:h1 "New game"]
    (cond
      (and (= 0 (count (:hat model))) (< (count (:players model)) 2))
      [:p "To be able to start the game you need to configure players and
          add words."]
      (= 0 (count (:hat model)))
      [:p "To be able to start the game you need to add words to the hat."]
      (< (count (:players model)) 2)
      [:p "To be able to start the game you need to configure players."]
      :else
      [:p (who-is-next model)])
      (when (and (seq (:hat model)) (> (count (:players model)) 1))
        [:button {:class "go" :onclick (constantly {:tag "go"})} "Go!"])
      [:button {:onclick (constantly {:tag "settings"})} "Settings"]
      [:button {:onclick (constantly {:tag "reset"})} "Reset"]]
    ; else
    [[:h1 (str "Transition " (:step model))]
    [:p
      [(prev-round-info model) " "]
      (cond
        (= 0 (count (:hat model)))
        ["There are no more words in the hat. End the game to view the scores.
        Alternatively you can add words and continue."]
        (< (count (:players model)) 2)
        ["There is not enough players to continue."]
        :else (who-is-next model))]
      (when (and (seq (:hat model)) (> (count (:players model)) 1))
        [:button {:class "go" :onclick (constantly {:tag "go"})} "Go!"])
      [:button {:onclick (constantly {:tag "lastround"})} "Edit last round"]
      [:button {:onclick (constantly {:tag "settings"})} "Settings"]
      [:button {:onclick (constantly {:tag "gameover"})} "End game"]]))

(defmethod view "settings" [model]
  [[:h1
    [:a {:href "#" :class "back"}
      {:onclick (constantly {:tag "back-from-settings"})} "←"]
    "Settings"]
  [:button {:onclick (constantly {:tag "players"})} "Players"]
  [:button {:onclick (constantly {:tag "words"})} "Words"]
  [:button {:onclick (constantly {:tag "options"})} "Options"]])

(defmethod view "words" [model]
  (let [parts (fn [e] (remove clojure.string/blank?
                        (clojure.string/split (.-value (.-target e)) #"\s+")))
        add (fn [e] {:tag "add-words" :coll (parts e)})
        del (fn [e] {:tag "remove-words" :coll (parts e)})]
    [[:h1
      [:a {:href "#" :class "back"}
          {:onclick (constantly {:tag "back-from-words"})} "←"]
      "Words"]    
    [:p 
      (if (= 1 (count (:hat model)))
        ["There is " [:strong "1"] " word in the hat."]
        ["There are " [:strong (str (count (:hat model)))] " words in the hat."])
      " Enter new ones here (separate them by space)."]
    [:input {:class "textbox" :type "text" :onchange add}]]))

(defmethod view "players" [model]
  (let [opt (fn [a] (fn [x] [:option (when (= a x) {:selected true}) x]))
        val (fn [e] (.-value (.-target e)))
        parts (fn [e] (remove clojure.string/blank?
                        (clojure.string/split (val e) #"\s+")))
        name (fn [e] (.-innerText (.-firstChild (.-parentNode (.-target e)))))]
    [[:h1
      [:a {:href "#" :class "back"}
        {:onclick (constantly {:tag "back-from-players"})} "←"]
      "Players"]
    (when (seq (:players model))
      [:ul {:class "players"}
        (for [x (:players model)]
          [:li
            [:label x]
            [:button {:class "up-btn"
                      :onclick (fn [e] {:tag "lift-player" :name (name e)})}
              "↑"]
            [:button {:class "del-btn"
                      :onclick (fn [e] {:tag "remove-player" :name (name e)})}
              "×"]])])
    [:p "Enter unique names of a new players (separate them by space)"]
    [:input {:class "textbox" :type "text"
              :onchange (fn [e] (mapv (fn [x] {:tag "add-player" :name x})
                                  (parts e)))}]
    (when-let [[a b] (:pair model)]
      [[:p "Game type:"]
      [:select (when (odd? (count (:players model))) {:disabled ""})
        {:onchange (fn [e] {:tag "set-personal"
                            :value (= "Personal" (val e))})}
        [:option (when-not (:personal? model) {:selected true}) "Pair"]
        [:option (when (:personal? model) {:selected true}) "Personal"]]
      [:p "Who explain in the next round?"]
      [:select {:class "dropdown"}
        {:onchange (fn [e] {:tag "change-speaker" :name (val e)})}
        (map (opt a) (:players model))]
      [:p "Who guess in the next round?"]
      [:select (when (not (:personal? model)) {:disabled ""})
        {:class "dropdown"}
        {:onchange (fn [e] {:tag "change-listener" :name (val e)})}
        (map (opt b) (remove #(= a %1) (:players model)))]])]))

(defmethod view "options" [model]
  (let [opt (fn [a] (fn [x] [:option (when (= a x) {:selected true}) x]))
        handler (fn [key f]
                  (fn [e] {:tag key :value (f (.-value (.-target e)))}))]
    [[:h1
      [:a {:href "#" :class "back"}
        {:onclick (constantly {:tag "back-from-options"})} "←"]
      "Options"]
    [:p "Round duration:"]
    [:select {:onchange (handler "set-explanation-tl" js/parseInt)}
      (map (opt (:explanation-tl model)) (range 5 41 5))]
    [:p "Extra seconds for a guess:"]
    [:select {:onchange (handler "set-guess-tl" js/parseInt)}
      (map (opt (:guess-tl model)) (range 0 6 1))]
    [:p "Number of cards in hands:"]
    [:select {:onchange (handler "set-number-of-cards" js/parseInt)}
      (map (opt (:cards-in-hands model)) (range 1 3 1))]
    [:p "Return words to the hat"]
    [:select {:onchange (handler "set-return-flag" (fn [x] (= "Yes" x)))}
      (map (opt (if (:return-words? model) "Yes" "No")) ["Yes" "No"])]]))

(defmethod view "lastround" [model app-class]
  (let [step (:step model)
        app (.item (js/document.getElementsByClassName app-class) 0)
        jfn (fn [x] (juxt identity (constantly (name x))))
        words (sort (concat
                      (map (jfn :ac) (get-in model [:history step :ac]))
                      (map (jfn :rj) (get-in model [:history step :rj]))
                      (map (jfn :hat) (get-in model [:history step :hat]))))
        inp (fn [word type radio-type]
              [:input {:type "radio" :name word :data-t radio-type}
                (when (= type radio-type) {:checked ""})])
        bfn (fn [_]
              (let [coll (.getElementsByTagName app "input")]
                (-> 
                  (keep
                    (fn[i]
                      (let [r (.item coll i)]
                        (when (.-checked r)
                          {:tag "fix" :word (.-name r)
                            :status (keyword (.getAttribute r "data-t"))})))
                    (range (.-length coll)))
                  (vec)
                  (conj {:tag "back-from-lastround"}))))]
    [[:h1
      [:a {:href "#" :class "back" :onclick bfn} "←"] "Last round"]
    [:ul
      (map (fn [[x y]] [:li (inp x y "ac") (inp x y "rj") (inp x y "hat") x])
        words)]]))

(defonce font-context (.getContext (js/document.createElement "canvas") "2d"))
(defn font-size
  ([s f w h] (font-size s f w 0 (quot (* 2 h) 3)))
  ([s f w l r]
    (if (= r (inc l)) l
      (let [m (quot (+ l r) 2)]
        (set! (.-font font-context) (str "bold " m "px " f))
        (if (> (.-width (.measureText font-context s)) (- w 10))
          (font-size s f w l m)
          (font-size s f w m r))))))

(defn app-font [class]
  (.getPropertyValue 
    (js/window.getComputedStyle
      (.item (js/document.getElementsByClassName class) 0) 
      nil)
    "font-family"))

(defn app-width [class]
  (let [root (.item (js/document.getElementsByClassName class) 0)
        styles (js/window.getComputedStyle root nil)
        px (fn [s]
            (if-not (clojure.string/ends-with? s "px") 0
              (js/parseInt (subs s 0 (- (count s) 2)) 0)))]
    (- (.-clientWidth root)
      (px (.getPropertyValue styles "padding-left"))
      (px (.getPropertyValue styles "padding-right")))))

(defmethod view "explanation" [model app-class]
  (let [f (app-font app-class) w (app-width app-class) h 80]
    [[:h1 (str "Explanation " (:step model))]
      [:div {:class "progress"}
        {:style (str "width: " (/ (* (:ticks (:round model)) w)
                                (:explanation-tl model)) "px")}]
      (map
        (fn [x]
          [[:button {:class "word" :onclick (constantly {:tag "ac" :word x})}
              {:style (str "height: " h "px; font: " (font-size x f w h) "px " f)}
              x]])
        (:hands (:round model)))
      [:button {:onclick (constantly {:tag "giveup"})} "Give up"]]))

(defmethod view "guess" [model app-class]
  (let [f (app-font app-class) w (app-width app-class) h 80]
    [[:h1 (str "Guess " (:step model))]
      [:div {:class "progress"}
        {:style (str "width: " (/ (* (:ticks (:round model)) w)
                                (:guess-tl model)) "px")}]
      (map
        (fn [x]
          [[:button {:class "word" :onclick (constantly {:tag "ac" :word x})}
              {:style (str "height: " h "px; font: " (font-size x f w h) "px " f)}
              x]])
        (:hands (:round model)))]))

(defmethod view "confirm-gameover" [model msg]
  [[:h1 "Confirmation"]
  [:p "Do you really want to end the game and view the results?"]
  [:button {:onclick (constantly {:tag "complete-gameover"})} "Yes"]
  [:button {:onclick (constantly {:tag "cancel-gameover"})} "No"]])

(defmethod view "confirm-reset" [model msg]
  [[:h1 "Confirmation"]
  [:p "Do you really want to reset the game?"]
  [:button {:onclick (constantly {:tag "complete-reset"})} "Yes"]
  [:button {:onclick (constantly {:tag "cancel-reset"})} "No"]])

(defmethod view "results" [model]
  (let [p (sort (set (mapcat (fn [[k v]] (:pair v)) (:history model))))
        a1 (mapcat (fn [[k v]] [[((:pair v) 0) (count (:ac v))]
                                [((:pair v) 1) (count (:ac v))]])
              (:history model))
        s1 (map (fn [[k v]] [(reduce + (map second v)) k]) (group-by first a1))
        a2 (map (fn [[k v]] [(:pair v) (count (:ac v))]) (:history model))
        s2 (map (fn [[k v]] [(reduce + (map second v)) k])
            (group-by (comp vec sort first) a2))]
    [[:h1 "Statisticts"]
      [:h2 "Pairs"]
      [:ul (map (fn [[k v]] [:li (str v) " " (str k)]) (reverse (sort s2)))]
      [:h2 "Personal"]
      [:ul (map (fn [[k v]] [:li v " " k]) (reverse (sort s1)))]
    [:button {:onclick (constantly {:tag "reset"})} "New game"]]))


(defn dom
  "Returns a collection of HTML elements."
  [root enqueue]
  (let [eh (fn [fun] (fn [e]
                      (let [a (fun e)]
                        (if (vector? a) (doseq [m a] (enqueue m)) (enqueue a)))))
        dfs (fn f [x]
              (cond
                (nil? x)
                  []
                (and (coll? x) (keyword? (first x)))
                  (let [element (js/document.createElement (name (first x)))]
                    (doseq [y (remove nil? (next x))]
                      (if (map? y)
                        (doseq [[k v] y]
                          (if (fn? v)
                            (aset element (name k) (eh v))
                            (.setAttribute element (name k) v)))
                        (doseq [child (f y)] (.appendChild element child))))
                    (vector element))
                (coll? x)
                  (mapcat f x)
                :else
                  (vector (js/document.createTextNode x))))]
    (dfs root)))

(defn render [app-class model enqueue]
  (let [app-element (.item (js/document.getElementsByClassName app-class) 0)]
    (while (.-lastChild app-element)
      (.removeChild app-element (.-lastChild app-element)))
    (doseq [x (dom (view model app-class) enqueue)]
      (.appendChild app-element x))
    (when-let [first-input (.item (js/document.getElementsByTagName "input") 0)]
      (.focus first-input))))

(defn handle [model msg]
  (let [x (move model msg)]
    (if (vector? x)
      (let [[m c] x]
        (case (:cmd c)
          "beep"
          (do (beep) m)
          
          "choose"
          (move m {:tag "choice" :coll (:coll c)
                    :selection (vec (take (:k c) (shuffle (:coll c))))})))
      x)))
