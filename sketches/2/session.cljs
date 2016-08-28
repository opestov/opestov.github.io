(load-file "session.css.cljs")
(load-file "game.cljs")

(require 'cljsjs.react)
(require 'cljsjs.react.dom)

(ns o8v.game)

(do 
(def x (run-app "root"))
(def state (x 1))
(def queue (x 2))
(def enqueue (x 3))
(def dequeue (x 4))
(def stop (x 0))
)
(stop)


(enqueue {:tag "newgame"})
(enqueue {:tag "tick"})
(enqueue {:tag "settings"})
(enqueue {:tag "lastround"})
(enqueue "step3")

@queue
@state

(clear)
(render "root" nil new-game println)
(render "root" new-game (move new-game {:tag "lastround"}) println)

(def new-game
{
:return-words? true, :cards-in-hands 2, :personal? true, :explanation-tl 20,
:pair ["Alice" "asnethuenothueanosthuenoathuensotuheosnatuheoansthuaoesnuhoeansuthaoesntuh"],
:history {1 {:pair ["Alice" "Bob"], :ac '("a" "b")} 2 {:pair ["asnethuenothueanosthuenoathuensotuheosnatuheoansthuaoesnuhoeansuthaoesntuh" "Vasya"] :ac nil} 3 {:pair ["Bob" "Alice"] :ac '("c")} 4 {:pair ["Vasya" "asnethuenothueanosthuenoathuensotuheosnatuheoansthuaoesnuhoeansuthaoesntuh"] :ac '("d" "e" "f")} 5 {:pair ["Alice" "Bob"] :ac '("g")} 6 {:pair ["asnethuenothueanosthuenoathuensotuheosnatuheoansthuaoesnuhoeansuthaoesntuh" "Vasya"] :ac '("h" "i")} 7 {:pair ["Bob" "Alice"] :ac '("j" "k" "l")} 8 {:pair ["Vasya" "asnethuenothueanosthuenoathuensotuheosnatuheoansthuaoesnuhoeansuthaoesntuh"] :ac nil}}
:node "confirm-gameover", :hat #{"apple"}, :guess-tl 3,
:players ["Alice" "Bob" "asnethuenothueanosthuenoathuensotuheosnatuheoansthuaoesnuhoeansuthaoesntuh" "Vasya"], :step 4}
)

(def new-game
{
:node "transition"
:return-words? true :cards-in-hands 2 :personal? true :explanation-tl 5 :guess-tl 3
:pair ["Alice" "Vasya"]
:step 8
:history {1 {:pair ["Alice" "Bob"] :ac '("a" "b")} 2 {:pair ["Petya" "Vasya"] :ac nil} 3 {:pair ["Bob" "Alice"] :ac '("c")} 4 {:pair ["Vasya" "Petya"] :ac '("d" "e" "f")} 5 {:pair ["Alice" "Bob"] :ac '("g")} 6 {:pair ["Petya" "Vasya"] :ac '("h" "i")} 7 {:pair ["Bob" "Alice"] :ac nil} 8 {:pair ["Vasya" "Petya"] :ac '("j" "k" "l")}}
:hat #{"apple" "orange" "dictionary" " "ice"}
:players ["Alice" Petya" "Bob" "Vasya"]}
)

(render "root" nil new-game println)

(clear)
(defn clear []
  (let [e (js/document.getElementById "root")]
    (while (.-lastChild e          )
      (.removeChild e (.-lastChild e)))))

(normalize [:ul [:li nil {:className "xyz"} nil "1" {:style "red"}] [[:li "2"] [:li "3"]]]))


    <audio id="play" src="http://www.soundjay.com/button/beep-07.wav"></audio>

(.play (js/document.getElementById "play"))


(require-macros '[cljs.repl :refer [doc source]])
(require-macros '[cljs.test :refer [deftest is run-tests testing]])
(cljs.test/run-tests)

