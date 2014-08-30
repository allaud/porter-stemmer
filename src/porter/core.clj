(ns porter.core
  (:require [clojure.string :refer  (replace)]))


(def rvre #"^(.*?[аеиоуыэюя])(.*)$")
(def r1re #"^.*?[аеиоуыэюя][^аеиоуыэюя](.*)$")
(def vovel #"аеиоуыэюя")
(def perfectiveground #"(ав|авши|авшись|яв|явши|явшись|ив|ивши|ившись|ыв|ывши|ывшись)$")
(def reflexive #"(с[яь])$")
(def adjective #"(ее|ие|ые|ое|ими|ыми|ей|ий|ый|ой|ем|им|ым|ом|его|ого|ему|ому|их|ых|ую|юю|ая|яя|ою|ею)$")
(def verb #"(ила|ыла|ена|ейте|уйте|ите|или|ыли|ей|уй|ил|ыл|им|ым|ен|ило|ыло|ено|ят|ует|уют|ит|ыт|ены|ить|ыть|ишь|ую|ю|ала|ана|аете|айте|али|ай|ал|аем|ан|ало|ано|ает|ают|аны|ать|аешь|анно|яла|яна|яете|яйте|яли|яй|ял|яем|ян|яло|яно|яет|яют|яны|ять|яешь|янно)$")
(def noun #"(а|ев|ов|ие|ье|е|иями|ями|ами|еи|ии|и|ией|ей|ой|ий|й|иям|ям|ием|ем|ам|ом|о|у|ах|иях|ях|ы|ь|ию|ью|ю|ия|ья|я)$")
(def i #"и$")
(def derivational #"(ост|ость)$")
(def superlative #"(ейш|ейше)$")
(def nn #"н(н)$")
(def n #"(н)$")
(def p #"ь$")


(defn rv
  [word]
  (last (re-find rvre word)))

(defn r1 [word]
  (or (last (re-find r1re word)) ""))

(defn r2 [word]
  (-> word r1 r1))

(defn del [word regexp]
  (replace word regexp ""))

(defn step-1 [word]
  (if-let [ending (re-find perfectiveground word)]
    (del word perfectiveground)
    (let [word (del word reflexive)]
      (if-let [ending (re-find adjective word)]
        (del word adjective)
        (if-let [ending (re-find verb word)]
          (del word verb)
          (if-let [ending (re-find noun word)]
            (del word noun)
            word))))))

(defn step-2 [word]
  (del word i))

(defn step-3 [word]
  (if-let [r2part (r2 word)]
    (if-let [ending (re-find derivational r2part)]
      (del word derivational)
      word)
    word))

(defn step-4a [word]
  (if (re-find nn word)
    (del word n)
    word))

(defn step-4b [word]
  (if (re-find superlative word)
    (step-4a (del word superlative))
    word))

(defn step-4c [word]
  (if (re-find p word)
    (del word p)
    word))

(defn stem [word]
  (if (empty? (rv word))
    word
    (-> word
      step-1
      step-2
      step-3
      step-4a
      step-4b
      step-4c)))

(defn -main
  []
  (println (stem "красный")))
