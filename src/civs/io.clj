(ns
  ^{:author ftomassetti}
  civs.io
  (:require
    [civs.cli :refer :all]
    [civs.model.core :refer :all]
    [civs.model.society :refer :all]
    [civs.model.history :refer :all]
    [civs.logic.core :refer :all]
    [civs.logic.basic :refer :all]
    [civs.logic.demographics :refer :all]
    [civs.logic.tribe-choices :refer :all]
    [civs.logic.stats :refer :all]
    [civs.graphics :refer :all]
    [clojure.tools.cli :refer [parse-opts]]
    [clojure.string :as string]
    [miner.tagged :as tag]
    [clojure.data.fressian :as fress])
  (:import [civs.model.core Population Group Culture Game Population Settlement PoliticalEntity]
           [org.fressian.handlers WriteHandler ReadHandler ILookup WriteHandlerLookup])
  (:use clojure.java.io))

(import '(com.github.lands.World))
(import '(com.github.langgen.Language))
(import '(com.github.lands.IncorrectFileException))
(import '(java.io.IOException))

(defmethod print-method Culture [this w]
  (tag/pr-tagged-record-on this w))

(defmethod print-method Group [this w]
  (tag/pr-tagged-record-on this w))

(defmethod print-method Game [this w]
  (tag/pr-tagged-record-on this w))

(defmethod print-method Population [this w]
  (tag/pr-tagged-record-on this w))

(defmethod print-method Settlement [this w]
  (tag/pr-tagged-record-on this w))

(defn set-world-print-method [world-filename]
  (defmethod print-method com.github.lands.World [this w]
    (.write w (str "#com.github.lands.World {:filename \"" world-filename "\"}"))))

(defn- to-serializable-data [original-data options]
  (assoc (dissoc original-data :world) :world-filename (:world-filename options)))

(defn- from-serializable-data [serializable-data options]
  (let [world-filename (:world-filename serializable-data)
        complete-world-filename ((:resolver options) world-filename)
        world (load-world complete-world-filename)]
    (assoc (dissoc serializable-data :world-filename) :world world)))

(defn dir-contains? [dir filename]
  (let [complete-filename (if (empty? dir) filename (str dir "/" filename))]
    (.exists (as-file complete-filename))))

(defn absolute-filename? [filename]
  (or
    (.isAbsolute (as-file filename))
    (if
      (re-matches (re-pattern "[A-Z]:\\\\.*") filename)
      true
      false)
    (.startsWith filename "/")))

(defn dir-lists-resolver [dirs]
  (fn [filename]
    (if (absolute-filename? filename)
      filename
      (first
        (map #(if (empty? %) filename (str % "/" filename)) (filter #(dir-contains? % filename) dirs))))))

(defn to-serialized-str [data options]
  (let [writer (java.io.StringWriter.)
        serializable-data data]
    (set-world-print-method (:world-filename options))
    (miner.tagged/pr-tagged-record-on serializable-data writer)
    (str writer)))

(defn world-reader [resolver]
  (fn [value]
    (let [ world-filename (:filename value)
           complete-world-filename (resolver world-filename)]
      (when
        (nil? complete-world-filename)
        (throw (java.lang.RuntimeException. (str "Cannot find " world-filename))))
      (load-world complete-world-filename))))

(defn PersistentArrayMapReader [value]
  value)

(def world-ftag "world")
(def language-ftag "language")

(def world-fwriter
  (reify WriteHandler
    (write [_ writer world]
      (.writeTag writer world-ftag 1)
      (.writeObject writer (.getName world) false))))

(defn- world-freader [resolve]
  (reify ReadHandler
    (read [_ reader tag component-count]
      (let [name (.readObject reader)]
        (resolve name)))))

(def language-fwriter
  (reify WriteHandler
    (write [_ writer language]
      (.writeTag writer language-ftag 1)
      (.writeObject writer (.getSamples language) false))))

(defn- language-freader []
  (reify ReadHandler
    (read [_ reader tag component-count]
      (let [samples (.readObject reader)]
        (com.github.langgen.Language. samples)))))

(def fress-write-handlers
  (-> (merge {
               com.github.lands.World      {world-ftag    world-fwriter}
               com.github.langgen.Language {language-ftag language-fwriter}
             }
        fress/clojure-write-handlers)
    fress/associative-lookup
    fress/inheritance-lookup))

(defn- fress-read-handlers [resolve]
  (fress/associative-lookup
    (merge
      {world-ftag (world-freader resolve),
       language-ftag (language-freader)}
      fress/clojure-read-handlers)))

(defn to-serialized-bytes [data]
  (let [os (java.io.ByteArrayOutputStream.)
        fw (fress/create-writer os :handlers fress-write-handlers)]
    (fress/write-object fw data)
    (.toByteArray os)))

(defn from-serialized-bytes [bytes resolve]
  (let [bais (java.io.ByteArrayInputStream. bytes)
        reader (fress/create-reader bais :handlers (fress-read-handlers resolve))
        out (fress/read-object reader)]
  out))

(defn- prepare-game-for-serialization
  "Return game, updated"
  [history game t prev-t]
  (reduce
    (fn [game group-id]
      (let [current-pe (political-entity-at history t group-id)
            prev-pe    (political-entity-at history prev-t group-id)]
        (if-not (nil? prev-pe)
          (if (= (.culture current-pe) (.culture prev-pe))
            (let [updated-pe (assoc current-pe :culture :unchanged)]
              (update-political-entity game (.id updated-pe) (fn [_ _]  updated-pe)))
          game)
        game)))
    game
    (political-entities-ids game)))

(defn- prepare-history-turn-for-serialization
  "Return history, updated"
  [history turn]
  (let [prev-t (dec turn)
        game (game-at history turn)
        game (prepare-game-for-serialization history game turn prev-t)]
    (update-game history turn game)))

(defn prepare-history-for-serialization
  "We replace elements unchanged from turn to turn with :unchanged"
  [history]
  (reduce prepare-history-turn-for-serialization history (rest (turns history))))

(defn- restore-game-from-serialization
  "Return game, updated"
  [history game t prev-t]
  (reduce
    (fn [game group-id]
      (let [current-group (political-entity-at history t group-id)
            prev-group    (political-entity-at history prev-t group-id)]
        (if-not (nil? prev-group)
          (if (= :unchanged (.culture current-group))
            (let [updated-group (assoc current-group :culture  (.culture prev-group))]
              (update-political-entity game (.id updated-group) (fn [_ _] updated-group)))
            game)
          game)))
    game
    (political-entities-ids game)))

(defn- restore-history-turn-from-serialization
  "Return history, updated"
  [history turn]
  (let [prev-t (dec turn)
        game (game-at history turn)
        game (restore-game-from-serialization history game turn prev-t)]
    (update-game history turn game)))

(defn restore-history-from-serialization [history]
  [history]
  (reduce restore-history-turn-from-serialization history (rest (turns history))))

(defn save-simulation-result-fressian [simulation-result history-filename]
  (let [ bytes (to-serialized-bytes (prepare-history-for-serialization simulation-result))
         bos (java.io.BufferedOutputStream. (java.io.FileOutputStream. (as-file history-filename)))]
    (.write bos bytes)
    (.flush bos)
    (.close bos)))

(defn load-simulation-result-fressian [history-filename resolve]
  (let [ raf (java.io.RandomAccessFile. history-filename "r")
         ba (byte-array (.length raf))]
    (.read raf ba)
    (restore-history-from-serialization (from-serialized-bytes ba resolve))))

(defn save-simulation-result [simulation-result history-filename world-filename use-fressian]
  (save-simulation-result-fressian simulation-result history-filename))
