(ns
  ^{:author ftomassetti}
  civs.io
  (:require
    [civs.cli :refer :all]
    [civs.model :refer :all]
    [civs.logic :refer :all]
    [civs.logic.basic :refer :all]
    [civs.logic.demographics :refer :all]
    [civs.logic.tribe-choices :refer :all]
    [civs.logic.stats :refer :all]
    [civs.society :refer :all]
    [civs.graphics :refer :all]
    [clojure.tools.cli :refer [parse-opts]]
    [clojure.string :as string]
    [miner.tagged :as tag]
    [clojure.data.fressian :as fress])
  (:import [civs.model Population Tribe Culture Game Population Town]
           [org.fressian.handlers WriteHandler ReadHandler ILookup WriteHandlerLookup])
  (:use clojure.java.io))

(import '(com.github.lands.World))
(import '(com.github.lands.IncorrectFileException))
(import '(java.io.IOException))

(defmethod print-method Culture [this w]
  (tag/pr-tagged-record-on this w))

(defmethod print-method Tribe [this w]
  (tag/pr-tagged-record-on this w))

(defmethod print-method Game [this w]
  (tag/pr-tagged-record-on this w))

(defmethod print-method Population [this w]
  (tag/pr-tagged-record-on this w))

(defmethod print-method Town [this w]
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
    (.toString writer)))

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

(defn from-serialized-str [serialized-str options]
  (let [ resolver (:resolver options)
         unserialized-data (clojure.edn/read-string
                            {
                              :readers {
                                         'com.github.lands.World (world-reader resolver)
                                         'clojure.lang/PersistentArrayMap PersistentArrayMapReader
                                       }
                              :default tag/tagged-default-reader
                            } serialized-str)
        original-data unserialized-data]
    original-data))

(defn save-simulation-result-edn [simulation-result history-filename world-filename]
  (let [ str (to-serialized-str simulation-result {:world-filename world-filename})]
    (spit history-filename str)))

(def world-ftag "world")

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

(def fress-write-handlers
  (-> (merge {com.github.lands.World {world-ftag world-fwriter}}
        fress/clojure-write-handlers)
    fress/associative-lookup
    fress/inheritance-lookup))

(defn- fress-read-handlers [resolve]
  (-> (merge {world-ftag (world-freader resolve)} fress/clojure-read-handlers)
          fress/associative-lookup))

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

(defn save-simulation-result-fressian [simulation-result history-filename]
  (let [ bytes (to-serialized-bytes simulation-result)
         bos (java.io.BufferedOutputStream. (java.io.FileOutputStream. (as-file history-filename)))]
    (.write bos bytes)
    (.flush bos)
    (.close bos)))

(defn load-simulation-result-fressian [history-filename resolve]
  (let [ raf (java.io.RandomAccessFile. history-filename "r")
         ba (byte-array (.length raf))]
    (.read raf ba)
    (from-serialized-bytes ba resolve)))

(defn save-simulation-result [simulation-result history-filename world-filename use-fressian]
  (if use-fressian
    (save-simulation-result-fressian simulation-result history-filename)
    (save-simulation-result-edn simulation-result history-filename world-filename)))
