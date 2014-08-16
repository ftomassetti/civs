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
    [miner.tagged :as tag])
  (:import [civs.model Population Tribe Culture Game Population Town])
  (:use clojure.java.io))

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
        (throw RuntimeException. (str "Cannot find " world-filename)))
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

(defn save-simulation-result [simulation-result history-filename world-filename]
  (let [ str (to-serialized-str simulation-result {:world-filename world-filename})]
    (spit history-filename str)))

