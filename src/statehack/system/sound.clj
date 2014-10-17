(ns statehack.system.sound
  (:require [clojure.java.io :as io]
            [clj-audio.core :as audio])
  (:import [clojure.lang ISeq Seqable Sequential]
           [javax.sound.sampled AudioSystem DataLine$Info Clip]))

(def resources
  {:gatling-wind-up "gatling-wind-up.wav"
   :gatling-wind-down "gatling-wind-down.wav"
   :gatling-wind-loop "gatling-wind-loop.wav"
   :punch-02 "punch-02.wav"
   :man-dying "man-dying.wav"
   :door "sshock/00206.wav"
   :airlock-door "sshock/00204.wav"
   :blast-door "sshock/00268.wav"
   :ion-rifle "sshock/00296.wav"
   :gauss-rifle "sshock/00230.wav"
   :pistol "sshock/00240.wav"
   :magnum-2100 "sshock/00241.wav"})

(defn load-resource [name]
  (or (io/resource (str "sounds/" name))
      (throw (ex-info (str "No such sound resource found") {:name name}))))

(defn load-stream [name]
  (-> name resources load-resource audio/->stream))

(defn play [name]
  (with-open [s (load-stream name)]
    (audio/play s)))
