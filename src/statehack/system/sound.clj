;;;; This file is part of statehack.
;;;;
;;;; statehack is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; statehack is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with statehack.  If not, see <http://www.gnu.org/licenses/>.

(ns statehack.system.sound
  (:require [clojure.java.io :as io]
            [clj-audio.core :as audio]
            [clj-audio.sampled :as sampled]))

(defonce mixer (first (audio/mixers)))

(defn cleanup []
  (when (.isOpen mixer) (.close mixer)))

(defn init []
  (cleanup)
  (if (= (.getName (class mixer)) "org.classpath.icedtea.pulseaudio.PulseAudioMixer")
    (.openLocal mixer "statehack")
    (.open mixer)))

(def sound-resources
  {:gatling-wind-up "gatling-wind-up.wav"
   :gatling-wind-down "gatling-wind-down.wav"
   :gatling-wind-loop "gatling-wind-loop.wav"
   :punch-02 "punch-02.wav"
   :man-dying "man-dying.wav"

   :player-hurt "sshock/00265.wav"
   
   :door "sshock/00206.wav"
   :airlock-door "sshock/00204.wav"
   :blast-door "sshock/00268.wav"

   :ion-rifle "sshock/00296.wav"
   :gauss-rifle "sshock/00230.wav"
   :dart "sshock/00287.wav"
   :pistol "sshock/00240.wav"
   :flechette "sshock/00239.wav"
   :magnum-2100 "sshock/00241.wav"
   :mark-3 "sshock/00218.wav"
   :skorpion "sshock/00266.wav"
   :plasma "sshock/00298.wav"
   :magpulse "sshock/00246.wav"

   :serv-bot-spot "sshock/00275.wav"
   :vmail "sshock/00293.wav"
   :radiation "sshock/00203.wav"

   :appendage-attack "sshock/00256.wav"
   :hopper-attack "sshock/00213.wav"

   :unknown-assault-rifle-2 "sshock/00210.wav"
   :unknown-assault-rifle-1 "sshock/00292.wav"})

(def music-resources
  {:medical "chicajo/Medical.ogg"})

(defn load-resource [prefix name]
  (io/resource (str prefix "/" name)))

(defn load-sound-resource [name]
  (or (load-resource "sounds" name)
      (throw (ex-info (str "No such sound file found") {:name name}))))

(defn load-music-resource [name]
  (or (load-resource "music" name)
      (throw (ex-info (str "No such music file found") {:name name}))))

(defn load-sound-stream [name]
  (if-let [file (sound-resources name)]
    (load-sound-resource file)
    (throw (ex-info (str "No such sound resource found") {:name name}))))

(defn load-music-stream [name]
  (if-let [file (music-resources name)]
    (load-music-resource file)
    (throw (ex-info (str "No such music resource found") {:name name}))))

(defn- play-stream [s]
  (audio/with-mixer mixer
    (binding [audio/*playing* (ref false)]
      (audio/play s))))

(defn play-sound [name]
  (with-open [s (audio/->stream (load-sound-stream name))
              d (sampled/convert s audio/*default-format*)]
    (play-stream d)))

(defn play-music [name]
  (with-open [s (audio/->stream (load-music-stream name))
              d (audio/decode s)]
    (play-stream d)))
