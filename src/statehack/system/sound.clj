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
  (:require [statehack.system.unique :as unique]
            [statehack.system.world :as world]
            [statehack.entity :as entity]
            [clojure.java.io :as io]
            [clj-audio.core :as audio]
            [clj-audio.sampled :as sampled]
            [simple-time.core :as time]))

(defonce mixer (first (audio/mixers)))

(def fade-duration 5)
(def fade-minimum -20)

(def music-channels
  (agent (for [_ (range 2)]
           [(audio/with-mixer mixer
              (sampled/make-line :output audio/*default-format* audio/default-buffer-size))
            (ref false)])
         :error-mode :continue))

(def current-track (ref nil))

(def sound-resources
  {:player-hurt "00265.wav"
   
   :door "00206.wav"
   :airlock-door "00204.wav"
   :blast-door "00268.wav"

   :ion-rifle "00296.wav"
   :gauss-rifle "00230.wav"
   :dart "00287.wav"
   :pistol "00240.wav"
   :flechette "00239.wav"
   :magnum-2100 "00241.wav"
   :mark-3 "00218.wav"
   :skorpion "00266.wav"
   :plasma "00298.wav"
   :magpulse "00246.wav"

   :serv-bot-spot "00275.wav"
   :vmail "00293.wav"
   :radiation "00203.wav"

   :appendage-attack "00256.wav"
   :hopper-attack "00213.wav"

   :unknown-assault-rifle-2 "00210.wav"
   :unknown-assault-rifle-1 "00292.wav"})

(def music-resources
  {:theme "chicajo/SSTheme06Retro.ogg"
   :elevator "chicajo/SsElevator.ogg"
   :cyber "chicajo/CyberSpace.ogg"
   :medical "chicajo/Medical.ogg"
   :science "chicajo/SsLev2.ogg"
   :maintenance "chicajo/SsLev3.ogg"
   :storage "chicajo/SsLev4.ogg"

   :executive "chicajo/SsLev6.ogg"})

(defn stop-music []
  (send music-channels
        (fn [[[c1 p1] [c2 p2]]]
          (dosync (ref-set p1 false)
                  (ref-set p2 false)
                  (ref-set current-track nil))
          [[c1 p1] [c2 p2]])))

(defn cleanup []
  (stop-music)
  (when (.isOpen mixer) (.close mixer)))

(defn init []
  (cleanup)
  (if (= (.getName (class mixer)) "org.classpath.icedtea.pulseaudio.PulseAudioMixer")
    (.openLocal mixer "statehack")
    (.open mixer)))

(defn load-resource [prefix name]
  (io/resource (str prefix "/" name)))

(defn load-sound-resource [name]
  (or (load-resource "sshock/sounds" name)
      (throw (ex-info (str "No such sound file found") {:name name}))))

(defn load-music-resource [name]
  (or (load-resource "sshock/music" name)
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

(defn stop-playback [line playing]
  (dosync (ref-set playing false))
  (loop []
    (when (sampled/running? line)
      (Thread/sleep 100)
      (recur))))

(defn fade-volume [control duration f]
  (let [start (time/now)]
    (loop [now start]
      (let [delta (time/- now start)]
        (when (time/< delta duration)
          (sampled/value control (f delta))
          (recur (time/now)))))))

(defn fade-out [line playing]
  (when @playing
    (let [control (:master-gain (sampled/controls-map line))
          min (max fade-minimum (:minimum (sampled/control-info control)))
          duration (time/seconds->timespan fade-duration)]
      (fade-volume control duration
                   (fn [delta]
                     (* min (/ (time/timespan->total-milliseconds delta)
                               (time/timespan->total-milliseconds duration)))))
      (stop-playback line playing))))

(defn fade-in [line playing resource]
  (stop-playback line playing)
  (future (binding [audio/*playing* playing]
            (with-open [s (audio/->stream resource)
                        d (audio/decode s)]
              (audio/play-with d line))))
  (let [control (:master-gain (sampled/controls-map line))
        min (max fade-minimum (:minimum (sampled/control-info control)))
        duration (time/seconds->timespan fade-duration)]
    (sampled/value control min)
    (fade-volume control duration
                 (fn [delta]
                   (+ min
                      (* (Math/abs min)
                         (/ (time/timespan->total-milliseconds delta)
                            (time/timespan->total-milliseconds duration))))))
    (sampled/value control 0.0)))

(defn crossfade [resource]
  (fn [[[c1 p1] [c2 p2]]]
    (audio/with-mixer mixer
      (doseq [f [(future (fade-out c1 p1))
                 (future (fade-in c2 p2 resource))]]
        (deref f)))
    [[c2 p2] [c1 p1]]))

(defn play-music [name]
  (send music-channels (crossfade (load-music-stream name))))

(defn music-system [game]
  (let [{:keys [position floor]} (unique/unique-entity game :player)
        {:keys [music]} (->> (world/entities-at game floor [position])
                             (entity/filter-capable [:music]) first)]
    (when (and music (not= @current-track music))
      (dosync (ref-set current-track music))
      (play-music music))
    game))
