(ns statehack.system.sound
  (:require [statehack.system.world :as world]
            [statehack.util :as util]
            [clojure.java.io :as io])
  (:import [clojure.lang ISeq Seqable Sequential]
           [javax.sound.sampled AudioSystem DataLine$Info Clip]))

(defprotocol Closeable
  (close [this]))

(deftype CloseableSeq [^ISeq coll]
  ISeq
  (first [_] (.first coll))
  (next [_] (.next coll))
  (more [_] (.more coll))
  (cons [_ o] (.cons coll o))

  Sequential
  Seqable
  (seq [_] coll)
  
  Closeable
  (close [_] (doseq [e coll] (.close e))))

(def sound-resources
  {:gatling-wind-up "gatling-wind-up.wav"
   :gatling-wind-down "gatling-wind-down.wav"
   :gatling-wind-loop "gatling-wind-loop.wav"
   :punch-02 "punch-02.wav"})

(defn resource [name]
  (or (io/resource (str "sounds/" name))
      (throw (ex-info (str "No such sound resource found") {:name name}))))

(defn load-sound [res]
  (let [snd (AudioSystem/getAudioInputStream res)
        clip (->> snd .getFormat (DataLine$Info. Clip) AudioSystem/getLine)]
    (doto clip (.open snd))))

(defn sound-format [snd] (.getFormat snd))

(defn sound [name]
  (load-sound (resource (sound-resources name))))

(def sounds
  (into {} (map (fn [[k v]] [k (load-sound (resource v))]) sound-resources)))

(defn clip-length [clip]
  (float (/ (.getMicrosecondLength clip) (Math/pow 10 6))))

(defn multi-sound [name time]
  (let [res (resource (sound-resources name))
        s (load-sound res)
        n (Math/ceil (/ (clip-length s)
                        (/ time 1000)))]
    (CloseableSeq. (cons s (repeatedly n #(load-sound res))))))

(defn ready-sound
  ([sound offset]
     (doto sound .stop (.setFramePosition offset)))
  ([sound]
     (ready-sound sound 0)))

(defn play-sound [sound]
  (doto sound .start))

(defn play-multi-sound [sound n time]
  (dotimes [x n]
    (let [s (nth sound (mod x (count sound)))]
      (-> s ready-sound play-sound)
      (Thread/sleep time))))

(defn wait-sound
  ([sound n]
     (Thread/sleep (- (* (clip-length sound) 1000 n) 100))) ; - 100 -> seamless
  ([sound]
     (wait-sound sound 1)))

(defn hz->mseconds [hz]
  (int (/ 1000 hz)))

(defn play [name]
  (-> name sounds ready-sound play-sound))

#_(defn event [game name]
  (assoc-in game [:events] #(play name)))

(comment
  (let [t (sound/hz->mseconds 10)]
    (with-open [up (sound/sound :gatling-wind-up)
                down (sound/sound :gatling-wind-down)
                loop (sound/multi-sound :gatling-wind-loop t)]
      (-> up sound/ready-sound sound/play-sound sound/wait-sound)
      (dotimes [n 20]
        (let [s (nth loop (mod n (count loop)))]
          (-> s sound/ready-sound sound/play-sound)
          (Thread/sleep t)))
      (-> down sound/ready-sound sound/play-sound sound/wait-sound)
      (println (count loop)))))
