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

(ns statehack.http
  (:require [org.httpkit.server :as http]
            [compojure.route :as route]
            [compojure.core :refer [defroutes GET]]
            [compojure.handler :refer [site]]
            [clojure.java.io :as io]))

(defonce server (atom nil))
(def socket (atom nil))
(add-watch socket :init
           (fn [_ _ _ socket]
             (http/on-receive socket (fn [data] (http/send! socket data)))))

(defn socket-handler [request]
  (http/with-channel request channel (reset! socket channel)))

(defroutes routes
  (GET "/socket" [] socket-handler)
  (GET "/" [] (io/resource "public/index.html"))
  (route/resources "/")
  (route/not-found (io/resource "public/404.html")))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn start-server []
  (reset! server (http/run-server (site #'routes) {:port 8080})))
