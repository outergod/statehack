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
  (:require [aleph.http :as http]
            [manifold.stream :as stream]
            [manifold.deferred :as deferred]
            [compojure.route :as route]
            [compojure.core :refer [defroutes GET]]
            [compojure.handler :refer [site]]
            [clojure.java.io :as io]))

(defonce server (atom nil))
(def socket (atom nil))

(def non-websocket-request
  {:status 400
   :headers {"content-type" "application/text"}
   :body "Expected a websocket request."})

(defn socket-handler [request]
  (deferred/catch
      (deferred/let-flow [websocket (http/websocket-connection request)]
        (reset! socket websocket)
        nil)
      (fn [_] non-websocket-request)))

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
  (reset! server (http/start-server (site #'routes) {:port 8080})))
