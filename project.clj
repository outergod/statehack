(defproject statehack "0.1.0-SNAPSHOT"
  :description "System Shock Roguelike"
  :url "https://github.com/e-user/statehack"
  :license {:name "GNU General Public License v3 (or later)"
            :url "http://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojars.wmealing/clj-audio "0.2.0-SNAPSHOT"]
                 [com.googlecode.lanterna/lanterna "3.0.0-alpha3"]
                 [com.googlecode.soundlibs/vorbisspi "1.0.3-1"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :main statehack.core}
             :dev {:resource-paths ["dev"]
                   :dependencies [[criterium "0.4.3"]
                                  [com.taoensso/timbre "3.4.0"]]}})
