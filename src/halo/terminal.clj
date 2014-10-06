(ns halo.terminal
  (:import [java.nio.charset Charset]
           [com.googlecode.lanterna TerminalSize TerminalPosition]
           [com.googlecode.lanterna.terminal Terminal ResizeListener]
           [com.googlecode.lanterna.terminal.ansi UnixTerminal]))

(def charsets {:utf-8 (Charset/forName "UTF-8")})

(defn add-resize-listener
  [^Terminal terminal listener-fn]
  (let [listener (reify ResizeListener
                   (^void onResized [_ ^Terminal term ^TerminalSize size]
                     (listener-fn term (.getColumns size) (.getRows size))))]
    (.addResizeListener terminal listener)
    listener))

(defn terminal
  [& {:keys [cols rows charset resize-listener]
      :or {cols 80
           rows 24
           charset :utf-8
           resize-listener nil}}]
  (let [term (UnixTerminal. System/in System/out (charsets charset))]
    (when resize-listener
      (add-resize-listener term resize-listener))
    term))
