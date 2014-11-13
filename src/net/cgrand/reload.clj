(ns net.cgrand.reload)

(defmulti watch-url (fn [^java.net.URL url ns] (.getProtocol url)))

(defmethod watch-url :default [^java.net.URL url ns]
  #_(do nothing))

(defn auto-reload [ns]
  (doseq [dep (-> ns meta ::deps)]
    (watch-url dep ns)))

(defn- watch-service [ns]
  (::watch-fn 
    (alter-meta!
      ns (fn [m] 
           (if (::watch-fn m)
             m
             (assoc m
                    ::watch-fn 
                    (let [ws (-> (java.nio.file.FileSystems/getDefault)
                               .newWatchService)
                          file-paths (atom #{})]
                      (future
                        (loop []
                          (let [wk (.take ws)
                                ^java.nio.file.Path dir (.watchable wk)
                                paths (map (fn [^java.nio.file.WatchEvent e]
                                             (.resolve dir (.context e))) 
                                           (.pollEvents wk))]
                            (if (some @file-paths paths)
                              (do
                                (.close ws)
                                ;; file updates concurrent with reloading may be skipped
                                (println "Reloading" (ns-name ns))
                                (alter-meta! ns assoc ::deps #{}
                                  ::watch-fn nil)
                                (require (ns-name ns) :reload)
                                (auto-reload ns))
                              (do
                                (.reset wk)
                                (recur))))))
                      (fn [path]
                        (swap! file-paths conj path)
                        (.register (.getParent path) ws 
                          (into-array [java.nio.file.StandardWatchEventKinds/ENTRY_MODIFY]))))))))))

(def ^:private no-strings (into-array String nil))

(defmethod watch-url "file" [^java.net.URL url ns]
  (let [path (java.nio.file.Paths/get (.toURI url))
        ws (watch-service ns)]
    (ws path)))
