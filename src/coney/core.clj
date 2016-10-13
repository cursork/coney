(ns coney.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as cset]
            [org.httpkit.client :as http]
            [cheshire.core :as cheshire]
            [coney.rabbit-password :as rp]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :refer [join]]))

(def root (atom "http://localhost:15672/api/"))
(def basic-auth (atom ["guest" "guest"]))

(defn core-params [] {:basic-auth @basic-auth :headers {"content-type" "application/json"}})

(defn expected-code [resp code]
  (if (= (:status resp) code)
    resp
    (do
      (println (format "Wanted HTTP code %d but got %d" code (:status resp)))
      (println resp)
      (throw (RuntimeException. (str resp))))))

(defn api-get
  [path-parts]
  (-> (apply str @root (interpose "/" path-parts))
      (http/get (core-params))
      deref
      :body
      (cheshire/decode true)))

(defn api-put
  [path-parts data]
  (let [req (merge (core-params) {:body (cheshire/encode data)})
        url (apply str @root (interpose "/" path-parts))]
    ;(prn :PUT url req)
    (-> (http/put url req)
        deref
        (expected-code 204))))

(defn api-post
  [path-parts data]
  (let [req (merge (core-params) {:body (cheshire/encode data)})
        url (apply str @root (interpose "/" path-parts))]
    ;(prn :POST url req)
    (-> (http/post url req)
        deref
        (expected-code 201))))

(def cli-options [["-h" "--help"]
                  [nil "--url URL" :default "http://localhost:15672/api/"]
                  ["-f" "--filetype FILETYPE" :default :json :parse-fn #(keyword %)]
                  [nil "--username USERNAME" :default "guest"]
                  [nil "--password PASSWORD" :default "guest"]
                  [nil "--dry-run"]])

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (join \newline errors)))

(defn usage [options-summary]
  (str "Usage: \n" options-summary))

(defn file-exists [path]
  (.exists (io/as-file path)))

(defn parse-file [filetype fname]
  (case filetype
    :edn (try
           (edn/read-string (slurp fname))
           (catch RuntimeException e (exit 1 (format "Bad EDN file '%s'" fname))))
    :json (cheshire/parse-string (slurp fname) true)
    (exit 1 (error-msg [(format "Don't know file format '%s'" (name filetype))]))))

(defn keyed-by
  [f coll]
  (when coll
    (into {} (map #(do [(f %) %])) coll)))

(defn keyed-config
  [config]
  {:users       (keyed-by :name (:users config))
   :vhosts      (keyed-by :name (:vhosts config))
   :queues      (keyed-by :name (:queues config))
   :exchanges   (keyed-by :name (:exchanges config))
   :bindings    (keyed-by
                  #(join "-" ((juxt :destination :destination_type :arguments :routing_key :source) %))
                  (:bindings config))
   :permissions (group-by :user (:permissions config))})

(defn verify-config
  [config]
  (doseq [[nme {:keys [password] :as user}] (:users config)]
    (when-not password
      (throw (ex-info (str "No password for user " user) {}))))
  config)

(defn diff-by-keys
  [ks]
  (fn [existing wanted]
    (when (not= (:vhost existing) (:vhost wanted))
      (throw (ex-info "vhost clash" {:existing existing :wanted wanted})))
    (if (= (select-keys existing ks) (select-keys wanted ks))
      nil
      wanted)))

(def diff-fns
  {:users (fn [existing wanted]
            (when-not (rp/check-rabbit-password (:password wanted) (:password_hash existing))
              wanted))
   ;; Names are all that exist for a vhost, so there's nothing to compare,
   ;; it's definitely there.
   :vhosts      (fn [_ _] nil)
   :permissions (fn [existing-perms wanted-perms]
                  (cset/difference (set wanted-perms) (set existing-perms)))
   :queues      (diff-by-keys [:arguments :durable :auto_delete])
   :exchanges   (diff-by-keys [:arguments :internal :type :durable :auto_delete])
   :bindings    (diff-by-keys [:destination :destination_type :arguments :routing_key :source])})

(defn diff
  [existing-config wanted-config]
  (into
    {}
    (for [[top-k diff-fn] diff-fns]
      (let [all-existing (get existing-config top-k)
            all-wanted   (get wanted-config top-k)
            rfn          (fn [acc [wanted-k wanted-v]]
                           (let [existing-v (get all-existing wanted-k)]
                             (if-not existing-v
                               (assoc acc wanted-k wanted-v)
                               (if-let [to-add (diff-fn existing-v wanted-v)]
                                 (assoc acc wanted-k to-add)
                                 acc))))]
        (if (seq all-wanted)
          [top-k (reduce rfn {} all-wanted)]
          nil)))))

(def apply-fns
  {:users (fn [user]
            (api-put ["users" (:name user)] user))
   :vhosts (fn [{vhost :name}]
             (api-put ["vhosts" (http/url-encode vhost)] {:name vhost})
             ;; TODO - look at what the existing code does - it does more than
             ;; just create them. But... is it actually needed? What's
             ;; different to the below?
             )
   :permissions (fn [perms-coll]
                  (doseq [{:keys [vhost user] :as perms} perms-coll]
                    (api-put ["permissions" (http/url-encode vhost) user] perms)))
   :queues (fn [{:keys [vhost name] :as queue}]
             (api-put ["queues" (http/url-encode vhost) name] queue))
   :exchanges (fn [{:keys [vhost name] :as exchange}]
                (api-put ["exchanges" (http/url-encode vhost) name] exchange))
   :bindings (fn [{:keys [vhost source] :as abinding}]
               (api-post ["bindings" (http/url-encode vhost) "e" source] abinding))})

(def application-order [:users :vhosts :permissions :queues :exchanges :bindings])

(defn apply-diff
  [diff]
  (doseq [top-k application-order]
    (println "Applying" top-k)
    (let [things-to-add (get diff top-k)
          apply-fn (get apply-fns top-k)]
      (doseq [thing (vals things-to-add)]
        (apply-fn thing)))))

(defn run
  [filetype filename dry-run]
  (let [existing (->> (api-get ["definitions"]) keyed-config)
        wanted   (->> (parse-file filetype filename) keyed-config verify-config)
        dff      (diff existing wanted)]
    (println "Diff to apply:")
    (clojure.pprint/pprint dff)
    (when-not dry-run
      (apply-diff dff))))

(defn -main
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (when-let [url (:url options)] (reset! root url))
    (reset! basic-auth [(:username options) (:password options)])
    (cond
      (:help options) (exit 0 (usage summary))
      errors (exit 1 (error-msg errors))
      (not= (count arguments) 1) (exit 1 (format "Need a single file argument, but got %d arguments" (count arguments)))
      (not (file-exists (first arguments))) (exit 1 (error-msg [(format "No such file '%s'" (first arguments))]))
      :default (run (:filetype options) (first arguments) (:dry-run options)))))

