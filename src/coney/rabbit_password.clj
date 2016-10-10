(ns coney.rabbit-password
  (:require[clojure.data.codec.base64 :as b64]
           [digest])
)

; http://stackoverflow.com/a/10065003
(defn- hexify [s]
  (apply str (map #(format "%02x" %) s)))

(defn- unhexify [s]
  (let [bytes (into-array Byte/TYPE
                 (map (fn [[x y]]
                    (unchecked-byte (Integer/parseInt (str x y) 16)))
                       (partition 2 s)))]
    bytes))

; based on https://gist.github.com/christianclinton/faa1aef119a0919aeb2e
(defn- decode-rabbit-password-hash [password-hash]
  (let [password-hash (b64/decode (.getBytes password-hash))
        decoded-hash (hexify password-hash)]
        [(apply str (take 8 decoded-hash)) (apply str (drop 8 decoded-hash))]
  )
)

(defn- encode-rabbit-password-hash [salt password]
  (let [salt-and-password (unhexify (str salt (hexify (.getBytes password "UTF-8"))))
        salted-md5 (digest/md5 salt-and-password)]
    (String. (b64/encode (unhexify (str salt salted-md5))))
  )
)

;; TODO this doesn't work
(defn check-rabbit-password [test-password password-hash]
  (if (or (contains? #{"" nil} password-hash) (nil? test-password))
    false
    (let [[salt hash-md5sum] (decode-rabbit-password-hash password-hash)
          test-password-hash (encode-rabbit-password-hash salt test-password)]
      (= test-password-hash password-hash)
    )
  )
)
