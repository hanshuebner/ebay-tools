(ns ebay-tools.core
  (:require [clj-http.client :as http]
            [clojure.data.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip :as data.zip]
            [clojure.data.zip.xml :as zip.xml :refer [xml-> xml1-> text]]
            [clojure.string :as string]
            [clj-time.core :as time]
            [clj-time.format :as time-format]
            [hickory.core :as hickory]
            [hickory.zip :refer [hickory-zip]]
            [clojure.edn :as edn]
            [clojure.zip :as zip]
            [clojure.java.io :as io]))

(xml/alias-uri 'ebay "urn:ebay:apis:eBLBaseComponents")

(def credentials (edn/read-string (slurp "credentials.edn")))
(def cookie-store (clj-http.cookies/cookie-store))

(defn add-token [[element & children]]
  `[~element
    [::ebay/RequesterCredentials
     [::ebay/eBayAuthToken ~(:token credentials)]]
    ~@children])

(defn make-request-body [xml]
  (-> xml
      (add-token)
      (xml/sexp-as-element)
      (xml/indent-str)))

(defn find-element [zip tag]
  (loop [zip zip]
    (if (= (-> zip zip/node :tag) tag)
      (zip/node zip)
      (let [more (zip/next zip)]
        (when-not (zip/end? zip)
          (recur more))))))

(defn find-elements [zip tag]
  (loop [zip zip
         result []]
    (if (zip/end? zip)
      result
      (recur (zip/next zip)
             (if (= (-> zip zip/node :tag) tag)
               (conj result (zip/node zip))
               result)))))

(defn ebay-api-request [api-call-name xml]
  (let [xml (-> (http/request {:method :post
                               :url "https://api.ebay.com/ws/api.dll"
                               :body (make-request-body xml)
                               :headers {"X-EBAY-API-CALL-NAME" api-call-name
                                         "Content-Type" "text/xml"
                                         "X-EBAY-API-COMPATIBILITY-LEVEL" "859"
                                         "X-EBAY-API-SITEID" "77"}})
                :body
                (xml/parse-str))
        response-zip (xml1-> (zip/xml-zip xml) zip/leftmost)]
    (if  (= (xml1-> response-zip ::ebay/Ack text)
            "Success")
      (first response-zip)
      (throw (ex-info "eBay API request failed"
                      {:type ::api-call-failed
                       :api-call-name api-call-name
                       :request-xml (xml/indent-str xml)
                       :message (xml1-> response-zip ::ebay/Errors ::ebay/LongMessage text)})))))

(defn get-seller-list [start-date end-date]
  (ebay-api-request "GetSellerList"
                    [::ebay/GetSellerListRequest
                     [::ebay/StartTimeFrom start-date]
                     [::ebay/StartTimeTo end-date]]))

(defn get-item [item-id]
  (ebay-api-request "GetItem"
                    [::ebay/GetItemRequest
                     [::ebay/ItemID item-id]]))

(defn high-bidder-status [item-id]
  (let [zip (-> (get-item item-id)
                (zip/xml-zip))
        high-bidder (xml1-> zip ::ebay/GetItemResponse ::ebay/Item ::ebay/SellingStatus ::ebay/HighBidder)]
    (when high-bidder
      {:item-id item-id
       :description (xml1-> zip ::ebay/GetItemResponse ::ebay/Item ::ebay/Title text)
       :user-id (xml1-> high-bidder ::ebay/UserID text)
       :feedback-score (when-let [feedback-score (xml1-> high-bidder ::ebay/FeedbackScore text)]
                         (Integer/parseInt feedback-score)) 
       :positive-feedback-percent (when-let [positive-feedback-percent (xml1-> high-bidder ::ebay/PositiveFeedbackPercent text)]
                                    (Float/parseFloat positive-feedback-percent))})))

(defn format-time [t]
  (time-format/unparse (time-format/formatter :date-time) t))

(defn all-selling-item-ids []
  (let [zip (-> (get-seller-list (format-time (time/minus (time/now) (time/months 1)))
                                 (format-time (time/now)))
                (zip/xml-zip))]
    (xml-> zip ::ebay/GetSellerListResponse ::ebay/ItemArray ::ebay/Item ::ebay/ItemID text)))

(defn load-whitelist []
  (-> (slurp "whitelist.txt")
      (string/split #"\s+")
      set))

(defn whitelisted-user? [user-id]
  ((load-whitelist) user-id))

(defn whitelist-user [user-id]
  (spit "whitelist.txt" (as-> (load-whitelist) _
                          (conj _ user-id)
                          (sort _)
                          (string/join "\n" _))))

(defn acceptable-bidder? [{:keys [user-id feedback-score positive-feedback-percent] :as bidder-status}]
  (or (not bidder-status)
      (whitelisted-user? user-id)
      (not feedback-score)
      (and (= (int positive-feedback-percent) 100)
           (> feedback-score 9))))

(defn blockable-bids []
  (->> (all-selling-item-ids)
       (map high-bidder-status)
       (remove acceptable-bidder?)))

(defn get-inputs [body]
  (-> body
      hickory-zip
      (find-elements :input)))

(defn get-page-input-values [hickory]
  (->> hickory
       (get-inputs)
       (map :attrs)
       (remove #(empty? (:value %)))
       (map #(vector (:name %) (:value %)))
       (into {})))

(defn send-login [page-parameters userid pass]
  (let [{:keys [status headers body trace-redirects]} (http/request {:method :post
                                                                     :url "https://www.ebay.de/signin/s"
                                                                     :cookie-policy :standard
                                                                     :cookie-store cookie-store
                                                                     :trace-redirects true
                                                                     :form-params (assoc page-parameters
                                                                                         :userid userid
                                                                                         :pass pass)})]
    (if (= status 302)
      (get headers "Location")
      (throw (ex-info "Could not log in to eBay"
                      {:status status :body body})))))

(defn log-in [login-page-parameters expected-redirection]
  (let [redirected-to (send-login login-page-parameters (:userid credentials) (:pass credentials))]
    (when-not (string/starts-with? redirected-to expected-redirection)
      (throw (ex-info "eBay redirected us to unexpected URL after login"
                      {:expected-redirection expected-redirection
                       :redirected-to redirected-to})))))

(defn ebay-web-request* [method api-name request try-logging-in?]
  (let [url (str "https://offer.ebay.de/ws/eBayISAPI.dll?" api-name)
        {:keys [status headers body trace-redirects]} (http/request (merge {:method method
                                                                            :url url
                                                                            :cookie-policy :standard
                                                                            :cookie-store cookie-store
                                                                            :trace-redirects true}
                                                                           request))]
    (if (seq trace-redirects)
      (if try-logging-in?
        (if (re-find #"^https://signin\.ebay\.de/ws/eBayISAPI\.dll" (first trace-redirects))
          (do (log-in (-> body
                          hickory/parse
                          hickory/as-hickory
                          get-page-input-values)
                      url)
              (ebay-web-request* method api-name request false))
          (throw (ex-info "Unexpected redirect from eBay"
                          {:trace-redirects trace-redirects})))
        (throw (ex-info "Unexpected redirect from eBay after allegedly successful login"
                        {:trace-redirects trace-redirects})))
      (if (= status 200)
        body
        (throw (ex-info "Unexpected status response from ebay"
                        {:status status
                         :headers headers
                         :body body}))))))

(defn ebay-web-request [method api-name request]
  (ebay-web-request* method api-name request true))

;; srt and stok are parameters that are required by some of eBay's
;; POST handlers.  They are set by hidden parameters and which are
;; saved by save-srt-and-stok below when they're seen in a response
;; page from ebay.

(def saved-srt (atom nil))
(def saved-stok (atom nil))

(defn save-srt-and-stok [hickory]
  (let [input-values (get-page-input-values hickory)
        srt (get input-values "srt")
        stok (get input-values "stok")]
    (when srt
      (reset! saved-srt srt))
    (when stok
      (reset! saved-stok stok)))
  hickory)

(defn ebay-html-request [method api-name request]
  (-> (ebay-web-request method api-name request)
      hickory/parse
      hickory/as-hickory
      (save-srt-and-stok)))

(defn get-blocked-bidders []
  (-> (ebay-html-request :get "BidderBlockLogin" {})
      hickory-zip
      (find-element :textarea)
      :content
      first
      (string/split #", *")
      set))

(defn get-h1-content [body]
  (-> body
      hickory-zip
      (find-element :h1)
      :content
      first))

(defn set-blocked-bidders [user-ids]
  (-> (ebay-html-request :post "BidderBlockResult"
                         {:form-params {"MfcISAPICommand" "BidderBlockResult"
                                        "userid" (:userid credentials)
                                        "stok" @saved-stok
                                        "srt" @saved-srt
                                        "bidderlist" (string/join ", " user-ids)}})
      (get-h1-content)))

(defn block-bidder [user-id]
  (set-blocked-bidders (conj (get-blocked-bidders) user-id)))

(defn unblock-bidder [user-id]
  (set-blocked-bidders (disj (get-blocked-bidders) user-id)))

(defn cancel-bid [item-id user-id]
  (-> (ebay-html-request :post "CancelBid"
                         {:form-params {"MfcISAPICommand" "CancelBid"
                                        "selleruserid" (:userid credentials)
                                        "stok" @saved-stok
                                        "item" item-id
                                        "buyeruserid" user-id
                                        "info" "Artikelbeschreibung nicht gelesen oder nicht verstanden"}})
      (get-h1-content)))

(defn block-and-cancel-bid [{:keys [description item-id user-id]}]
  (println "Blocking bidder" user-id "-" (block-bidder user-id))
  (println "Cancel bid from" user-id "for" description "-" (cancel-bid item-id user-id)))

(defn block-and-cancel []
  (doseq [bid (blockable-bids)]
    (block-and-cancel-bid bid)))

(defn run []
  (while true
    (println (format-time (time/now)) "checking")
    (block-and-cancel)
    (java.lang.Thread/sleep 60000)))
