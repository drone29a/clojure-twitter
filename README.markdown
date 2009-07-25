# Twitter client API for Clojure #
This library is going through a major reworking.  
The new API is stabilizing and the current version should be considered a pre-release.

# Example #

(require 'twitter
         ['oauth.client :as 'oauth])

;; Make a OAuth consumer
(def oauth-consumer (oauth/make-consumer <key>
                                         <secret>       
                                         "http://twitter.com/oauth/request_token"
                                         "http://twitter.com/oauth/access_token"
                                         "http://twitter.com/oauth/authorize"
                                         :hmac-sha1))

(def oauth-access-token 
     ;; Look up an access token you've stored away after the user
     ;; authorized a request token and you traded it in for an
     ;; access token.  See [clj-oauth](http://github.com/mattrepl/clj-oauth) for an example.

;; Post to twitter
(twitter/with-oauth oauth-consumer oauth-access-token
                    (twitter/update-status "posting from #clojure with #oauth"))

;; Find out who follows dons
(twitter/followers-of-name "dons")