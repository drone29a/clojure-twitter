# Twitter client API for Clojure #

Access the Twitter API from Clojure.


# Building #

    lein deps
    lein jar


# Example #

    (require 'twitter
             ['oauth.client :as 'oauth])

    ;; Make a OAuth consumer
    (def oauth-consumer (oauth/make-consumer <key>
                                             <secret>       
                                             "https://api.twitter.com/oauth/request_token"
                                             "https://api.twitter.com/oauth/access_token"
                                             "https://api.twitter.com/oauth/authorize"
                                             :hmac-sha1))

    (def oauth-access-token 
         ;; Look up an access token you've stored away after the user
         ;; authorized a request token and you traded it in for an
         ;; access token.  See clj-oauth (http://github.com/mattrepl/clj-oauth) for an example.)
    (def oauth-access-token-secret
         ;; The secret included with the access token)

    ;; Post to twitter
    (twitter/with-oauth oauth-consumer 
                        oauth-access-token
                        oauth-access-token-secret
                        (twitter/update-status "posting from #clojure with #oauth"))

    ;; Find out who follows dons
    (twitter/followers-of-name "donsbot")

# Authors #

Development funded by LikeStream LLC (Don Jackson and Shirish Andhare), see [http://www.likestream.org/opensource.html](http://www.likestream.org/opensource.html).

Designed and developed by Matt Revelle of [Lightpost Software](http://lightpostsoftware.com).