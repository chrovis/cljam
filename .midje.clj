;;; see https://github.com/marick/Midje/wiki/Using-metadata-to-filter-facts#lein-midje-and-config-files

(change-defaults :fact-filter (complement :heavy))
(change-defaults :visible-future false)

;;; NOTE: `:heavy` tests are to fetch large files (over 1G),
;;;       that use many cpu times and memories.
;;;       And, it required "shell environment variables" for fetch files
;;;       from `share.xcoo.jp`;
;;;       `JVM_OPTS="-Xmx8g -server" AUTHUSER="usename" AUTHPASS="password" lein midje :filter heavy`
;;;
;;;       `:heavy` tests must have to not only `:heavy` but also `:slow`.
;;;       Because this `:fact-filter` can override by `:filter` like;
;;;       `lein midje :filter -slow`
