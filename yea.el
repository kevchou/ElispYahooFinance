;; Call Yahoo to get equity prices
;;
;; Yahoo Input:
;;   http://download.finance.yahoo.com/d/quotes.csv?s=AAPL+GOOG&f=sb2b3jkm6
;; Yahoo Output: (.csv file)
;;  "GOOG",602.94,601.69,282.75,629.51,+18.27%
;;
;; More Info: http://www.gummy-stuff.org/Yahoo-data.htm
;;
;; Yahoo format described here: http://www.gummy-stuff.org/Yahoo-data.htm

(setq symbol          "s")
(setq price           "l1")
(setq ask             "b2")
(setq bid             "b3")
(setq change-raw      "c")
(setq last-trade-date "d1")
(setq last-trade-time "t1")
(setq name            "n")

(defun get-yahoo-stats()
  (interactive)
  (let ((quotes (get-quotes
                 '("VUN.TO" "VCN.TO" "IMG.TO" "SGR.TO" "SU.TO" "CSIQ")
                 (concat symbol price ask bid change-raw last-trade-date last-trade-time name)
                 )))
    (show-quotes quotes)))

(defun show-quotes(quotes)
  (erase-buffer)
  (dolist (quote quotes)
    (let* ((symbol (remove-quotes (nth 0 quote)))
           (price (nth 1 quote))
           (ask (nth 2 quote))
           (bid (nth 3 quote))
           (change-raw (remove-quotes (nth 4 quote)))
           (change-value (first (split-string change-raw " - ")))
           (change-percent (second (split-string change-raw " - ")))
           (last-trade-date (remove-quotes (nth 5 quote)))
           (name (remove-quotes (nth 6 quote))))
    (insert name " (" symbol ")")
    (newline-and-indent)
    (insert "Price: $" price)
    (newline-and-indent)
    (insert "Last Trade Date: " last-trade-date)
    (newline-and-indent)
    (insert "Bid: " bid ", Ask: " ask)
    (newline-and-indent)
    (insert "Change: " change-value " (" change-percent ")" )
    (newline-and-indent)
    (insert "---------------------------")
    (newline-and-indent))))

(defun remove-quotes (quoted-string)
  "Removes quotes from a string"
  (setq quoted-string (replace-regexp-in-string "\"" "" quoted-string))
  (replace-regexp-in-string "\r\n" "" quoted-string))


(defun get-quotes(tickers field-string)
  "Given a list of ticker names and a string of fields to return as above, this grabs them
from Yahoo, and parses them"
  (let ((results-buffer (get-yahoo-quotes-to-buffer (get-price-url tickers field-string))))
    (switch-to-buffer results-buffer)
    (parse-quote-buffer results-buffer)))

(defun get-price-url (tickers field-string)
  "Set up the get url"
  (concat "http://download.finance.yahoo.com/d/quotes.csv?s="
      (mapconcat 'identity tickers "+")
      "&f=" field-string))

(defun get-yahoo-quotes-to-buffer(url)
  "Retrieve the quotes to a buffer and return it"
  (url-retrieve-synchronously url))

(defun parse-quote-buffer(b)
  "Parse the buffer for quotes"
  (goto-line 1)
  (re-search-forward "^\n")
  (beginning-of-line)
  (let ((res nil))
    (while (> (point-max) (point))
      (setf res (cons  (split-string (thing-at-point 'line) ",") res))
      (forward-line 1))
    (reverse res)))
