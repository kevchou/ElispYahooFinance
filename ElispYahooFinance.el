;; Call Yahoo to get equity prices
;;
;; Yahoo Input:
;;   http://download.finance.yahoo.com/d/quotes.csv?s=AAPL+GOOG&f=sb2b3jkm6
;; Yahoo Output: (.csv file)
;;  "GOOG",602.94,601.69,282.75,629.51,+18.27%
;;
;; More Info: http://www.gummy-stuff.org/Yahoo-data.htm
;;

;; After evaluating the buffer, use:
;; M-x get-yahoo-stats
;; to get the stock information


;; Tickers of interest
(setq tickers '("AAPL" "GOOG" "MSFT"))


;; mnemonics for yahoo finance API
(setq AfterHoursChangeRealtime                     "c8")
(setq AnnualizedGain                               "g3")
(setq Ask                                          "a0")
(setq AskRealtime                                  "b2")
(setq AskSize                                      "a5")
(setq AverageDailyVolume                           "a2")
(setq Bid                                          "b0")
(setq BidRealtime                                  "b3")
(setq BidSize                                      "b6")
(setq BookValuePerShare                            "b4")
(setq Change                                       "c1")
(setq Change                                       "c0")
(setq ChangeFromFiftydayMovingAverage              "m7")
(setq ChangeFromTwoHundreddayMovingAverage         "m5")
(setq ChangeFromYearHigh                           "k4")
(setq ChangeFromYearLow                            "j5")
(setq ChangeInPercent                              "p2")
(setq ChangeInPercentRealtime                      "k2")
(setq ChangeRealtime                               "c6")
(setq Commission                                   "c3")
(setq Currency                                     "c4")
(setq DaysHigh                                     "h0")
(setq DaysLow                                      "g0")
(setq DaysRange                                    "m0")
(setq DaysRangeRealtime                            "m2")
(setq DaysValueChange                              "w1")
(setq DaysValueChangeRealtime                      "w4")
(setq DividendPayDate                              "r1")
(setq TrailingAnnualDividendYield                  "d0")
(setq TrailingAnnualDividendYieldInPercent         "y0")
(setq DilutedEPS                                   "e0")
(setq EBITDA                                       "j4")
(setq EPSEstimateCurrentYear                       "e7")
(setq EPSEstimateNextQuarter                       "e9")
(setq EPSEstimateNextYear                          "e8")
(setq ExDividendDate                               "q0")
(setq FiftydayMovingAverage                        "m3")
(setq SharesFloat                                  "f6")
(setq HighLimit                                    "l2")
(setq HoldingsGain                                 "g4")
(setq HoldingsGainPercent                          "g1")
(setq HoldingsGainPercentRealtime                  "g5")
(setq HoldingsGainRealtime                         "g6")
(setq HoldingsValue                                "v1")
(setq HoldingsValueRealtime                        "v7")
(setq LastTradeDate                                "d1")
(setq LastTradePriceOnly                           "l1")
(setq LastTradeRealtimeWithTime                    "k1")
(setq LastTradeSize                                "k3")
(setq LastTradeTime                                "t1")
(setq LastTradeWithTime                            "l0")
(setq LowLimit                                     "l3")
(setq MarketCapitalization                         "j1")
(setq MarketCapRealtime                            "j3")
(setq MoreInfo                                     "i0")
(setq Name                                         "n0")
(setq Notes                                        "n4")
(setq OneyrTargetPrice                             "t8")
(setq Open                                         "o0")
(setq OrderBookRealtime                            "i5")
(setq PEGRatio                                     "r5")
(setq PERatio                                      "r0")
(setq PERatioRealtime                              "r2")
(setq PercentChangeFromFiftydayMovingAverage       "m8")
(setq PercentChangeFromTwoHundreddayMovingAverage  "m6")
(setq ChangeInPercentFromYearHigh                  "k5")
(setq PercentChangeFromYearLow                     "j6")
(setq PreviousClose                                "p0")
(setq PriceBook                                    "p6")
(setq PriceEPSEstimateCurrentYear                  "r6")
(setq PriceEPSEstimateNextYear                     "r7")
(setq PricePaid                                    "p1")
(setq PriceSales                                   "p5")
(setq Revenue                                      "s6")
(setq SharesOwned                                  "s1")
(setq SharesOutstanding                            "j2")
(setq ShortRatio                                   "s7")
(setq StockExchange                                "x0")
(setq Symbol                                       "s0")
(setq TickerTrend                                  "t7")
(setq TradeDate                                    "d2")
(setq TradeLinks                                   "t6")
(setq TradeLinksAdditional                         "f0")
(setq TwoHundreddayMovingAverage                   "m4")
(setq Volume                                       "v0")
(setq YearHigh                                     "k0")
(setq YearLow                                      "j0")
(setq YearRange                                    "w0")


(defun get-yahoo-stats()
  (interactive)
  (let ((quotes (get-quotes
                 tickers
                 (concat Symbol LastTradePriceOnly Ask Bid Change LastTradeDate LastTradeTime Name)
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



;;; FUNCTION TO SCROLL MESSAGE.. want to implement it to scroll daily change
(defun scroll (msg)
  (let* (msg-len l-bound r-bound buffer sub)
      (setq msg-len (length msg))
      (setq max-len (window-width (selected-window)))
      (setq l-bound 0)  ;; left boundary of string
      (setq r-bound 1)  ;; right boundary of string
      (while (<= r-bound msg-len)
        (if (> (- r-bound l-bound) max-len)
            (setq l-bound (+ l-bound 1))) ;; move over left boundary

        (setq sub (substring msg l-bound r-bound))  ;;; the portion of the string we are to display:
        (setq buffer (make-string (- max-len (length sub)) 32)) ;;; leading white space:
        (setq buffer (concat buffer sub))  ;;; put them together
        (message buffer)   ;;; display it
        (sit-for .07)
        (setq r-bound (+ r-bound 1)))))

(scroll "This is my test message...")
