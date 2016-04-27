ruleSignal1<-function (mktdata = mktdata, timestamp, sigcol, sigval, orderqty = 0,
          ordertype, orderside = NULL, orderset = NULL, threshold = NULL,
          tmult = FALSE, replace = TRUE, delay = 1e-04, osFUN = "osNoOp",
          pricemethod = c("market", "opside", "active"), portfolio,
          symbol, symb, ..., ruletype, TxnFees = 0, prefer = NULL, sethold = FALSE,
          label = "", order.price = NULL, chain.price = NULL, time.in.force = "")
{
  if(symbol==symb) {
  if (!is.function(osFUN))
    osFUN <- match.fun(osFUN)
  if (hasArg(curIndex))
    curIndex <- eval(match.call(expand.dots = TRUE)$curIndex,
                     parent.frame())
  else curIndex <- mktdata[timestamp, which.i = TRUE]
  if (curIndex > 0 && curIndex <= nrow(mktdata) && (ruletype ==
                                                    "chain" || (!is.na(mktdata[curIndex, sigcol]) && mktdata[curIndex,
                                                                                                             sigcol] == sigval))) {
    pricemethod <- pricemethod[1]
    if (hasArg(prefer))
      prefer = match.call(expand.dots = TRUE)$prefer
    else prefer = NULL
    if (!is.null(threshold)) {
      if (!is.numeric(threshold)) {
        col.idx <- grep(threshold, colnames(mktdata))
        if (length(col.idx) < 1)
          stop(paste("no indicator column in mktdata matches threshold name \"",
                     threshold, "\"", sep = ""))
        if (length(col.idx) > 1)
          stop(paste("more than one indicator column in mktdata matches threshold name \"",
                     threshold, "\"", sep = ""))
        threshold <- as.numeric(mktdata[curIndex, col.idx])
      }
    }
    if (is.null(orderside) & !isTRUE(orderqty == 0)) {
      curqty <- getPosQty(Portfolio = portfolio, Symbol = symb,
                          Date = timestamp)
      if (curqty > 0) {
        orderside <- "long"
      }
      else if (curqty < 0) {
        orderside <- "short"
      }
      else {
        if (orderqty > 0)
          orderside <- "long"
        else orderside <- "short"
      }
    }
    if (orderqty == "all") {
      if (orderside == "long") {
        tmpqty <- 1
      }
      else {
        tmpqty <- -1
      }
    }
    else {
      tmpqty <- orderqty
    }
    if (!is.null(order.price)) {
      orderprice <- order.price
    }
    else if (!is.null(chain.price)) {
      orderprice <- chain.price
    }
    else {
      switch(pricemethod, market = , opside = , active = {
        if (is.BBO(mktdata)) {
          if (tmpqty > 0) prefer = "ask" else prefer = "bid"
        }
        orderprice <- try(getPrice(x = mktdata[curIndex,
                                               ], prefer = prefer)[, 1])
      }, passive = , work = , join = {
        if (is.BBO(mktdata)) {
          if (tmpqty > 0) prefer = "bid" else prefer = "ask"
        }
        orderprice <- try(getPrice(x = mktdata[curIndex,
                                               ], prefer = prefer)[, 1])
      }, maker = {
        if (hasArg(price) & length(match.call(expand.dots = TRUE)$price) >
            1) {
          orderprice <- try(match.call(expand.dots = TRUE)$price)
        } else {
          if (!is.null(threshold)) {
            baseprice <- last(getPrice(x = mktdata[curIndex,
                                                   ])[, 1])
            if (hasArg(tmult) & isTRUE(match.call(expand.dots = TRUE)$tmult)) {
              baseprice <- last(getPrice(x = mktdata[curIndex,
                                                     ])[, 1])
              if (length(threshold) > 1) {
                orderprice <- baseprice * threshold
              } else {
                orderprice <- c(baseprice * threshold,
                                baseprice * (1 + 1 - threshold))
              }
            } else {
              if (length(threshold) > 1) {
                orderprice <- baseprice + threshold
              } else {
                orderprice <- c(baseprice + threshold,
                                baseprice + (-threshold))
              }
            }
          } else {
            stop("maker orders without specified prices and without threholds not (yet?) supported")
            if (is.BBO(mktdata)) {
            } else {
            }
          }
        }
        if (length(orderqty) == 1) orderqty <- c(orderqty,
                                                 -orderqty)
      })
      if (inherits(orderprice, "try-error"))
        orderprice <- NULL
      if (length(orderprice) > 1 && pricemethod != "maker")
        orderprice <- last(orderprice[timestamp])
      if (!is.null(orderprice) && !is.null(ncol(orderprice)))
        orderprice <- orderprice[, 1]
    }
    if (is.null(orderset))
      orderset = NA
    if (orderqty != "all") {
      orderqty <- osFUN(strategy = strategy, data = mktdata,
                        timestamp = timestamp, orderqty = orderqty, ordertype = ordertype,
                        orderside = orderside, portfolio = portfolio,
                        symbol = symb, ... = ..., ruletype = ruletype,
                        orderprice = as.numeric(orderprice))
    }
    if (!is.null(orderqty) && orderqty != 0 && length(orderprice)) {
      addOrder(portfolio = portfolio, symbol = symb,
               timestamp = timestamp, qty = orderqty, price = as.numeric(orderprice),
               ordertype = ordertype, side = orderside, orderset = orderset,
               threshold = threshold, status = "open", replace = replace,
               delay = delay, tmult = tmult, ... = ..., prefer = prefer,
               TxnFees = TxnFees, label = label, time.in.force = time.in.force)
    }
  }
  if (sethold)
    hold <<- TRUE
  }
}
