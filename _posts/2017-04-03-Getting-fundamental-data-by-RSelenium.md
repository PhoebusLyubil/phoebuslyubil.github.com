---
layout: post
subtitle: 
tags: [RSelenium, R]
comments: true
---




Getting fundatmental data
-------------------------

I have tried to obtain cash flow and balance sheet data from
[tsetmc.com](http://www.tsetmc.com). It was not as straightforward as I
thought. Here is what I encountered:

-   Easy parts:
    -   Each stock has a unique number at the at the end of address
    -   Mutual funds and delisted and trade-able stocks have different
        structure than others
    -   Numbers are in English and not Persian
    -   Balance sheets and Cash flow data have separate pages that has
        no unique addresses and need to be clicked on
-   Troublesome parts:
    -   Different years balance sheets and cash flow data have no
        special "id" or "name" or anything else that would make it easy
        to separate them
    -   data for different years are just separable by text part
    -   With R I could not use regex on the text and separate different
        fields automatically
    -   It seems that the site uses some Arabic characters instead of
        Persian for some sounds like "ii"
    -   there is no unique pattern for balance sheet or cash flow data,
        it seemed to me that they could be distinguished only by using
        regex

By seeing these I reached to the conclusion that since regex with partly
Persian character and partly Arabic is not possible in R- by my
knowledge- so I would use just the data that have unique path. I used
RSelenium for web scrapping but since I would not click on the pages or
fill forms, RSelenium is not necessary. The some of the data that have
unique path included: Book value of outstanding stocks, free float rate,
MCap, EPS, P/E and groups P/E. The last one as I said before and
considering the kind of group separation that is used is almost useless
yet I get it in order to see whether it has any value that I am
neglecting. These data seemed to me to be more than enough for splitting
the stocks by small vs big criteria.

The URL for each stock is like this:
"<http://www.tsetmc.com/Loader.aspx?ParTree=151311&i=42354736493447489>"
which has two part the part before equal sign is common part for
different stocks and the number after that is unique to each stock.

I used Docker for Selenium server on Firefox.

    tsetmc_export<-"http://www.tsetmc.com/loader.aspx?ParTree=151311&i="
    sym.id<- "42354736493447489"
    link_dn<- paste(tsetmc_export, sym.id, sep= "")
    Doc_name<- "Pars.Khodro"

    remDr <- remoteDriver(port = 4445L)
    remDr$open()
     
    remDr$navigate(link_dn)
    Sys.sleep(4)   
    remDr$screenshot(display = TRUE)


    is.mutual.fund<- tryCatch( remDr$findElement( using = 'xpath',
                                                  ".//*[@id='d11']/div")),
             error = function(e){ TRUE})

    if( is.mutual.fund != TRUE){
    wxbox <- remDr$findElement(using = 'xpath',
                               ".//*[@id='d11']/div")
    MCAP<- wxbox$getElementAttribute("title")[[1]]
    } else { temp <- NULL}

     is.delisted<- tryCatch( remDr$findElement( using = 'xpath',
                                               ".//*[@id='TopBox']/div[1]/div[4]/table/tbody/tr[1]/td[2]/div"),
             error=function(e){ TRUE})
    if( is.delisted != TRUE ){
    wxbox <- remDr$findElement(using = 'xpath',
                               ".//*[@id='TopBox']/div[1]/div[4]/table/tbody/tr[1]/td[2]/div")
    BookValue<- wxbox$getElementAttribute("title")[[1]]
    wxbox <- remDr$findElement(using = 'xpath',
                               ".//*[@id='TopBox']/div[1]/div[4]/table/tbody/tr[3]/td[2]")
    freefloat<-wxbox$getElementText()[[1]]
    wxbox <- remDr$findElement(using = 'xpath',
                               ".//*[@id='TopBox']/div[1]/div[6]/table/tbody/tr[1]/td[2]")
    EPS<- wxbox$getElementText()[[1]]
    wxbox <- remDr$findElement(using = 'xpath',
                               ".//*[@id='TopBox']/div[1]/div[6]/table/tbody/tr[2]/td[4]")
    G.PE<- wxbox$getElementText()[[1]]
    wxbox <- remDr$findElement(using = 'xpath',
                               ".//*[@id='d12']")
    PE<- wxbox$getElementText()[[1]]
    temp<- cbind.data.frame(sym = Doc_name, MCAP = MCAP,
                            BookValue = BookValue, freefloat = freefloat,
                            EPS = EPS, PE = PE, G.PE = G.PE)
    } else{ temp<- NULL}

The first if statement checks whether the id is for a mutual fund or not
and the second one checks whether the stock is delisted or not.

Conclusion
----------

By making a list of the ids for URL I made a data frame of some
fundamental data. Using that in next post I would consider momentum
strategy. Please let me know if you have a find a good way for regex in
R or any other language that could be used for getting data from
tsetmc.com, I would deeply appreciate it.

###### *Please inform me about your feedback, I will be deeply grateful for that :)*

###### For disclaimer please see about page.
