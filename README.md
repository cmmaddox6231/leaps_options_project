# leaps_options_project
Starter dashboard for finding volatile companies with a large drop, a trend finder for stocks, and a custom stock searcher. 

The UI and Server files are both used to create a webapp with 3 different parts: https://cameronmaddox.shinyapps.io/mongo_files/.
The webapp contains 3 major sections consisting of finding Under/Overvalued companies that have had a very recent spike in either a positive or negative direction. The User and choose a minimum change threshhold and a date range to generagte a list of companies that fit the criteria. The User can also selevct different financial values to check out the company historically.

The second section of the app is the Trend Finder page. This page is designed to find trending companies over the past 6 days with a minimum of a 9% weekly increase. If the stock has 6 consecutive days of a 9% weekly increase, it will be detected by the Trend Finder. These companies can then be compared with a SMA and EMA, interactive charts, and a fundamental analysis to determine if the trend will or will not continue. This also flows nicely with the undervalued company finder since these companies will likley also have a bullish/bearish trend. 

The last section is just an individual stock analyzer. Simply put in the ticker of the stock desired and find a chart and fundamental analysis of it. This page is great for further analysis of a stock after finding it on a previous tab. 
