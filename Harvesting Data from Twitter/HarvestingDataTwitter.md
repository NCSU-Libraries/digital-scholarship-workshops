##Harvest Tweets using R

by Alison Blaine

This is a script and tutorial (the directions are in the script) for getting and analyzing Twitter data with R, using the rtweet package. See more info on this package: (http://rtweet.info).
In order to use this, you will need the following:
* R installed on your computer
* a Twitter app - create your own at http://apps.twitter.com.
* R Studio (recommended but not required)

Setup
Please read and carefully follow these instructions for setting up your Twitter app: (http://rtweet.info/articles/auth.html)

Troubleshooting
Even with following setup perfectly, you may get an error when trying to harvest tweets. A common error is not having the httpuv package installed in R. To fix that, type the following in the console and hit enter:
install.packages("httpuv")
