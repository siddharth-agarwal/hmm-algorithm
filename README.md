# hmm-algorithm
"Predicting the S&P 500 with Hidden Markov Models"

Developed in R as a Shiny web app and deployed to https://siddharth-a.shinyapps.io/hmm-algorithm/ (with shinyapps.io)

See below for a summary. The .docx file contains the full write-up with results/analysis.


Abstract: “This paper aims to predict the S&P 500 on regular trading days using trends obtained by training a Hidden Markov Model (HMM) using commonly available market data. Daily market information is fed into this model with a window that shifts forward each day. Output probabilities are calculated by the HMM for the chance that the market is in a given state for the next day. This probability is passed on to an algorithm that interprets it to create a signal for the next day and make trades in the open market.”
