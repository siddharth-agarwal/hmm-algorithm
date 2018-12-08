**Predicting the S&P 500 with Hidden Markov Models**

Developed in R, designed as a *Shiny* webapp. Hosted using [shinyapps.io](https://www.shinyapps.io/) @ **[siddharth-a.shinyapps.io/hmm-algorithm](https://siddharth-a.shinyapps.io/hmm-algorithm/)**

See below for a summary of the project. **A full write-up (discussing methodology, analysis, and results) can be found here: [HMMPaper.pdf](../master/presentation/MA491Agarwal.pdf). An accompanying [slide deck](../master/presentation/MA491Presentation_SiddharthAgarwal.pptx) is also available.**

**Abstract**: *This paper aims to predict the S&P 500 on regular trading days using trends obtained by training a Hidden Markov Model (HMM) using commonly available market data. Daily market information is fed into this model with a window that shifts forward each day. Output probabilities are calculated by the HMM for the chance that the market is in a given state for the next day. This probability is passed on to an algorithm that interprets it to create a signal for the next day and make trades in the open market.*
