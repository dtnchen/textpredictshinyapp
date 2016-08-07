Pre-Text: Text prediction using N-grams models with Katz backoff
========================================================
author: DTN Chen
date: 4/25/2016

Introduction
========================================================

![width](phonetext.jpg)

* Mobile devices are ubiquitous in modern life
     
* Text input into these devices is error-prone due to small size of touchpad 

* Therefore, text prediction apps have become indispensible for efficient text entry 

* To this end, here I describe a small app that implements N-gram language modeling with Katz backoff and Good-Turing discounting for text prediction.

N-gram Language Model
========================================================
<small>N-grams are sequences of N words found in a corpus of text. They are useful as simple models for text prediction. For example, if "thanks for the fish" is a fourgram that frequently occurs in a training text corpus, then the occurance of the threegram "thanks for the" is a likely predictor for next word "fish" in a text input scenario.     

*Due to the finite size of any text corpus, and memory limitations of mobile devices, there are always text input combinations that do not appear in models based on maximum likelihood estimates (MLE) of N-grams. Therefore, a better model is the Katz backoff model (Katz 1987) with Good-Turing discounting to estimate the text probabilities for prediction. 

*Below is an example backoff algorithm for estimation of Katz probability for the threegram (x,y,z):

$$latex
P_{katz}(z|x,y) = \left\{
                \begin{array}{ll}
                  P^*(z|x,y) \ if \, C(x,y,z) > 0\\
                  \alpha P_{katz}(z|y) \ otherwise \, backoff\\
                \end{array}
              \right.
$$
$$latex
P_{katz}(z|y) = \left\{
                \begin{array}{ll}
                  P^*(z|y) \ if \, C(y,z) > 0\\
                  \alpha P^*(z) \ otherwise \, stop\\
                \end{array}
              \right.
$$</small>

Training the Model
========================================================

<small> The N-grams models were extracted from Twitter and News Corpora provided by Swiftkey
* 2% of the Twitter and News corpora were sampled, combined, and subsequently tokenized using NGramTokenizer from the RWeka library, corresponding to a unigram vocabulary of ~46,000 words.
* 1,..4-grams were extracted from the combined corpus along with their MLE frequencies
* The MLE frequencies were smoothed using Good-Turing discounting (Gale and Sampson 1995) and the discounted frequencies were used to compute Katz backoff factors $\alpha$
* The 1-4-gram strings, Good-Turing discounted Probabilities $P^*$ and backoff factors $\alpha$ are saved as .rds files and uploaded to the ShinyAppsIO website. Upon loading the files, the App takes an input string and attempts to match the highest order N-gram up to fourgram. If exact matches are found, they are output as predicted words in the wordcloud. Otherwise, the recursively backs off to the (N-1)gram, endpointing at most common onegrams.</small> 

Shiny App
========================================================
To use simply type into the text box and the app will react after a brief (< 5 second) latency and output the prediction in a wordcloud with fontsize scaling with the relative probability of the predicted word in the N-gram model. Up to ten word predictions will be shown, at least one word prediction will always appear.

![width](screenshot.png)
