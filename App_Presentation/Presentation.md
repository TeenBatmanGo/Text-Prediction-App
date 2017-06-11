Text Prediction App
========================================================
type: prompt
author: Wang
date: 14 May 2017
autosize: true
transition: concave
transition-speed: slow
font-family: 'Helvetica'

Introduction
========================================================
type: prompt

This app is a text prediction model. It will predict the next word when given any English text. It is inspired by Swiftkey, a compony which builds a smart keyboard that makes it easier for people to type on their mobile devices. 

If someone types

<strong> I'd like to </strong>

This App will presents five options for what the next word might be.

By applying data science in the area of natural language processing, this app performs pretty well when predicting the next word for real-life texts. 



Instructions
========================================================
type: prompt

1. Enter your text into the given box
2. Choose a <strong>backoff</strong> method to use (see [More Info](#/slide5) section)
3. Choose the number of predictions this App will show
4. Click the Update button

![alt text](demo.jpg)


Performance
========================================================
type: prompt




```
Method: stupid_backoff  Predict from previous 1-word
Top1 accuracy: 8.54%
Top3 accuracy: 14.63%
```

```
Method: katz_backoff  Predict from previous 1-word
Top1 accuracy: 9.76%
Top3 accuracy: 18.29%
```

```
Method: stupid_backoff  Predict from previous 2-word
Top1 accuracy: 8.11%
Top3 accuracy: 18.92%
```

```
Method: katz_backoff  Predict from previous 2-word
Top1 accuracy: 10.81%
Top3 accuracy: 24.32%
```


More Info
========================================================
type: prompt
id: slide5

<strong>What is backoff?</strong>

Take Katz back-off as an example. It is a generative n-gram language model that estimates the conditional probability of a word given its history in the n-gram. It accomplishes this estimation by "backing-off" to models with smaller histories under certain conditions. By doing so, the model with the most reliable information about a given history is used to provide the better results.

<strong> Where can I use this App? </strong>

The shiny app is here: https://teenbatmango.shinyapps.io/textprediction/
