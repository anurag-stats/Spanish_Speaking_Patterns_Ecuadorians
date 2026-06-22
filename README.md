# Spanish_Speaking_Patterns_Ecuadorians

Objective: After migrating to Spain and reverse migrating back to Ecuador, which factors point to Ecuadorians producing Spanish patterns different from the local ones.  

This was a project from when I worked at Statistical Consulting Service at the Ohio State University. All of the statistical and data analysis was done by me and I took into consideration the regular feedback from my client.

The researcher (my client) collected the data by talking to Ecuadorians who had returned to Ecuador, after having lived in Spain for a certain amount of time, they collected demographic data such as Age, Gender and Education level, along with duration of stay in Spain and how long had it been since they returned to Ecuador. My client asked them certain questions to make them produce specific sentence types and recorded which particular pattern, Spain, Ecuador, Mixed, Different or Others, did they produce. There were 6 unique speakers and each of them had about 50 observations. There were 17 unique sentence types and several replications. This was a longitudnal design. 

The major challenge was the sparsity in the data. The response was nominal. Initially, I performed some exploratory analysis using data visualisations and descriptive statistics like sample proportions, I then moved onto computing confidence intervals to answer certain research questions. The code and analysis can be found in the file titled intonation_analysis. 

The next phase was to use a model to answer research questions, this proved difficult due to the aforementioned sparsity in the data, so I collapsed the sentence type to classify sentences more generally and we came up with a new encoding for the outcome (type of Spanish) as Spain, Ecuador and Different. The most appropriate model here would have been a mixed effects model with a random intercept for Speakers (it seemed that sentence type should also be random) but due to the lack of unique data and rather lack of data, in general, the reliable estimation of parameters proved to be very difficult. I ended up using a general (fixed effects) multinomial model along with bootstrapping to compute confidence and/or prediction intervals which answered the research question. The code and analysis can be found in the files titled report_3categories. 

A final report was written which can be found in the file titled Ecuadorian_Spanish_Pattern_Analysis, however, a major caveat about the reliability of the analysis still remains due to the lack of unique data.


