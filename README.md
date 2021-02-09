# Final project: Predict likelihood of customer churn

### Problem being solved
This project aims to proactively identify venues that are at risk of churning.  There are two types of Churn - Voluntary Churn (Bad experience, Lack of features) and Involuntary Churn (Out of business).

### Why does the client care about this problem?  What is the implication of the data?
With this information, the company can reach out to customers and attempt to prevent them from Churning, increasing the average customer lifetime value (LTV).   In addition, proactively identifying customers at risk of Churn can help obtain better information about why they might leave.  This is very difficult to obtain once the customer has churned (the current process).

### What data will you use?  How will you obtain it?
All of this information will be obtained from internal systems.  The data is can be categorized into a few buckets:

#### Diagnostic Customer information
* Customer Status (Churn, Not Churn)
* Churn Type (Voluntary, Involuntary)
* Venue type
* Time since acquisition
* License plan (solo, dual, team, unlimited)
* Region (Country, State, City)

#### Activity information
* Time between server pings
* Time between uploaded sales bills (-60 days, -14 days, -7 days)
* Indicators of Involuntary Churn (customer’s business is struggling)
* Sales amount processed in the 90/60/14/7 days before Churn
* Labour cost ratio in the 90/60/14/7 days before Churn
* Bounced/unpaid licensing bills - full history
* Bounced/unpaid licensing bills - 60/14/7 days before Churn

#### Indicators of Voluntary Churn (customer had a bad experience)
* Bugs & severity of bugs
* Support calls in the last 30, 60, 90 days before Churn
* Total Support calls all time
* Cancelled or partially completed training & install appointments


### Outline Approach (how to solve)
Venues that are Seasonal, that have opted out of sharing their data, and that are still onboarding will need to be removed from the dataset.  Next, missing values will need to be found & removed.  Lastly, we can use Logistic Regression to predict the likelihood of Churn.


### Please see [this google doc for the full writeup and outcome.](https://docs.google.com/document/d/1uenJNgxEqluHw3BMscCdZeeTmZh4NpVyieYMYIyUE-A/edit?ts=5cdd56be)
