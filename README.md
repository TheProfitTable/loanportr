#### loanportr
**Loan portfolio analysis with R**

#### Background 

Ever had to analyse a loan performance data set and wished you had a few basic functions and a framework to quickly get an idea of things like:   

* the mix (distribution) of loans originated over time
* vintage analysis of default rates
* roll rates 
* analysis of the distribution of the loan book over time

Well, I certainly have.   

This is our attempt at creating a great tool for all consumer / retail credit analysts out there!   

This package should be useful as an additional tool in any of the following areas:  

 * Loan portfolio analysis 
 * Modelling for IFRS 9 Provisions
 * Reporting 
 * Credit Scoring 

#### The Package 

We believe R functions are best organised and collaborated on in a R package, so loanportr is a R package. 

**This is how you install the package:** 

Use [devtools](https://github.com/hadley/devtools)

run this in R: 

```devtools::install_github("TheProfitTable/loanportr")```

#### Data  

We want the only "hard" work in doing loan portfolio analyis to be the prepping of the data. We have created a standard loan data dictionary which works with loanportr. In other words, your loan data needs to be in this format to use loanportr successfully. Not to worry, this mostly means changing the names of the variables and making sure that the definition of the variable corresponds to our definition of the variable. 

As an example we have taken the [Freddie Mac](http://www.freddiemac.com/research/datasets/sf_loanlevel_dataset.html) data and converted it to our naming format in [this repo](https://github.com/TheProfitTable/freddiemacdata) 

#### Work in progress 

This is literally still a work in progress, it's starting to shape now and we're very exciting to get more of you working on and using loanportr! 

We make use of GitHub issues to track tasks and log bugs, changes of fixes needed. You are therefore welcome to check out the issues if you want to:   

* Try your hand at collaborating with us in building this package
* Log an issue that you encountered while usin the package. 

#### The Invitation 

We want you! ...to:    

* to use loanportr and give us feedback 
* collaborate with us in building an awesome tool!

 

