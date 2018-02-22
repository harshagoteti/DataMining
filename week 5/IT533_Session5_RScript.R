## Download the Titanic raw data from http://www.rdatamining.com/data/titanic.raw.rdata 
## and put on C:/ drive
## Load Titanic raw data
## We are walking through Yanchang Zhao's Association Rule Mining with R slide deck of 2014
## http://www.rdatamining.com/docs/association-rule-mining-with-r

View(titanic.raw)
View(Movie_Ratings)
str(titanic.raw)
str(Movie_Ratings)
summary(titanic.raw)
summary(Movie_Ratings)

## Install and deploy the arules package

install.packages("arules")
install.packages("arulesViz")

## Call the arules library and execute apriori

library(arules)
rules.all <- apriori(titanic.raw)
rules.complete <- apriori(titanic.raw)
## Parameter specification:
## confidence minval smax arem aval originalSupport support
## 0.8 0.1 1 none FALSE TRUE 0.1
## minlen maxlen target ext
## 1 10 rules FALSE
##
## Algorithmic control:
## filter tree heap memopt load sort verbose
## 0.1 TRUE TRUE FALSE TRUE 2 TRUE

inspect(rules.all)

## All the rules show the connections between the attributes, in the order of strength
## lhs                                   rhs           support   confidence lift     
## 1  {}                                 => {Age=Adult}   0.9504771 0.9504771  1.0000000
## 2  {Class=2nd}                        => {Age=Adult}   0.1185825 0.9157895  0.9635051
## 3  {Class=1st}                        => {Age=Adult}   0.1449341 0.9815385  1.0326798
## 4  {Sex=Female}                       => {Age=Adult}   0.1930940 0.9042553  0.9513700
## 5  {Class=3rd}                        => {Age=Adult}   0.2848705 0.8881020  0.9343750
## 6  {Survived=Yes}                     => {Age=Adult}   0.2971377 0.9198312  0.9677574
## 7  {Class=Crew}                       => {Sex=Male}    0.3916402 0.9740113  1.2384742
## 8  {Class=Crew}                       => {Age=Adult}   0.4020900 1.0000000  1.0521033
## 9  {Survived=No}                      => {Sex=Male}    0.6197183 0.9154362  1.1639949
## 10 {Survived=No}                      => {Age=Adult}   0.6533394 0.9651007  1.0153856
## 11 {Sex=Male}                         => {Age=Adult}   0.7573830 0.9630272  1.0132040
## 12 {Sex=Female,Survived=Yes}          => {Age=Adult}   0.1435711 0.9186047  0.9664669
## 13 {Class=3rd,Sex=Male}               => {Survived=No} 0.1917310 0.8274510  1.2222950
## 14 {Class=3rd,Survived=No}            => {Age=Adult}   0.2162653 0.9015152  0.9484870
## 15 {Class=3rd,Sex=Male}               => {Age=Adult}   0.2099046 0.9058824  0.9530818
## 16 {Sex=Male,Survived=Yes}            => {Age=Adult}   0.1535666 0.9209809  0.9689670
## 17 {Class=Crew,Survived=No}           => {Sex=Male}    0.3044071 0.9955423  1.2658514
## 18 {Class=Crew,Survived=No}           => {Age=Adult}   0.3057701 1.0000000  1.0521033
## 19 {Class=Crew,Sex=Male}              => {Age=Adult}   0.3916402 1.0000000  1.0521033
## 20 {Class=Crew,Age=Adult}             => {Sex=Male}    0.3916402 0.9740113  1.2384742
## 21 {Sex=Male,Survived=No}             => {Age=Adult}   0.6038164 0.9743402  1.0251065
## 22 {Age=Adult,Survived=No}            => {Sex=Male}    0.6038164 0.9242003  1.1751385
## 23 {Class=3rd,Sex=Male,Survived=No}   => {Age=Adult}   0.1758292 0.9170616  0.9648435
## 24 {Class=3rd,Age=Adult,Survived=No}  => {Sex=Male}    0.1758292 0.8130252  1.0337773
## 25 {Class=3rd,Sex=Male,Age=Adult}     => {Survived=No} 0.1758292 0.8376623  1.2373791
## 26 {Class=Crew,Sex=Male,Survived=No}  => {Age=Adult}   0.3044071 1.0000000  1.0521033
## 27 {Class=Crew,Age=Adult,Survived=No} => {Sex=Male}    0.3044071 0.9955423  1.2658514

## Too Much Information!
## We are only interested in the "Survived" attribute (i.e. our class attribute)
## Show rules with rhs containing "Survived" only

rules <- apriori(titanic.raw,
                 control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No",
                                         "Survived=Yes"),
                                   default="lhs"))

## Too many numbers in the output!  Round to three decimal places
quality(rules) <- round(quality(rules), digits=3)

## We want to see the rules ordered by their quality, so we will order rules by lift

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

##     lhs                                  rhs            support confidence lift 
## 1  {Class=2nd,Age=Child}             => {Survived=Yes} 0.011   1.000      3.096
## 7  {Class=2nd,Sex=Female,Age=Child}  => {Survived=Yes} 0.006   1.000      3.096
## 4  {Class=1st,Sex=Female}            => {Survived=Yes} 0.064   0.972      3.010
## 10 {Class=1st,Sex=Female,Age=Adult}  => {Survived=Yes} 0.064   0.972      3.010
## 2  {Class=2nd,Sex=Female}            => {Survived=Yes} 0.042   0.877      2.716
## 5  {Class=Crew,Sex=Female}           => {Survived=Yes} 0.009   0.870      2.692
## 11 {Class=Crew,Sex=Female,Age=Adult} => {Survived=Yes} 0.009   0.870      2.692
## 8  {Class=2nd,Sex=Female,Age=Adult}  => {Survived=Yes} 0.036   0.860      2.663
## 9  {Class=2nd,Sex=Male,Age=Adult}    => {Survived=No}  0.070   0.917      1.354
## 3  {Class=2nd,Sex=Male}              => {Survived=No}  0.070   0.860      1.271
## 12 {Class=3rd,Sex=Male,Age=Adult}    => {Survived=No}  0.176   0.838      1.237
## 6  {Class=3rd,Sex=Male}              => {Survived=No}  0.192   0.827      1.222

## AAAAAAAAHHH!  Beautiful!  Sort of ... 
## The output shows several duplicate (=redundant) rules
## Handling redundant rules

inspect(rules.sorted[1:2])
inspect(rules.sorted[6:7])

## Rule #2 provides no extra knowledge in addition to rule #1,
## Since rules #1 tells us that all 2nd-class children survived.
## When a rule (such as #2) is a super rule of another rule (#1)
## and the former has the same or a lower lift, the former rule
## (#2) is considered to be redundant.
## Other redundant rules in the above result are rules #4, #7
## and #8, compared respectively with #3, #6 and #5.

## Find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
redundant <- colSums(subset.matrix, na.rm = T) >= 1

## Which rules are redundant
which(redundant)

## [1] 2 4 7 8

## remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

## lhs                               rhs            support confidence lift 
## 1  {Class=2nd,Age=Child}          => {Survived=Yes} 0.011   1.000      3.096
## 4  {Class=1st,Sex=Female}         => {Survived=Yes} 0.064   0.972      3.010
## 2  {Class=2nd,Sex=Female}         => {Survived=Yes} 0.042   0.877      2.716
## 5  {Class=Crew,Sex=Female}        => {Survived=Yes} 0.009   0.870      2.692
## 9  {Class=2nd,Sex=Male,Age=Adult} => {Survived=No}  0.070   0.917      1.354
## 3  {Class=2nd,Sex=Male}           => {Survived=No}  0.070   0.860      1.271
## 12 {Class=3rd,Sex=Male,Age=Adult} => {Survived=No}  0.176   0.838      1.237
## 6  {Class=3rd,Sex=Male}           => {Survived=No}  0.192   0.827      1.222

## And now, so what?  What does it all mean?

inspect(rules.pruned[1])
## lhs                      rhs            support confidence lift 
## 1 {Class=2nd,Age=Child} => {Survived=Yes} 0.011   1          3.096

## Did children in 2nd class have a higher survival rate than other children?
## The rule states only that all children in class 2 survived, but
## provides no information at all to compare the survival rates of different classes.
## What about everybody else?  We need to investigate further!

rules <- apriori(titanic.raw, control = list(verbose=F),
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(default="none", rhs=c("Survived=Yes"),
                                   lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                         "Age=Child", "Age=Adult")))
rules.sortedbyconf <- sort(rules, by="confidence")
inspect(rules.sortedbyconf)

## lhs                      rhs            support     confidence lift     
## 1 {Class=2nd,Age=Child} => {Survived=Yes} 0.010904134 1.0000000  3.0956399
## 2 {Class=1st,Age=Child} => {Survived=Yes} 0.002726034 1.0000000  3.0956399
## 5 {Class=1st,Age=Adult} => {Survived=Yes} 0.089504771 0.6175549  1.9117275
## 4 {Class=2nd,Age=Adult} => {Survived=Yes} 0.042707860 0.3601533  1.1149048
## 3 {Class=3rd,Age=Child} => {Survived=Yes} 0.012267151 0.3417722  1.0580035
## 6 {Class=3rd,Age=Adult} => {Survived=Yes} 0.068605179 0.2408293  0.7455209

## Here is our answer.  Children in 2nd class were, indeed, the luckier ones on the ship.

## Ensure your arulesViz package installed correctly for the next few lines
## Let's visualize the output with a scatter plot

library(arulesViz)
plot(rules.all)

## Let's visualize the output with a grouped matrix

plot(rules.all, method = "grouped")

## Let's make a graph for the rules

plot(rules.all, method = "graph")

## How about a parallel coordinates plot?

plot(rules.all, method = "paracoord", control = list(reorder = TRUE))

