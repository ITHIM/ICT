## Welcome to the Impacts of Cycling Tool (ICT)

The ICT is designed to do two things:

1. As a data visualisation tool to look at how people travel in England and how active they are
2. To look at how things would change under different scenarios in which the probability of being a cyclist increases

The starting point for the ICT is weekly data from the National Travel Survey for England 2012. This allows us to look at how different groups in the population travel. To estimate physical activity we use both travel activity and also non-travel physical activity &#45; the latter comes from combining the National Travel Survey data with data on similar people from the Health Survey for England 2012.

The scenarios are based around assuming that some non-cyclists become cyclists. By this we mean they have the same probability of cycling each trip as current cyclists. Note that if someone becomes a cyclist then that doesn&#39;t mean they cycle all or even most of their trips - people are more likely to cycle short trips than long ones, and our analysis indicates that the distance people are willing to cycle falls more quickly for women and older people. For this reason we probabilistically model which of the trips are cycled based on the trip distances and the person&#39;s age and gender. This process means that some people who become cyclists may not cycle any trips in a given week.

In modelling scenarios in which some non-cyclists become cyclists, we use three input parameters:

1. The *Cycling Multiplier* is the overall population-level increase in the probability of being a cyclist.
2. In *Equity* scenarios we assume that men and women and older and younger people are equally likely to become cyclists. Without Equity we assume current inequalities remain in the probability of becoming a cyclist.
3. In *Ebike* scenarios we assume that new cyclists have got an ebike and so will be willing to cycle longer trips. We assume that with an ebike all population groups have the same probability of cycling a trip of a given length.

Thus by itself the Ebike scenario equalises the distances that people of different ages and genders are willing to cycle, but leaves unchanged age/gender differences in the probability of becoming a cyclist. By itself, the *Equity*  scenario does the opposite, equalising the probability that people of different ages and genders become cyclsits, but leaving unchanged age/gender differences in the distance they are willing to cycle. In combination an *Equity + Ebike* scenario equalises both the probability of being a cyclist and the distance that people are willing to cycle.

The effects of people switching their trips to cycling are assessed across multiple domains including:

*	health (physical activity levels, mortality rates)
*	transport related CO2 emissions
*	transport (trips time and mode shift)
*	equity (uptake rate of cycling by women, the elderly or ethnic minorities)

The impact on mortality risk of increasing activity is estimated based on this study <a href="http://www.thelancet.com/journals/lancet/article/PIIS0140-6736%2811%2960749-6/abstract" target="_blank">here</a>.
