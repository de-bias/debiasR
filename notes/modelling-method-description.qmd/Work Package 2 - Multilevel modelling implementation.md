# Work Package 2 - Multilevel modelling implementation  
  
The WP will deliver a generalisable multilevel modelling regression modelling framework to create correction factors to mitigate biases in mobile-phone (MP) derived mobility data, and generate bias-adjusted mobility counts. Intuitively, the idea is to follow similar principles to post-stratification and imputation methods used for the creation of weights to correct survey estimates, impute missing data, and make these estimates representative of the census population. Our proposed approach will be designed to correct bias arising from differences in the access and use of digital technology. It builds on recent work by Aparicio-Castro et al. (2023) which is designed to generate unbiased migration estimates from census data, correcting for biases due to migration measurement and data quality differences. We will develop a two-stage Bayesian modelling framework. The first stage focuses on generating bias-adjusted mobility counts from observed MP data (MPD). The second stage focuses on imputing data for specific origin-destination mobility counts which are not captured in MPD or removed due to privacy concerns.   
  
In the first stage, we will develop a two-level hierarchical model to estimate bias-adjusted “true” mobility flows. This model assumes that observed mobility counts derived from MPD offer an imperfect representation of the “true” unobserved mobility counts. Thus, MP-derived mobility counts can be assumed to be a function of two components: (1) the true mobility count, and (2) biases arising from differences in the access and use of digital technology. This specification implies that if the biases were zero, MP-derived mobility counts would converge to the true mobility counts. Formally, this can be expressed as a two-level model using a varying intercept as a function of origin and destinations attributes, as follows:   
  
Level 1:  
$$  
F^{mpd}_{ij} = f( F^{t}_{ij}, \omega e_{i})  
$$  
  
Level 2:  
$$  
F^{t}_{ij} = f(F^{t}_{0}, \beta X_{i}, \delta X_{j} )  
$$  
  
The first level focuses on estimating biases arising from MPD. The second focuses on estimating the true unobserved mobility counts as a function of differences in relevant socioeconomic, demographic and geographic attributes across origins and destinations. $F^{mpd}_{ij}$ represents MP-derived mobility counts between origin 𝑖 and destination 𝑗; $F^{t}_{ij}$ captures true unobserved mobility counts; $e_{I}$ is our origin-specific measure of bias due to differences in the use and accessibility of digital technology; $\omega$ captures the average degree of relationship between the MP-induced bias ($\omega$) and MP-derived mobility counts. It can be thought of as a correction factor indicating by how much we need to alter the true mobility counts to match the MP-derived counts given their coverage. The outcome variable $F^{mpd}_{ij}$ and $e_{I}$ represent observed data. $F^{t}_{ij}$ and $\omega$ are parameters to be estimated.   
  
In the second level, true mobility counts are estimated in a gravity-like spatial interaction modelling framework (Rowe et al. 2023). They are modelled as a function of an intercept $F^{t}_{0}$ and socioeconomic, demographic and geographic attributes of origins $X_{I}$ and destinations $X_{j}$, including geographical distance. The intercept captures the average national level of mobility. $\beta$ and $\delta$ are parameters to be estimated and capture the strength of association between the true mobility flows and origin and destination attributes. Estimating true mobility counts ($F^{t}_{ij}$) is the key objective from our model.   
