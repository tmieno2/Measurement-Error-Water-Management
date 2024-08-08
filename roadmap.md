# 11/23/2023 

## Analysis

Codes to do analysis on quota and trading systems with heterogeneous farmers are created (**Codes/1_2_quota_analysis_heterogeneous_prod.qmd**) 

**Findings**:
  + Water quota is better than energy quota
  + Water trading is better than energy trading
  + Welfare loss of using energy is less when trading is used

**Questions**:
  + Is energy quota always worse? Probably not. Correlation between optimal water use and effective water quota is the key. 

# Points to make (12/09/2023)

## Using a single coefficient (**average** water to energy conversion rate) to find the target proxy use from target water use
  + `results`: welfare loss and greater water use reduction than water-based quota
  + `why``: give too much water to those who do not need (allocation limits going over the optimal water user under no water use constraint)

  + `results`: the greater the measurement error, the greater the economic welfare loss and lower water use (comparative statics on the variance of the measurement error)
  + `question`: how much do you need to overshoot?

  + `question`: how do the results above differ based on how stringent the target water use is?

## Using a single coefficient that is larger than the **average** water to energy conversion rate

  + If you manage to identify the target proxy level that would achieve the target water level, how do water-based and energy based compare to each other in welfare?

## What happens under the proxy and water trading market?

# Notes (12/10/2023)

+ When quota is used, those who have lower optimal water use than the quota does not use up the quota, resulting in a slack in water use (overachieve the water use target).

+ Measurement error that is independent of water use does not necessarily create more slack. A lower water limit can be given to a low-water-use farmers, and a high water limit can be given to a high-water-use farmers, which would result in higher water use. However, the opposite happens with the same probability, which would result in a lower water use.

+ Energy use limit obtained by applying average energy efficiency to the target water quota would result in a higher water use because energy limit and optimal water use are negatively correlated. 