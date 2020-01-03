import pandas as pd
# import numpy as np
import scipy 
from scipy import stats
import statsmodels.stats.descriptivestats as sd
# import plotly.plotly as py
# import plotly.graph_objs as go
# from plotly.tools import FigureFactory as FF

############## 1 Sample Sign Test(Student_Scores) ################
# import statsmodels.stats.descriptivestats as sd
data=pd.read_excel("E:/Day Wise/Day 08 Hypothesis Testing/Data/Marks-1sample sign test.xlsx")
data
#############Normality test###############
print(stats.shapiro(data.Marks))

###################1 Sample Sign Test #############
sd.sign_test(data.Marks,mu0=82)
help(sd.sign_test)

############### Mann-Whitney test(Vehicles with and without addictive) ############
data=pd.read_excel("E:\Excelr Data\RCodes\Hyothesis Testing\Mann_whitney.xlsx")
data
data.columns="Without_additive","With_additive"
#############Normality test###############
print(stats.shapiro(data.Without_additive))
print(stats.shapiro(data.With_additive))

############## Mann-Whitney test #############
# import statsmodels.stats.descriptivestats as sd
scipy.stats.mannwhitneyu(data.Without_additive, data.With_additive)


############2 sample T Test(Marketing Strategy) ##################
#############Normality test###############
#promotion=pd.read_excel("C:\Datasets_BA\Hypothesis\Hypothesis Testing_R&Python_codes\Promotion.xlsx")

promotion=pd.read_excel("E:/Day Wise/Day 08 Hypothesis Testing/Data/Promotion.xlsx")
promotion
promotion.columns = "Credit","Promotion.Type","InterestRateWaiver","StandardPromotion"
##########Normality Test ############

print(promotion.columns)
print(scipy.stats.anderson(promotion.InterestRateWaiver,dist = 'norm'))
#15%, 10%, 5%, 2.5%, 1%    
print(scipy.stats.anderson(promotion.StandardPromotion,dist = 'norm'))
help(stats.shapiro)

######## 2 Sample T test ################
scipy.stats.ttest_ind(promotion.InterestRateWaiver,promotion.StandardPromotion,nan_policy='omit')
help(scipy.stats.ttest_ind)


############# One - Way Anova###################

cof=pd.read_excel("E:/Excelr Data/RCodes/Hyothesis Testing/ContractRenewal_Data(unstacked).xlsx")
cof
cof.columns="SupplierA","SupplierB","SupplierC"

scipy.stats.levene(cof.SupplierA, cof.SupplierB,cof.SupplierC)
help(scipy.stats.levene)

stats.f_oneway(cof.SupplierA , cof.SupplierB , cof.SupplierC)


################Chi-Square Test ################

Bahaman=pd.read_excel("E:/Day Wise/Day 08 Hypothesis Testing/Data/Bahaman.xlsx")
Bahaman

count=pd.crosstab(Bahaman["Defective"],Bahaman["Country"])
count
Chisquares_results=scipy.stats.chi2_contingency(count)
help(scipy.stats.chi2_contingency)
Chisquares_results
Chi_square=[['','Test Statistic','p-value'],['Sample Data',Chisquares_results[0],Chisquares_results[1]]]
#chisample_results=FF.create_table(Chi_square,index=True)
#chisample_results


