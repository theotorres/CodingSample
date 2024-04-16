i) The database used: sample_data.Rda
This database is a subset of the original data I used in my undergraduate thesis
entitled "A Study on the Effect of the Brazilian Conditional Cash Transfer Program on Child
Nutrition." This extracted sample covers data from the northern region of Brazil, with 1786
observations of 39 variables. The original data comes from the POF (IBGE) for the years
2017-2018. The variables are:
- ESTRATO_POF: strata of the original research sampling plan;
- ID: household identifier;
- carboidrato_f: average consumption of carbohydrates by children in the household,
measured in grams;
- trat_dom: dummy variable that identifies a household that is in the treatment group,
that receives the PBF.
- pesofinal_c: sample weights;
- branco: dummy variable that identifies if the guardians of the children are white;
- chefefamilia_mulher: dummy variable that identifies if the head of household is a
woman;
- casado: dummy variable that identifies if the guardians of the children are married;
- n_filhos: number of children in the household;
- idaderesponsavel_m: average age of guardians in the household;
- idadefilhos_m: average age of children in the household;
- renda_percapita: per capita income;
- UF_i: dummy variable that identifies each state of the country.
ii) What the code does: sample_script.R
The purpose of the script is to measure the impact of receiving conditional income
transfers through the Bolsa Família Program (PBF) on children's carbohydrate consumption.
To assess this, three methods have been considered and their algorithms developed. The
primary method is Propensity Score Matching (PSM), given that the treated households
self-select to participate in the program, leading to selection bias. The PSW method
(considering the inverse probability of treatment) and Ordinary Least Squares (OLS) are
used as comparison methods. It is important to highlight that all three models take into
account the complex sample structure, meaning they incorporate the sample weights.
Consequently, the algorithms were developed with this purpose in mind.
iii) Main outputs
Regarding the PSM method, the balance of covariates is assessed after the
matching algorithm. Thus, the "bal_t1" table shows that the variables used to align the
treatment group with the control group are comparable in their means. Furthermore, the "t1"
table presents the overall results, indicating a positive and significant impact of the Bolsa
Família Program (PBF) on carbohydrate consumption by children beneficiaries of the
program, across the three models used.
