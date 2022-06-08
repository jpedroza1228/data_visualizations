---
output: 
  html_document: 
    theme: cerulean
    css: https://rsacdn.link/teachthis/css/style.css
---

## Modification of Diet in Renal Disease (MDRD)

From [NIDDK](https://repository.niddk.nih.gov/studies/mdrd/):  

> The Modification of Diet in Renal Disease (MDRD) study consisted of two randomized clinical trials that investigated whether protein restriction and control of blood pressure had an effect on the progression of chronic kidney disease (CKD). The study tested two hypotheses—that (1) a reduction in dietary protein and phosphorous intake and (2) the maintenance of blood pressure at a level below that usually recommended safely and effectively delays the progression of CKD.

Our data is from Study 2, which included patients with relatively advanced renal disease (GFR between 13 and 24 ml/min). From [NIDDK](https://repository.niddk.nih.gov/studies/mdrd/):

> In study 2, 255 patients with GFR of 13 to 24 ml/min/1.73 m2 were randomly assigned to the low-protein diet (0.58 g per kilogram per day) or a very-low-protein diet (0.28 g per kilogram per day) with a keto acid-amino acid supplement, and a usual- or a low-blood-pressure group (same values as those in study 1). The length of follow-up varied from 18-to-45-months, with monthly evaluations of the patients. The primary outcome was the change in GFR rate over time. 

### Dropout

Many patients dropped out of the study before completion. Whether or not a patient dropped out is captured in the `dropout` variable. Reasons for dropout included dialysis, kidney transplant, death, and other medical reasons. 

### Variables

`ptid`: Patient identifier

`gfr`: Glomerular filtration rate in milliliters per minute. A measure of how much blood the kidneys filter per minute. 

`months`: Number of months after the start of the study that the measurement was taken. 

`dietl_n`: Was the participant assigned to the low-protein, normal-blood pressure diet? (0 = No, 1 = Yes)

`dietl_l`: Was the participant assigned to the low-protein, low-blood prssure diet? (0 = No, 1 = Yes)

`dietk_n`: Was the participant assigned to the very low-protein, normal-blood pressure diet? (0 = No, 1 = Yes)

`dietk_l`: Was the participant assigned to the very low-protein, low-blood pressure diet? (0 = No, 1 = Yes)

`log_prot`: Logarithm of the grams of protein consumed per day.

`followup`: Number of months until patient follow-up.

`dropout`: Did the patient drop out of the study? (0 = No, 1 = Yes)

### Variables - Supplemental

Note: this data is simulated and is not from the original MDRD study.

`ptid`: Patient identifier

`sex`: Sex

`age`: Age (years)

`height`: Height (meters)

`weight`: Weight (kilograms)
