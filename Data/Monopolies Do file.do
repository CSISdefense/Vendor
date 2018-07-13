*LOGIT MODELS WITH ONE KEY VARIABLE OR THE OTHER, INTERACTIONS INCLUDED*

*Terminations on Consolidation*
logit b_Term cl_HHI_lag1 cl_Ceil cl_Days SIDV MIDV FSSGWAC BPABOA n_Fixed b_UCA HHISIDV HHIMIDV HHIFSSGWAC HHIBPABOA HHIUCA i.NAICS2

*Ceiling Breaches on Consolidation
logit b_CRai cl_HHI_lag1 cl_Ceil cl_Days SIDV MIDV FSSGWAC BPABOA n_Fixed b_UCA HHISIDV HHIMIDV HHIFSSGWAC HHIBPABOA HHIUCA i.NAICS2

*Terminations on Competition*
logit b_Term n_EffComp cl_Ceil cl_Days SIDV MIDV FSSGWAC BPABOA n_Fixed b_UCA HHISIDV HHIMIDV HHIFSSGWAC HHIBPABOA HHIUCA i.NAICS2

*Ceiling Breaches on Competition*
logit b_CRai n_EffComp cl_Ceil cl_Days SIDV MIDV FSSGWAC BPABOA n_Fixed b_UCA HHISIDV HHIMIDV HHIFSSGWAC HHIBPABOA HHIUCA i.NAICS2

*Terminations on Consolidation and Competition*
logit b_Term cl_HHI_lag1 n_EffComp cl_Ceil cl_Days SIDV MIDV FSSGWAC BPABOA n_Fixed b_UCA HHISIDV HHIMIDV HHIFSSGWAC HHIBPABOA HHIUCA i.NAICS2

*Ceiling Breaches on Consolidation and Competition*
logit b_CRai cl_HHI_lag1 n_EffComp cl_Ceil cl_Days SIDV MIDV FSSGWAC BPABOA n_Fixed b_UCA HHISIDV HHIMIDV HHIFSSGWAC HHIBPABOA HHIUCA i.NAICS2


