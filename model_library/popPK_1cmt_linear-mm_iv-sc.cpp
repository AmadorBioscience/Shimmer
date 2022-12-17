
$PARAM @annotated
TVCL   : 1.0000 : Clearance (L/day)
TVV2   : 25.000 : Central volume of distribution (L)
TVKA   : 0.5000 : Absorption rate constant (1/day)
TVF1   : 0.7500 : Bioavailability (unitless)
TVVMAX : 2.5000 : Maximum MM reaction velocity (mg/day)
TVKM   : 1.1000 : Michaelis-Menten (MM) constant (mg/L)
DOSE   : 0      : Dose amount (mg)
WT     : 70.0   : Baseline subject body weight (kg)
WTREF  : 70.0   : Reference body weight (kg)
WTCL   : 0.75   : Power law exponent for body weight on CL (unitless)
WTV2   : 1.00   : Power law exponent for body weight on V2 (unitless)


$CMT @annotated
ABS    : SC dose absorption compartment (mg)
CENT   : Central compartment (mg)


$OMEGA @annotated
ETA_CL   : 0.1000 : IIV on CL (L/day)
ETA_V2   : 0.1000 : IIV on V2 (L)
ETA_KA   : 0.0100 : IIV on KA (1/day)
ETA_F1   : 0.0100 : IIV on F1 (unitless)
ETA_VMAX : 0.0250 : IIV on VMAX (mg/day)
ETA_KM   : 0.0100 : IIV on KM (mg/L)


$SIGMA @annotated
PROP : 0.0001 : Proportional residual error (unitless)
ADD  : 0.1000 : Additive residual error (mg/L)


$MAIN
double CL = TVCL * exp(ETA_CL) * pow(WT/WTREF, WTCL);
double V2 = TVV2 * exp(ETA_V2) * pow(WT/WTREF, WTV2);
double KA = TVKA * exp(ETA_KA);
double F1 = (exp(TVF1 + ETA_F1) / (1 + exp(TVF1 + ETA_F1)));
double VMAX = TVVMAX * exp(ETA_VMAX);
double KM = TVKM * exp(ETA_KM);

F_ABS = F1;

double S2 = V2;  // Scaling factor for central volume:  dose = mg; conc = mg/L


$ODE
double CLNL =  VMAX/(KM+(CENT/S2));                     // non-linear MM clearance (L/day)
dxdt_ABS    = -KA*ABS;
dxdt_CENT   =  KA*ABS - (CL/V2)*CENT - (CLNL/V2)*CENT;


$TABLE
double IPRED = (CENT/S2);
double CP = IPRED;
double DV = IPRED*(1+PROP)+ADD;


// resimulate residual error (SIGMA) parameters if concentration is negative 
int i = 1;
while(DV < 0 & i <= 100) {
	simeps();
	DV = IPRED * (1 + PROP) + ADD;
	i++;
}

if(i > 100) mrg::report("Positive concentrations could not be obtained.");


$CAPTURE @annotated
DOSE   : Dose amount (mg)
WT     : Baseline subject body weight (kg)
WTREF  : Reference body weight (kg)
DV     : Plasma concentration (mg/L)
IPRED  : Individual predicted plasma concentration (mg/L)
 
