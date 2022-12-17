
$PARAM @annotated
TVCL   : 1.0000 : Clearance (L/day)
TVV2   : 25.000 : Central volume of distribution (L)
TVKA   : 0.5000 : Absorption rate constant (1/day)
TVF1   : 0.7500 : Bioavailability (unitless)
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


$SIGMA @annotated
PROP : 0.0001 : Proportional residual error (unitless)
ADD  : 0.1000 : Additive residual error (mg/L)


$MAIN
double CL = TVCL * exp(ETA_CL) * pow(WT/WTREF, WTCL);
double V2 = TVV2 * exp(ETA_V2) * pow(WT/WTREF, WTV2);
double KA = TVKA * exp(ETA_KA);
double F1 = (exp(TVF1 + ETA_F1) / (1 + exp(TVF1 + ETA_F1)));

F_ABS = F1;

double S2 = V2;  // Scaling factor for central volume:  dose = mg; conc = mg/L


$ODE
dxdt_ABS    = -KA*ABS;
dxdt_CENT   =  KA*ABS - (CL/V2)*CENT;


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
 
