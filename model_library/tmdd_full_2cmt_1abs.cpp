
$PARAM @annotated
TVCL   : 0.150 : clearance (L/day)
TVVC   : 3.000 : central volume of distribution for free drug (assume same volume for free target & drug-target complex)
Q      : 0.450 : intercompartmental clearance (L/day)
VP     : 3.000 : peripheral volume for free drug (L)
F1     : 0.750 : Bioavailability for absorption compartment (unitless)  
KA1    : 1.000 : absorption rate constant (1/day)
KON    : 8.000 : association rate constant
KOFF   : 8.000 : dissociation rate constant
KINT   : 0.040 : internalization rate constant
TVR0   : 5.000 : initial concentration of target
KDEG   : 0.200 : target degredation rate constant (1/day)
WTCL   : 0.750 : Power law exponent for body weight on CL (unitless)
WTVC   : 1.000 : Power law exponent for body weight on VC (unitless)
DOSE   : 0     : Dose amount (nmoles)
WT     : 70.0  : Baseline subject body weight (kg)
WTREF  : 70.0  : Reference body weight (kg)


$CMT @annotated
ABS     : extravascular dosing compartment
CENT    : unbound drug in central compartment
TARGET  : concentration of target
COMPLEX : concentration of drug-target complex
PERIPH  : unbound drug in peripheral compartment


$OMEGA @annotated @block @correlation
ETA_CL   : 0.116         : IIV on CL (L/day)
ETA_VC   : 0.700  0.116  : IIV on VC (L)

$OMEGA @annotated
ETA_R0   : 0.040  : IIV on R0 (nM)


$SIGMA @annotated
PROP_DRUG    : 0.040 : Free Drug    - Proportional residual error (unitless)
PROP_TARGET  : 0.040 : Free Target  - Proportional residual error (unitless)
PROP_COMPLEX : 0.000 : Free Complex - Proportional residual error (unitless)


$GLOBAL
#define KSYN (R0*KDEG)  // target synthesis rate
#define CP (CENT/VC)    // unbound drug in the central compartment (amount/volume)


$MAIN
double CL  = TVCL * exp(ETA_CL) * pow(WT/WTREF, WTCL);
double VC  = TVVC * exp(ETA_VC) * pow(WT/WTREF, WTVC);
double R0  = TVR0 * exp(ETA_R0);  // baseline target concentration (nM)
double K20 = CL/VC;  					  	// elimination rate constant for free drug (day-1)
double K25 = Q/VC;    						// central    -> peripheral rate constant for free drug (day-1)
double K52 = Q/VP;    						// peripheral -> central rate constant for free drug (day-1)

// Initial Conditions
TARGET_0 = R0;          // initial concentration of target

// Dosing Parameters
_F(1) = F1;
_F(2) = 1.0;      // keep IV infusion doses in units of molar amount (nmoles)


$ODE
dxdt_ABS     = -KA1*ABS;
dxdt_CENT    =  KA1*ABS - (K20+K25)*CP*VC - KON*CP*TARGET*VC + KOFF*COMPLEX*VC + K52*PERIPH;
dxdt_TARGET  =  KSYN - KDEG*TARGET - KON*CP*TARGET + KOFF*COMPLEX;
dxdt_COMPLEX =  KON*CP*TARGET - (KINT+KOFF)*COMPLEX;
dxdt_PERIPH  =  K25*CP*VC - K52*PERIPH;


$TABLE
double TOTAL = TARGET+COMPLEX;
double IPRED = CP;
double DV = CP * (1 + PROP_DRUG);


// resimulate residual error (SIGMA) parameters if concentration is negative 
int i = 1;
while(DV < 0 & i <= 100) {
	simeps();
	DV = IPRED * (1 + PROP_DRUG);
	i++;
}

if(i > 100) mrg::report("Positive concentrations could not be obtained.");



$CAPTURE @annotated
CP     : Unbound drug in the central compartment
TOTAL  : Total concentration of target (complexed and uncomplexed)
DOSE   : Dose amount (mg)
WT     : Baseline subject body weight (kg)
WTREF  : Reference body weight (kg)
DV     : Unbound drug in the central compartment
IPRED  : Unbound drug in the central compartment
 
