
$PARAM @annotated
CLIN   : 0.150 : clearance (L/day)
VCIN   : 3.000 : central volume of distribution for free drug (assume same volume for free target & drug-target complex)
Q      : 0.450 : intercompartmental clearance (L/day)
VP     : 3.000 : peripheral volume for free drug (L)
F1     : 0.750 : Bioavailability for absorption compartment (unitless)
KA1    : 1.000 : absorption rate constant (1/day)
KON    : 8.000 : association rate constant
KOFF   : 8.000 : dissociation rate constant
KINT   : 0.040 : internalization rate constant
R0IN   : 5.000 : initial concentration of total target
KDEG   : 0.200 : target degredation rate constant (1/day)
WTCL   : 0.750 : Power law exponent for body weight on CL (unitless)
WTVC   : 1.000 : Power law exponent for body weight on VC (unitless)
DOSE   : 0     : Dose amount (nmoles)
WT     : 70.0  : Baseline subject body weight (kg)
WTREF  : 70.0  : Reference body weight (kg)


$CMT @annotated
ABS     : extravascular dosing compartment
CENT    : total drug in central compartment (molar amount)
PERIPH  : free drug in peripheral compartment (molar amount)
TARGET  : total target concentration in central (concentration)


$OMEGA @annotated @block @correlation
ETA_CL   : 0.116         : IIV on CL (L/day)
ETA_VC   : 0.700  0.116  : IIV on VC (L)

$OMEGA @annotated
ETA_R0   : 0.040  : IIV on R0 (nM)


$SIGMA @annotated
PROP_DRUG    : 0.040 : Free Drug    - Proportional residual error (unitless)
PROP_TARGET  : 0.000 : Free Target  - Proportional residual error (unitless)
PROP_COMPLEX : 0.000 : Free Complex - Proportional residual error (unitless)


$GLOBAL
namespace tmdd {
  double _dxdt_CP=0.0;
  double TMDDR0 = 0.0;
}
#define KSYN (R0*KDEG)    // target synthesis rate
// #define CTOT (CENT/VC)    // total drug in the central compartment (amount/volume)


$MAIN
double CL  = CLIN * exp(ETA_CL) * pow(WT/WTREF, WTCL);
double VC  = VCIN * exp(ETA_VC) * pow(WT/WTREF, WTVC);
double R0  = R0IN * exp(ETA_R0);  // baseline total target concentration (nM)
double KM  = (KINT + KOFF)/KON;   // dissociation constant (nM)
double K20 = CL/VC;  					  	// elimination rate constant for free drug (day-1)
double K23 = Q/VC;    						// central    -> peripheral rate constant for free drug (day-1)
double K32 = Q/VP;    						// peripheral -> central rate constant for free drug (day-1)

// Initial Conditions
TARGET_0 = R0;          // initial concentration of total target (nM)

// Dosing Parameters
tmdd::TMDDR0 = _R(2);   // infusion rate of free drug into central compartment (molar amount/time)
_F(1) = F1;             // bioavailability of free drug in depot (ABS)
_F(2) = 1/VC;           // convert IV infusion doses (molar amount) into concentration units


$ODE
// double CONC  = 0.5*((CTOT-TARGET-KM) + sqrt(pow((CTOT-TARGET-KM), 2.0) + 4.0*KM*CTOT));                       // Free  Drug concentration (central)
// dxdt_CENT    =  (KA1*ABS + tmdd::TMDDR0)/VC - (K20 + K23)*CENT - (VMAX*CENT/(KM + CENT)) + K32*PERIPH/VC;     // Free  Drug concentration (central)

double VMAX  =  TARGET * KINT;                                                                                // Max elimination rate of complex (nM/day)	
dxdt_ABS     = -KA1*ABS;                                                                                      // Free  Drug molar amount (depot)
dxdt_CENT    =  (KA1*ABS)/VC - (K20 + K23)*CENT - (VMAX*CENT/(KM + CENT)) + K32*PERIPH/VC;                    // Free  Drug concentration (central)
dxdt_PERIPH  =  K23*CENT*VC - K32*PERIPH;                                                                     // Free  Drug molar amount (peripheral)
dxdt_TARGET  =  0;                                                                                            // Total Target concentration (central)


$TABLE
// double CONCFD = 0.5*((CTOT-TARGET-KM) + sqrt(pow((CTOT-TARGET-KM), 2.0)+4.0*KM*CTOT));     // Free Drug concentration (central)
// double CONCRC = (TARGET*CONCFD)/(KM + CONCFD);													                    // Drug-Target Complex concentration (central)
// double CONCFR = TARGET - CONCRC;                                                           // Free Target concentration (central)

double CONCFD = CENT;                               // Free Drug concentration (central)         
double IPRED = CONCFD;
double DV = CONCFD * (1 + PROP_DRUG);


// resimulate residual error (SIGMA) parameters if concentration is negative 
int i = 1;
while(DV < 0 & i <= 100) {
  simeps();
  DV = IPRED * (1 + PROP_DRUG);
  i++;
}

if(i > 100) mrg::report("Positive concentrations could not be obtained.");



$CAPTURE @annotated
// CTOT   : Total drug in the central compartment
DOSE   : Dose amount (mg)
WT     : Baseline subject body weight (kg)
WTREF  : Reference body weight (kg)
DV     : Unbound drug in the central compartment
IPRED  : Unbound drug in the central compartment
 
