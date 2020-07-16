#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <gsl/gsl_sf.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_deriv.h>
     
//Program to give partial width

using namespace std;


double f (double x, void * params)
     {
       return -(18+3*pow(x,2))/(9+3*pow(x,2)+pow(x,4));
     }

double g (double x, void * params)
     {
       return -1/(1+pow(x,2));
     }
     
int main(void) {

  double E,W;
  int L,fM;

  cout << "Enter E(MeV) ,W (MeV),L , Frag Mass (amu)" << endl;
  cin >>E  >> W >> L>> fM;

  // Get Constants
    double hBarC = 197.3269631; // MeV fm
    double Nmass = 939.565346; //  MeV
    double amu = 931.494028; // from Nist
    double nucRad = 1.4; // fm from Lane and Thomas pg 266 *1.2*
    double redGamma = W; // the reduced width
    double Const = (1.0 /3.14159265 ); // Const for BW
    int angMom = L; // set ang mom for decay
    //double fragMass = 24; // in amu
    double fragMass = fM;
    double Pmass = 938.272;
    double redMass = (((Nmass/amu)*fragMass)/(Nmass/amu + fragMass))*amu; // reduced mass from lane in amu
cout << "reduced mass is : " << redMass/amu << endl;	
    double k = sqrt(2.0 * redMass) / hBarC; // k in terms of sqrt(Energ)
    double Radius = nucRad * (pow(fragMass,1.0/3.0) + pow(Nmass/amu, 1.0/3.0)); // Nuclear Radius (A1^(1/3) + A2^(1/3)) 
    //A1 = fragment A2 = neutron pg 266-267 Lane and Thomas
    
    // Create the BW function //    
     double En=E; // Convert to energy in MeV
     double x = k*Radius*sqrt(En); // X = k*R with energy included
double xCheck = 0.21954*sqrt(En)*Radius*sqrt((redMass/Pmass));	      
      //Call functions for Penetribility
     double jl = x * gsl_sf_bessel_jl (angMom,x); // Regular Spherical Bessel * x
     double nl = x * gsl_sf_bessel_yl (angMom,x); // Irregular Spherical Bessel * x
     double PenL = x / (nl*nl + jl*jl); // Penetrability as a function of E,W,L 

     double Gamma = 2 * redGamma * redGamma * PenL; // Width 
     
double result,abserr;

 if (angMom == 2) {
   gsl_function F;
   
   F.function = &f;
   F.params = 0;
   
   
   gsl_deriv_central (&F, x, 1e-10, &result, &abserr);
 }
 
 if (angMom == 1) {
   gsl_function G;
   
   G.function = &g;
   G.params = 0;
   
   
   gsl_deriv_central (&G, x, 1e-10, &result, &abserr);
 }
 
 
 double dSdE = (x/(2*E))*result;
 double dSdE2 = ((3)*(k*k*Radius*Radius))*(pow(x,4)+9+12*pow(x,2))/(pow((pow(x,4)+9+3*pow(x,2)),2));
 
     
 double GammaObs = Gamma*(1+(redGamma*redGamma/Radius*dSdE)); //L = 2
 
 cout << " the partial width in MeV: " << Gamma << endl;
 cout << " the observed width is (MeV): " << GammaObs << endl;
 cout << " Radius Value Is : " << nucRad << endl;
 
 cout << " the (dSdE): " << dSdE << endl;
 cout << " the dsde2 is " << dSdE2 << endl;
 cout << " the new x is : " << x << endl;
 cout << " xCheck is  " << xCheck << endl;
 
}
