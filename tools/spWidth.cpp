#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <gsl/gsl_sf.h>

//Program to give single particle  width

using namespace std;

int main(void) {

  double E,r0;
  int L,fM;

  cout << "Enter E(MeV) ,L(ang mom), r0 (radius constant), Frag mass" << endl;
  cin >>E  >> L >> r0 >> fM;

  // Get Constants
    double hBarC = 197.32696310; // MeV fm
    double Nmass = 939.565346; //  MeV
    double amu = 931.494028; // from Nist
    double nucRad = r0; // fm, i.e. r = r0 * A^1/3
    int angMom = L; // set ang mom for decay
    double fragMass = fM; // in amu
   
    
    double redMass = (((Nmass/amu)*fragMass)/(Nmass/amu + fragMass))*amu; // reduced mass from lane in amu
    double k = sqrt(2.0 * redMass) / hBarC; // k in terms of sqrt(Energ)
    double En=E; // Convert to energy in MeV

    
    int NUM;
    double Radius = 0.0;
    
    for (NUM = 1; NUM <= 2; NUM++ ) {

      if (NUM == 1) {
	Radius = nucRad * (pow(fragMass,1.0/3.0)); // Nuclear Radius (A1^(1/3)) 
      }
      if (NUM == 2) { 
	Radius = nucRad * (pow(fragMass,1.0/3.0) + 1); //Channel Radius    
      }
      double x = k*Radius*sqrt(En); // X = k*R with energy included
    
    //Call functions for Penetribility
    double jl = gsl_sf_bessel_jl (angMom,x) * x; // Regular Spherical Bessel * x
    double nl = gsl_sf_bessel_yl (angMom,x) * x; // Irregular Spherical Bessel * x 
    double PenL = 1 / (nl*nl + jl*jl); // Penetrability as a function of E,W,L 
    
    
    double Gamma = 0.0;
    
    if (angMom == 0) {
      Gamma = (2 * hBarC * hBarC * x) / (redMass * Radius * Radius);
    } else {
      Gamma = (2 * hBarC * hBarC * x * PenL * (2 * angMom - 1)) / (redMass * Radius * Radius * (2 * angMom + 1));
    }
    
    //cout << " NUM is :" << NUM << endl;
    cout << " Radius Value Is : " << Radius << endl;
    cout << " the single partial width in MeV: " << Gamma << endl;
  
    }
    
    //cout << " the single partial width in MeV (ch): " << chGamma << endl;
    //cout << " Channel Radius Value Is : " << chRadius << endl;
    
}
