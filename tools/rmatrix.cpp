#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <gsl/gsl_sf_coulomb.h>

//Program to give r-matrix single-level cross section (Breit-Wigner)

using namespace std;

int main(void) {

  double E;
  double fM;
  double BigZ;

  cout << "Enter E(MeV) ; Frag mass ; Z" << endl;
  cin >>E  >> fM >> BigZ;

  // Get Constants
    double hBarC = 197.32696310; // MeV fm
    double Nmass = 939.565346; //  MeV
    double amu = 931.494028; // from Nist
    double nucRad = 1.25;

    //Calculate
    double Radius = nucRad*(1.0+pow(fM,1./3.)); // fm, i.e. r = r0 * A^1/3
    double fragMass = fM; // in amu
    double redMass = (((Nmass/amu)*fragMass)/(Nmass/amu + fragMass))*amu; // reduced mass from lane in amu
    double velocity = sqrt(2.0*E/redMass); printf("Velocity: %f\n",velocity);
    
    //Inputs for Coulomb functions
    double x = sqrt(E)*Radius*sqrt(2.0 * redMass) / hBarC; // x in terms of sqrt(Energ)
    double eta = BigZ / hBarC / velocity;
    double L=0;
    int k=0;
    
    /*____________________*/
    printf("RedMass:%f Radius:%f x:%f eta:%f\n",redMass,Radius,x,eta);

 
    double F[100],G[100];
    double Fp[100],Gp[100];
    double Fexp[100],Gexp[100];
    double Fpexp[100],Gpexp[100];

    //L,k=0,eta(10),X(20)
    int results = gsl_sf_coulomb_wave_FGp_array(L,k,eta,x,F,Fp,G,Gp,Fpexp,Gpexp);

    printf("results %d F:%f Fexp:%f\n",results,F[0],Fexp[0]);
    printf("results %d G:%f Gexp:%f\n",results,G[0],Gexp[0]);
    printf("results %d Fp:%f Fpexp:%f\n",results,Fp[0],Fpexp[0]);
    printf("results %d Gp:%f Gpexp:%f\n",results,Gp[0],Gpexp[0]);

    int l = (int)L;
    double PenL = x / (F[0]*F[0] + G[0]*G[0]);
    double Gamma = 2.0 * PenL;
    double Gamma2 = 2.0 * PenL * hBarC * hBarC / redMass / Radius / Radius;

    printf("Gamma: %f, Gamma2:%f \n",Gamma,Gamma2);

    


    
}
