#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <gsl/gsl_sf_coupling.h>

//Program to give clebsch gordans

using namespace std;

int main(int argc, char *argv[]) {

  if (argc!=7) {
    printf("(j1 j2 m1 m2 | j m)\n");
  } else {
    
    double j1_d=atof(argv[1]);
    double j2_d=atof(argv[2]);
    double m1_d=atof(argv[3]);
    double m2_d=atof(argv[4]);
    double j_d=atof(argv[5]); 
    double m_d=atof(argv[6]);

    if (m1_d+m2_d!=m_d) printf("m1+m2!=m\n");
    
    int j1=j1_d*2;
    int j2=j2_d*2;
    int j=j_d*2;
    int m1=m1_d*2;
    int m2=m2_d*2;
    int m=m_d*(-2);
    
    
    double const1=pow((-1.),(m_d+j1_d-j2_d));
    double const2=sqrt(2.*j_d+1);
    double cg = const1*const2*gsl_sf_coupling_3j(j1,j2,j,m1,m2,m);
    
    printf("%2.4f\n",cg);
  }
  return 0;    
}
