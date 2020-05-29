#!/bin/bash
cmd=../tools/num_to_letter

run=2; x=$($cmd $run); 


GammaE=1439
aligment=2.3

J1=3.5
J2=2.5


# counts measured under 1695 gate.
#-------------------------------------------------------
(
cat<<EOF
 Gamma-ray energy = ${GammaE}
 Theta    Yexp    Yerr 
 ----------------------- 
17.27	100.10	3.00
31.72	98.90	2.97
37.38	94.73	2.84
50.07	96.33	2.89
58.28	100.77	3.02
69.82	101.95	3.06
79.95	101.68	3.05
90	111.18	3.34

EOF
) > ${GammaE}${x}.0


if [ -e "ad_input.txt" ]; then rm ad_input.txt; fi
if [ -e "fort.3" ]; then rm fort.3; fi
if [ -e "fort.4" ]; then rm fort.4; fi

#--------------------------------------( check detector Det. dimension )
(
	cat<<EOF
3.5,25,8.5        !detector dimension.
${GammaE}         !energy of gamma ray.
${GammaE}${x}.0   !the filename of the angular distribution.	
${aligment},0     !aligment, and feeding.
${J1}
${J2}
EOF
)>>ad_input.txt


xterm -fg black -bg white -fa  faceSize4 -e ../AD_code/ad &
