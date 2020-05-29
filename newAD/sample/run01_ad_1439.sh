#!/bin/bash
cmd=../tools/num_to_letter

run=1; x=$($cmd $run); 


GammaE=1439
aligment=1.4

J1=4.5
J2=2.5


# counts measured under total projection.
#-------------------------------------------------------
(
cat<<EOF
 Gamma-ray energy = ${GammaE}
 Theta    Yexp    Yerr 
 ----------------------- 
17.27	96.85	4.84
31.72	99.49	4.97
37.38	98.44	4.92
50.07	98.14	4.91
58.28	94.47	4.72
69.82	99.49	4.97
79.95	105.67	5.28
90	111.73	5.59

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
