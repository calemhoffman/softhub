#/bin/bash



if [ -e "fort.3" ]; then 
	echo ""
	echo "processing data"
	echo ""
else
	echo ""	
	echo "You need to create fort.3 form AD fisrt."
	echo ""
fi

if [ -e "fort3.dat" ]; then rm fort3.dat; fi

cmd='../tools/convert_to_xmgrace'
format='../tools/par/a1.4_1439-1.par' # the parameters from xmgrace
# note, menu->Plot->Save parameters


${cmd}

xmgrace -nxy fort3.dat -par ${format}  &
