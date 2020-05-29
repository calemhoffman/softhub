	dimension bk1(6,4), rk0(6,4), rk1(6,4), rk2(6,4)
	dimension thet(10), yexp(10), yerr(10), yth(10)
	dimension atwo(6,181), afour(6,181), AJ1(6), AJ2(6), deltamin(6)

	common /tensors/ Rk0, Rk1, Rk2, Bk1, Qd2, Qd4
	common /A2_A4/ atwo, afour, deltamin, AJ1, AJ2
	common /data/ faclog
	common /input/ thet, yexp, yerr
	common /number/ num, lmax
	common /terminal/ jterm
