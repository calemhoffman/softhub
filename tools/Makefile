
LIBS=-L/usr/lib -lgsl -lgslcblas -lm

clebsch:
	rm -rf clebsch
	g++ $(LIBS) -o clebsch clebsch.cpp

spWidth:
	rm -rf spWidth
	g++ $(LIBS) -o spWidth spWidth.cpp

partWidth:
	rm -rf partWidth
	g++ $(LIBS) -o partWidth partWidth.cpp

rmatrix:
	rm -rf rmatrix
	g++ $(LIBS) -o rmatrix rmatrix.cpp

clean:
	rm -rf clebsch spWidth partWidth rmatrix *~
