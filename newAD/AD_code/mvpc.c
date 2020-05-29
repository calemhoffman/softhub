#include <stdio.h>
#define  AND &&  /* define logical and symbol */
#define  OR ||   /* define logical or symbol */

adein_(x,iarray)
int *x,*iarray;
{
	int c;
	*x = 0;
	while (( c = getchar()) != '\n' AND *x <= 79 ) {
		*( iarray+ *x ) = c ;
		++*x ;
	}
}
adeout_(x,iarray)
int *x,*iarray;
{

        int k ,c;
	fflush (stdout);
       			k=0;
			while ( k < 622500)
			{
        			k = k+1;
			}
	c=0;
	while ( c < *x) {
		putchar( *(iarray + c));

		++c;
	}
}


