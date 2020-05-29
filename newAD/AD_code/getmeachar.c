#include <termio.h>

/********************************************/
/** getmeachar()                           **/
/** Waits for a key to be pressed.         **/
/** Returns the key upon successful read.  **/
/** It returns -1 upon an error            **/
/** and -2 if INTR was struck.              **/
/********************************************/


getmeachar_()
{
char buf;
struct termio t;
unsigned char oldeof;
int oldlflag;

if(ioctl(0,TCGETA,&t)==-1) printf("\nError in ioctl.\n"); /**GET old values**/

oldeof=t.c_cc[VEOF];  /**Save old values**/
oldlflag=t.c_lflag;

if(t.c_lflag&ICANON) t.c_lflag-=ICANON;   /**Change some values**/
if(t.c_lflag&ECHO) t.c_lflag-=ECHO;
if(t.c_lflag&ISIG) t.c_lflag-=ISIG;
t.c_cc[VEOF]=1;  /*set eof to ^A*/

if(ioctl(0,TCSETA,&t)==-1) printf("Error in ioctl.\n");  /**Put new values **/

/*if(read(0,buf,1)!=1) printf("Error reading.\n");    *Actual read of stdin*/
buf=getchar();

t.c_cc[VEOF]=oldeof;  /**Restore old values**/
t.c_lflag=oldlflag;
if(ioctl(0,TCSETA,&t)==-1) printf("Error in ioctl.\n");

if(buf==t.c_cc[VINTR]) buf= -2;  /**Interrupt was struck**/

return(buf);

}
