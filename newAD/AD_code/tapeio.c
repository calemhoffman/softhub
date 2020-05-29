/* file: tapeio.c
Subroutines to open, read, and close a multiparameter file written
by the PC.
Written 7-29-93,     S.L. Tabor
To use:  CHAR name*40 /  name = "/dev/nrmt0l"  / kskip = # tape marks to skip  CALL opentape(idev,name,kskip)
idev will be positive if the open was successful

INTEGER*2 buf(2048) / icount = 1024 /  CALL readtape(idev,buf,icount)
The number of bytes read will be returned in icount.  0 for EOF or error.

CALL close(idev)     */


#include <stdio.h>
#include <sys/types.h>
#include <sys/mtio.h>
#include <sys/ioctl.h>
void opentape_(idev,name,kskip)
char *name;
int *idev, *kskip;
{
char t,fname[40];
int i,j,err,skip;

struct   mtop   m;
m.mt_count = 1;
m.mt_op    = MTFSF;

skip = *kskip;

for (i = 0, j = 0; i < 38; i ++)
  {t = *(name+i);
  if ( t != ' ')
     fname[j++] = t; }
fname[j] = '\0';

*idev = open(fname,0);

if (skip > 0 )
  {
for (i = 0 ; i < skip && err >= 0 ; i++)
{
  err = ioctl (*idev,MTIOCTOP,&m);
}
  if (err < 0)
     { *idev = err; }
}
}


void readtape_(idev, buffer, icount)
char *buffer;
int *idev, *icount;
{
char buf[4097];
int i,j,kount;

kount = *icount;
*icount = read(*idev, buf, kount);

if (*icount == kount)
  {for (i = 0; i < kount; i++)
      *(buffer+i) = *(buf+i);  }
   }

void closetape_(idev)
int *idev;
{
close(*idev);
}
