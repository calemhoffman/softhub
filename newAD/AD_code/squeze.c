squeze_(f1,f2)
char *f1,*f2;
{int i, j, t;
 for (i=0, j=0; i < 40; i++)
   {t = *(f1+i);
    if (t != ' ')
    f2[j++] = t;}
 for (; j < 40; j++)
   f2[j] = ' ';
 return 0;}
