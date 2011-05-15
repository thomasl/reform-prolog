#include <stdio.h>

#define ReadLine(B,F)							   \
{									   \
  int rl_ch;								   \
  int rl_i;								   \
									   \
  rl_ch = getc(F);							   \
  rl_i = 0;								   \
                                                                           \
  while(rl_ch != '\n')							   \
    {									   \
      B[rl_i] = (char) rl_ch;						   \
      rl_i += 1;							   \
      rl_ch = getc(F);							   \
      if (rl_ch == EOF) 						   \
	exit(1);							   \
    }									   \
  B[rl_i] = (char) 0;                                                      \
}

main(int argc, char **argv)
{
  FILE *in1, *in2;
  int crs;
  char line1[255], line2[255];
  
     
  if ((in1 = fopen(argv[1],"r"))==0)
    {
      printf("cannot open %s for reading\n",argv[1]);
      exit(0);
    }

  if ((in2 = fopen(argv[2],"r"))==0)
    {
      printf("cannot open %s for reading\n",argv[2]);
      exit(0);
    }

  crs = 0;
  while(1)
    {
      ReadLine(line1,in1);
      ReadLine(line2,in2);
      
      if(strcmp(line1,line2) != 0)
	{
	  printf("files differ in line %d\n",crs);
	}

      crs += 1;
    }
}
