// hough transform gonzales p. 587ff

#include <iostream>
#include <stdio.h>
#include <math.h>
using namespace std;

enum{
    NX=512,
    NY=NX,
    NTHETA=120,
    NRHO=300
    };
unsigned char buf[NX*NY],hough_buf[NTHETA*NRHO];
unsigned int hough_hist[NTHETA*NRHO]; 

const double 
theta_min=0,//-M_PI,
  theta_max=M_PI,
  rho_max=M_SQRT2*NX,
  rho_min=-rho_max;

void
readim(char*fn,unsigned char*dst)
{
  FILE*f=fopen(fn,"r");
  fscanf(f,"P5\n512 512\n255\n");
  fread(dst,NX,NY,f);
  fclose(f);
}

// linear interpolation
inline double
stretch(int i,int n,double min,double max)
{
  double t=i*1./n;
  return t*(max-min)+(1-t)*min;
}

inline double
theta(int i)
{
  return stretch(i,NTHETA,theta_min,theta_max);
}

double cos_tab[NTHETA],sin_tab[NTHETA];
void init_hough()
{
  for(int i=0;i<NTHETA;i++){
    double t=theta(i);
    cos_tab[i]=cos(t);
    sin_tab[i]=sin(t);
  }
}

void
insert_hough(int x,int y,unsigned int*hist)
{
  const static double sfrho=NRHO*1./(rho_max-rho_min);
  for(int i=0;i<NTHETA;i++){
    double rho=x*cos_tab[i]+y*sin_tab[i];
    hist[i+NTHETA*((int)((rho-rho_min)*sfrho))]++;
  }
}

int
main()
{
  readim("grid25.pgm",buf);
  int l=0;
  init_hough();
  for(int i=0;i<NX;i++)
    for(int j=0;j<NY;j++){
      if(buf[l++]==8)
	insert_hough(i,j,hough_hist);
    }
  
  int max=hough_hist[0];
  for(int i=1;i<NTHETA*NRHO;i++){
    int v=hough_hist[i];
    if(v>max)
      max=v;
  }
  cout<<"max "<<max<<endl;
  for(int i=0;i<NTHETA*NRHO;i++){
    int v=hough_hist[i];
    unsigned char c=(unsigned char)(v*255./max);
    //    unsigned char c=(unsigned char)v==0?0:log(v)*255./log(max);
    hough_buf[i]=c;
  }
  FILE*f=fopen("hough.pgm","w");
  fprintf(f,"P5\n%d %d\n255\n",NTHETA,NRHO);
  fwrite(hough_buf,NTHETA,NRHO,f);
  fclose(f);
  return 0;
}
