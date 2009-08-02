// hough transform gonzales p. 587ff

#include <iostream>
#include <stdio.h>
#include <math.h>
#include <vector>

using namespace std;

enum{
    NX=512,
    NY=NX,
    NTHETA=1360,
    NRHO=1380
    };
unsigned char buf[NX*NY],hough_buf[NTHETA*NRHO];
unsigned int hough_hist[NTHETA*NRHO]; 

const double 
theta_min=-M_PI/2,
  theta_max=M_PI/2,
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
  return (1-t)*min+t*max;
}

inline double
stretch_inclusive(int i,int n,double min,double max)
{
  double t=i*1./(n-1);
  return (1-t)*min+t*max;
}

inline double
theta(int i)
{
  return stretch(i,NTHETA,theta_min,theta_max);
}

inline double
rho(int i)
{
  return stretch_inclusive(i,NRHO,rho_min,rho_max);
}
#include <string.h>
double cos_tab[NTHETA],sin_tab[NTHETA];
void init_hough()
{
  memset(hough_hist,0,sizeof(hough_hist));
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

double
calc_avg(vector<int>x)
{
  double sum=0;
  for(unsigned int i=0;i<x.size();i++)
    sum+=x[i];
  return sum/x.size();
}

double
calc_x2(vector<int>x)
{
  double sum=0;
  for(unsigned int i=0;i<x.size();i++)
    sum+=x[i]*x[i];
  return sum;
}

double
calc_xy(vector<int>x,vector<int>y)
{
  double sum=0;
  for(unsigned int i=0;i<x.size();i++)
    sum+=x[i]*y[i];
  return sum;
}



class Error{};
// y=a+b*x  -> find a and b
double
fit_line(vector<int>x,vector<int>y,double&a,double&b)
{
  if(x.size()!=y.size() || x.size()==0)
    throw Error();
  int n=x.size();
  double 
    xbar=calc_avg(x),x2=calc_x2(x),
    ybar=calc_avg(y),y2=calc_x2(y),
    xy=calc_xy(x,y);
  double
    ssxx=x2-n*xbar*xbar,
    ssyy=y2-n*ybar*ybar,
    ssxy=xy-n*xbar*ybar;
  cout << "xbar=" << xbar << " ybar="
       << ybar << " x2="
       << x2 << " y2="
       << y2 << " xy="
       << xy << " ssxx="
       << ssxx << " ssxy="
       << ssxy << " "<<endl;
  b=ssxy/ssxx;
  cout<<"b="<<b<<endl;
  a=ybar-b*xbar;
  cout<<"a="<<a<<endl;
  double r2=ssxy*ssxy/(ssxx*ssyy);
  cout<<"correlation coeff r^2="<<r2<<" proportion of ssyy which is accounted for by the refgression"<<endl;
  return r2;
}

// x=a+b*y
void
draw_line_vert(double a,double b,int nx,int ny)
{
  FILE*f=fopen("fit-line.dat","w");
  for(int i=0;i<ny;i++){ 
    double x=a+b*i;
    fprintf(f,"%g %d\n",x,i);
    int ix=(int)x;
    if(ix>=3 && ix<NX-3){
      buf[(ix-3)+nx*i]=255;
      buf[(ix+3)+nx*i]=255;
    } 
  }  
  fclose(f);
}

// y=a+b*x
void
draw_line_hori(double a,double b,int nx,int ny)
{
  FILE*f=fopen("fit-line.dat","w");
  for(int i=0;i<nx;i++){ 
    double y=a+b*i;
    fprintf(f,"%d %g\n",i,y);
    int iy=(int)y;
    if(iy>=3 && iy<ny-3){
      buf[i+nx*(iy-3)]=255; 
      buf[i+nx*(iy+3)]=255; 
    }
  }  
  fclose(f);
}

unsigned int
aref(unsigned int*h,int nx,int ny,int i,int j)
{
  if(i>=0 && i<nx && j>=0 && j<ny)
    return h[i+nx*j];
  return 0;
}

int
ismax_p(unsigned int*h,int nx,int ny,int i,int j)
{
  double 
    q=aref(h,nx,ny,i,j);
  if(q>aref(h,nx,ny,i+1,j) && 
     q>aref(h,nx,ny,i-1,j) &&
     q>aref(h,nx,ny,i+1,j+1) && 
     q>aref(h,nx,ny,i-1,j-1) &&
     q>aref(h,nx,ny,i+1,j-1) && 
     q>aref(h,nx,ny,i-1,j+1) &&
     q>aref(h,nx,ny,i,j+3) &&
     q>aref(h,nx,ny,i,j-3) &&
     q>aref(h,nx,ny,i+3,j) &&
     q>aref(h,nx,ny,i-3,j) &&
     q>aref(h,nx,ny,i,j+1) &&     
     q>aref(h,nx,ny,i,j-1))
    return 1;
  return 0;
}


// collect points that are in the bin [maxtheta,maxrho] of the hough transform
void
extract_line_points(int maxtheta,int maxrho,vector<int>&x,vector<int>&y)
{
  int l=0;
  const static double sfrho=NRHO*1./(rho_max-rho_min);
  for(int j=0;j<NY;j++)
    for(int i=0;i<NX;i++){
      if(buf[l++]<80){
	double rho=i*cos_tab[maxtheta]+j*sin_tab[maxtheta];
	int irho=(int)((rho-rho_min)*sfrho);
	if(maxrho==irho){
	  x.push_back(i);
	  y.push_back(j);
	}
      }
    }
}


int
main()
{

  readim("grid25.pgm",buf);
  int l=0;
  init_hough();
  // first do a hough trafo using all the dark pixels
  for(int j=0;j<NY;j++)
    for(int i=0;i<NX;i++){
      if(buf[l++]<80)
	insert_hough(i,j,hough_hist);
    }
  

  // find the maximum in the hough transform image, most significant
  // line
  int max=hough_hist[0],maxtheta=0,maxrho=0;
  l=0;
  for(int j=0;j<NRHO;j++)
    for(int i=0;i<NTHETA;i++){
      int v=hough_hist[l++];
      if(v>max){
	max=v;
	maxtheta=i;
	maxrho=j;
      }
    }

  // calculate a histogram over local maxima in the hough transform image
  enum { HISTN=100 };
  int histogram[HISTN];
  memset(histogram,0,sizeof(histogram));
  vector<pair<int,int> > local_max;
  for(int j=0;j<NRHO;j++)
    for(int i=0;i<NTHETA;i++){
      if(ismax_p(hough_hist,NTHETA,NRHO,i,j)){
	// use max+1 so that the highest value isn't lost
	double v=hough_hist[i+NTHETA*j]*HISTN*1./(max+1);
	histogram[(int)v]++;
	if(v>=60)
	  local_max.push_back(pair<int,int>(i,j));
      }
    }
    
  FILE*f=fopen("hough-histogram.dat","w");
  for(int i=0;i<HISTN;i++)
    fprintf(f,"%d %d\n",i,histogram[i]);
  fclose(f);


  
  



  for(unsigned int k=0;k<local_max.size();k++){
    int 
      maxtheta=local_max[k].first,
      maxrho=local_max[k].second;
    
    cout << "maxtheta_idx=" << maxtheta << " maxrho_idx="<<maxrho << endl
	 << "maxtheta[degree]=" << theta(maxtheta)*180/M_PI << " maxrho[pixel]="<<rho(maxrho)<< endl;
  
    
    // collect points from image
    vector<int> x,y;
    extract_line_points(maxtheta,maxrho,x,y);
    
    double a,b,r2;
    if(fabs(theta(maxtheta))<45*M_PI/180){ 
      cout << "vertical line" << endl;
      r2=fit_line(y,x,a,b);
      if(r2>0.7)
	draw_line_vert(a,b,NX,NY);
    }else{
      cout << "horizontal line" << endl;
      r2=fit_line(x,y,a,b);
      if(r2>0.5)
	draw_line_hori(a,b,NX,NY);
    }
    
    
    // if the fit is good, draw all the collected points into original
    // image
    if(r2>0.5)
      for(unsigned int i=0;i<x.size();i++){
	buf[x[i]+NX*y[i]]=230;
      }
    
  }


  f=fopen("grid-augment.pgm","w");
  fprintf(f,"P5\n%d %d\n255\n",NX,NY);
  fwrite(buf,NX,NY,f);
  fclose(f);

  

  for(int i=0;i<NTHETA*NRHO;i++){
    int v=hough_hist[i];
    unsigned char c=(unsigned char)(v*255./max);
    //    unsigned char c=(unsigned char)v==0?0:log(v)*255./log(max);
    hough_buf[i]=c;
  }

  for(unsigned int k=0;k<local_max.size();k++){
    int 
      maxtheta=local_max[k].first,
      maxrho=local_max[k].second;
    for(int i=-6;i<=6;i++)
      hough_buf[maxtheta+i+NTHETA*maxrho]=255;
    for(int i=-6;i<=6;i++)
      hough_buf[maxtheta+NTHETA*(maxrho+i)]=255;
  }
  f=fopen("hough.pgm","w");
  fprintf(f,"P5\n%d %d\n255\n",NTHETA,NRHO);
  fwrite(hough_buf,NTHETA,NRHO,f);
  fclose(f);
  return 0;
}
