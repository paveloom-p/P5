#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define vc 299792458.0
#define ao 6378137.0
#define bo 6356752.0
typedef struct {
double a; double b; double c; double d;
} four;

struct coord { 
four P0; four X; four Y; four Z;
};

typedef struct {
double a; double b; double c;
} tree;

struct Dcoord { 
tree DP; tree DX; tree DY; tree DZ;
};

struct dcoord { 
tree dP; tree dX; tree dY; tree dZ;
};

int main() {
    struct coord sp;
    struct Dcoord Dc;
    struct dcoord dc;
    four Pi, Dti, A;
    double D[4], d[3], X_, Y_, Z_, ax, ay, az, DX4_, DY4_, DZ4_, a4, b4, c4, e1, e2, e12 , Dt1, Dt2, Dt;
    double Xpr, Ypr, Zpr, pow_e, pow_e_, L, O, Phi;
    scanf("%lf %lf %lf %lf", &(sp.P0.a), &(sp.P0.b),&(sp.P0.c), &(sp.P0.d) );
    scanf("%lf %lf %lf %lf", &(sp.X.a), &(sp.X.b),&(sp.X.c), &(sp.X.d) );
    scanf("%lf %lf %lf %lf", &(sp.Y.a), &(sp.Y.b),&(sp.Y.c), &(sp.Y.d) );
    scanf("%lf %lf %lf %lf", &(sp.Z.a), &(sp.Z.b),&(sp.Z.c), &(sp.Z.d) );
    scanf("%lf %lf %lf %lf", &(Dti.a), &(Dti.b),&(Dti.c), &(Dti.d) );

//    printf("%lf %lf %lf %lf\n", (sp.P0.a), (sp.P0.b),(sp.P0.c), (sp.P0.d) );
//    printf("%lf %lf %lf %lf\n", (sp.X.a), (sp.X.b),(sp.X.c), (sp.X.d) );
//    printf("%lf %lf %lf %lf\n", (sp.Y.a), (sp.Y.b),(sp.Y.c), (sp.Y.d) );
//    printf("%lf %lf %lf %lf\n", (sp.Z.a), (sp.Z.b),(sp.Z.c), (sp.Z.d) );
//    printf("%lf %lf %lf %lf\n", (Dti.a), (Dti.b),(Dti.c), (Dti.d) );

// коррекция псевдодальностей за поправку часов спутников
    Pi.a=sp.P0.a+vc*Dti.a;
    Pi.b=sp.P0.b+vc*Dti.b;
    Pi.c=sp.P0.c+vc*Dti.c;
    Pi.d=sp.P0.d+vc*Dti.d;

//    printf("Pi.a=%lf\n",Pi.a);
//    printf("Pi.b=%lf\n",Pi.b);
//    printf("Pi.c=%lf\n",Pi.c);
//    printf("Pi.d=%lf\n",Pi.d);

// вычисление вспомогательных сумм и разностей
    Dc.DP.a=Pi.b-Pi.a;
    Dc.DP.b=Pi.c-Pi.b;
    Dc.DP.c=Pi.d-Pi.c;

//    printf("Dc.DP.a=%lf\n",Dc.DP.a);
//    printf("Dc.DP.b=%lf\n",Dc.DP.b);
//    printf("Dc.DP.c=%lf\n",Dc.DP.c);

    dc.dP.a=Pi.b+Pi.a;
    dc.dP.b=Pi.c+Pi.b;
    dc.dP.c=Pi.d+Pi.c;

    Dc.DX.a=sp.X.b-sp.X.a;
    Dc.DX.b=sp.X.c-sp.X.b;
    Dc.DX.c=sp.X.d-sp.X.c;

//    printf("Dc.DX.a=%lf\n",Dc.DX.a);
//    printf("Dc.DX.b=%lf\n",Dc.DX.b);
//    printf("Dc.DX.c=%lf\n",Dc.DX.c);

    dc.dX.a=sp.X.b+sp.X.a;
    dc.dX.b=sp.X.c+sp.X.b;
    dc.dX.c=sp.X.d+sp.X.c;

    Dc.DY.a=sp.Y.b-sp.Y.a;
    Dc.DY.b=sp.Y.c-sp.Y.b;
    Dc.DY.c=sp.Y.d-sp.Y.c;

//    printf("Dc.DY.a=%lf\n",Dc.DY.a);
//    printf("Dc.DY.b=%lf\n",Dc.DY.b);
//    printf("Dc.DY.c=%lf\n",Dc.DY.c);

    dc.dY.a=sp.Y.b+sp.Y.a;
    dc.dY.b=sp.Y.c+sp.Y.b;
    dc.dY.c=sp.Y.d+sp.Y.c;

    Dc.DZ.a=sp.Z.b-sp.Z.a;
    Dc.DZ.b=sp.Z.c-sp.Z.b;
    Dc.DZ.c=sp.Z.d-sp.Z.c;

//    printf("Dc.DZ.a=%lf\n",Dc.DZ.a);
//    printf("Dc.DZ.b=%lf\n",Dc.DZ.b);
//    printf("Dc.DZ.c=%lf\n",Dc.DZ.c);

    dc.dZ.a=sp.Z.b+sp.Z.a;
    dc.dZ.b=sp.Z.c+sp.Z.b;
    dc.dZ.c=sp.Z.d+sp.Z.c;

    A.a=(Dc.DX.a*dc.dX.a+Dc.DY.a*dc.dY.a+Dc.DZ.a*dc.dZ.a-Dc.DP.a*dc.dP.a)/2;
    A.b=(Dc.DX.b*dc.dX.b+Dc.DY.b*dc.dY.b+Dc.DZ.b*dc.dZ.b-Dc.DP.b*dc.dP.b)/2;
    A.c=(Dc.DX.c*dc.dX.c+Dc.DY.c*dc.dY.c+Dc.DZ.c*dc.dZ.c-Dc.DP.c*dc.dP.c)/2;

//    printf("A.a=%lf\n",A.a);
//    printf("A.b=%lf\n",A.b);
//    printf("A.c=%lf\n",A.c);

    D[0]=Dc.DX.a*(Dc.DY.b*Dc.DZ.c-Dc.DY.c*Dc.DZ.b)-Dc.DX.b*(Dc.DY.a*Dc.DZ.c-
    Dc.DY.c*Dc.DZ.a)+Dc.DX.c*(Dc.DY.a*Dc.DZ.b-Dc.DY.b*Dc.DZ.a);
    D[1]=A.a*(Dc.DY.b*Dc.DZ.c-Dc.DY.c*Dc.DZ.b)-A.b*(Dc.DY.a*Dc.DZ.c-
    Dc.DY.c*Dc.DZ.a)+A.c*(Dc.DY.a*Dc.DZ.b-Dc.DY.b*Dc.DZ.a);
    D[2]=Dc.DX.a*(A.b*Dc.DZ.c-A.c*Dc.DZ.b)-Dc.DX.b*(A.a*Dc.DZ.c-
    A.c*Dc.DZ.a)+Dc.DX.c*(A.a*Dc.DZ.b-A.b*Dc.DZ.a);
    D[3]=Dc.DX.a*(Dc.DY.b*A.c-Dc.DY.c*A.b)-Dc.DX.b*(Dc.DY.a*A.c-
    Dc.DY.c*A.a)+Dc.DX.c*(Dc.DY.a*A.b-Dc.DY.b*A.a);

    d[0]=-Dc.DP.a*(Dc.DY.b*Dc.DZ.c-Dc.DY.c*Dc.DZ.b)+Dc.DP.b*(Dc.DY.a*Dc.DZ.c-
    Dc.DY.c*Dc.DZ.a)-Dc.DP.c*(Dc.DY.a*Dc.DZ.b-Dc.DY.b*Dc.DZ.a);
    d[1]=Dc.DX.a*(-Dc.DP.b*Dc.DZ.c+Dc.DP.c*Dc.DZ.b)-Dc.DX.b*(-Dc.DP.a*Dc.DZ.c+
    Dc.DP.c*Dc.DZ.a)+Dc.DX.c*(-Dc.DP.a*Dc.DZ.b+Dc.DP.b*Dc.DZ.a);
    d[2]=Dc.DX.a*(-Dc.DY.b*Dc.DP.c+Dc.DY.c*Dc.DP.b)-Dc.DX.b*(-Dc.DY.a*Dc.DP.c+
    Dc.DY.c*Dc.DP.a)+Dc.DX.c*(-Dc.DY.a*Dc.DP.b+Dc.DY.b*Dc.DP.a);

//    printf("D[0]=%lf\n",D[0]);
//    printf("D[1]=%lf\n",D[1]);
//    printf("D[2]=%lf\n",D[2]);
//    printf("D[3]=%lf\n",D[3]);
//    printf("d[0]=%lf\n",d[0]);

    X_=D[1]/D[0];
    Y_=D[2]/D[0];
    Z_=D[3]/D[0];

//    printf("X_=%lf\n",X_);
//    printf("Y_=%lf\n",Y_);
//    printf("Z_=%lf\n",Z_);

    ax=d[0]/D[0];
    ay=d[1]/D[0];
    az=d[2]/D[0];

//    printf("ax=%0.15lf\n",ax);
//    printf("ay=%0.15lf\n",ay);
//    printf("az=%0.15lf\n",az);

    DX4_=sp.X.d-X_;
    DY4_=sp.Y.d-Y_;
    DZ4_=sp.Z.d-Z_;

//    printf("DX4_=%lf\n",DX4_);
//    printf("DY4_=%lf\n",DY4_);
//    printf("DZ4_=%lf\n",DZ4_);

    a4=1-pow(ax,2)-pow(ay,2)-pow(az,2);
    b4=Pi.d+DX4_*ax+DY4_*ay+DZ4_*az;
    c4=-pow((DX4_),2)-pow((DY4_),2)-pow((DZ4_),2)+pow(Pi.d,2);

//    printf("a4=%0.15lf\n",a4);
//    printf("b4=%lf\n",b4);
//    printf("c4=%lf\n",c4);

    e1=-b4/a4+sqrt(pow(b4,2)-a4*c4)/a4;
    e2=-b4/a4-sqrt(pow(b4,2)-a4*c4)/a4;

    Dt1=e1/vc;
    Dt2=e2/vc;
    Dt=Dt1;
    e12=e1;
    if (Dt1>=0.0005 || Dt1<=-0.0005) {Dt=Dt2; e12=e2;}

//    printf("e12=%lf\n",e12);

    Xpr=X_+e12*ax;
    Ypr=Y_+e12*ay;
    Zpr=Z_+e12*az;

//    printf("Xpr=%lf\n",Xpr);
//    printf("Ypr=%lf\n",Ypr);
//    printf("Zpr=%lf\n",Zpr);

// параметры трансформации координат
    pow_e=(pow(ao,2)-pow(bo,2))/pow(ao,2);
    pow_e_=(pow(ao,2)-pow(bo,2))/pow(bo,2);

//    printf("pow_e=%0.10lf\n",pow_e);
//    printf("pow_e_=%0.10lf\n",pow_e_);

// долгота
    L=atan(Ypr/Xpr);
//    printf("L=%0.15lf\n",L);
    printf("L=%0.15lf\n",L/atan(1)*45);

    O=atan(ao*Zpr/(bo*sqrt(pow(Xpr,2)+pow(Ypr,2))));
//    printf("O=%0.15lf\n",O);

// широта
    Phi=atan((Zpr+pow_e_*bo*pow(sin(O),3))/(sqrt(pow(Xpr,2)+pow(Ypr,2))-pow_e*ao*pow(cos(O),3)));
    Phi=Phi/atan(1)*180/4;
    printf("Phi=%0.15lf\n",Phi);
    
}
