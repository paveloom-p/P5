#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define mu 3.986004418e+14
#define Odote 7.2921151467e-5
#define c 299792458.0
#define a 6378137.0
#define b 6356752.0
#define t_obs 484266.0
int main(){

    double P; // псевдодательность до спутника
    double t_oe, WN;// эпоха, неделя
    double e, sqrtA, O_0, i_0, w, M_0, dn, Odot, IDOT;
    double Cuc, Cus, Crc, Crs, Cic, Cis;
    double n_0, t_em, t, n, M, E_0, E_n, E, nu, phi, du, dr, di, u, r, i;
    double Xorb, Yorb, O, X, Y, Z;
    int j ;
    int numb_of_sat, year, month, day, hour, minute;
    double second, dt, v, IODE, stuff_0, stuff_1, stuff_2;

    scanf("%i %i %i %i %i %i %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf", &numb_of_sat, &year, &month, &day, &hour, &minute, &second, &dt, &stuff_0, &v, &IODE, &Crs, &dn, &M_0, &Cuc, &e, &Cus, &sqrtA, &t_oe, &Cic, &O_0, &Cis, &i_0, &Crc, &w, &Odot, &IDOT, &stuff_1, &WN, &stuff_2, &P);

    // printf("%0.15lf\n %0.15lf\n %0.15lf\n %0.15lf\n %0.15lf\n %0.15lf\n %0.15lf\n %0.15lf\n %0.15lf\n %0.15lf\n %0.15lf\n %0.15lf\n %0.15lf\n %0.15lf\n %0.15lf\n %0.15lf\n  %0.15lf\n %0.15lf\n  %0.15lf\n", P, t_obs, t_oe, WN,  e, sqrtA, O_0, i_0, w, M_0, dn, Odot, IDOT, Cuc, Cus, Crc, Crs, Cic, Cis);

    n_0=sqrt(mu/pow(sqrtA,6));// среднее движение (рад/с)

    printf("1. n_0=%0.20lf\n", n_0);
    //printf("dn=%0.20lf\n", dn);
    
    //t_oe=5*24*3600+hour*3600+minute*60+second;
    t_em=t_obs-P/c; // момент излучения сигнала спутником

    printf("2. t_em=%lf\n", t_em);

    t=t_em-t_oe; //время от начала эпохи
    
    if (t>302400) 
        t=t-604800; 
    else if (t<-302400)
        t=t+604800;

    printf("3. t=%lf\n", t);

    n=n_0+dn;// исправленное среднее движение

    printf("4. n=%0.20lf\n", n);

    M=M_0+n*t;//средняя аномалия

    printf("5. M=%0.10lf\n", M);

    E_0=M+e*sin(M);
    E=E_0;
    
    do {
        E_n=E; // эксцентрическая аномалия ! |E_n-E_n-1|<10^(-8)
        E=M+e*sin(E_n);
        }    
    while ( fabs(E_n-E)>=1e-8 ) ;
    
    E=E_n;

    printf("6. E=%0.10lf\n", E);

    nu=2*atan(sqrt((1+e)/(1-e))*tan(E/2));// истиная аномалия

    printf("7. nu=%0.20lf\n", nu);

    //printf("w=%0.20lf\n", w);

    phi=nu+w;// аргумент широты

    printf("8. phi=%0.20lf\n", phi);

    du=Cus*sin(2*phi)+Cuc*cos(2*phi);// поправки аргумента широты, радиуса и наклона
    dr=Crs*sin(2*phi)+Crc*cos(2*phi);
    di=Cis*sin(2*phi)+Cic*cos(2*phi);

    printf("9. du=%0.20lf\n", du);
    printf("   dr=%0.20lf\n", dr);
    printf("   di=%0.20lf\n", di);

    u=phi+du; // исправленные значения аргумента широты, радиуса и наклонения
    r=pow(sqrtA,2)*(1-e*cos(E))+dr;
    i=i_0+di+ IDOT*t;

    printf("10. u=%0.20lf\n", u);
    printf("    r=%0.20lf\n", r);
    printf("    i=%0.20lf\n", i);

    Xorb=r*cos(u); //координаты спутника в орбитальный плоскости
    Xorb=r*cos(u);
    Yorb=r*sin(u);
    printf("11. Xorb=%0.20lf\n", Xorb);
    printf("    Yorb=%0.20lf\n", Yorb);

    //printf("O_0=%0.20lf\n", O_0);
    //printf("Odot=%0.20lf\n", Odot);
    //printf("Odote=%0.20f\n", Odote);
    
    O=O_0 +(Odot-Odote)*t-Odote*t_oe; // исправленная долгота восходящего узла
    printf("12. O=%lf\n", O);
    X=Xorb*cos(O)-Yorb*cos(i)*sin(O); // положение в земной системе координат
    Y=Xorb*sin(O)+Yorb*cos(i)*cos(O);
    Z=Yorb*sin(i);

    printf("13. X=%lf\n", X);
    printf("    Y=%lf\n", Y);
    printf("    Z=%lf\n", Z);
    
    printf("P=%lf\n", P);
    printf("dt=%0.20lf\n", dt);
}
