#ifndef __QUATERNION_HPP__
#define __QUATERNION_HPP__

#include<iomanip> 
#include <cmath>

/**
 * Quaternion implementation class
 */
class Quaternion {
    double real, imI, imJ, imK;
public:
    Quaternion(double real, double imI, double imJ, double imK);

    double r();
    double i();
    double j();
    double k();

    double norm();
    Quaternion conjugate();
    Quaternion inverse();
    Quaternion versor();
    double dot(Quaternion other);

    /**
     * Get complex components (remove the real component)
     */
    Quaternion complexComponents();

    static Quaternion identity();
};

// Unary operators
Quaternion operator - (Quaternion q);

// Quaternion <-> Quaternion operators
Quaternion operator + (Quaternion a, Quaternion b);
Quaternion operator - (Quaternion a, Quaternion b);
Quaternion operator * (Quaternion a, Quaternion b);
Quaternion operator / (Quaternion a, Quaternion b);

// double -> Quaternion operators
Quaternion operator + (double num, Quaternion obj);
Quaternion operator - (double num, Quaternion obj);
Quaternion operator * (double num, Quaternion obj);
Quaternion operator / (double num, Quaternion obj);

// Quaternion -> double operators
Quaternion operator + (Quaternion obj, double num);
Quaternion operator - (Quaternion obj, double num);
Quaternion operator * (Quaternion obj, double num);
Quaternion operator / (Quaternion obj, double num);

// Quaternion literals
Quaternion operator"" _i(long double num);
Quaternion operator"" _j(long double num);
Quaternion operator"" _k(long double num);
Quaternion operator"" _i(unsigned long long int num);
Quaternion operator"" _j(unsigned long long int num);
Quaternion operator"" _k(unsigned long long int num);

// cmath compatability
Quaternion exp(Quaternion q);
Quaternion log(Quaternion q);
Quaternion pow(Quaternion q, int exponent);
Quaternion pow(Quaternion q, double exponent);
#endif
