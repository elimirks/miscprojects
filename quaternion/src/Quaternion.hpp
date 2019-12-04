#ifndef __QUATERNION_HPP__
#define __QUATERNION_HPP__

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
};

// Quaternion <-> Quaternion operators
Quaternion operator + (Quaternion &a, Quaternion &b);
Quaternion operator - (Quaternion &a, Quaternion &b);
Quaternion operator * (Quaternion &a, Quaternion &b);
Quaternion operator / (Quaternion &a, Quaternion &b);

// Quaternion <-> double operators
Quaternion operator * (double num, Quaternion &obj);
Quaternion operator * (Quaternion &obj, double num);
Quaternion operator / (double num, Quaternion &obj);
Quaternion operator / (Quaternion &obj, double num);
#endif
