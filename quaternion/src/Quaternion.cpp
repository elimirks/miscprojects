#include <cmath>
#include "Quaternion.hpp"

Quaternion Quaternion::identity() {
    return Quaternion(1, 0, 0, 0);
}

Quaternion::Quaternion(double real, double imI, double imJ, double imK) :
    real(real), imI(imI), imJ(imJ), imK(imK) {
}

double Quaternion::r() {
    return real;
}

double Quaternion::i() {
    return imI;
}

double Quaternion::j() {
    return imJ;
}

double Quaternion::k() {
    return imK;
}

double Quaternion::norm() {
    return sqrt(pow(real, 2) +
            pow(imI, 2) +
            pow(imJ, 2) +
            pow(imK, 2));
}

Quaternion Quaternion::conjugate() {
    return Quaternion(real, -imI, -imJ, -imK);
}

Quaternion Quaternion::inverse() {
    return conjugate() / pow(norm(), 2);
}

Quaternion Quaternion::versor() {
    return *this / norm();
}

double Quaternion::dot(Quaternion other) {
    return r() * other.r() +
        i() * other.i() +
        j() * other.j() +
        k() * other.k();
}

Quaternion Quaternion::complexComponents() {
    return Quaternion(0, i(), j(), k());
}



Quaternion operator - (Quaternion q) {
    return Quaternion(-q.r(),
                 -q.i(),
                 -q.j(),
                 -q.k());
}



Quaternion operator + (Quaternion a, Quaternion b) {
    return Quaternion(a.r() + b.r(),
                 a.i() + b.i(),
                 a.j() + b.j(),
                 a.k() + b.k());
}

Quaternion operator - (Quaternion a, Quaternion b) {
    return Quaternion(a.r() - b.r(),
                 a.i() - b.i(),
                 a.j() - b.j(),
                 a.k() - b.k());
}

Quaternion operator * (Quaternion a, Quaternion b) {
    return Quaternion(a.r() * b.r() - a.i() * b.i() - a.j() * b.j() - a.k() * b.k(),
                 a.r() * b.i() + a.i() * b.r() + a.j() * b.k() - a.k() * b.j(),
                 a.r() * b.j() - a.i() * b.k() + a.j() * b.r() + a.k() * b.i(),
                 a.r() * b.k() + a.i() * b.j() - a.j() * b.i() + a.k() * b.r());
}

Quaternion operator / (Quaternion a, Quaternion b) {
    return a * b.inverse();
}



Quaternion operator + (double num, Quaternion obj) {
    return Quaternion(num + obj.r(),
                 obj.i(),
                 obj.j(),
                 obj.k());
}

Quaternion operator - (double num, Quaternion obj) {
    Quaternion negative = -obj;
    return num + negative;
}

Quaternion operator * (double num, Quaternion obj) {
    return Quaternion(num * obj.r(),
                 num * obj.i(),
                 num * obj.j(),
                 num * obj.k());
}

Quaternion operator / (double num, Quaternion obj) {
    return num * obj.inverse();
}



Quaternion operator + (Quaternion obj, double num) {
    return num + obj;
}

Quaternion operator - (Quaternion obj, double num) {
    return Quaternion(obj.r() - num,
                 obj.i(),
                 obj.j(),
                 obj.k());
}

Quaternion operator * (Quaternion obj, double num) {
    return num * obj;
}

Quaternion operator / (Quaternion obj, double num) {
    return Quaternion(obj.r() / num,
                 obj.i() / num,
                 obj.j() / num,
                 obj.k() / num);
}

Quaternion operator"" _i(long double num) {
    return Quaternion(0, num, 0, 0);
}

Quaternion operator"" _j(long double num) {
    return Quaternion(0, 0, num, 0);
}

Quaternion operator"" _k(long double num) {
    return Quaternion(0, 0, 0, num);
}

Quaternion operator"" _i(unsigned long long int num) {
    return Quaternion(0, num, 0, 0);
}

Quaternion operator"" _j(unsigned long long int num) {
    return Quaternion(0, 0, num, 0);
}

Quaternion operator"" _k(unsigned long long int num) {
    return Quaternion(0, 0, 0, num);
}



Quaternion exp(Quaternion q) {
    Quaternion c = q.complexComponents();
    double cNorm = c.norm();

    if (cNorm == 0) {
        return Quaternion(exp(q.r()), 0, 0, 0);
    }

    return exp(q.r()) * (cos(cNorm) + c * sin(cNorm) / cNorm);
}

Quaternion log(Quaternion q) {
    Quaternion c = q.complexComponents();
    return log(q.norm()) + c * acos(q.r() / q.norm()) / c.norm();
}

Quaternion pow(Quaternion q, double exponent) {
    Quaternion c = q.complexComponents();

    // Uses the polar decomposition to compute the power

    // Poler decomposition angle and versor
    double phi = acos(q.r() / q.norm());
    Quaternion cVersor = c.versor();

    return pow(q.norm(), exponent) * (cos(exponent * phi) + cVersor * sin(exponent * phi));
}

Quaternion pow(Quaternion q, int exponent) {
    return pow(q, (double)exponent);
}
