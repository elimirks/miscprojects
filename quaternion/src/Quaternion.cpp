#include "Quaternion.hpp"

Quaternion identity() {
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
    double ns = pow(norm(), 2);
    Quaternion conj = conjugate();

    return Quaternion(conj.real / ns,
                 conj.imI  / ns,
                 conj.imJ  / ns,
                 conj.imK  / ns);
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



Quaternion operator - (Quaternion &q) {
    return Quaternion(-q.r(),
                 -q.i(),
                 -q.j(),
                 -q.k());
}



Quaternion operator + (Quaternion &a, Quaternion &b) {
    return Quaternion(a.r() + b.r(),
                 a.i() + b.i(),
                 a.j() + b.j(),
                 a.k() + b.k());
}

Quaternion operator - (Quaternion &a, Quaternion &b) {
    return Quaternion(a.r() - b.r(),
                 a.i() - b.i(),
                 a.j() - b.j(),
                 a.k() - b.k());
}

Quaternion operator * (Quaternion &a, Quaternion &b) {
    return Quaternion(a.r() * b.r() - a.i() * b.i() - a.j() * b.j() - a.k() * b.k(),
                 a.r() * b.i() + a.i() * b.r() + a.j() * b.k() - a.k() * b.j(),
                 a.r() * b.j() - a.i() * b.k() + a.j() * b.r() + a.k() * b.i(),
                 a.r() * b.k() + a.i() * b.j() - a.j() * b.i() + a.k() * b.r());
}

Quaternion operator / (Quaternion &a, Quaternion &b) {
    Quaternion bInv = b.inverse();
    return a * bInv;
}



Quaternion operator + (double num, Quaternion &obj) {
    return Quaternion(num + obj.r(),
                 obj.i(),
                 obj.j(),
                 obj.k());
}

Quaternion operator - (double num, Quaternion &obj) {
    Quaternion negative = -obj;
    return num + negative;
}

Quaternion operator * (double num, Quaternion &obj) {
    return Quaternion(num * obj.r(),
                 num * obj.i(),
                 num * obj.j(),
                 num * obj.k());
}

Quaternion operator / (double num, Quaternion &obj) {
    Quaternion inv = obj.inverse();
    return num * inv;
}



Quaternion operator + (Quaternion &obj, double num) {
    return num + obj;
}

Quaternion operator - (Quaternion &obj, double num) {
    return Quaternion(obj.r() - num,
                 obj.i(),
                 obj.j(),
                 obj.k());
}

Quaternion operator * (Quaternion &obj, double num) {
    return num * obj;
}

Quaternion operator / (Quaternion &obj, double num) {
    return Quaternion(obj.r() / num,
                 obj.i() / num,
                 obj.j() / num,
                 obj.k() / num);
}
