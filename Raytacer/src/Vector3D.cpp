//
//  Vector3D.cpp
//  Raytracer
//
//  Created by MUNRO, CHRISTOPHER on 15/02/2017.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//


#include "Vector3D.h"
#include <math.h>
#include <iostream>
#include <stdio.h>

//
// Constructors
//

Vector3D::Vector3D()
{
    //ctor
}

Vector3D::Vector3D(float x, float y, float z) : x(x), y(y), z(z)
{
    //ctor
}

Vector3D::Vector3D(const Vector3D& vector) : x(vector.x), y(vector.y), z(vector.z)
{
    //ctor
}

Vector3D::~Vector3D()
{
    //dtor
}

void Vector3D::extractVerts(float *px, float *py, int which) const
{
    switch(which){

    case 0:
        *px = y;
        *py = z;
        break;
    case 1:
        *px = x;
        *py = z;
        break;
    case 2:
        *px = x;
        *py = y;
        break;

    }
}

float Vector3D::magnitude(const Vector3D& vector)
{
    return sqrt(vector.x * vector.x + vector.y * vector.y + vector.z * vector.z);
}

float Vector3D::magnitude()
{
    return sqrt(x * x + y * y + z * z);
}

Vector3D Vector3D::normalize(const Vector3D& vector)
{
    return vector / magnitude(vector);
}

Vector3D Vector3D::normalize()
{
    Vector3D temp = Vector3D(x, y, z);
    return temp / magnitude(temp);
}

//
// Unary Operators
//

Vector3D Vector3D::operator+() const
{
    return *this;
}

Vector3D Vector3D::operator-() const
{
    return Vector3D(-x, -y, -z);
}

//
// Assignment Operators
//

Vector3D Vector3D::operator+=(const Vector3D& vector)
{
    x += vector.x;
    y += vector.y;
    z += vector.z;
    return *this;
}

Vector3D Vector3D::operator-=(const Vector3D& vector)
{
    x -= vector.x;
    y -= vector.y;
    z -= vector.z;
    return *this;
}

Vector3D Vector3D::operator*=(const Vector3D& vector)
{
    x *= vector.x;
    y *= vector.y;
    z *= vector.z;
    return *this;
}

Vector3D Vector3D::operator*=(const float value)
{
    x *= value;
    y *= value;
    z *= value;
    return *this;
}

Vector3D Vector3D::operator/=(const float value)
{
    x /= value;
    y /= value;
    z /= value;
    return *this;
}

//
// Binary Operators
//

Vector3D operator+ (const Vector3D& A, const Vector3D& B)
{
    return Vector3D(A.x + B.x, A.y + B.y, A.z + B.z);
}

Vector3D operator+ (float& A, const Vector3D& B)
{
    return Vector3D(A + B.x, A + B.y, A + B.z);
}

Vector3D operator- (const Vector3D& A, const Vector3D& B)
{
    return Vector3D(A.x - B.x, A.y - B.y, A.z - B.z);
}

Vector3D operator* (const Vector3D& A, const Vector3D& B)
{
    return Vector3D(A.x * B.x, A.y * B.y, A.z * B.z);
}

Vector3D operator* (const float A, const Vector3D& B)
{
    return Vector3D(B.x * A, B.y * A, B.z * A);
}

Vector3D operator* (const Vector3D& A, const float B)
{
    return Vector3D(A.x * B, A.y * B, A.z * B);
}

Vector3D operator/ (const Vector3D& A, const Vector3D& B)
{
    return Vector3D(A.x / B.x, A.y / B.y, A.z / B.z);
}

Vector3D operator/ (const Vector3D& A, const float B)
{
    return Vector3D(A.x / B, A.y / B, A.z / B);
}

std::ostream& operator<< (std::ostream& stream, const Vector3D& vector)
{
    stream << "(" << vector.x << ", " << vector.y << ", " << vector.z << ")" << std::endl;
    return stream;
}
/*
float Vector3D::dotProduct(const Vector3D& A, const Vector3D& B)
{
    return A.x * B.x + A.y * B.y + A.z * B.z;
}
*/
Vector3D Vector3D::crossProduct(const Vector3D& A, const Vector3D& B)
{
    return Vector3D(A.y * B.z - A.z * B.y,
                    A.z * B.x - A.x * B.z,
                    A.x * B.y - A.y * B.x);
}

void Vector3D::minimize(Vector3D& min, const Vector3D& candidate)
{
    if(candidate.x < min.x)
        min.x = candidate.x;
    if(candidate.y < min.y)
        min.y = candidate.y;
    if(candidate.z < min.z)
        min.z = candidate.z;
}

void Vector3D::maximize(Vector3D& max, const Vector3D& candidate)
{
    if(candidate.x > max.x)
        max.x = candidate.x;
    if(candidate.y > max.y)
        max.y = candidate.y;
    if(candidate.z > max.z)
        max.z = candidate.z;
}













