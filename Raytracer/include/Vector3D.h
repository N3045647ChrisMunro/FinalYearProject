//
//  Vector3D.h
//  Raytracer
//
//  Created by MUNRO, CHRISTOPHER on 15/02/2017.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//

#pragma once

#include <iostream>
#include <math.h>
//class ostream;

class Vector3D
{
public:
    Vector3D();
    Vector3D(float x, float y, float z);
    Vector3D(const Vector3D& vector);
    virtual ~Vector3D();

    float x, y, z;

    void extractVerts(float *px, float *py, int which) const;

    float magnitude(const Vector3D& vector);
    float magnitude();
    Vector3D normalize(const Vector3D& vector);
    Vector3D normalize();

    // Unary Operators
    Vector3D operator+ () const;
    Vector3D operator- () const;

    // Assignemt Operators
    Vector3D operator+= (const Vector3D& vector);
    Vector3D operator-= (const Vector3D& vector);
    Vector3D operator*= (const Vector3D& vector);
    Vector3D operator*= (const float value);
    Vector3D operator/= (const float value);

    // Binary Operators
    friend Vector3D operator+ (const Vector3D& A, const Vector3D& B);
    friend Vector3D operator+ (const float A, const Vector3D& B);
    friend Vector3D operator- (const Vector3D& A, const Vector3D& B);
    friend Vector3D operator* (const Vector3D& A, const Vector3D& B);
    friend Vector3D operator* (const float A, const Vector3D& B);
    friend Vector3D operator* (const Vector3D& A, const float B);
    friend Vector3D operator/ (const Vector3D& A, const Vector3D& B);
    friend Vector3D operator/ (const Vector3D& A, const float B);
    friend std::ostream& operator<< (std::ostream& stream, const Vector3D& vector);

    float dotProduct (const Vector3D& A, const Vector3D& B);
    Vector3D crossProduct (const Vector3D& A, const Vector3D& B);

    void minimize(Vector3D& min, const Vector3D& candidate);
    void maximize(Vector3D& max, const Vector3D& candidate);
};

inline float dotProduct(const Vector3D& A, const Vector3D& B)
{
    return A.x * B.x + A.y * B.y + A.z * B.z;
}

inline Vector3D crossProduct(const Vector3D& A, const Vector3D& B)
{
        return Vector3D(A.y * B.z - A.z * B.y,
                        A.z * B.x - A.x * B.z,
                        A.x * B.y - A.y * B.x);
}

inline float distance(const Vector3D& A, const Vector3D& B)
{
    return sqrt( ((B.x - A.x) * (B.x - A.x)) + ((B.y - A.y) * (B.y - A.y)) + ((B.z - A.z) * (B.z - A.z)) );
}


