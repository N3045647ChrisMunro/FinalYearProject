//
//  Ray.cpp
//  Raytracer
//
//  Created by MUNRO, CHRISTOPHER on 22/03/2017.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//

#include "Ray.h"
#include <limits>

Ray::Ray()
{
    //ctor
    origin_ = Vector3D(0, 0, 0);
    direction_ = Vector3D(1.0, 0, 0);

    maxDistance_ = std::numeric_limits<float>::infinity();
    minDistance_ = 0.0001f;
}

Ray::Ray(const Vector3D origin, const Vector3D direction) : origin_(origin), direction_(direction)
{
    maxDistance_ = std::numeric_limits<float>::infinity();
    minDistance_ = 0.0001f;
}

Ray::~Ray()
{
    //dtor
}

//Returns a position along the ray
Vector3D Ray::calculatePointOnRay(const float t)
{
    return origin_ + direction_.normalize() * t;
}
