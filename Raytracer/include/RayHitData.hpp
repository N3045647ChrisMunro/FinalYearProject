#pragma once

#include "Ray.h"
#include "Colour.h"
#include "Shape.h"
#include "Vector3D.h"

class Shape;

struct RayHitData
{
    Ray ray;
    Shape *shape;
    Colour colour;
    Vector3D intersectionPoint;

    float t;

    RayHitData();
};

inline RayHitData::RayHitData()
{
    ray = Ray();
    shape = nullptr;
    colour = Colour();
    intersectionPoint = Vector3D();

    t = ray.getMaxDistance();
}
