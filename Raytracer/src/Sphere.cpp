//
//  Sphere.h
//  Raytracer
//
//  Created by MUNRO, CHRISTOPHER on 25/03/2017.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//

#include "Sphere.h"

#include <iostream>
#include "Ray.h"
#include "math.h"
Sphere::Sphere()
{
    //ctor
    type_ = "Sphere";
}

Sphere::Sphere(Vector3D centerPosition, float radius, Colour colour) : position_(centerPosition), radius_(radius), colour_(colour)
{
    type_ = "Sphere";
}

Sphere::~Sphere()
{
    //dtor
}

bool Sphere::checkForIntersection(RayHitData &rayHitData)
{
    Ray ray = rayHitData.ray;
    const Vector3D rayOrigin = ray.getRayOrigin();
    const Vector3D rayDirection = ray.getRayDirection();

    Vector3D distanceToSphere = rayOrigin - position_;

    float A = dotProduct(rayDirection, rayDirection);
    float B = dotProduct(rayDirection, distanceToSphere) * 2;
    double C = dotProduct(distanceToSphere, distanceToSphere) - radius_ * radius_;

    //std::cout << "A: " << A << " B: " << B << " C: "<< C << std::endl;

    float discriminant = B * B - 4 * A * C;

    if(discriminant > 0){

        float t1 = (-B - sqrt(discriminant)) / (2 * A);
        float t2 = (-B + sqrt(discriminant)) / (2 * A);

        if(t1 > ray.getMinDistance() && t1 < rayHitData.t){
            rayHitData.intersectionPoint = ray.calculatePointOnRay(t1);
            rayHitData.t = t1;
        }
        else if(t2 > ray.getMinDistance() && t2 < rayHitData.t){
            rayHitData.t = t2;
            rayHitData.intersectionPoint = ray.calculatePointOnRay(t2);
        }
        else
            return false;

        rayHitData.shape = this;
        rayHitData.colour = colour_;


        return true;
    }

    return false;
}

Colour Sphere::getColour() const
{
    return colour_;
}

Vector3D Sphere::getNormalAtPoint(Vector3D point)
{
    const Vector3D normal = point - position_;
    return normal;
}


