//
//  Shape.h
//  Raytracer
//
//  Created by MUNRO, CHRISTOPHER on 25/03/2017.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//

#pragma once

#include "RayHitData.hpp"
#include <string>

class Ray;
class Colour;

class Shape
{
    public:
        Shape();
        virtual ~Shape();

        virtual bool checkForIntersection(RayHitData &rayHitData);
        virtual Colour getColour() const;
        virtual Vector3D getNormalAtPoint(Vector3D point);
        virtual Vector3D getNormal() const; // For Planes
        virtual std::string getType() const;

        virtual Vector3D getPosition() const;
        virtual float getRadius() const; // For Spheres

    private:
};

