//
//  Shape.cpp
//  Raytracer
//
//  Created by MUNRO, CHRISTOPHER on 25/03/2017.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//
#include "Shape.h"

#include "Ray.h"
#include "Colour.h"

Shape::Shape()
{
    //ctor
}

Shape::~Shape()
{
    //dtor
}

bool Shape::checkForIntersection(RayHitData &rayHitData)
{
    return false;
}

Colour Shape::getColour() const
{
    return Colour();
}

Vector3D Shape::getNormalAtPoint(Vector3D point)
{
    return Vector3D();
}

Vector3D Shape::getNormal() const
{
    return Vector3D();
}

std::string Shape::getType() const
{
    return "";
}

Vector3D Shape::getPosition() const
{
    return Vector3D();
}

float Shape::getRadius() const
{
    return 0.f;
}
