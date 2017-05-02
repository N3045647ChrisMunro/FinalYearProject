
#pragma once

#include "Shape.h"
#include "Vector3D.h"
#include "Colour.h"
#include "RayHitData.hpp"

#include <string>

class Ray;

class Plane : public Shape
{
    public:
        Plane();
        Plane(const Vector3D& position, const Vector3D& normal, const Colour& colour);
        ~Plane();

        virtual bool checkForIntersection(RayHitData &rayHitData);
        virtual Colour getColour() const { return colour_; }
        virtual Vector3D getNormalAtPoint(Vector3D point);
        virtual Vector3D getNormal() const { return normal_; }
        virtual std::string getType() const { return type_; }
        virtual Vector3D getPosition() const { return position_; }

    private:

        Vector3D position_;
        Vector3D normal_;
        Colour colour_;

        std::string type_;

};

