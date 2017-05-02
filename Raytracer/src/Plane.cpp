#include "Plane.h"
#include "Ray.h"

Plane::Plane()
{
    //ctor
    type_ = "Plane";
}

Plane::Plane(const Vector3D& position, const Vector3D& normal, const Colour& colour) : position_(position), normal_(normal), colour_(colour)
{
    type_ = "Plane";
}

Plane::~Plane()
{
    //dtor
}

bool Plane::checkForIntersection(RayHitData &rayHitData)
{
    Vector3D temp;
    Ray ray = rayHitData.ray;
    float dn = dotProduct(ray.getRayDirection(), normal_);

    if(dn == 0.0f)
        return false;

    // fin the point of intersection with the ray, and check to see if it is with range
    float t = dotProduct(position_ - ray.getRayOrigin(), normal_) / dn;

    if( t <= ray.getMinDistance() || t >= rayHitData.t)
        return false;

    //Ray is within the relevant range, so return all the required data
    rayHitData.shape = this;
    rayHitData.colour = colour_;
    rayHitData.intersectionPoint = ray.calculatePointOnRay(t);
    rayHitData.t = t;

    return true;
}

Vector3D Plane::getNormalAtPoint(Vector3D point)
{
    return normal_;
}
