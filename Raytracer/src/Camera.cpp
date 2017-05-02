#include "Camera.h"

#include <cmath>

Camera::Camera()
{
    //ctor
    position_ = Vector3D(0.f, 0.f, 0.f);
    direction_ = Vector3D(0.f, 0.f, 1.f);
    upVector_ = Vector3D(0.f, 0.f, 0.f);
    rightVector_ = Vector3D(0.f, 0.f, 0.f);
}

Camera::Camera(Vector3D position, Vector3D target, Vector3D upVector, const float fieldOfView, const float aspectRatio) :
               position_(position)
{

    direction_ = target - position;
    direction_.normalize();

    rightVector_ = crossProduct(direction_, upVector.normalize());
    upVector_ = crossProduct(direction_, rightVector_.normalize());

    height_ = tan(fieldOfView);
    width_ = height_ * aspectRatio;
}

Camera::~Camera()
{
    //dtor
}

Ray Camera::generateRay(float u, float v)
{
    Vector3D rayDirection = direction_ + u * width_ * rightVector_ + v * height_ * upVector_;
    return Ray(position_, rayDirection.normalize());
}

void Camera::setPosition(Vector3D pos)
{
    position_ = pos;
}

void Camera::setDirection(Vector3D dir)
{
    direction_ = dir;
}

void Camera::setUpVector(Vector3D upVec)
{
    upVector_ = upVec;
}

void Camera::setRightVector(Vector3D rightVec)
{
    rightVector_ = rightVec;
}

void Camera::setWidth(float width)
{
    width_ = width;
}

void Camera::setHeight(float height)
{
    height_ = height;
}
