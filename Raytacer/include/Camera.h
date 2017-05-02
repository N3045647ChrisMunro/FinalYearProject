//
//  Camera.h
//  Raytracer
//
//  Created by MUNRO, CHRISTOPHER on 25/03/2017.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//

#pragma once

#include "Vector3D.h"
#include "Ray.h"

class Camera
{
    public:
        Camera();
        Camera(Vector3D position, Vector3D target, Vector3D upVector, const float fieldOfView, const float aspectRation);

        ~Camera();

        Ray generateRay(float u, float v);

        // Getters
        Vector3D getPosition() const { return position_; }
        Vector3D getDirection() const { return direction_; }
        Vector3D getUpVector() const { return upVector_; }
        Vector3D getRightVector() const { return rightVector_; }

        float getWidth() const { return width_; }
        float getHeight() const { return height_; }

        // Setters
        void setPosition(Vector3D pos);
        void setDirection(Vector3D dir);
        void setUpVector(Vector3D upVec);
        void setRightVector(Vector3D rightVec);
        void setWidth(float width);
        void setHeight(float height);


    private:

        Vector3D position_;
        Vector3D direction_;
        Vector3D upVector_;
        Vector3D rightVector_;

        float height_;
        float width_;
};
