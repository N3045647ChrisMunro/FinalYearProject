//
//  Ray.h
//  Raytracer
//
//  Created by MUNRO, CHRISTOPHER on 22/03/2017.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//

#pragma once

#include "Vector3D.h"

class Ray
{
    public:
        Ray();
        Ray(const Vector3D origin, const Vector3D direction);
        ~Ray();

        //Getters
        Vector3D getRayOrigin() const { return origin_; }
        Vector3D getRayDirection() const { return direction_; }

        float getMaxDistance() const { return maxDistance_; }
        float getMinDistance() const { return minDistance_; }

        Vector3D calculatePointOnRay(const float t);

    private:

        Vector3D origin_;
        Vector3D direction_;

        float maxDistance_;
        float minDistance_;

};

