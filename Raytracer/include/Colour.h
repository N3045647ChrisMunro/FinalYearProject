//
//  Colour.h
//  Raytracer
//
//  Created by MUNRO, CHRISTOPHER on 15/02/2017.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//

#pragma once
#include <iostream>

class Colour
{
    public:
        Colour();
        Colour(float r, float g, float b);
        Colour(const Colour& colour);
        Colour(const float colour);
        virtual ~Colour();

        float r, g, b;

        friend Colour normalizeColour(const Colour& colour);

        // Operators without clamping
        Colour& operator=(const Colour& other);
        Colour& operator+=(const Colour& other);
        Colour& operator*=(const Colour& other);
        Colour& operator*=(float value);
        Colour& operator/=(const Colour& other);
        Colour& operator/=(float value);

        Colour operator+() const { return *this; }
        Colour operator-() const { return Colour(-this->r, -this->g, -this->b); }

        void clamp();

        friend std::ostream& operator<<(std::ostream& out, const Colour& colour);
        friend Colour operator*(const Colour& col, float f);
        friend Colour operator*(float f, const Colour& col);
        friend Colour operator*(const Colour& col1, const Colour& col2);

        friend Colour operator/(const Colour& col, float f);
        friend Colour operator/(const Colour& col1, const Colour& col2);

        friend Colour operator+(const Colour& col1, const Colour& col2);

};
