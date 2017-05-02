//
//  Colour.cpp
//  MainApp
//
//  Created by MUNRO, CHRISTOPHER on 15/02/2017.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//

#include "Colour.h"

Colour::Colour()
{
    //ctor
}

Colour::Colour(float r, float g, float b) : r(r), g(g), b(b)
{
    //ctor
}

Colour::Colour(const Colour& colour) : r(colour.r), g(colour.g), b(colour.b)
{
    //ctor
}

Colour::Colour(float colour) : r(colour), g(colour), b(colour)
{
    //ctor
}

Colour::~Colour()
{
    //dtor
}
/*
Colour normalizeColour(const Colour& colour)
{
    if(colour.r > 1.0 || colour.g > 1.0 || colour.z > 1.0){
        float max = colour.r;
        if(colour.g > max)
            max - colour.g;
        if(colour.b > max)
            max = colour.b;
        if(max != 0.0)
            return colour / max;
    }
    return colour;
}
*/
void Colour::clamp()
{
    if(r > 255.f) r = 255.f;
    if(g > 255.f) g = 255.f;
    if(b > 255.f) b = 255.f;

    if(r < 0.f) r = 0.f;
    if(g < 0.f) g = 0.f;
    if(b < 0.f) b = 0.f;

}

Colour& Colour::operator=(const Colour& other)
{
    r = other.r;
    g = other.g;
    b = other.b;
    return *this;
}

Colour& Colour::operator+=(const Colour& other)
{
    r += other.r;
    g += other.g;
    b += other.g;
    return *this;
}

Colour& Colour::operator*=(const Colour& other)
{
    r *= other.r;
    g *= other.g;
    b *= other.b;
    return *this;
}

Colour& Colour::operator*=(float value)
{
    r *= value;
    g *= value;
    b *= value;
    return *this;
}

Colour& Colour::operator/=(const Colour& other)
{
    r /= other.r;
    g /= other.g;
    b /= other.b;
    return *this;
}

Colour& Colour::operator/=(float value)
{
    r /= value;
    g /= value;
    b /= value;
    return *this;
}

std::ostream& operator<<(std::ostream& out, const Colour& colour)
{
    out << "(" << colour.r << ", " << colour.g << ", " << colour.b << ")" << std::endl;
    return out;
}

Colour operator*(const Colour& col, float value)
{
    return Colour(col.r * value, col.g * value, col.b * value);
}

Colour operator*(float value, const Colour& col)
{
    return Colour(col.r * value, col.g * value, col.b * value);
}

Colour operator*(const Colour& col1, const Colour& col2)
{
    return Colour(col1.r * col2.r, col1.g * col2.g, col1.b * col2.b);
}

Colour operator/(const Colour& col, float f)
{
    return Colour(col.r / f, col.g / f, col.b / f);
}

Colour operator/(const Colour& col1, const Colour& col2)
{
     return Colour(col1.r / col2.r, col1.g / col2.g, col1.b / col2.b);
}

Colour operator+(const Colour& col1, const Colour& col2)
{
    return Colour(col1.r + col2.r, col1.g + col2.g, col1.b + col2.b);
}


