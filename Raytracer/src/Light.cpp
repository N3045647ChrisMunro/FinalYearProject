#include "Light.h"

Light::Light()
{
    //ctor
    //Default to the centre of the 'World'
    position_ = Vector3D(0.f, 0.f, 0.f);

    //Default to white
    colour_ = Colour(255.f, 255.f, 255.f);
    intensity_ = 1.0f;
}

Light::Light(Vector3D position, Colour lightColour, float intensity) : position_(position), colour_(lightColour), intensity_(intensity)
{

}

Light::~Light()
{
    //dtor
}


