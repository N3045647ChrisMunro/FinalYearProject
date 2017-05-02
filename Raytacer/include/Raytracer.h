#pragma once

#include <string>

class Scene;
class Image;


class Raytracer
{
    public:
        Raytracer();
        ~Raytracer();

        bool renderScene(Scene *scene, Image *image, std::string filepath, const int startIDX_X,
                         const int startIDX_Y, const int endIDX_X, const int endIDX_Y);



    private:

};

