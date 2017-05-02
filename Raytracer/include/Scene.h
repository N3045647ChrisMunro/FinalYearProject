//
//  Scene.h
//  Raytracer
//
//  Created by MUNRO, CHRISTOPHER on 25/03/2017.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//

#pragma once

#include <vector>

class Camera;
class Shape;
class Light;

class Scene
{
    public:
        Scene();
        ~Scene();

        void setSceneCamera(Camera *camera);
        void addObject(Shape *object);
        void addObjects(std::vector<Shape *> objects);
        void addLight(Light* light);
        void addLights(std::vector<Light *> lights);
        void setAASampling(const int value);

        char genSceneRating();

        Camera *getCamera() const { return sceneCamera_; }
        std::vector<Shape *> getAllObjects() const { return objects_; }
        std::vector<Light *> getAllLights() const { return lights_; }
        int getAASampling() const { return aa_; }

    private:

        Camera *sceneCamera_ = nullptr;

        std::vector<Shape*> objects_;
        std::vector<Light*> lights_;
        int aa_;

};

