//
//  Scene.cpp
//  Raytracer
//
//  Created by MUNRO, CHRISTOPHER.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//

#include "Scene.h"

#include "Camera.h"
#include "Shape.h"
#include "Light.h"

Scene::Scene()
{
    //ctor
}

Scene::~Scene()
{
    //dtor
    //Free up the memory, consumed by the scene components
    delete sceneCamera_;

    for(unsigned int i = 0; i < objects_.size(); i++){
        delete objects_[i];
    }

    for(unsigned int i = 0; i < lights_.size(); i++){
        delete lights_[i];
    }

}

void Scene::setSceneCamera(Camera *camera)
{
    sceneCamera_ = camera;
}

void Scene::addObject(Shape *shape)
{
    objects_.push_back(shape);
}

void Scene::addObjects(std::vector<Shape *> objects)
{
    objects_ = objects;
}
void Scene::addLight(Light *light)
{
    lights_.push_back(light);
}

void Scene::addLights(std::vector<Light *> lights)
{
    lights_ = lights;
}

void Scene::setAASampling(const int value)
{
    aa_ = value;
}

char Scene::genSceneRating()
{
    char LGrade;
    float grade = 0;

    grade += ((float)aa_ / 10) * 100;

    for(unsigned int i = 0; i < objects_.size(); i++){

        if(objects_[i]->getType() == "Plane"){
            grade += 1.5f;
        }
        else if(objects_[i]->getType() == "Sphere"){
            grade += 1.0f;
        }

    }

    for(unsigned int i = 0; i < lights_.size(); i++){
        grade += 2.0f;
    }

    //Determine letter grade
    if(grade > 85.0f){
        return LGrade = 'A';
    }
    else if(grade > 70.0f && grade < 85.f){
        return LGrade = 'B';
    }
    else if(grade > 60.0f && grade < 70.f){
        return LGrade = 'C';
    }
    else if(grade > 50.0f && grade < 60.f){
        return LGrade = 'D';
    }
    else if(grade > 40.0f && grade < 50.f){
        return LGrade = 'E';
    }
    else if(grade < 40.f){
        return LGrade = 'F';
    }

    return LGrade;
}
