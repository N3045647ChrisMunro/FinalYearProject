#include "Raytracer.h"

#include <iostream>

#include "Image.h"
#include "Ray.h"
#include "Camera.h"
#include "Scene.h"
#include "Shape.h"
#include "Light.h"
#include "Colour.h"
#include "Sphere.h"
#include "Plane.h"
#include <limits>

Raytracer::Raytracer()
{
    //ctor
}

Raytracer::~Raytracer()
{
    //dtor
}

bool Raytracer::renderScene(Scene *scene, Image *image, std::string filepath, const int startIDX_X,
                            const int startIDX_Y, const int endIDX_X, const int endIDX_Y)
{
    try{
        std::cout << "Rendering..." << std::endl;

        int antiAliasedSampling = scene->getAASampling();

        int x = startIDX_X;
        int y = startIDX_Y;

        bool isShadow = false;

        for(y = startIDX_Y; y < endIDX_Y; y++){
            for(x = startIDX_X; x < endIDX_X; x++){

                const float u = (2.0f * x) / image->getWidth() - 1.0f;
                const float v = (-2.0f * y) / image->getHeight() + 1.0f;

                Ray ray;
                Camera *mainCamera = scene->getCamera();
                ray = mainCamera->generateRay(u, v);

                RayHitData rayHitData;
                rayHitData.ray = ray;

                Vector3D intersectionPoint;
                Vector3D intersectNormal;

                Vector3D lightDirection;

                std::vector<Shape*> objects = scene->getAllObjects();
                std::vector<Light*> lights = scene->getAllLights();

                Colour col = Colour(0, 0, 0);

                if(objects.size() > 0){

                    for(unsigned int i = 0; i < objects.size(); i++){

                        if(objects[i]->checkForIntersection(rayHitData)){

                            intersectionPoint = rayHitData.intersectionPoint;
                            intersectNormal = objects[i]->getNormalAtPoint(intersectionPoint);

                            if(lights.size() > 0){

                                for(unsigned int k = 0; k < lights.size(); k++){

                                    lightDirection = lights[k]->getPosition() - intersectionPoint;
                                    Vector3D shadowRayDirection = intersectionPoint - lights[k]->getPosition();

                                    Ray shadowRay(intersectionPoint, shadowRayDirection.normalize());
                                    RayHitData shadowRayHitData;
                                    shadowRayHitData.ray = shadowRay;

                                    for(unsigned int j = 0; j < objects.size(); j++){

                                        if(objects[j]->checkForIntersection(shadowRayHitData)){
                                            //isShadow = true;
                                            //std::cout << "T" << std::endl;
                                        }
                                        else{
                                            isShadow = false;
                                        }
                                        break;
                                    }

                                    if(!isShadow){
                                        col += ((lights[k]->getColour() * lights[k]->getIntensity() ) * dotProduct(intersectNormal.normalize(), lightDirection.normalize()) * (objects[i]->getColour()*1.0f));
                                    }
                                    else{
                                        col += Colour(255,255,255);
                                    }
                                }

                        }
                        col = col / 255;
                        col.clamp();
                        image->set(x, y, col);
                    }
                }

            }

        }
    }

    image->saveImage(filepath.c_str());

    std::cout << "Done." << std::endl;
    }
    catch(std::exception& e){
        std::cerr << "Rendering Failed: Exception Occurred: " << e.what() << std::endl;
    }

}
