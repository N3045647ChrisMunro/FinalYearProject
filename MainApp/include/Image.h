//
//  Image.h
//  MainApp
//
//  Created by MUNRO, CHRISTOPHER on 17/03/2017.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//

#pragma once

#include <cmath>
#include <string>
#include <fstream>

#include "Colour.h"

class Image
{
    public:

        Image();
        Image(int width, int height);
        Image(int width, int height, Colour backgroundColour);

        virtual ~Image();

        // return false if x or y are out of bounds
        bool set(int x, int y, const Colour& colour);

        void gammaCorrect(float gamma);

        void saveImage(const char* filename);

        // Getters
        int getWidth() const { return width_ ;}
        int getHeight() const { return height_; }
        Colour getBGColour() const { return bgCol_; }

    private:
        Colour** raster_;
        Colour bgCol_;
        int width_;
        int height_;
};
