#include "Image.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <cmath>
#include <limits>

#include <stdlib.h>
#include <stdio.h>


Image::Image()
{
    //ctor
}

Image::Image(int width, int height)
{
    width_ = width;
    height_ = height;
    bgCol_ = Colour(0, 0, 0);

    //Allocate memory for the raster
    raster_ = new Colour*[width_];
    for(int i = 0; i < width_; i++){
        raster_[i] = new Colour[height_];
    }
}

Image::Image(int width, int height, Colour backgroundColour)
{
    width_ = width;
    height_ = height;
    bgCol_ = backgroundColour;

    //Allocate memory for the raster
    raster_ = new Colour*[width_];
    for(int i = 0; i < width_; i++){
        raster_[i] = new Colour[height_];

        //Assign the bg colour
        for(int j = 0; j < height_; j++){
            raster_[i][j] = backgroundColour;
        }

    }
}

Image::~Image()
{
    delete raster_;
}

bool Image::set(int x, int y, const Colour& colour)
{
    // Check for out of bounds
    if(0 > x || 0 > width_)
        return false;
    if(0 > y || 0 > height_)
        return false;

    raster_[x][y] = colour;
}

void Image::gammaCorrect(float gamma)
{
    Colour temp;
    float power = 1.f / gamma;
    for(int i = 0; i < width_; i++){
        for(int j = 0; j < height_; j++){
            temp = raster_[i][j];
            raster_[i][j] = Colour(pow(temp.r, power), pow(temp.g, power), pow(temp.b, power));
        }
    }
}

void Image::saveImage(const char* filename)
{
    FILE *file;
    int totalPixelCount = width_ * height_;
    int s = 4 * totalPixelCount;
    int fileSize = 54 + s;

    int dpi = 72;

    float factor = 39.375;
    int m = static_cast<int>(factor);

    int ppm = dpi * m;

    unsigned char bmpFileHeader[14] = {'B','M', 0,0,0,0, 0,0,0,0, 54,0,0,0};
    unsigned char bmpInfoHeader[40] = {40,0,0,0, 0,0,0,0, 0,0,0,0, 1,0,24,0};

    bmpFileHeader[2] = (unsigned char)(fileSize);
    bmpFileHeader[3] = (unsigned char)(fileSize>>8);
    bmpFileHeader[4] = (unsigned char)(fileSize>>16);
    bmpFileHeader[5] = (unsigned char)(fileSize>>24);

    bmpInfoHeader[4] = (unsigned char)(width_);
    bmpInfoHeader[5] = (unsigned char)(width_>>8);
    bmpInfoHeader[6] = (unsigned char)(width_>>16);
    bmpInfoHeader[7] = (unsigned char)(width_>>24);

    bmpInfoHeader[8] = (unsigned char)(height_);
    bmpInfoHeader[9] = (unsigned char)(height_>>8);
    bmpInfoHeader[10] = (unsigned char)(height_>>16);
    bmpInfoHeader[11] = (unsigned char)(height_>>24);

    bmpInfoHeader[21] = (unsigned char)(s);
    bmpInfoHeader[22] = (unsigned char)(s>>8);
    bmpInfoHeader[23] = (unsigned char)(s>>16);
    bmpInfoHeader[24] = (unsigned char)(s>>24);

    bmpInfoHeader[25] = (unsigned char)(ppm);
    bmpInfoHeader[26] = (unsigned char)(ppm>>8);
    bmpInfoHeader[27] = (unsigned char)(ppm>>16);
    bmpInfoHeader[28] = (unsigned char)(ppm>>24);

    bmpInfoHeader[29] = (unsigned char)(ppm);
    bmpInfoHeader[30] = (unsigned char)(ppm>>8);
    bmpInfoHeader[31] = (unsigned char)(ppm>>16);
    bmpInfoHeader[32] = (unsigned char)(ppm>>24);

    file = fopen(filename, "wb");

    fwrite(bmpFileHeader, 1, 14, file);
    fwrite(bmpInfoHeader, 1, 40, file);

    // output clamped [0, 255] value
    // 0, 0 index is bottom left
    for(int i = 0; i < height_; i++){
        for(int j = 0; j < width_; j++){

            double red = raster_[j][i].r;
            double green = raster_[j][i].g;
            double blue = raster_[j][i].b;

            unsigned char colourData[3] = {(int)floor(blue), (int)floor(green), (int)floor(red)};

            fwrite(colourData, 1, 3, file);

        }
    }

    fclose(file);

}
