#pragma once

#include <iostream>

#include "TCPNetwork.h"
#include "TCPMessenger.h"

class Colour;

class App
{
    public:
        App();
        ~App();

        bool init();
        void runApp();
        void excuteRender(std::string filepath);


    private:

        bool extractSceneSettings(std::string filepath, char *grade, int *width, int *height, Colour *bgColour);

        void recvMessage();

        //TCPMessenger *tcpMessenger_{ nullptr };

        //TCPNetwork *network_{ nullptr };

};

