#include "App.h"

#include "tinyXML/tinyxml.h"
#include "Colour.h"
#include "Image.h"

#include <thread>

App::App()
{
    //ctor
}

App::~App()
{
    //dtor
    //delete network_;
}

bool App::init()
{
    try{
        //network_ = new TCPNetwork();
        //tcpMessenger_ = new TCPMessenger();

        //network_->initSocket();
        //network_->establishConnection("127.0.0.1");

        return true;
    }
    catch(std::exception &e){
        std::cout << "Setup Failed: " << e.what() << std::endl;
        return false;
    }

}

void App::runApp()
{
    bool exit = false;
    char input;

    while(exit == false){

        std::cout << "Press: '1' To Render Scene" << std::endl;
        std::cout << "Press: '0' To Exit" << std::endl;

        std::cin >> input;

        switch(input){
            case '1':
                excuteRender("Scene.xml");
            break;
            case '0':
                exit = true;
            break;
        }

    }

    std::cout << "Exit" << std::endl;
}

void App::excuteRender(std::string filepath)
{
    bool done = false;

    char sceneGrade;
    int width = 0;
    int height = 0;
    Colour bgCol = Colour();

    extractSceneSettings(filepath, &sceneGrade, &width, &height, &bgCol);
    Image *image = new Image(width, height, bgCol);

    //std::thread recvThread(&App::recvMessage, this);

    //network_->sendMessage("Render:" + filepath + ":" + sceneGrade);

    std::cout << "Rendering...." << std::endl;

    while(done == false){




        if(image->checkIsFilled()){
            done = true;
        }
    }

    //recvThread.join();

    std::cout << "Rendering Complete" << std::endl;
}

// Opens the scene.xml file and extract the settings data
bool App::extractSceneSettings(std::string filepath, char *grade, int *width, int *height, Colour *bgColour)
{
    TiXmlDocument xmlDoc(filepath.c_str());
    xmlDoc.LoadFile();

    TiXmlElement *rootNode = xmlDoc.RootElement();
    TiXmlElement *nodeData;

    if(rootNode != nullptr){

        TiXmlElement *settingsNode = rootNode->FirstChildElement("Settings");
        if(settingsNode != nullptr){
            //****Get the Scene Grade Rating
            nodeData = settingsNode->FirstChildElement("Grade");

            if(nodeData != nullptr){
                int a = 0;
                nodeData->QueryIntAttribute("grade", &a);
                grade = (char*)a;
            }
            else{
                std::cout << "Failed to extract data, Please make sure this is a SceneFile" << std::endl;
                return false;
            }

            //****Get The Image Data
            TiXmlElement *imageNode = settingsNode->FirstChildElement("Image");

            if(imageNode != nullptr){

                //****Get the Dimensions
                nodeData = imageNode->FirstChildElement("Dimensions");

                if(nodeData != nullptr){
                    nodeData->QueryIntAttribute("Width", width);
                    nodeData->QueryIntAttribute("Height", height);
                }

                //****Get the Background Colour
                nodeData = imageNode->FirstChildElement("Colour");

                if(nodeData != nullptr){
                    int r = 0, g = 0, b = 0;

                    nodeData->QueryIntAttribute("r", &r);
                    nodeData->QueryIntAttribute("g", &g);
                    nodeData->QueryIntAttribute("b", &b);

                    bgColour->r = r;
                    bgColour->g = g;
                    bgColour->b = b;

                }

            }


        }else{
            std::cout << "Failed to extract data, Please make sure this is a SceneFile" << std::endl;
            return false;
        }
    }

    return true;
}

void App::recvMessage()
{
    while(true){

        //std::string str = network_->receiveMessage();

        //tcpMessenger_->addMsgToRecvQueue(str);

    }

}
