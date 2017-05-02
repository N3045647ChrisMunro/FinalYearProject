#include <iostream>

void excuteRender();

int main()
{
    bool exit = false;
    char input;

    while(exit == false){

        std::cout << "Press: '1' To Render Scene" << std::endl;
        std::cout << "Press: '0' To Exit" << std::endl;
        std::cin >> input;

        switch(input){
            case '1':
                excuteRender();
            break;
            case '0':
                exit = true;
            break;
        }

    }

    std::cout << "Exit" << std::endl;
    return 0;
}

void excuteRender()
{
    bool done = false;

    while(done == false){

        std::cout << "Rendering...." << std::endl;

        done = true;
    }

    std::cout << "Rendering Complete" << std::endl;
}
