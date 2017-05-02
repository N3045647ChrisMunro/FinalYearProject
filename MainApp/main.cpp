#include <iostream>

#include "App.h"

int main()
{
    App *app = new App();
    if(app->init()){
        app->runApp();
    }

    delete app;
    return 0;
}
