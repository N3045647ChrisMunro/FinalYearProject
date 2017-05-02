#pragma once

#include <iostream>

class TCPNetwork
{
public:
    TCPNetwork();
    ~TCPNetwork();

    void initSocket();
    void sendMessage(std::string msg);
    std::string receiveMessage();

    void establishConnection(std::string hostname);
    void cleanUp();


private:

    int sockfd_ {NULL};

    int iResult_{NULL};
};
