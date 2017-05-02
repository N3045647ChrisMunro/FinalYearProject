#include "TCPNetwork.h"
/*
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <string.h>
#include <iostream>


#define PORT 8001
#define BUFF_SIZE 1024

TCPNetwork::TCPNetwork()
{
    //ctor
}

TCPNetwork::~TCPNetwork()
{
    //dtor
}

void TCPNetwork::initSocket()
{
    sockfd_ = socket(AF_INET, SOCK_STREAM, 0);

    if(sockfd_ < 0){
            std::cerr << "Socket Init Error" << std::endl;
    }
}

void TCPNetwork::establishConnection(std::string hostname)
{
    struct sockaddr_in address;
	struct addrinfo hints, *res0;

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;    /* Allow IPv4 or IPv6 */
    //hints.ai_socktype = SOCK_STREAM; /* Datagram socket */
    //hints.ai_flags = 0;
    //hints.ai_protocol = 0;          /* Any protocol */
/*
	std::string port = std::to_string(PORT);

	bool error = getaddrinfo(hostname.c_str(), port.c_str(), &hints, &res0);
	if (error) {
       std::cerr << "Couldnt get addrinfo: " << gai_strerror(error) << std::endl;
       /*NOTREACHED*/
/*    }

	if(connect(sockfd_, res0->ai_addr, res0->ai_addrlen) < 0){
		std::cerr << "Socket Connect Error" << std::endl;
    }
    else{
        std::cout << "Connected To Master Node" << std::endl;
	}
}

void TCPNetwork::sendMessage(std::string msg)
{
    char buffer[BUFF_SIZE];
    int n;
    memset(buffer, 0, BUFF_SIZE);
    memcpy(buffer, msg.c_str(), msg.length());

	n = write(sockfd_, &buffer, msg.length());
	std::cout << "message sent: " << msg << std::endl;

	if(n < 0)
		std::cerr << "Send MSG Error!" << std::endl;
}

std::string TCPNetwork::receiveMessage()
{
    char buffer[BUFF_SIZE];
    memset(buffer, 0, BUFF_SIZE);

	int n = read(sockfd_, &buffer, BUFF_SIZE);

	if(n < 0)
		std::cerr << "Receive MSG Error!" << std::endl;

    return std::string(buffer);
}

void TCPNetwork::cleanUp()
{
     close(sockfd_);
}

*/
